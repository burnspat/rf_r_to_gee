# Allow R to handle larger integers
library(bit64)

# Prep a random forest model for conversion
# Adds relevant metadata to model object
# 
#' @param init_mod A random forest model fitted in R using either ranger or randomForest
#' @param model_type Name of function used to fit model - 'ranger' or 'randomForest'
#' @param response_type Random forest type - 'classification', 'regression' or 'probability'
#' @param caret Caret used to fit model?
#' @param tidymodels Tidymodels used to fit model?
prep.mod = function(init_mod, model_type, response_type, caret = FALSE, tidymodels = FALSE){
  
  # Get base model from caret/tidymodels
  if(caret) init_mod = init_mod$finalModel
  if(tidymodels) init_mod = init_mod$fit
  
  # Get model specific parameters
  if(model_type == 'ranger'){
    n_tree_var = 'num.trees'
    response_var = init_mod$dependent.variable.name
    if(response_type == 'classification'){
      pred_var = 'prediction'
    }else if(response_type == 'probability'){
      pred_var = paste0('pred.', levels(data[, response_var])[1])
    }else if(response_type == 'regression'){
      pred_var = 'prediction'
    }else{
      stop('Response type not recognized, choose "classification" or "probability" or "regression"')
    }
    init_mod$model_type = model_type
    init_mod$response_type = response_type
    init_mod$pred_var = pred_var
    init_mod$n_tree_var = n_tree_var
  }else if(model_type == 'randomForest'){
    n_tree_var = 'ntree'
    response_var = names(attributes(init_mod$terms)$dataClasses)[1]
    if(response_type == 'classification'){
      pred_var = 'prediction'
    }else if(response_type == 'probability'){
      stop('Probability models not available for randomForest, either change model type to "ranger", or change response type')
    }else if(response_type == 'regression'){
      pred_var = 'prediction'
    }else{
      stop('Response type not recognized, choose "classification" or "probability" or "regression"')
    }
    init_mod$model_type = model_type
    init_mod$response_type = response_type
    init_mod$pred_var = pred_var
    init_mod$n_tree_var = n_tree_var
  }else{
    stop('Model type not recognized, please choose "ranger" or "randomForest"')
  }
  
  return(init_mod)
  
}

# Format a tree fitted using 'ranger' to match 'randomForest' formatting
# 
#' @param ranger_tree A random forest tree fitted in R using 'ranger'
#' @param pred_var Name of column storing predicted values
tree.ranger.to.randomForest = function(ranger_tree, pred_var){
  
  tree_formatted = ranger_tree %>% dplyr::mutate(prediction = !!as.name(pred_var)) # Rename response variable
  tree_formatted = tree_formatted %>% dplyr::select(-c('nodeID', 'splitvarID')) # Remove unnecessary columns
  tree_formatted$leftChild = tree_formatted$leftChild+1 # Fix child indexing
  tree_formatted$rightChild = tree_formatted$rightChild+1 # Fix child indexing
  tree_formatted = tree_formatted %>% dplyr::mutate(terminal = if_else(terminal, -1, 1)) # Convert from TRUE/FALSE to -1/1
  tree_formatted = tree_formatted %>% dplyr::mutate(leftChild = if_else(is.na(leftChild), 0, leftChild)) # Convert NAs to zeros
  tree_formatted = tree_formatted %>% dplyr::mutate(rightChild = if_else(is.na(rightChild), 0, rightChild)) # Convert NAs to zeros
  tree_formatted = tree_formatted %>% dplyr::mutate(splitval = if_else(is.na(splitval), 0, splitval)) # Convert NAs to zeros
  tree_formatted = tree_formatted %>% dplyr::rename('left daughter' = 'leftChild', 'right daughter' = 'rightChild', 'split var' = 'splitvarName', 'split point' = 'splitval', 'status' = 'terminal') # Rename columns
  
  return(tree_formatted)
  
}

# Convert a random forest model generated in R to a text format compatible with \code{tree} and readable by Google Earth Engine
# 
#' @param rf_mod A random forest model fitted in R using either 'ranger' or 'randomForest'
#' @param out_file Output file  name
convert.forest = function(rf_mod = NULL, out_file = NULL){
  
  # Create output file
  sink(file = out_file, append = TRUE) 
  
  # Get total number of trees
  ntrees = rf_mod[[rf_mod$n_tree_var]]
  
  for(i in 1:ntrees){
    
    # Get tree
    if(rf_mod$model_type == 'ranger'){
      tree = ranger::treeInfo(rf_mod, tree = i)
      tree = tree.ranger.to.randomForest(tree, rf_mod$pred_var) # Convert to 'randomForest' format
    }else if(rf_mod$model_type == 'randomForest'){
      tree = randomForest::getTree(rf_mod, i, TRUE)
    }else{
      stop('Model type not recognized, please choose "ranger" or "randomForest"')
    }
    
    # Convert to 'tree' formatting
    tree_formatted = as.tree(gTree = tree, 
                             rforest = rf_mod)
    
    # Write tree
    print(tree_formatted)
    
  }
  
  # Close connection
  sink()
  closeAllConnections()
  
  # Read back in file and tidy
  tree_file = readLines(out_file)
  tree_file  = gsub(pattern = "node\\), split, n, deviance, yval", replace = "", x = tree_file) # Remove header
  tree_file  = gsub(pattern = "      \\* denotes terminal node", replace = "", x = tree_file) # Remove header
  tree_file  = gsub(pattern = "\\.(?![0-9])", replace = "", x = tree_file, perl=TRUE) # Periods cannot be read by GEE, remove periods if not followed by a number
  if(rf_mod$response_type == 'classification'){
    tree_file  = gsub(pattern = ", \\(yprob\\)", replace = "", x = tree_file) # Remove header
  }
  tree_file = tree_file[which(tree_file!="")] # Remove blank lines
  
  # Overwrite tidy trees
  writeLines(tree_file, out_file)
  closeAllConnections()
  
}

# Functions modified from the reprtree package =================================
# SOURCE CODE: https://github.com/araastat/reprtree/blob/master/R/functions.R

# Convert the result of a getTree call to a format compatible with tree
# 
# This function takes the results of a \code{randomForest::getTree} call and 
# converts the results to a form compatible with \code{tree}
#' @param gTree The results of a call to \code{getTree}
#' @param rforest The randomForest object 
#  @return An object of class \code{tree}, which has a \code{frame} and sufficient
#     attributes to enable plotting
as.tree <- function(gTree,rforest){
  
  # Populate data from tree
  if(is.numeric(gTree[,'split var'])) stop("labelVar=T required")
  bl <- matrix("", nrow=nrow(gTree), ncol=3)
  for(row in 1:nrow(gTree)){
    if(row==1){
      bl[row, 1:2] <- c('10','11')
      next
    }
    if(gTree[row,"left daughter"]>0){
      bl[row,1:2] <- paste0(bl[which(gTree[,c("left daughter","right daughter")]==row,arr.ind=T)], c('0','1'))
    } else {
      bl[row,3] <- bl[which(gTree[,c("left daughter","right daughter")]==row, arr.ind=T)]
    }
  }
  bl <- data.frame(bl, stringsAsFactors=F); names(bl) <- c('left','right','terminal')
  fr <- list()
  fr$var <- as.character(gTree[,"split var"])
  fr$var[is.na(fr$var)] <- '<leaf>'
  fr$n <- fr$dev <- rep(0,length(fr$var)) # Deviation (splitStat) and n (numSamples) are not needed
  fr$yval <- gTree[,'prediction']
  
  # Tidy yval, NAs not read in GEE
  # Values at splits not used in GEE, can safely be assigned -1
  if(rforest$response_type == 'classification'){
    yval_tmp = as.character(fr$yval)
    yval_tmp = ifelse(is.na(yval_tmp), -1, yval_tmp)# changed to have -1 val for prediction at splits
    fr$yval <- yval_tmp
  } else{
    yval_tmp = as.numeric(fr$yval)
    yval_tmp = ifelse(is.na(yval_tmp), -1, yval_tmp) # changed to have -1 val for prediction at splits
    fr$yval <- yval_tmp
  }
  
  # Get predictor classes
  if(rforest$model_type == 'randomForest'){
    classes = attributes(rforest$terms)$dataClasses
  } else if(rforest$model_type == 'ranger'){
    classes = rforest$forest$covariate.levels
    if(is.null(classes)){
      # If the forest object does NOT have a 'covariate.levels' slot...
      # It means there were no categorical variables and/or
      # It means step_dummy was used to encode categorical variables
      # We can safely assign all covariates as 'numeric'
      classes = rep('numeric', length(rforest$forest$independent.variable.names))
    } else{
      # Otherwise, use the 'covariate.levels' slot to assign covariate classes
      classes[!sapply(classes, is.null)]  = 'factor'
      classes[sapply(classes, is.null)]  = 'numeric'
      classes = unlist(classes)
    }
  } else{
    stop('Model type not recognized')
  }
  
  # Identify categorical predictors
  blah <- data.frame(var=fr$var, splits=as.character(gTree[,'split point']), 
                     classes=unname(classes[fr$var]), stringsAsFactors=F)
  index <- which(blah$classes=='factor' & !is.na(blah$classes))
  blah$splits[index] <- sapply(blah$splits[index], factor.repr)
  
  # Format splits
  # Removed space after ':' ...
  # This results in NAs in output
  # Per 'tree' code, space is added in after factor levels are back referenced
  splits <- cbind(
    cutleft=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),':','<'),
                   blah$splits),
    cutright=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),
                           ':','>'),
                    blah$splits))
  splits[fr$var=='<leaf>',] <- ""
  
  # Convert tree data to data frame
  fr <- as.data.frame(fr, stringsAsFactors=F)
  
  # Add splits
  fr$splits <- splits
  x <- ifelse(fr$var=='<leaf>', bl[,3], gsub('.{1}$', '', bl[,1]))
  if(nrow(gTree) == 1){x = c("1")} # If there is only one row, assign it row name = 1 so that it is designated as the root
  
  # Get predictor classes
  if(rforest$model_type == 'randomForest'){
    fr$yprob = matrix(1/length(rforest$classes),nrow=nrow(fr), ncol=length(rforest$classes))
  } else if(rforest$model_type == 'ranger'){
    fr$yprob = matrix(1/length(rforest$forest$levels),nrow=nrow(fr), ncol=length(rforest$forest$levels))
  } else{
    stop('Model type not recognized')
  }
  
  # Order rows
  row.names(fr) <- bit64::as.integer64(unlist(lapply(x, strtoi_2)))
  fr <- fr[order(x),]
  
  # Copy to tree framework
  newtr <- list()
  newtr$frame=fr
  
  # Get number of levels for each predictor
  if(rforest$model_type == 'randomForest'){
    xlevels = rforest$forest$xlevels
  }else if(rforest$model_type == 'ranger'){
    xlevels = rforest$forest$covariate.levels
    if(is.null(xlevels)){
      # If the forest object does NOT have a 'covariate.levels' slot...
      # It means there were no categorical variables and/or
      # It means step_dummy was used to encode categorical variables
      # We can safely assign all covariates as 0 levels
      xlevels = as.list(rep(0, length(rforest$forest$independent.variable.names)))
      names(xlevels) = rforest$forest$independent.variable.names
    }else{
      # Otherwise, convert NULL to zero in 'covariate.levels' list
      xlevels[sapply(xlevels, is.null)]  = 0
    }
  }
  
  # Assign predictor variable levels
  attr(newtr,'xlevels') <- xlevels
  
  # Assign response variable levels, if necessary
  if(rforest$response_type == 'classification'){
    if(rforest$model_type == 'randomForest'){
      attr(newtr,'ylevels') <- rforest$classes
    } else if(rforest$model_type == 'ranger'){
      attr(newtr,'ylevels') <- rforest$forest$levels
    } else{
      stop('Model type not recognized')
    }
  }
  
  # Convert to tree
  class(newtr) <- 'tree'
  return(newtr)
}

# Convert strings to integers according to the given base
# R strtoi function cannot handle numbers greater than 2^31
# Therefore, user defined function is necessary
# https://stackoverflow.com/questions/13536832/strtoi-fails-to-convert-string-to-integer-returns-na
#
# @param x binary string
strtoi_2 <- function(x) {
  y <- as.numeric(strsplit(x, "")[[1]])
  sum(y * 2^rev((seq_along(y)-1)))
}

# Convert integers to binary representation
# 
# @param x integer to be converted
# @param reverse Should the ordering be reversed
int2bin <- function(x, reverse=F){
  y <- intToBits(x)
  yy <- paste(sapply(strsplit(paste(rev(y)),""),`[[`,2),collapse="")
  out <- gsub('^[0]+','',yy)
  if(reverse){
    bl <- rev(unlist(strsplit(out,'')))
    out <- paste(bl, collapse='')
  }
  return(out)
}

# Represent factor splits using letters
# 
# @param x character representation of integer in "split point"
factor.repr <- function(x){
  x <- int2bin(as.integer(x), reverse=T)
  n <- nchar(x)
  paste(letters[1:n][unlist(strsplit(x,''))=='1'],collapse='')
}

# Functions copied/modified from the tree package ==========================
# SOURCE CODE: https://github.com/cran/tree/blob/master/R/treemisc.R

# modified some functions to handle 64bit int

tree.depth <- function(nodes)
{
  depth <- floor(log(nodes, base = 2) + 1e-7)
  as.vector(depth - min(depth))
}

labels.tree <- function(object, pretty = TRUE, collapse = TRUE, ...)
{
  if(!inherits(object, "tree")) stop("not legitimate tree")
  frame <- object$frame
  xlevels <- attr(object, "xlevels")
  var <- as.character(frame$var)
  splits <- matrix(sub("^>", " > ", sub("^<", " < ", frame$splits)),, 2L)
  lt <- c(letters, 0:5) # max 32 levels
  if(!is.null(pretty)) {
    if(pretty) xlevels <- lapply(xlevels, abbreviate, minlength=pretty)
    for(i in grep("^:", splits[, 1L],))
      for(j in 1L:2L) {
        sh <- splits[i, j]
        nc <- nchar(sh)
        sh <- substring(sh, 2L:nc, 2L:nc)
        xl <- xlevels[[var[i]]][match(sh, lt)]
        splits[i, j] <- paste0(": ", paste(as.vector(xl), collapse=","))
        
      }
  }
  if(!collapse) return(array(paste0(var, splits), dim(splits)))
  node <- bit64::as.integer64(row.names(frame))
  parent <- match((node %/% 2L), node)
  odd <- as.logical(node %% 2L)
  node <- rep(0,length(node))
  node[odd] <- paste0(var[parent[odd]], splits[parent[odd], 2L])
  node[!odd] <- paste0(var[parent[!odd]], splits[parent[!odd], 1L])
  node[1L] <- "root"
  node
}


print.tree <- function(x, pretty = 0, spaces = 2, digits = getOption("digits")-3, ...)
{
  if(!inherits(x, "tree")) stop("not legitimate tree")
  is.prob <- !is.null(ylevels <- attr(x, "ylevels"))
  if(is.prob) cat("node), split, n, deviance, yval, (yprob)\n")
  else cat("node), split, n, deviance, yval\n")
  cat("      * denotes terminal node\n\n")
  frame <- x$frame
  node <- bit64::as.integer64(row.names(frame))
  depth <- tree.depth(node)
  indent <- paste(rep(" ", spaces * 32), collapse = "")
  #32 is the maximal depth
  if(length(node) > 1L) {
    indent <- substring(indent, 1L, spaces * seq(depth))
    indent <- paste0(c("", indent[depth]), format(node), ")")
  } else
    indent <- paste0(format(node), ")")
  if(is.prob) {
    yval <- paste0(as.character(frame$yval), " (")
    yprob <- format(frame$yprob, digits = digits)
    for(i in seq(ylevels)) yval <- paste(yval, yprob[, i])
    yval <- paste(yval, ")")
  } else
    yval <- format(signif(frame$yval, digits = digits))
  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  z <- labels.tree(x, pretty = pretty)
  z <- paste(indent, z, round(frame$n, 2L),
             format(signif(frame$dev, digits = digits)),
             yval, term)
  cat(z, sep = "\n")
  invisible(x)
}



