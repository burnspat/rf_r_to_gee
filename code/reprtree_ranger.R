# SOURCE CODE: https://github.com/araastat/reprtree/blob/master/R/functions.R

# Allow R to handle larger integers
library(bit64)

# Represent factor splits using letters
# 
# @param x character representation of integer in "split point"
factor.repr <- function(x){
  x <- int2bin(as.integer(x), reverse=T)
  n <- nchar(x)
  paste(letters[1:n][unlist(strsplit(x,''))=='1'],collapse='')
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

# Convert the result of a getTree call to a format compatible with tree
# 
# This function takes the results of a \code{randomForest::getTree} call and 
# converts the results to a form compatible with \code{tree}
#' @param gTree The results of a call to \code{getTree}
#' @param rforest The randomForest object 
#' @param training_data The training data as a data.frame
#' @param response_type The type of model response, either 'classification' or 'regression'
# @return An object of class \code{tree}, which has a \code{frame} and sufficient
#     attributes to enable plotting
as.tree.ranger <- function(gTree,rforest,training_data,response_type){
  
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
  fr$n <- gTree[,'numSamples']
  fr$dev <- gTree[,'splitStat']
  fr$yval <- gTree[,'prediction']
  
  classes <- sapply(training_data, class)
  
  blah <- data.frame(var=fr$var, splits=as.character(gTree[,'split point']), 
                     classes=unname(classes[fr$var]), stringsAsFactors=F)
  index <- which(blah$classes=='factor' & !is.na(blah$classes))
  blah$splits[index] <- sapply(blah$splits[index], factor.repr)  
  
  splits <- cbind(
    cutleft=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),': ','<'),
                   blah$splits), 
    cutright=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),
                           ': ','>'),
                    blah$splits))
  splits[fr$var=='<leaf>',] <- ""
  
  fr <- as.data.frame(fr, stringsAsFactors=F)
  fr$splits <- splits
  x <- ifelse(fr$var=='<leaf>', bl[,3], gsub('.{1}$', '', bl[,1]))
  if(nrow(gTree) == 1){x = c("1")} # If there is only one row, assign it row name = 1 so that it is designated as the root
  if(response_type=='classification'){
    fr$yprob = unname(as.matrix(gTree[,grepl("pred\\.", colnames(gTree))]))
  }
  row.names(fr) <- as.integer64(unlist(lapply(x, strtoi_2)))
  fr <- fr[order(x),]
  
  newtr <- list()
  newtr$frame=fr
  
  xlevels_tmp <- sapply(data, levels)
  xlevels_tmp <- lapply(xlevels_tmp, length)
  
  attr(newtr,'xlevels') <- xlevels_tmp
  if(response_type=='classification') attr(newtr,'ylevels') <- unique(as.character(gTree[,'prediction']))
  class(newtr) <- 'tree'
  return(newtr)
}

# Convert the result of a getTree call to a format compatible with tree
# Shortens tree output so it doesn't overflow GEE objects 
#
# This function takes the results of a \code{randomForest::getTree} call and 
# converts the results to a form compatible with \code{tree}
#' @param gTree The results of a call to \code{getTree}
#' @param rforest The randomForest object 
#' @param training_data The training data as a data.frame
#' @param response_type The type of model response, either 'classification' or 'regression'
# @return An object of class \code{tree}, which has a \code{frame} and sufficient
#     attributes to enable plotting
as.tree.ranger.short <- function(gTree,rforest,training_data,response_type){
  
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
  fr$n <- 0
  fr$dev <- 0
  fr$yval <- gTree[,'prediction']
  
  classes <- sapply(training_data, class)
  
  blah <- data.frame(var=fr$var, splits=as.character(gTree[,'split point']), 
                     classes=unname(classes[fr$var]), stringsAsFactors=F)
  index <- which(blah$classes=='factor' & !is.na(blah$classes))
  blah$splits[index] <- sapply(blah$splits[index], factor.repr)  
  
  splits <- cbind(
    cutleft=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),': ','<'),
                   blah$splits), 
    cutright=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),
                           ': ','>'),
                    blah$splits))
  splits[fr$var=='<leaf>',] <- ""
  
  fr <- as.data.frame(fr, stringsAsFactors=F)
  fr$splits <- splits
  x <- ifelse(fr$var=='<leaf>', bl[,3], gsub('.{1}$', '', bl[,1]))
  if(nrow(gTree) == 1){x = c("1")} # If there is only one row, assign it row name = 1 so that it is designated as the root
  if(response_type=='classification'){
    fr$yprob = unname(as.matrix(gTree[,grepl("pred\\.", colnames(gTree))]))
  }
  row.names(fr) <- unlist(lapply(x, strtoi_2))
  fr <- fr[order(x),]
  
  newtr <- list()
  newtr$frame=fr
  
  xlevels_tmp <- sapply(data, levels)
  xlevels_tmp <- lapply(xlevels_tmp, length)
  
  attr(newtr,'xlevels') <- xlevels_tmp
  if(response_type=='classification') attr(newtr,'ylevels') <- unique(as.character(gTree[,'prediction']))
  class(newtr) <- 'tree'
  return(newtr)
}
