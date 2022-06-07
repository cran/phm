#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate Text Distance
#'
#' When two vectors are given, this calculates the text distance between them;
#' text distance is calculated as the proportion of unmatched frequencies, i.e.,
#' the number of unmatched frequencies divided by the total frequencies among
#' the two vectors. However, if neither vector has any values
#' at all, their distance equals the number provided in the zeroes argument,
#' which is .5 by default. When two matrices are given, the text distance
#' between corresponding columns is calculated.
#'
#' @param x A numeric vector or matrix
#' @param y A numeric vector or matrix of the same dimension as x
#' @param zeroes Text distance when both vectors are zero vectors; default is .5
#' @return When x and y are vectors, the text distance between them. For
#' example, between vectors (1,2,0) and (0,1,1), a total of 5 frequencies are 
#' present. However, position 1 matches nothing when it could have
#' matched 1 frequency, position 2 matches 1 frequency when it could have  
#' matched both positions, so 1 remains unmatched. Position 3 matches nothing  
#' when it could have matched 1. So we have 3 unmatched positions divided by 5 
#' frequencies, resulting in a text distance of 3/5=.6. If x and y are matrices, 
#' a vector with the text distance between corresponding columns is returned. So 
#' for two 4x2 matrices, a vector with two values is returned, one with the text 
#' distance between the first columns of the matrices, and the second one with 
#' the text distance between the second columns of the matrices. For large sets 
#' of data, it is recommended to use matrices as it is much more efficient than 
#' calculating column by column. 
#' @examples
#' #text distance between two vectors
#' textDist(c(1,2,0),c(0,1,1))
#' (M1=matrix(c(0,1,0,2,0,10,0,14),4))
#' (M2=matrix(c(12,0,8,0,1,3,1,2),4))
#' #text distance between corresponding columns of M1 and M2
#' textDist(M1,M2)
#' @export
################################################################################
textDist=function(x,y,zeroes=.5) {
  if (!inherits(zeroes,"numeric")) stop("zeroes must be numeric")
  if (zeroes[1]<0 | zeroes[1]>1) stop("zeroes must be between zero and 1")
  if (!inherits(x,c("integer","numeric","matrix","dgCMatrix"))) {
    stop("x must be a numeric vector or a matrix")
  }
  if (inherits(x,c("matrix","dgCMatrix"))) {
    if (!inherits(y,c("matrix","dgCMatrix"))) stop("y must be a matrix")
    if (nrow(x)!=nrow(y)) stop("x must have the same number of rows as y")
    if (ncol(x)!=ncol(y)) stop("x must have the same number of columns as y")
    if (nrow(x)<1) stop("x and y are empty")
    num=colSums(abs(x-y))
    den=colSums(x)+colSums(y)
    idx=which(den==0)
    num[idx]=zeroes
    den[idx]=1
    return(num/den)
  }
  if (!inherits(y,c("integer","numeric"))) stop("y must be a numeric vector")
  if (length(x)!=length(y)) stop("x and y must have equal length")
  if (length(x)<1) stop("x and y are empty")
  if (sum(x,y)==0) zeroes[1] else sum(abs(x-y))/sum(x,y)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate Canberra Distance
#'
#' When two vectors are given, this calculates the Canberra distance between
#' them; This is calculated as the sum of the absolute difference between 
#' corresponding elements divided by the sum of their absolute values, for 
#' elements that are not both zero only.
#'
#' @param x A numeric vector
#' @param y A numeric vector of the same dimension as x
#' @return The Canberra distance between x and y. For example, between vectors
#' (1,2,0) and (0,1,1), for position 1 we have (1-0)/1, for position 2 we have
#' (2-1)/3, and for position 3 we have abs(0-1)/1, added together this results
#' in 2 1/3, or 2.33. Note that a text distance of zero indicates that the
#' two vectors are equal, while a text distance of 1 indicates that they have
#' no terms in common.
#' @examples
#' canberra(c(1,2,0),c(0,1,1))
#' @export
################################################################################
canberra=function(x,y) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be a numeric vector")
  if (!inherits(y,c("integer","numeric"))) stop("y must be a numeric vector")
  a=abs(x-y)
  b=(abs(x)+abs(y))
  sum(a[b!=0]/b[b!=0])
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate a Distance Matrix
#'
#' Calculate a distance matrix for a numeric matrix, where a distance function
#' is used to calculate the distance between all combinations of the columns
#' of the matrix \code{M}.
#'
#' @param M A numeric matrix
#' @param fn The name of a distance function, default is "textDist".
#' @param ... Additional arguments to be passed to the distance function 
#' @return The distance matrix with the distance between all combinations
#' of the columns of \code{M} according to the distance function in \code{fn}.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' M
#' #Text distance matrix
#' distMatrix(M)
#' #Canberra distance matrix
#' distMatrix(M,"canberra")
#' @export
################################################################################
distMatrix=function(M,fn="textDist",...) {
  if (!inherits(M,"matrix")||!inherits(M[1,1],c("integer","numeric"))) 
    stop("M must be a numeric matrix")
  if (!exists(fn, mode='function')) stop("fn must have the name of a distance
                                         function")
  fun=get(fn)
  x=utils::combn(ncol(M),2,function(x) {fun(M[,x[1]],M[,x[2]],...)})
  attr(x,"Size")=ncol(M)
  if (is.null(colnames(M))) lab=as.character(1:ncol(M)) else lab=colnames(M)
  attr(x,"Labels")=lab
  attr(x,"Diag")=FALSE
  attr(x,"Upper")=FALSE
  attr(x,"method")=fn
  class(x)="dist"
  x
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate a Text Distance Matrix
#'
#' Calculate a distance matrix for a numeric matrix, using the textDist
#' function. It is used to calculate the text distance between all combinations 
#' of the columns of the matrix \code{M}.
#'
#' @param M A numeric matrix
#' @param zeroes Text distance when both vectors are zero vectors; default is .5
#' @return The text distance matrix with the text distance between all
#' combinations of the columns of \code{M}. This will give the same result as 
#' the function distMatrix when run with its default distance function 
#' "textDist"; however, for large matrices textDistMatrix is much more 
#' efficient. In addition, for very large matrices distMatrix may not run, 
#' while textDistMatrix will.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' M
#' #Text distance matrix
#' textDistMatrix(M)
#' @import Matrix
#' @export
################################################################################
textDistMatrix=function(M,zeroes=.5) {
  if (!inherits(M,"matrix")||!inherits(M[1,1],c("integer","numeric"))) 
    stop("M must be a numeric matrix")
  if (!inherits(zeroes,"numeric")) stop("zeroes must be numeric")
  if (zeroes[1]<0 | zeroes[1]>1) stop("zeroes must be between zero and 1")
  #Turn a matrix that is sparse into a sparse matrix if not already
  if (!inherits(M,"dgCMatrix")) if (mean(M==0) > .7) M=Matrix::Matrix(M,sparse=TRUE)
  com=utils::combn(ncol(M),2)
  x=textDist(M[,com[1,]],M[,com[2,]],zeroes)
  attr(x,"Size")=ncol(M)
  if (is.null(colnames(M))) lab=as.character(1:ncol(M)) else lab=colnames(M)
  attr(x,"Labels")=lab
  attr(x,"Diag")=FALSE
  attr(x,"Upper")=FALSE
  attr(x,"method")="textDist"
  class(x)="dist"
  x
}
