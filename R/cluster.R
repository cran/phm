#########1#########2#########3#########4#########5#########6#########7#########8
#' Cluster a Term-Document Matrix
#'
#' Combine documents (columns) into k clusters that have texts that are most
#' similar based on their text distance. Documents with no terms are assigned
#' to the last cluster.
#'
#' @param tdm A term document matrix with terms on the rows and documents on 
#' the columns.
#' @param k A positive integer with the number of clusters needed
#' @param mx Maximum number of times to iterate (default 100)
#' @param md Maximum number of documents to use for the initial setup (default 
#' 5*\code{k}).
#' @return A textcluster object with three items; cluster, centroids, and size,
#' where cluster contains a vector indicating for each column in \code{M} what
#' cluster they have been assigned to, centroids contains a matrix with each
#' column the centroid of a cluster, and size a named vector with the size of
#' each cluster.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' textCluster(M,2)
#' @export
################################################################################
textCluster=function(tdm,k,mx=100,md=5*k) {
  if (!inherits(tdm,"matrix")||!inherits(tdm[1,1],c("integer","numeric"))) 
    stop("tdm must be a term-document matrix")
  if (!smallstuff::isInt(k)||k<2) stop("k must be an integer greater than 1")
  nc=ncol(tdm);nr=nrow(tdm)
  if (nc<2*k) stop ("Too many clusters")
  sums=Matrix::colSums(tdm)
  #Check if we need a zeroes cluster
  if (sum(sums==0)>0) {
    #Create a zeroes cluster
    idx=which(sums==0)
    if ((nc-length(idx))<2*(k-1)) stop("Too many clusters requested")
    clust=rep(NA,nc)
    names(clust)=colnames(tdm)
    clust[idx]=k
    tc=textCluster(tdm[,-idx],k-1,mx=mx,md=md)
    clust[-idx]=tc$cluster
    centers=cbind(tc$centroids,rep(0,nr))
    colnames(centers)=1:k
    sz=as.integer(table(clust))
    names(sz)=names(table(clust))
    lis <- list(cluster=clust,centroids=centers,size=sz)
    class(lis)="textCluster"
    return(lis)
  }
  md=max(md,2*k)
  if (nc>md) {
    sm=order(sums,decreasing = TRUE)
    dm=textDistMatrix(tdm[,sm][,1:md])
    cm=utils::combn(md,2)[,order(dm)]
  } else {
    sm=1:nc
    dm=textDistMatrix(tdm)
    cm=utils::combn(nc,2)[,order(dm)]
  }
  p=min(nc,md)
  c1=rep(NA,p)
  #Create an initial clustering
  for (j in 1:k) {
    #Indices of documents that have not yet been assigned
    v=which(is.na(c1))
    #Find the first closest pair where both have not yet been assigned
    i=cm[,which(cm[1,]%in%v & cm[2,]%in%v)[1]]
    c1[i]=j
    if (sum(is.na(c1))<2*(k-j)+1) next
    fin=min(2*k*j,ncol(cm))
    #Find documents similar to those in i
    i=unique(c(cm[,j:fin][,(cm[1,j:fin] %in% i)|(cm[2,j:fin] %in% i)]))
    #Only keep the ones that have not already been assigned
    i=i[!(i %in% which(!is.na(c1)))]
    if ((sum(is.na(c1))-length(i))<2*(k-j)+1) next
    c1[i]=j
  }
  clust=rep(NA,nc)
  clust[sm[1:p]]=c1
  centers=sapply(seq_len(k),function(x) {centroid(which(clust==x),tdm)})
  A=rbind(rep(1:nc,each=k),rep(1:k,nc))
  #iterate until the centers no longer change (but no more than mx times)
  for (i in 1:mx) {
    MD <- matrix(textDist(tdm[,A[1,]],centers[,A[2,]]),k)
    clust=apply(MD,2,which.min)
    centers2=sapply(seq_len(k),function(x) {centroid(which(clust==x),tdm)})
    if (identical(centers,centers2)) break else centers=centers2
  }
  colnames(centers)=1:k
  names(clust)=colnames(tdm)
  sz=as.integer(table(clust))
  names(sz)=names(table(clust))
  lis=list(cluster=clust,centroids=centers,size=sz)
  class(lis)="textCluster"
  lis
}
################################################################################
# Internal Functions
###################################################################################
# centroid: calculate the centroid of a cluster 
# Input:  idx: Column numbers of columns in the cluster
#         M: Term document matrix
# Output: The centroid of the columns of the matrix M indicated by the column
#         numbers in M
###################################################################################
centroid <- function(idx,M) {
  rowMeans(M[,idx,drop=F])
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Show Cluster Contents
#'
#' Show all documents and their non-zero terms in a cluster, with the terms
#' first ordered by highest number of documents the term appears in, then total
#' frequency.
#'
#' @param tdm A term frequency matrix.
#' @param clust A vector indicating for each column in \code{tdm} what cluster
#' they belong to
#' @param cl Cluster number
#' @param n Integer showing the maximum number of terms to be returned (default 10)
#' @return A matrix with document names of \code{tdm} on the columns and terms
#' on the rows for all columns in the cluster, where terms that appear in the
#' most documents (columns), and within that have the highest frequency in the
#' cluster, are shown first. Two columns are added at the end of the matrix
#' with the the number of documents each term appears in and its total frequency
#' in the cluster. The number of terms displayed equals the number in \code{n}, 
#' or less if there are less terms in the cluster.
#' If there are no terms at all in the cluster, a list is output with the items
#' docs and note, where docs is a vector with all document names of documents in 
#' the cluster, and the note stating that the cluster has no terms. 
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' tc=textCluster(M,2)
#' showCluster(M,tc$cluster,1)
#' @export
################################################################################
showCluster <- function(tdm,clust,cl,n=10L) {
  if (!inherits(tdm,"matrix")||!inherits(tdm[1,1],c("integer","numeric"))) 
    stop("tdm must be a term-document matrix")
  if (length(clust)!=ncol(tdm)) stop("clust must contain the cluster for each
                                     column of tdm")
  if (!(cl %in% clust)) stop("cl must be a cluster in clust")
  if (!smallstuff::isInt(n)||n<1) stop("n must be an integer greater than 0")
  M=tdm[,clust==cl,drop=F]
  frq=rowSums(M)
  frq2=apply(M,1,function(x) {sum(x!=0)})
  n=min(n,sum(frq!=0))
  if (n==0) return(list(docs=names(clust[clust==cl]),
                        note="Documents have no terms"))
  idx=order(frq2,frq,decreasing=TRUE)[1:n]
  cbind(M[idx,,drop=F],nDocs=frq2[idx],totFreq=frq[idx])
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Print a textCluster Object
#'
#' @param x Object of type textCluster
#' @param ... Additional arguments
#' @return The total number of clusters and total number of documents are 
#' printed. There is no return value.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' tc=textCluster(M,2)
#' tc
#' @method print textCluster
#' @export
################################################################################
print.textCluster <- function(x,...) {
  writeLines(paste("<<",class(x),">>"))
  writeLines(paste(length(x$size),"clusters"))
  writeLines(paste(length(x$cluster),"documents"))
}

