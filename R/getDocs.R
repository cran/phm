#########1#########2#########3#########4#########5#########6#########7#########8
#' Display Frequency Matrix for Phrases
#'
#' Display a frequency matrix containing all the documents that contain any of
#' the phrases in phrs and the number of times they occur in that document.
#'
#' @param pd A phraseDoc object.
#' @param phrs A set of phrases.
#' @param ids A logical value with TRUE (default) to return ids (if available), 
#' FALSE to return indices.
#' @return A matrix with the documents and # of occurrences for the phrases in
#' phrs.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' getDocs(pd, c("test text","another test text"))
#' @import data.table
#' @export
################################################################################
getDocs=function(pd,phrs,ids=TRUE) {
  #Error checking of the input arguments
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (!inherits(phrs,"character")) stop("phrs must be a character vector")
  if (!inherits(ids,"logical")) stop("ids must be a logical value")
  phrs=trimws(phrs)
  idx=which(pd$phrases$phrase %in% phrs)
  if (length(idx)==0) stop("No such phrases in the phraseDoc")
  idx2=which(pd$phrase %in% idx)
  dta=data.table(phrase=pd$phrase[idx2],doc=pd$doc[idx2])
  x=dta[,.N,keyby=c("doc", "phrase")]
  docs=factor(x$doc)
  phrs=factor(x$phrase)
  p=length(levels(docs))
  n=length(levels(phrs))
  i=as.numeric(phrs)
  j=as.numeric(docs)
  m=rep(0,n*p)
  m[((i-1)*p)+j]=x$N
  M=matrix(m,n,byrow=TRUE)
  
  if (ids) colnames(M)=pd$docs[as.numeric(levels(docs))] else {
    colnames(M)=levels(docs)
  }
  rownames(M)=pd$phrases$phrase[as.numeric(levels(phrs))]
  M
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Display Frequency Matrix for Documents
#'
#' Display a frequency matrix containing all the documents for which the indices
#' are given in docs with their principal phrases and the number of times they
#' occur in each document.
#'
#' @param pd A phraseDoc object
#' @param doc An integer vector containing indices of documents, or a character
#' vector containing the ids of documents (column names)
#' @param ids A logical value with TRUE (default) to return ids (if available), 
#' FALSE to return indices
#' @return A matrix with the documents and # of occurrences of principal phrases
#' for the documents in \code{docs}
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' getPhrases(pd, c(1,3))
#' @import data.table
#' @importFrom smallstuff isInt
#' @export
################################################################################
getPhrases <- function(pd,doc,ids=TRUE) {
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (inherits(doc,"character")) {
    doc=which(pd$docs %in% doc)
  } else {
    #if we're not dealing with an integer vector, then all entries must be integers
    if (!inherits(doc,"integer") && sum(isInt(doc))!=length(doc))
        stop("doc must be a vector of indices or document names")
  }
  if (length(doc)==0) stop("No such documents found")
  if (!inherits(ids,"logical")) stop("ids must be a logical value")
  idx = which(pd$doc %in% doc)
  if (length(idx) == 0) stop("No such documents in the phraseDoc")
  dta = data.table(phrase = pd$phrase[idx], doc = pd$doc[idx])
  x=dta[,.N,keyby=c("doc", "phrase")]
  docs=factor(x$doc)
  phrs=factor(x$phrase)
  p=length(levels(docs))
  n=length(levels(phrs))
  i=as.numeric(phrs)
  j=as.numeric(docs)
  m=rep(0,n*p)
  m[((i-1)*p)+j]=x$N
  M=matrix(m,n,byrow=TRUE)
  if (ids) colnames(M)=pd$docs[as.numeric(levels(docs))] else {
    colnames(M)=levels(docs)
  }
  rownames(M)=pd$phrases$phrase[as.numeric(levels(phrs))]
  M
}
