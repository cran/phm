#########1#########2#########3#########4#########5#########6#########7#########8
#' Display Frequency Matrix for Phrases
#'
#' Display a frequency matrix containing all the documents that contain any of
#' the phrases in phrs and the number of times they occur in that document.
#'
#' @param pd A phraseDoc object.
#' @param phrs A set of phrases.
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
#' @export
################################################################################
getDocs=function(pd,phrs) {
  #Error checking of the input arguments
  if (class(pd)!="phraseDoc") stop("pd must be a phraseDoc")
  if (class(phrs)!="character") stop("phrs must be character")
  phrs=trimws(phrs)
  idx=which(pd$phrases$phrase %in% phrs)
  if (length(idx)==0) stop("No such phrases in the phraseDoc")
  idx2=which(pd$phrase %in% idx)

  dta=data.table(phrase=pd$phrase[idx2],doc=pd$doc[idx2],pos=pd$pos[idx2])
  x=dta[,.N,by=c("doc", "phrase")]
  x$doc=factor(x$doc)
  x$phrase=factor(x$phrase)
  phrases=as.character(pd$phrases$phrase[as.numeric(levels(x$phrase))])
  as.matrix(slam::simple_triplet_matrix(i=as.numeric(x$phrase),
                                        j=as.numeric(x$doc),v=x$N,
                                        dimnames=list(phrases=phrases,
                                                      docs=levels(x$doc))))
}
