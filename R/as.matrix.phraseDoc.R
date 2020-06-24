#########1#########2#########3#########4#########5#########6#########7#########8
#' Convert a phraseDoc Object to a Matrix
#'
#' @param x A phraseDoc object.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A matrix with phrases as rows, texts as columns, and elements
#' containing the number of times the phrase occurs in the text.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' as.matrix(pd)
#' @import data.table
#' @export
################################################################################
as.matrix.phraseDoc=function(x,...) {
  dta=data.table(phrase=x$phrase,doc=x$doc,pos=x$pos)
  y=dta[,.N,by=c("doc", "phrase")]
  as.matrix(slam::simple_triplet_matrix(i=y$phrase,j=y$doc,v=y$N,
                                  dimnames=list(phrases=as.character(x$phrases$phrase),
                                                docs=as.character(1:max(y$doc)))))
}
