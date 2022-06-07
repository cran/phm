#########1#########2#########3#########4#########5#########6#########7#########8
#' Display Frequent Principal Phrases
#'
#' Display the most frequent principal phrases in a phraseDoc object.
#'
#' @param pd A phraseDoc object.
#' @param n Number of principal phrases to display.
#' @return A vector with the \code{n} most frequent principal phrases and their
#'         frequencies.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' freqPhrases(pd, 2)
#' @export
################################################################################
freqPhrases=function(pd,n=10) {
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (!isInt(n)||n<1) stop("n must be an integer greater than or equal to 1")
  n=min(n,length(pd$phrases$freq))
  x=tail(pd$phrases$freq[order(pd$phrases$freq)],n)
  names(x)=tail(pd$phrases$phrase[order(pd$phrases$freq)],n)
  m=as.matrix(rev(x))
  colnames(m)="frequency"
  m
}
