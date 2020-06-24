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
  #Error checking of the input arguments
  if (class(pd)!="phraseDoc") stop("pd must be a phraseDoc")
  if (class(n)!="numeric"&&class(n)!="integer") stop("n must be numeric")
  n=min(n,length(pd$phrases$freq))
  x=utils::tail(pd$phrases$freq[order(pd$phrases$freq)],n)
  names(x)=utils::tail(pd$phrases$phrase[order(pd$phrases$freq)],n)
  rev(x)
}
