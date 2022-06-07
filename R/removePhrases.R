#########1#########2#########3#########4#########5#########6#########7#########8
#' Remove Phrases from phraseDoc Object
#'
#' Remove a set of phrases from a phraseDoc object.
#'
#' @param pd A phraseDoc object.
#' @param phrs A set of phrases.
#' @return A phraseDoc object with the phrases in \code{phrs} removed.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' removePhrases(pd, c("test text","another test text"))
#' @import data.table
#' @export
################################################################################
removePhrases=function(pd,phrs) {
  #Error checking of the input arguments
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (!inherits(phrs,"character")) stop("phrs must be a character vector")
  phrs=trimws(phrs)
  qppos=data.table(doc=pd$doc,block=pd$block,pos=pd$pos,
                 phrase=pd$phrases$phrase[pd$phrase],
                 pwrds=pd$phrases$pwrds[pd$phrase])
  idx=which(qppos$phrase %in% phrs)
  if (length(idx)==0) return(pd)
  qppos=qppos[-idx]
  qppos$phrase=factor(qppos$phrase)
  phr=qppos[,.N,by=c("phrase","pwrds")]
  x=list(phrase=as.integer(qppos$phrase),doc=qppos$doc,
         block=qppos$block,pos=qppos$pos,
         phrases=list(phrase=phr$phrase,pwrds=phr$pwrds,freq=phr$N),
         docs=pd$docs)
  class(x)="phraseDoc"
  x
}
