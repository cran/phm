#########1#########2#########3#########4#########5#########6#########7#########8
#' phraseDoc Creation
#'
#' Create an object of class phraseDoc. This will hold all principal phrases of
#' a collection of texts that occur a minimum number of times, plus the texts
#' they occur in and their position within those texts.
#'
#' @param co A corpus or a character vector with each element the text of a 
#' document.
#' @param mn Minimum number of words in a phrase.
#' @param mx Maximum number of words in a phrase.
#' @param ssw A set of words no phrase should start with.
#' @param sew A set of words no phrase should end with.
#' @param sp A set of phrases to be excluded.
#' @param min.freq The minimum frequency of phrases to be included.
#' @param principal Function that determines if a phrase is a principal phrase. 
#' By default, FALSE is returned if the phrase occurs less often than the number
#' in \code{min.freq}.
#' @param max.phrases Maximum number of phrases to be included.
#' @param shiny TRUE if called from a shiny program. This will allow progress
#' to be recorded on a progress meter; the function uses about 100 progress
#' steps, so it should be created inside a withProgress function with the 
#' argument max set to at least 100.
#' @param silent TRUE if you do not want progress messages.
#' @return Object of class phraseDoc
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' phraseDoc(tst)
#' @import data.table
#' @importFrom smallstuff isInt
#' @export
################################################################################
phraseDoc<-function(co,mn=2,mx=8,ssw=stopStartWords(),
                     sew=stopEndWords(),sp=stopPhrases(),min.freq=2,
                     principal=function(phrase, freq) {freq>=min.freq},
                     max.phrases=1500,shiny=FALSE,silent=FALSE) {
  freq <- posl <- pos <- pwrds <- idx <- keep <- doc <- block <- . <- NULL
  #Error checking the input arguments
  if (inherits(co,"Corpus")) corp=TRUE else corp=FALSE
  if (!corp&&!inherits(co,"character")) {
    stop("co must be a character vector or a VCorpus")
  }
  if (!isInt(mn)) stop("mn must be an integer")
  if (!isInt(mx)) stop("mx must be an integer")
  if (!inherits(ssw,"character")) stop("ssw must be character")
  if (!inherits(sew,"character")) stop("sew must be character")
  if (!inherits(sp,"character")) stop("sp must be character")
  if (!isInt(min.freq)) stop("min.freq must be an integer")
  if (!inherits(principal,"function")) stop("principal must be a function")
  if (!isInt(max.phrases)) stop("max.phrases must be an integer")
  if (!inherits(shiny,"logical")) stop("shiny must be logical")
  if (!inherits(silent,"logical")) stop("silent must be logical")
  #Initialize
  #Determine all n-grams within the blocks that do not start with a word in ssw,
  #do not end with a word in sew, and are not equal to phrases in sp. Record the
  #position of each n-gram found, in a list called ps containing the n-gram,
  #the number of words in the n-gram (pwrds), and the document, block, and
  #position numbers.
  k=1
  for (j in 1:length(co)) { #For each document
    if (shiny&(j %in% round(1:57*length(co)/57))) shiny::incProgress(1)
    if (corp) bl=getBlocks(NLP::content(co[[j]])) else bl=getBlocks(co[j])
    if (is.na(bl[1])) next
    for (b in 1:length(bl)) { #For each block
      wds=getWords(bl[b])
      for (w in (1:length(wds))) { # For each word within block
        if (wds[w] %chin% ssw) next #Phrase may not start with a stopStartWord
        for (l in (mn:mx)) {
          if (w+l-1>length(wds)) break #Phrase cannot go past the end of block
          if (wds[w+l-1] %chin% sew) next #Phrase may not end with a stopEndword
          phrase=paste(wds[w:(w+l-1)],collapse=" ") #Create a phrase
          if (phrase %chin% sp) next #Ignore phrases in stopPhrases
          if (k==1) { #First position record
            ps<-list(phrase=phrase, pwrds=l, doc=j,block=b,pos=w)
          } else { #Add a position
            x<-list(phrase=phrase, pwrds=l, doc=j,block=b,pos=w)
            for (p in 1:5) ps[[p]][k]<-x[[p]]
          }
          k=k+1
        }
      }
    }
  }
  if (!shiny&&!silent) print(Sys.time())
  if (k==1) stop("No phrases found")
  setDT(ps)
  setkeyv(ps,"phrase")
  if (shiny) shiny::incProgress(22)
  #Create pup with all unique n-grams in ps (but no positions)
  pup=ps[,.(freq=.N),by=c("phrase","pwrds")]
  
  #Aim to have not much more than max.phrases left in the end (but use the freq)
  if (nrow(pup)>max.phrases) {
    min.freq=max(min.freq,pup$freq[order(pup$freq,decreasing=TRUE)][max.phrases])
  }
  pup=pup[freq>=min.freq] #Only keep phrases that occur the minimum or more
  if (nrow(pup)==0) stop(paste("No phrases have frequency greater than", 
                               min.freq))
  ppos=ps[phrase %chin% pup$phrase] #Only keep positions for phrases that exist
  ppos[,posl := pos+pwrds-1]       #Last position
  
  setkeyv(pup,"phrase")
  pup[,idx := 1:nrow(pup)]
  setkeyv(ppos,"phrase")
  ppos=ppos[pup[,.(phrase,idx)]]
  ppos[,keep := TRUE]
  pup=pup[pup$pwrds!=1] #We don't need to do anything about single words
  setorder(pup,-pwrds,-freq)
  setkeyv(ppos,"idx")
  setindex(ppos,doc,block,pos)
  setindex(ppos,doc,block,posl)
  if (!shiny&&!silent) print(Sys.time())
  
  #Rectification process
  if (nrow(pup)!=0) {
    if (shiny) shiny::incProgress(0,message="Rectifying frequencies...") else {
      if (!silent) print("Rectifying frequencies...")
    }
    for (i in 1:nrow(pup)) {
      if (shiny&(i %in% round(1:16*nrow(pup)/16))) shiny::incProgress(1)
      phr=pup$phrase[i]
      indx=pup$idx[i]
      tp <- ppos[.(indx)][(keep)]
      if (nrow(tp)==0) next
      if (!principal(phr,nrow(tp))) {
        ppos[.(indx),keep := FALSE]
      } else {
        j=pup$pwrds[i]-1
        tmp=tp[,.(doc=rep(doc,j),block=rep(block,j),
                  pos=rep(pos,j)+rep(1:j,each=nrow(tp)))]
        ppos[.(tmp$doc,tmp$block,tmp$pos),on=c("doc","block","pos"),keep:=FALSE]
        tmp[,pos := pos-1]
        ppos[.(tmp$doc,tmp$block,tmp$pos),on=c("doc","block","posl"),
             keep:=FALSE]
      }
    }
  }
  if (!shiny&&!silent) print(Sys.time())
  ppos=ppos[(keep)]
  setkeyv(ppos,"phrase")
  phr=ppos[,.(freq=.N),by=c("phrase","pwrds")]
  setkeyv(phr,"phrase")
  phr[,idx := 1:nrow(phr)]
  ppos[,idx := NULL]
  ps=ppos[phr[,.(phrase,idx)]]
  phr[,idx:=NULL]
  ids=names(co)
  if (length(ids)!=length(co)) ids=1:length(co)
  x=list(phrase=ps$idx,doc=ps$doc,block=ps$block,pos=ps$pos, 
         phrases=as.list(phr),docs=ids)
  class(x) <- "phraseDoc"
  x
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Words that Principal Phrases do not Start with
#'
#' Create a vector with words that principal phrases should not start with.
#'
#' @return vector with words
#' @examples
#' stopStartWords()
#' @export
################################################################################
stopStartWords<-function() {
  myStopwords <- c(tm::stopwords('english'), letters, "0","1","2","3","4","5",
                   "6","7","8","9", "=","+/-",">/=","<",">","and/or", "-","+",
                   "_","#","%","&","*")
  myStopwords[-which(myStopwords=="not"|myStopwords=="no")]
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Words that Principal Phrases do not End with
#'
#' Create a vector with words that principal phrases should not end with.
#'
#' @return vector with words
#' @examples
#' stopEndWords()
#' @export
################################################################################
stopEndWords<-function() {
  myStopwords <- c(tm::stopwords('english'), letters, "0","1","2","3","4","5",
                   "6","7","8","9", "=","+/-",">/=","<",">","and/or", "-","+",
                   "_","#","$","&","*")
  myStopwords
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Phrases that are not Principal Phrases
#'
#' Create a vector with phrases that are not principal phrases.
#'
#' @return vector with phrases
#' @examples
#' stopPhrases()
#' @export
################################################################################
stopPhrases<-function() {""}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Print a phraseDoc Object
#'
#' @param x Object of type phraseDoc
#' @param ... Additional arguments
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' (pd=phraseDoc(tst))
#' @method print phraseDoc
#' @export
################################################################################
print.phraseDoc <- function(x,...) {
  writeLines(paste("<<",class(x),">>"))
  writeLines(paste(length(x$phrases$phrase),"principal phrases"))
  writeLines(paste(length(x$docs),"documents"))
  invisible(x)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Convert a phraseDoc Object to a Matrix
#'
#' @param x A phraseDoc object.
#' @param ids A logical value with TRUE (default) to use ids (if available), 
#' FALSE to use indices
#' @param sparse A logical value indicates whether a sparse matrix should be
#' returned (default FALSE)
#' @param ... Additional arguments
#' @return A matrix with phrases as rows, texts as columns, and elements
#' containing the number of times the phrase occurs in the text
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
#' @method as.matrix phraseDoc
#' @export
################################################################################
as.matrix.phraseDoc <- function(x,ids=TRUE,sparse=FALSE,...) {
  if (!inherits(ids,"logical")) stop("ids must be a logical value")
  if (!inherits(sparse,"logical")) stop("sparse must be a logical value")
  dta=data.table::data.table(phrase=x$phrase,doc=x$doc)
  y=dta[,.N,by=c("doc", "phrase")]
  if (ids) docs=x$docs else docs=1:length(x$docs)
  sm=Matrix::sparseMatrix(y$phrase,y$doc,x=y$N,
                  dims=c(length(x$phrases$phrase),length(x$docs)),
                  dimnames=list(phrases=x$phrases$phrase,
                                docs=as.character(docs)))
  if (sparse) sm else as.matrix(sm)
}
################################################################################
# Internal Functions
################################################################################
# getBlocks: Split a document into blocks
# Input:  txt: contents of a document to be split into pieces
# Output: A character vector containing a block of text in each field
################################################################################
getBlocks<-function(txt) {
  txt=gsub("http[^[:space:]]*", "", txt)
  splits <- unlist(strsplit(txt,"[].!()\"\'\\,:;?|{}[]"))
  # Remove extra white space and all leading and trailing spaces
  splits=tm::stripWhitespace(gsub("^\\s+|\\s+$", "", splits))
  splits[splits!=""] #Remove all empty elements
}
################################################################################
# getWords: Split a block of words into words
# Input:  txt: A block of text
# Output: A character vector containing a a word in each field
################################################################################
getWords<-function(txt) {
  txt=tolower(txt)
  wds=unlist(strsplit(txt," "))
}
