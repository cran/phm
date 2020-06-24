#########1#########2#########3#########4#########5#########6#########7#########8
#' phraseDoc Creation
#'
#' Create an object of class phraseDoc. This will hold all principal phrases of
#' a collection of texts that occur a minimum number of times, plus the texts
#' they occur in and their position within those texts.
#'
#' @param txts A character vector with each element the contents of a document,
#' or a VCorpus object.
#' @param mn Minimum number of words in a phrase.
#' @param mx Maximum number of words in a phrase.
#' @param ssw A set of words no phrase should start with.
#' @param sew A set of words no phrase should end with.
#' @param sp A set of phrases to be excluded.
#' @param min.freq The minimum frequency of phrases to be included.
#' @param pp Function that determines if a phrase is a principal phrase. By
#' default, FALSE is returned if the phrase occurs less often than the number
#' in \code{min.freq}.
#' @param max.phrases Maximum number of phrases to be included.
#' @param shiny TRUE if called from a shiny program.
#' @param silent TRUE if you do not want progress messages.
#' @return object of class phraseDoc.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' @import data.table
#' @export
################################################################################
phraseDoc<-function(txts,mn=2,mx=8,ssw=stopStartWords(),
                    sew=stopEndWords(),sp=stopPhrases(),min.freq=2,
                    pp=function(phrase, freq) {ifelse(freq<min.freq,return(FALSE),
                                                      return(TRUE))},
                    max.phrases=1500,shiny=FALSE,silent=FALSE) {
#Error checking of the input arguments
  if (inherits(txts,"VCorpus")) corp=TRUE else corp=FALSE
  if (!corp&&class(txts)!="character") {
    stop("txts must be character or a VCorpus")
  }
  if (class(mn)!="numeric"&&class(mn)!="integer") stop("mn must be numeric")
  if (class(mx)!="numeric"&&class(mx)!="integer") stop("mx must be numeric")
  if (class(ssw)!="character") stop("ssw must be character")
  if (class(sew)!="character") stop("sew must be character")
  if (class(sp)!="character") stop("sp must be character")
  if (class(min.freq)!="numeric"&&class(min.freq)!="integer") {
    stop("min.freq must be numeric")
  }
  if (class(pp)!="function") stop("pp must be a function")
  if (class(max.phrases)!="numeric"&&class(max.phrases)!="integer") {
    stop("max.phrases must be numeric")
  }
  if (class(shiny)!="logical") stop("shiny must be logical")
  if (class(silent)!="logical") stop("silent must be logical")
  #Initialize
  #Determine all n-grams within the blocks that do not start with a word in ssw,
  #do not end with a word in sew, and are not equal to phrases in sp. Record the
  #position of each n-gram found, in a list called ps containing the n-gram,
  #the number of words in the n-gram (pwrds), and the document, block, and
  #position numbers.
  k=1
  for (j in 1:length(txts)) { #For each document
    if (shiny&(j %in% round(1:57*length(txts)/57))) shiny::incProgress(1)
    if (corp) bl=getBlocks(as.character(txts[[j]])) else bl=getBlocks(txts[j])
    if (is.na(bl[1])) next
    for (b in 1:length(bl)) { #For each block
      wds=getWords(bl[b])
      for (w in (1:length(wds))) { # For each word within block
        if (wds[w] %in% ssw) next #Phrase may not start with a stopStartWord
        for (l in (mn:mx)) {
          if (w+l-1>length(wds)) break #Phrase cannot go past the end of block
          if (wds[w+l-1] %in% sew) next  #Phrase may not end with a stopEndword
          phrase=paste(wds[w:(w+l-1)],collapse=" ") #Create a phrase
          if (phrase %in% sp) next #Ignore phrases in stopPhrases
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
  ps$phrase=factor(ps$phrase)
  if (shiny) shiny::incProgress(22) else if (!silent) print(Sys.time())
  #Create an index field in ps for the n-gram
  ps$idx=as.integer(ps$phrase)
  #Create pppos=ps as a data.table
  pppos=data.table(doc=ps$doc,block=ps$block,pos=ps$pos,
                               phrase=ps$phrase,idx=ps$idx,
                 pwrds=ps$pwrds)
  #Create pup with all unique n-grams in ps (but no positions)
  pup=pppos[,.N,by=c("idx","pwrds")]
  colnames(pup)=c("idx","nwords","freq")

  #Aim to have not much more than max.phrases left in the end (but use the freq)
  if (nrow(pup)>max.phrases) {
    min.freq=max(min.freq,pup$freq[order(pup$freq,decreasing=TRUE)][max.phrases])
  }
  pup=pup[pup$freq>=min.freq] #Only keep phrases that occur the minimum or more
  if (nrow(pup)==0) stop(paste("No phrases have frequency greater than",min.freq))
  pup=pup[order(pup$idx)]
  pppos=pppos[pppos$idx %in% pup$idx,] #Only keep positions for phrases that exist

  pppos$ppos1=paste(pppos$doc,pppos$block,pppos$pos,sep='-')
  pppos$ppos2=paste(pppos$doc,pppos$block,(pppos$pos+pppos$pwrds-1),sep='-')
  #Create Rectified positions and frequencies
  pup=pup[pup$nwords!=1] #We don't need to do anything about single words
  pup=pup[order(pup$nwords,pup$freq,decreasing=TRUE)]
  setkeyv(pppos,"idx")
  setindexv(pppos,"ppos1")
  setindexv(pppos,"ppos2")
  if (nrow(pup)!=0) {
    if (shiny) shiny::incProgress(0,message="Rectifying frequencies...") else {
      if (!silent) print("Rectifying frequencies...")
    }
    for (i in 1:nrow(pup)) {
      if (shiny&(i %in% round(1:16*nrow(pup)/16))) shiny::incProgress(1)
      phr=pup$idx[i]
      tp=pppos[pppos$idx==phr]
      frq=nrow(tp)
      if (!pp(tp$phrase[1],frq)) {
        pppos=pppos[!pppos$idx==phr]
      } else {
        phr.pos=paste(tp$doc,tp$block,tp$pos,sep='-')
        for (j in 1:(pup$nwords[i]-1)) phr.pos=c(phr.pos,paste(tp$doc,tp$block,tp$pos+j,sep='-'))
        indx=which((pppos$ppos1 %in% phr.pos | pppos$ppos2 %in% phr.pos)&pppos$idx!=phr)
        if (length(indx)!=0) pppos=pppos[-indx]
      }
    }
  }
  pppos$phrase=factor(pppos$phrase)
  phr=pppos[,.N,by=c("phrase","pwrds")]
  x=list(phrase=as.integer(pppos$phrase),doc=pppos$doc,block=pppos$block,
         pos=pppos$pos, phrases=list(phrase=phr$phrase,pwrds=phr$pwrds,
                                     freq=phr$N))
  class(x)="phraseDoc"
  x
}
################################################################################
# Internal Functions
###################################################################################
# stopStartWords: Obtain a set of words no phrase should start with
###################################################################################
stopStartWords<-function() {
  myStopwords <- c(tm::stopwords('english'), letters, "0","1","2","3","4","5","6","7","8",
                   "9", "=", "via", "also", "mm","st","th","rd","nd","ve","ll","us",
                   "+/-",">/=","<",">","and/or", "95%", "ci","-","+")
  myStopwords[-which(myStopwords=="not"|myStopwords=="no")]
}
###################################################################################
# stopEndWords: Obtain a set of words no phrase should end with
###################################################################################
stopEndWords<-function() {
  myStopwords <- c(tm::stopwords('english'), letters, "0","1","2","3","4","5","6","7","8",
                   "9", "=", "via", "also", "mm","st","th","rd","nd","ve","ll","us",
                   "+/-",">/=","<",">","and/or", "95%", "ci","-","+",
                   "presenting")
  myStopwords
}
###################################################################################
# stopPhrases: Obtain a set of phrases to be excluded
###################################################################################
stopPhrases<-function() {
  c("patients without",
    "et al",
    "significantly higher",
    "patients with")
}
###################################################################################
# getBlocks: Split a document into blocks
# Input:  txt: contents of a document to be split into pieces
# Output: A character vector containing a block of text in each field
###################################################################################
getBlocks<-function(txt) {
  txt=gsub("http[^[:space:]]*", "", txt)
  txt=as.character(txt)
  splits <- unlist(strsplit(unlist(strsplit(txt, "[,] ")),"[].!():;?|{}[]"))
  # Remove extra white space and all leading and trailing spaces
  splits=tm::stripWhitespace(gsub("^\\s+|\\s+$", "", splits))
  splits[splits!=""] #Remove all empty elements
}
###################################################################################
# getWords: Split a block of words into words
# Input:  txt: A block of text
# Output: A character vector containing a a word in each field
###################################################################################
getWords<-function(txt) {
  txt=tolower(txt)
  wds=unlist(strsplit(txt," "))
}
