\name{phm-package}
\alias{phm-package}
\alias{phm}
\docType{package}
\title{
  Phrase Mining
}
\description{
  Obtain all principal phrases from a corpus or a collection of texts and their 
  frequencies in each of those texts. A principal phrase is a phrase that does 
  not cross punctuation marks, does not start with a stop word, with the 
  exception of the stop words "not" and "no", does not end with a stop word, is 
  frequent within those texts without being double counted, and is meaningful to
  the user.
}
\details{
  The function PhraseDoc will extract all principal phrases from a corpus with
  documents or a character vector with texts and creates an object of class
  phraseDoc. The method as.matrix on a phraseDoc object converts the phraseDoc
  to a term-frequency matrix. The function freqPhrases displays the most 
  frequent principal phrases in a phraseDoc object. The function getDocs will
  create a frequency matrix with all documents/texts that contain certain 
  phrases, while getPhrases will create a frequency matrix with all phrases
  present in a specific collection of documents/texts.
}
\author{
Ellie Small

Maintainer: Ellie Small <esmall1@drew.edu>
}
\keyword{ phrases }
