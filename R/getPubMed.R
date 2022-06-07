#########1#########2#########3#########4#########5#########6#########7#########8
#' Create a data table from a text file in PubMed format
#'
#' This function takes as input a file produced via PubMed in PubMed format
#' and outputs a data frame with the id equal to the PMID, text equal to the
#' abstract, date, title, and author for each publication in the file.
#'
#' @param file path to the PubMed file
#' @return A data table with a row for each publication holding the id
#' equal to the PMID, text equal to the abstract, date, title, and author for
#' that publication.
#' @examples
#' #Go to Pubmed and enter search criteria, save the result to PubMed format.
#' #If the file is called pubmed_result.txt and located in the current
#' #directory:
#' #PM=getPubMed("pubmed_result.txt")
#' #Will load the data from the search into a data table called PM
#' @import data.table
#' @export
################################################################################
getPubMed<-function(file) {
  x=data.table::fread(file,sep="\n",header=FALSE,
                      strip.white=FALSE)[[1]]
  PMID=NULL;date=NULL;title=NULL;BTI=NULL;abstract=NULL;cur=0;i=1;author=NULL
  while (i<=length(x)) {
    if (substr(x[i],1,4)=="PMID") {
      cur=cur+1
      PMID[cur]=as.numeric(substring(x[i],7))
      i=i+1
    }
    if (substr(x[i],1,2)=="TI") {
      title[cur]=substring(x[i],7)
      i=i+1
      while (substr(x[i],1,2)=="  ") {
        title[cur]=paste(title[cur],substring(x[i],7))
        i=i+1
      }
    }
    if (substr(x[i],1,3)=="BTI") {
      BTI[cur]=substring(x[i],7)
      i=i+1
      while (substr(x[i],1,2)=="  ") {
        BTI[cur]=paste(BTI[cur],substring(x[i],7))
        i=i+1
      }
    }
    if (substr(x[i],1,2)=="AB") {
      abstract[cur]=substring(x[i],7)
      i=i+1
      while (substr(x[i],1,2)=="  ") {
        abstract[cur]=paste(abstract[cur],substring(x[i],7))
        i=i+1
      }
    }
    if (substr(x[i],1,2)=="AU") {
      author[cur]=substring(x[i],7)
      i=i+1
    }
    if (substr(x[i],1,4)=="EDAT") {
      date[cur]=as.character(as.Date(substring(x[i],7,16),"%Y/%m/%d"))
      i=i+1
    } else i=i+1
  }
  n<-max(length(PMID),length(date),length(title),length(abstract))
  if (length(PMID)!=n) PMID[n]=NA
  if (length(date)!=n) date[n]=NA
  if (length(title)!=n) title[n]=NA
  if (length(BTI)!=n) BTI[n]=NA
  if (length(author)!=n) author[n]=NA
  #If the title is missing, use the BTI instead
  title[is.na(title)]=BTI[is.na(title)]
  if (length(abstract)!=n) abstract[n]=NA
  #If the abstract is missing, use the title as abstract
  abstract[is.na(abstract)]=title[is.na(abstract)]
  data.table::data.table(id=PMID,text=abstract,PMID,date,title,author)
}
