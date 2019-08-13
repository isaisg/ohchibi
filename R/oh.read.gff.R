#' Read a gff3 file (IMG Data)  and converts into df 
#' Aditionally extracts the gene_id and locus_tag and place them into a new column
#'
#' 
#' @keywords gff,IMG,JGI,read
#' @export
#' @examples
#' oh.read.gff()



oh.read.gff <- function(file = NULL,withOGs = FALSE,skip = 1,comment.char = \"\"){
  if(withOGs == TRUE){
    tfile <- read.table(file = file,header = F,
          skip = skip,comment.char = comment.char,quote = "",sep = "\t")
  colnames(tfile) <-  c("seqid", "source", "type", "start", "end", 
                        "score", "strand", "phase", "attributes","orthogroup_id")
    gid <- NULL
    glocus <- NULL
    for(i in 1:nrow(tfile)){
      #Check if the type is crispr
      if(tfile$type[i] == "CRISPR"){
        gid <- c(gid,NA)
        glocus <- c(glocus,NA)
      }else{
        gid <- c(gid,gsub(pattern = "ID=(.*?)\\;locus_tag=(.*?)\\;.*",
                          replacement ="\\1",tfile$attributes[i]))
        glocus <- c(glocus,gsub(pattern = "ID=(.*?)\\;locus_tag=(.*?)\\;.*",
                                replacement ="\\2",tfile$attributes[i]))
      }
    }
    tfile$gene_oid <- factor(gid)
    tfile$locus_tag <- factor(glocus)
    return(tfile)

  }else{
    tfile <- read.table(file = file,header = F,
               skip = 1,comment.char = "",quote = "",sep = "\t")
    colnames(tfile) <-  c("seqid", "source", "type", "start", "end", 
                          "score", "strand", "phase", "attributes")
    gid <- NULL
    glocus <- NULL
    for(i in 1:nrow(tfile)){
      #Check if the type is crispr
      if(tfile$type[i] == "CRISPR"){
        gid <- c(gid,NA)
        glocus <- c(glocus,NA)
      }else{
        gid <- c(gid,gsub(pattern = "ID=(.*?)\\;locus_tag=(.*?)\\;.*",
                          replacement ="\\1",tfile$attributes[i]))
        glocus <- c(glocus,gsub(pattern = "ID=(.*?)\\;locus_tag=(.*?)\\;.*",
                                replacement ="\\2",tfile$attributes[i]))
      }
    }
    tfile$gene_oid <- factor(gid)
    tfile$locus_tag <- factor(glocus)
    return(tfile)
  }

}

