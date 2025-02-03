#' @title Reformat old subplotIDs
#'
#' @author
#' Eric Sokol \email{esokol@battelleecology.org} \cr
#'
#' @description Helper function to reformat old subplotIDs
#'
#' @param x old subplotID (text string)
#'
#' @return This function returns a data frame
reformatSubplotID <- function(x){
  x_split <- strsplit(x,split = "\\.") %>% unlist()
  if((length(x_split)==3) & x_split[3]%in%c(1,10,100,400)){
    return(
      paste(x_split[c(1,3,2)], collapse = "_"))
  }else{return(x)}
}
