#' @title Convert old NEON subplotIDs to new format
#'
#' @author Eric Sokol \email{esokol@battelleecology.org} \cr
#'
#' @description Helper function to reformat old subplotIDs. SubplotIDs are updated from "X.X.X" format to "X_X_X" format.
#'
#' @param x Old subplotIDs with "X.X.X" format. [character]
#'
#' @return Data frame with subplotIDs reformatted to new "X_X_X" style.
 
reformatSubplotID <- function(x) {
  
  x_split <- strsplit(x, 
                      split = "\\.") %>% 
    unlist()
  
  if ((length(x_split) == 3) & x_split[3] %in% c(1, 10, 100, 400)) {
    
    return(paste(x_split[c(1, 3, 2)], collapse = "_"))
    
  } else {return(x)}
  
}
