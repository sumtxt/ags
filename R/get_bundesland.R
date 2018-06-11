#' Get the name or label of the Bundesland from the AGS
#'
#'
#' @param num a \code{vector} of codes to be converted 
#' @param render Return \code{name} (default), the \code{label}, the English name (\code{name_eng})?  
#' @param ags Only use the first two digits (default)? 
#' 
#' @details The first two digits of the 8 digit Amtlicher Gemeindeschlüssel (AGS) encode  
#' the Bundesland. This function converts them either to the name of the Bundesland 
#' (if \code{render="name"}) or to the two-character label (\code{render="label"}). 
#' 
#' \code{num} can either be the 8 digit AGS (set \code{ags=TRUE})
#' or the extracted two leading digits (\code{ags=FALSE}). If \code{num} is not 
#' a numeric vector, it is coerced to one.
#' 
#'  
#' @return a character vector of names or labels. 
#'
#'
#' @examples 
#'  \dontrun{
#'  
#'   get_bundesland( c("02", "03") )
#'   get_bundesland( c("03254021"), ags=TRUE )
#' 
#'  }
#' 
#' 
#' 
#' 
#' @export
get_bundesland <- function(num, render="name", ags=FALSE){
		if (ags==TRUE) num <- substr(num, 0,2)
		if ( !is.numeric(num) ) num <- as.numeric(num)
		if (render=="name") return(bl$name_de[num])
		if (render=="label") return(bl$label[num])
		if (render=="name_eng") return(bl$name_eng[num])
		return(num)
		}

