#' Transforms the name of a Bundesland into the Bundesland ID 
#'
#'
#' @param x string (or vector of strings) to be converted into ID
#' @param render return ID (default), \code{name}, the \code{label}, English name (\code{name_eng})?  
#' 
#' @details The first two digits of the 8 digit Amtlicher Gemeindeschlüssel (AGS) encode  
#' the Bundesland (Bundesland ID). This function converts the name of Bundesland into 
#' the Bundesland ID, or the standardized (English) name, the label. 
#'  
#' @return a character vector of valid Bundesland ID
#'
#' @seealso \link{get_bundesland}
#'
#' @examples 
#'  \dontrun{
#'  
#'   code_bundesland( c("NRW", "Berlin") )
#' 
#'  }
#' 
#' 
#' 
#' 
#' @export
code_bundesland <- function(string, render="id"){
	code <- rep(NA,length(string))
	for(i in 1:16){
		code <- ifelse(str_detect(string, bl[i,"regex"]), bl[i,render], code) 
	}
	return(code)
	}

