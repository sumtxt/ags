#' Convert the Name or the AGS of a Bundesland 
#'
#' @param sourcevar Vector which contains the codes or names 
#' to be converted. 
#' 
#' @param origin The following options are available: 
#' 
#' * \code{ags}: AGS (default). 
#' * \code{name}: Bundesland name.
#'
#' @param destination The following options are available: 
#' 
#' * \code{ags}: Bundesland AGS (default).
#' * \code{iso}: The Bundesland two-character abbreviation.
#' * \code{name}: Bundesland name.
#' * \code{name_eng}: Bundesland name in English.
#'
#' @param factor If \code{TRUE} returns ordered factor.
#' 
#' @details This function converts a string of Bundesland names into
#' the AGS, the standardized (English) name, or the Bundesland abbreviation.
#' 
#' If \code{origin="AGS"}, the first two digits will be used to identify 
#' a Bundesland. It is therefore important that \code{sourcevar} is supplied 
#' as a character vector with a leading zeros if applicable.
#'
#' @return A character vector. 
#'
#' @examples
#' library(dplyr)
#' data(btw_sn)
#'
#' btw_sn %>% 
#'  mutate(bl=code_bundesland(district, origin="ags", 
#'      destination="name"))
#' 
#' @seealso  [format_ags()] for formatting AGS. 
#' 
#' @import stringr
#' @export
code_bundesland <- function(
    sourcevar, 
    origin="ags", 
    destination="name", 
    factor=FALSE) {

    if(is.factor(sourcevar)) sourcevar <- as.character(sourcevar)

    output <- rep(NA, length(sourcevar))

    if (!(destination %in% c("ags", "name","name_eng", "iso"))) {
        stop("Option for destination unrecognized.")
    }

    if (!(origin %in% c("name", "ags"))) {
        stop("Option for origin unrecognized.")
    }

    if(origin=="name"){
        for (i in 1:16) {
            output <- ifelse(
                str_detect(sourcevar, bl[i, "regex"]), 
                bl[i, destination], 
                output
                )
        }
    }

    if(origin=="ags"){
        
        if( !is.character(sourcevar) ) { stop(sourcevar, 
            "needs to be a character vector.")
        }
        sourcevar <- str_sub(sourcevar, 0, 2)
        sourcevar <- as.integer(sourcevar)

		if (destination=="name") output <- bl$name[sourcevar]
		if (destination=="name_eng") output <- bl$name_eng[sourcevar]
		if (destination=="iso") output <- bl$iso[sourcevar]
        if (destination=="ags") output <- bl$ags[sourcevar]

    }

    if (factor) {
        output <- factor(output, 
            levels=bl[order(bl$ns_order),destination], 
            ordered=TRUE)
    }

    return(output)
}
