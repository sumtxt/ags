#' Formats AGS with a Leading Zero 
#' 
#' @param ags Input vector that will be coerced into an integer
#' vector. Factor vectors are first coerced to a character vector
#' and then to an integer vector.
#' 
#' @param type Type of AGS supplied as \code{ags}. Three options are available:
#' 
#' * \code{land}: Bundesland AGS (Bundeslandschlüssel, 2 digits)
#' * \code{district}: District AGS (Kreisschlüssel, 5 digits)
#' * \code{municipality}: Municipality AGS (Gemeindeschlüssel, 8 digits)
#' 
#' The abbreviations \code{l}, \code{d}, and \code{m} are also accepted.
#' 
#' @param verbose If \code{TRUE} the function outputs additional information. 
#' 
#' @return A character vector.
#'
#' @examples 
#' format_ags(c(1,14), type="land")
#' format_ags(c(1002,14612), type="district")
#' format_ags(c(01002000,14612000), type="municipality")
#' 
#' @export
format_ags <- function(ags, type, verbose=FALSE){

    if( is.factor(ags) ) ags <- as.character(ags)
    ags <- as.integer(ags)

    len <- str_length(na.omit(ags))
    len_un <- unique(len)

    if(verbose){
        cat("\n")
        tab <- as.data.frame(table(len))
        colnames(tab)[1] <- "len(AGS)"
        rownames(tab) <- NULL
        print(tab,row.names = FALSE)
        cat("\n")
    }

    if(type %in% c("land", "l") ){
        if( sum(len_un>2) ) { stop("Some values in ags have more than 2 digits.")
        }
        return(str_pad(ags, width=2, side='left', pad="0"))
    } else if(type %in% c("district", "d") ){
        if( sum(len_un>5) ) { stop("Some values in ags have more than 5 digits.")
        }
        return(str_pad(ags, width=5, side='left', pad="0"))
    } else if(type %in% c("municipality", "muni", "m") ){
        if( sum(len_un>8) ) { stop("Some values in ags have more than 8 digits.")
        }
        return(str_pad(ags, width=8, side='left', pad="0"))
    } else {
        stop("Type not recognized.")
    }
    }
