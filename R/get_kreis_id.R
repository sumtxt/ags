#' @export
get_kreis_id <- function(x, from="SHN"){

	if ( from == "SHN" ) { 
		if( !( n_dist( str_length(x) ) == 1 & 
			unique ( str_length(x) ) == 10) ) stop("The supplied ID does not look like a SHN.")
		return(str_sub(x, 1,5))
		} 

	if ( from == "AGS" ){
		if( !( n_dist( str_length(x) ) == 1 & 
			unique ( str_length(x) ) == 8) ) stop("The supplied ID does not look like a AGS.")
		return(str_sub(x, 1,5))
	}

	}


