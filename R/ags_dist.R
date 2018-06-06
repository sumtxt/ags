#' Calculates the distance between two AGS numbers 
#' 
#' 
#' @details 
#' The default is just the difference between 
#' the two integer numbers. 
#' 
#' 
#' @export
ags_dist <- function(x,y, landw=10^6, kreisw=10^3, gemw=1, ceiling=99999999) {
	
	x <- as.numeric(x)
	y <- as.numeric(y)

	# Gemeinde
	gem_x <- (x %% 10^3)
	gem_y <- (y %% 10^3)

	# Land 
	ln_x <- floor(x/(10^6))
	ln_y <- floor(y/(10^6))

	# Kreis
	kr_x <- floor((x - (ln_x*10^6))/10^3)
	kr_y <- floor((y - (ln_y*10^6))/10^3)

	gem <- abs(gem_x-gem_y)
	ln <- abs(ln_x-ln_y)
	kr <- abs(kr_x-kr_y)
	
	dist <-  (ln*landw)  + (kr*kreisw) + (gem*gemw)
	ifelse(dist>ceiling,ceiling,dist)
	
	}