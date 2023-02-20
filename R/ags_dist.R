#' Defines a distance metric for the AGS 
#'
#' @param x,y vectors of AGS values
#' @param landw weight of the Bundesland (Land) integers
#' @param kreisw weight of the Kreis (district) integers
#' @param gemw weight of the Gemeinde (municipality) integers
#' @param ceiling truncate all distances at this value
#'
#' @details
#' The distance metric is defined as
#'
#' abs(x\[1:2\]- y\[1:2\])*landw  + 
#' abs(x\[3:5\]- y\[3:5\])*kreisw + 
#' abs(x\[6:8\]- y\[6:8\])*gemw,
#'
#' where z\[a:b\] means all digits between a and b for integer z.
#'
#' With the default weights, this sum is the absolute difference 
#' between x and y.
#' 
#' @return A numerical vector.
#'
#' @examples 
#' ags_dist(14053,14059)
#'
#' @export
ags_dist <- function(
		x, y, 
		landw = 10^6, 
		kreisw = 10^3, 
		gemw = 1, 
		ceiling = 99999999){
			
    x <- as.numeric(x)
    y <- as.numeric(y)

    # Gemeinde
    gem_x <- (x %% 10^3)
    gem_y <- (y %% 10^3)

    # Land
    ln_x <- floor(x / (10^6))
    ln_y <- floor(y / (10^6))

    # Kreis
    kr_x <- floor((x - (ln_x * 10^6)) / 10^3)
    kr_y <- floor((y - (ln_y * 10^6)) / 10^3)

    gem <- abs(gem_x - gem_y)
    ln <- abs(ln_x - ln_y)
    kr <- abs(kr_x - kr_y)

    dist <- (ln * landw) + (kr * kreisw) + (gem * gemw)
    return(ifelse(dist > ceiling, ceiling, dist))
}
