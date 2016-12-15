#' @export
merge_vg250_shp <- function(base_shp, past_shp, base_new_version=TRUE, past_new_version=TRUE, tol=0.025, verbose=0){
	
	if(verbose != 0) cat("Cleaning Polygons...\n")
	base_shp <- prep_vg250_shp(base_shp, pre="_base", new_version=base_new_version)
	past_shp <- prep_vg250_shp(past_shp, pre="_past", new_version=past_new_version)

	past_shp <- calc_area(past_shp, pre="_past")

	if(verbose != 0) cat("Calculating Intersections...\n")
	join_test <- gIntersects(base_shp,past_shp, byid=TRUE)


	N <- nrow(base_shp)
	iterator <- seq(1, N)

	if(verbose != 0) { 
		cat("Calculating areas .... \n")
		tmp <- pblapply(iterator, function(x) get_calc(x, base_shp,past_shp, join_test) )
	} else { 
		tmp <- lapply(iterator, function(x) get_calc(x, base_shp,past_shp, join_test) )
		}

	join_data <- do.call(rbind, tmp)

	join_data$share <- join_data$area/join_data$area_past
	join_data$share <- coarser_areashare(join_data$share, tol=tol)

	join_data <- subset(join_data, share!=0)

	if ( verbose != 0 ){ 

		nBaseID <- n_dist(base_shp$AGS_base)
		nPastID <- n_dist(past_shp$AGS_past)

		nBaseIDmatch <- n_match(join_data$AGS_base, base_shp$AGS_base)
		nPastIDmatch <- n_match(join_data$AGS_past, past_shp$AGS_past)

		cat("n(base_shp): ", nBaseID, "\n") 
		cat("n(past_shp): ", nPastID, "\n" )
		cat("n(base_shp) in n([join_shp]): ", nBaseIDmatch, "\n" )
		cat("n(past_shp) in n([join_shp]): ", nPastIDmatch, "\n" )

		}

	return(join_data)
	}
