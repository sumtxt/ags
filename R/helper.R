subset_shp <- function(shp, pre, new_version=TRUE){
	if ( new_version == TRUE) { shp <- shp[, c("AGS", "GEN") ] } 
	else { shp <- shp[, c("KEY", "GEN") ] } 
	colnames(shp@data) <- paste(c("AGS", "GEN"), pre, sep="")
	return(shp)
	}

get_calc <- function(id, polyA, polyB, interAB){
	res <- intersect(polyA[id,], polyB[interAB[,id]==TRUE,])
	return(as.data.frame(calc_area(res, pre="")))
	}

calc_area <- function(shp, pre){ 
	areas <- data.frame(area=sapply(shp@polygons, FUN=function(x) {slot(x, 'area')}) )
	row.names(areas) <- sapply(shp@polygons, FUN=function(x) {slot(x, 'ID')})
	colnames(areas) <- paste("area", pre, sep="")
	shp <- spCbind(shp, areas)
	return(shp)
	}

#' @export
prep_vg250_shp <- function(shp, pre, new_version=new_version){
	shp <- subset_shp(shp, pre=pre, new_version=new_version)
	shp <- spTransform( shp, CRS( "+init=epsg:25832" ) ) 
	shp <- gBuffer(shp, byid=TRUE, width=0)
	shp <- aggregate(shp, by=paste('AGS',pre,sep=""), sums=list(list(unique, paste('GEN',pre,sep="") ) ) )
	return(shp)
	}

coarser_areashare <- function(area, tol){
	area <- ifelse( area > 1-tol, 1, area )
	area <- ifelse( area < tol, 0, area )
	}

n_dist <- function(x) length(unique(x))

n_match <- function(x,y) sum( unique(x) %in% unique(y) ) 
