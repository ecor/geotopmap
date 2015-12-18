# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL
#'
#' Geographical representation of a data frame or raster map.
#' 
#' @param x a \code{Raster*} class object
#' @param map geograghical map on which \code{x} is plotted. It is an object returned by \code{\link{get_map}} or similars.
#' @param latlon_crs string containing the utilized latitude longitude Coordinate Refarance System. See default in \code{Usage}.
#' @param layer brick layer utilized for geographical plotting. Default is 1. 
#' @param title string title of the graphic 
#' @param label string title (label) of the legend. It is used as the name of the scale if \code{scale.fill.gradient} is \code{TRUE}, otherwise it is ignored.
#' @param high colour for low end of gradient. See \code{\link{scale_fill_gradient}}.
#' @param low  colourf or high end of gradient. See \code{\link{scale_fill_gradient}}. 
#' @param alpha alpha coefficient. See \url{http://en.wikipedia.org/wiki/Alpha_compositing}. 
#' @param facet_wrap logical value. If \code{TRUE} it uses \code{\link{facet_wrap}} to print all plots. 
#' @param nrow,ncol number of rows and columns. See \code{\link{facet_wrap}}. 
#' @param scale.fill.gradient logical value. If it is \code{TRUE} (Default), it uses \code{\link{scale.fill.gradient}}
#' @param scale alternative parameter to \code{scale.fill.gradient}, it is a term potentially added for color scale (see \code{\link{scale_colour_hue}} o similars). It is used only if it is not \code{NULL} or \code{scale.fill.gradient} is \code{FALSE}
#' @param plot logical value. If \code{TRUE} (Default) the function also plots the map, otherwise returns its value without any preliminary plot.
#' @param scale_colour_gradientn_scale color scale for \code{\link{scale_colour_gradientn}}. It is used if it is nott \code{NULL}. Default is \code{NULL}.
#' @param method see \code{\link{projectRaster}}
#' @param palette palette function used for \code{\link{scale_fill_manual}} . 
#' @param shape,size see \code{\link{geom_point}}
#' @param ... further arguments
#' 
#' @return A \code{"ggplot"} object.
#' @export 
#' @seealso \code{\link{geom_point}},\code{\link{ggmap}},\code{\link{facet_wrap}}
#' 
#' @importFrom raster as.data.frame 
#' @import scales
#' @import ggmap
#' @import ggplot2
# @import ggmap
#' @note Useful link: \url{http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf}
#' @examples
#'  
#' library(geotopVis)
#' data(soilwatercontent)
#'
#' x <- plotOn(x=soilwatercontent[[1]])
#' 
#' ##' ANOTHER EXAMPLE 
#' library(geotopbricks)
#' # The data containing in the link are only for educational use
#' wpath <- "http://www.rendena100.eu/public/geotopbricks/simulations/idroclim_test1"
#' tz <-  "Etc/GMT-1"
#' when <- as.POSIXct("2002-03-22",tz=tz)
#' 
#' # a 2D map:
#' x_e <- "SnowDepthMapFile"
#' # Not Run: uncomment the following line
#' m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
#'                            tz=tz,use.read.raster.from.url=TRUE)
#' ## NOTE: set use.read.raster.from.url=FALSE (default)
#' # if the "wpath" directorty is in the local file system.
#' # Not Run: uncomment the following line
#' m/10
#' 
#' plot(m/10,main="Snow Depth")
#' 
#' 
#' 
#'  location = c(10.5,45.5,12.5,46.5)
#'  map=get_map(location = location) ###, zoom = 9)
#' 
#' 
#' xmap <- plotOn(m/10,map=map,high="blue",low="white",title="Snow Depth",label="depth[cm]",plot=TRUE,alpha=c(0,0.3),extent="device")
#' xmap





#' 
#' 
#' 




plotOn <- function(x,
		map=ggmap::get_map(location = 'trentino', zoom = 9),
		latlon_crs=as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),layer=1,
		legend="bottomright",
		title="Soil Water Content",
		label="swc",
		high="blue",
		low="white",
		alpha=c(0.1,0.8),
		facet_wrap=FALSE,
		nrow=NULL,
		ncol=NULL,
		range=NULL,
		scale="continuous_2",
		scale.fill.gradient=TRUE,plot=TRUE,
		scale_colour_gradientn_scale=NULL,
		extent="normal",
		method=c("bilinear","ngb"),
		palette=rainbow,
		shape=15,size=NA,
		...) 
{
	
	out <- NULL 
	discrete_scales <- c("landuse","soiltype","discrete")
	
	if (scale %in% discrete_scales) method <- "ngb"
	if (length(layer)>1) facet_wrap <- TRUE
	
	condRaster <- (class(x) %in% c("RasterBrick","RasterStack","RasterLayer"))
	conddf <- (is.data.frame(x))
	if (conddf) {
		
		conddflatlon <- (c("lat") %in% names(x)) & (c("lon") %in% names(x))
	
		
		
	}
	
	
	if (condRaster) {
		y <- raster::projectRaster(x,crs=latlon_crs,method=method)
	
		df <- as.data.frame(y,xy=TRUE)
	
		names(df)[names(df)=="x"] <- "lon"
		names(df)[names(df)=="y"] <- "lat"
	
		names_xy <- names(df)[names(df) %in% c("lat","lon")]
	} else if (conddflatlon){ 
	
		df <- x 
		names_xy <- names(df)[names(df) %in% c("lat","lon")]
		df$label <- df[,label]
	} else { 
	
		stop("x has incorrect type!")
	}
	

	isNA <- is.na(as.data.frame(df[,!(names(df) %in% names_xy)])[,1])

	df <- df[!isNA,]	
	
	
	names <- names(df)
	names(names) <- names 
	names <- names[!(names %in% names_xy)]
	layer <- names[layer]

	

	df <- df[,c(names_xy,layer)]
	if (facet_wrap) {
		
		
		df <- reshape2::melt(df,id.vars=names_xy)
		df <- df[,c(1,2,4,3)]
		names(df)[3] <- "labelv" ## DA LAVORARCI

		
	} else {
		
		
		names(df) <- c(names_xy,"labelv")
		
		
		
	}
	
	if (scale %in% c("landuse")) { 
	
		df$labelv <- factor(df$labelv)
	}
		
	aes <- aes(x=lon,y=lat,fill=labelv,colour=labelv,alpha=labelv) ###labelv,alpha=labelv)
		
	
		
		
		
	

	if (!is.null(range)) range <- range(df[,labelv])
	
	if (length(alpha)==1) alpha <- c(alpha,apha)
	if (length(alpha)>2) alpha <- alpha[1:2]
	alpha_range <- alpha
	
	if (is.null(size)) size <- NA 
	if (is.null(legend)) legend <- "none"
	if (is.na(legend))   legend <- "none"
	if (legend==FALSE)   legend <- "none"

	if (legend=="none") {
		
		guide <- FALSE
	} else {
		
		guide <- TRUE
	}
	if (is.na(size)) {
		
		p <- ggmap(map,legend=legend,extent=extent,...)+geom_point(data=df,mapping=aes,shape=shape) 
		
		
	} else {
		
		p <- ggmap(map,legend=legend,extent=extent,...)+geom_point(data=df,mapping=aes,shape=shape,size=size) 
		
		
	}
	
	#+####,shape=15)
	if (is.null(scale)) {
		scale.fill.gradient <- TRUE 
	} else {
		
		scale.fill.gradient <- FALSE
	}	
	
####	if ((scale.fill.gradient | is.null(scale)) & (is.null(scale_colour_gradientn_scale))) {
	if (scale=="continuous_2") {
		
		if (guide==TRUE) guide <- formals(scale_fill_gradient)$guide
		p<- p+scale_fill_gradient(name=label,low=low,high=high,limits=range,guide=guide)+scale_color_gradient(name=label,low=low,high=high,limits=range,guide=guide)+scale_alpha_continuous(name=label,range = alpha_range,guide=FALSE) ## 
		## scale_alpha_continuous(..., range = c(0.1, 1))
		##scale_name=label
	
	} else if (scale=="landuse") { 
		
		## http://stackoverflow.com/questions/11625005/ggplot2-plot-discrete-factors-with-a-set-of-similar-colours
		ncc <- length(unique(df$labelv))
		fun_palette <- palette
		if (guide==TRUE) guide <- formals(discrete_scale)$guide
		p <- p+scale_fill_manual(name=label,values=fun_palette(ncc),guide=guide)+scale_color_manual(name=label,values=fun_palette(ncc),guide=guide)+scale_alpha_discrete(name=label,range = c(max(alpha_range),max(alpha_range)),guide=FALSE)##+scale_color_discrete(name=label,palette=rainbow)   #scale_colour_brewer(type = "seq", palette = 3) ##scale_colour_hue()
		
	} else if (scale=="other")   {
	}
#	if (!is.null(scale)) {
#		
#		for (it in scale) p <- p+it
#		
#		
#	}
	if ((!is.null(title)) | (!is.na(title))) p <- p+ggtitle(title)
	if (facet_wrap) p <- p+facet_wrap(~ variable,nrow=nrow,ncol=ncol)
	##		scale_alpha(range=range(alpha))
	
	## check alpha before XXX
	
##	p <- p+geom_text(mapping=aes(x=lon,y=lat,label=id,color=obs),data=x,size = 3, vjust = 0, hjust = -0.5)
##	if (print.p)
	if (plot) print(p)
	
	
	return(p)
	
	
}


