##	Thresholds for N and S transect:
##		6451870 N
##		2317627 S
##
##	Northern Hu point:
##		1525348.068, 6113380.046
##
##	Southern Hu point:
##		-672622.094, 3178865.248

setwd("C:/tmp/CHN/")

##	Slice width in meters:
slice_width <- 10000


##	Calculate angle of line for Northern Hu point:
x1 <- -672622.094
y1 <- 3178865.248

x2 <- 1525348.068
y2 <- 6113380.046

dx <- x2 - x1
dy <- y2 - y1

angle <- atan(dx/dy)

##	Now, find the endpoints of the line, with that angle passing through our 
##		Hu line but ending with the N and S transect y values:
new_y <- 6451870
dy <- new_y - y2
dx <- tan(angle) * dy
new_x <- x2 + dx
Hu_N_transect <- list(x=new_x, y=new_y)

new_y <- 2317627
dy <- Hu_N_transect$y - new_y
dx <- tan(angle) * dy
new_x <- Hu_N_transect$x - dx
Hu_S_transect <- list(x=new_x, y=new_y)


##	So, to create a series of lines that have a distance perpendicular to
##		each other equal to the slice_width, the next one to the west is:
dx <- (1 / cos(angle)) * slice_width
new_x1 <- Hu_S_transect$x - dx
new_y1 <- Hu_S_transect$y
new_x2 <- Hu_N_transect$x - dx
new_y2 <- Hu_N_transect$y


##	Create series of lines based on dx and the coordinates along transects:
##		Based on solution at StackOverflow.com:
##			http://stackoverflow.com/questions/20531066/convert-begin-and-end-coordinates-into-spatial-lines-in-r
require(sp)

##	Raw list to store Lines objects
x2_transect <- c( seq(from=Hu_N_transect$x, to=3903000, by=dx), seq(from=Hu_N_transect$x - dx, to=-1658000, by=-1*dx) )
y2_transect <- rep(Hu_N_transect$y, length(x2_transect))

dy_trans <- Hu_N_transect$y - Hu_S_transect$y
dx_trans <- tan(angle) * dy_trans
x1_transect <- x2_transect - dx_trans
y1_transect <- rep(Hu_S_transect$y, length(x2_transect))

begin.coord <- data.frame("x"=x1_transect, "y"=y1_transect)
begin.coord <- begin.coord[order(begin.coord$x),]
end.coord <- data.frame("x"=x2_transect, "y"=y2_transect)
end.coord <- end.coord[order(end.coord$x),]

##	NOTE: You have to be careful with rownames and the sp ...DataFrame()
##		functions, as the rownames will override the order of the data.frame
##		items when the object is being created, and the original data and
##		spatial objects may not align, for example if you order the data
##		and then create the spatial objects.  Therefore we are overwriting
##		the rownames...
row.names(begin.coord) <- as.character(1:nrow(begin.coord))
row.names(end.coord) <- as.character(1:nrow(end.coord))


require(sp)

l <- vector("list", nrow(begin.coord))

for (i in seq_along(l)) {
	l[[i]] <- Lines(list(Line(rbind(begin.coord[i,], end.coord[i,]))), as.character(i))
}

sl <- SpatialLines(l)

##	Load our Census Shapefile to pull coordinate system from:
require(rgdal)
census <- readOGR(dsn="data", layer="census_covariates")

names(begin.coord) <- c("begin_x", "begin_y")
names(end.coord) <- c("end_x", "end_y")
sldf <- SpatialLinesDataFrame(sl, data.frame("lineID"=1:nrow(begin.coord), begin.coord, end.coord))

##	Set CRS from existing census file:
proj4string(sldf) <- proj4string(census)

##	Write it out:
writeOGR(sldf, "output", "hu_lines", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)



##	Do the slicing using RGEOS as the ArcGIS method is extremely slow.  Note that this produces
##		polygons whose boundaries are 0.000002 meters separated so they are not ideal for 
##		further processing but they should work ok for our purposes:
##			http://stackoverflow.com/questions/5726297/cut-polygons-using-contour-line-beneath-the-polygon-layers
require(rgeos)

census_diss <- readOGR(dsn="data", layer="census_covariates_diss")
#lpi <- gIntersection(census_diss, sldf)
#blpi <- gBuffer(lpi, width = 0.000001)
#dpi <- gDifference(census_diss, blpi)
#dpi_df <- SpatialPolygonsDataFrame(dpi, data.frame("diss"=1))
#writeOGR(dpi_df, "output", "census_covariates_diss_slices", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)

##	The above works but creates a single multi-part polygon output.  Insteac, we'll
##		grow the lines to have a buffer that matches the distance between lines...
blpi <- gBuffer(sldf, byid=TRUE, id=sldf$lineID, width = slice_width/2)
writeOGR(blpi, "output", "census_covariates_diss_slices", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)

