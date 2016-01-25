##	Thresholds for N and S transect boundaries:
##		332140 N
##		330460 S
##
##	Northern transect point:
##		181000, 331500
##
##	Southern transect point:
##		180000, 330500

library(sp)
library(rgeos)

data(meuse.grid)

##	Slice width in meters:
slice_width <- 500


##	Calculate angle of line for transects:
x1 <- 181000
y1 <- 331500

x2 <- 180000
y2 <- 330500

dx <- x2 - x1
dy <- y2 - y1

angle <- atan(dx/dy)

##	Now, find the endpoints of the transects, with that angle passing through our 
##		original line but ending with the N and S transect boundary y values:
new_y <- 332140
dy <- new_y - y2
dx <- tan(angle) * dy
new_x <- x2 + dx
N_transect <- list(x=new_x, y=new_y)

new_y <- 330460
dy <- N_transect$y - new_y
dx <- tan(angle) * dy
new_x <- N_transect$x - dx
S_transect <- list(x=new_x, y=new_y)


##	So, to create a series of lines that have a distance perpendicular to
##		each other equal to the slice_width, the next one to the west is:
dx <- (1 / cos(angle)) * slice_width
new_x1 <- S_transect$x - dx
new_y1 <- S_transect$y
new_x2 <- N_transect$x - dx
new_y2 <- N_transect$y


##	Create series of lines based on dx and the coordinates along transects:
##		Based on solution at StackOverflow.com:
##			http://stackoverflow.com/questions/20531066/convert-begin-and-end-coordinates-into-spatial-lines-in-r

##	Raw list to store Lines objects
x2_transect <- c( seq(from=N_transect$x, to=3903000, by=dx), seq(from=N_transect$x - dx, to=-1658000, by=-1*dx) )
y2_transect <- rep(N_transect$y, length(x2_transect))

dy_trans <- N_transect$y - S_transect$y
dx_trans <- tan(angle) * dy_trans
x1_transect <- x2_transect - dx_trans
y1_transect <- rep(S_transect$y, length(x2_transect))

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


l <- vector("list", nrow(begin.coord))

for (i in seq_along(l)) {
	l[[i]] <- Lines(list(Line(rbind(begin.coord[i,], end.coord[i,]))), as.character(i))
}

sl <- SpatialLines(l)


##	Plot to confirm locations:
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
proj4string(meuse.grid) <- CRS(paste("+init=epsg:28992","+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812"))
plot(meuse.grid)
lines(sl)

##	Load our Census Shapefile to pull coordinate system from:
names(begin.coord) <- c("begin_x", "begin_y")
names(end.coord) <- c("end_x", "end_y")
sldf <- SpatialLinesDataFrame(sl, data.frame("lineID"=1:nrow(begin.coord), begin.coord, end.coord))

##	Set CRS from existing census file:
proj4string(sldf) <- proj4string(meuse.grid)


##	Do the slicing using RGEOS as the ArcGIS method is extremely slow.  Note that this produces
##		polygons whose boundaries are 0.000002 meters separated so they are not ideal for 
##		further processing but they should work ok for our purposes:
##			http://stackoverflow.com/questions/5726297/cut-polygons-using-contour-line-beneath-the-polygon-layers
blpi <- gBuffer(sldf, byid=TRUE, id=sldf$lineID, width = slice_width/2)
plot(blpi, col="red", add=T)


##	If necessary, write out lines and buffers:

#writeOGR(sldf, ".", "lines", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
#writeOGR(blpi, ".", "slices", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
