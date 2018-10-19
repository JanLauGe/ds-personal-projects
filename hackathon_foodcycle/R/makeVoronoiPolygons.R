
makeVoronoiPolygons <- function(coords, poly_country) {
  
  # get IDs
  IDs <- sapply(strsplit(poly_country$names, ":"), function(x) x[1])
  
  # make map into polygon
  poly_country <- map2SpatialPolygons(poly_country, IDs=IDs,
    proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # limit extent
  bb <- bbox(poly_country)
  rw <- as.numeric(t(bbox(poly_country)))
  
  # create sp poly voroni map
  z <- deldir(pull(coords, x), pull(coords, y), rw=rw)
  w <- tile.list(z)
  
  polys <- vector(mode='list', length=length(w))
  
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  
  poly_voronoi <- SpatialPolygons(polys)
  
  # out <- SpatialPolygonsDataFrame(
  #   poly_voronoi, 
  #   data.frame(
  #     x=coords[,1], 
  #     y=coords[,2],
  #     row.names=sapply(slot(poly_voronoi, 'polygons'), 
  #                      function(x) slot(x, 'ID'))))
  
  return(poly_voronoi)
}
