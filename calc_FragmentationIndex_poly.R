library(sf)

##### Example data 1 (GIS data (points)) #####

#here, original data given in table and convert to GIS object (st_points)
#instead, you can import GIS files like .shp by st_read function

points_df <- data.frame(rec_name = paste("record", c(1:4), sep=""), Ne = c(100, 200, 350, 250), 
			x = c(1, 2, 3, 4), y = c(1, 3, 2, 4)) 
print(points_df)

#makes GIS (sf points) object from the data.frame

points_sf <- st_as_sf(points_df, coords = c("x", "y"))

plot(points_sf$Ne)

##### Example data 2 (GIS data (polygon)) ####

record1 <- st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)))) 
record2 <- st_polygon(list(rbind(c(2, 0), c(2, 1), c(3, 1), c(3, 0), c(2, 0)))) 
record3 <- st_polygon(list(rbind(c(1, 2), c(1, 3), c(2, 3), c(2, 2), c(1, 2)))) 
record4 <- st_polygon(list(rbind(c(3, 2), c(3, 3), c(4, 3), c(4, 2), c(3, 2)))) 
polys <- st_sf(geometry = st_sfc(record1, record2, record3, record4), 
			rec_name = paste("record", c(1:4), sep=""), Ne = c(100, 200, 350, 250)) 

plot(polys[,"Ne"])

##### Set the parameter

dist_thre <- 2  #threshold for dispersal distance

param_decay <- 0.7 
#if you prefer distance decay instead of simple thresholding
#param_decay = 0.7 results in distance decay of ca. 0.5 at a unit distance (dist=1) 

###### 
# Function to calculate distance-dependent weight (default is threshold function by dispersal distance) 

distanceDecay <- function(d, dist_thre=NULL, param_decay=NULL, method="threshold"){
	#method: could be "threshold" or "exponential" 
	#param_decay: parameter for distance decay (the smaller the value, the slower the decay)

	if(method=="threshold"){
		weight <- (d <= dist_thre)*1
	} 

	if(method=="exponential") {
		weight <- exp(param_decay*(-1)*d)
	}

	return(weight)
}


##### Calculate distance between point or polygons 

dists <- st_distance(points_sf) 

#dists <- st_distance(polys)
# in the case of polygon

print(dists)

##### Calculate distance-decay weight (here, 0 or 1 by threshold distance)

dist_weight <- distanceDecay(dists,dist_thre)

#dist_weight <- distanceDecay(dists,param_decay=param_decay, method="exponential")
#in the case of distance decay weight with exponential function

print(dist_weight)

##### Calculate the neighbor Ne 

neighbor_Ne <- colSums(points_df$Ne * dist_weight)
names(neighbor_Ne) <- points_df$rec_name

print(neighbor_Ne) 

##### Calcurate the Ne fragmentation index

NFI <- sum(neighbor_Ne>500)/length(neighbor_Ne)
# it should be > 5000 when the population size is Nc

print(NFI)



