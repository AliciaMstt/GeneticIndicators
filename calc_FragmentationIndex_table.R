# create example data 

points <- data.frame(rec_name = paste("record", c(1:4), sep=""), 
			x = c(1, 2, 3, 4), y = c(1, 3, 2, 4), Ne = c(100, 200, 350, 250)) 
print(points)
 
coord_col <- c("x","y")

# Set threshold (dispersal distance) 

disp_dist <- 2 


# Calculate pairwise differences between coordinates

n_points <- dim(points)[1]
dim_coord <- length(coord_col)

mat1 <- matrix(rep(t(points[ , coord_col]),dim(points)[1]),n_points*dim_coord)
mat2 <- t(matrix(rep(as.matrix(points[ , coord_col]),n_points),n_points))

diffs <- mat1-mat2 

# Calculate distance between points

dists <- sqrt(colSums(matrix(diffs,2)^2)) 
dists <- matrix(dists, dim(points)[1])

# Calculate neighborhood Ne 

neighbor_Ne <- colSums(points$Ne * (dists < disp_dist)) 
names(neighbor_Ne) <- points$rec_name

print(neighbor_Ne)

# Calcurate the Ne fragmentation index

NFI <- sum(neighbor_Ne>500)/length(neighbor_Ne)
# it should be > 5000 when the population size is Nc
print(NFI)

 
