
# Import necessary libraries ####
library(morphomap)
library(Morpho)
library(geomorph)
library(abind)
library(rgl)
library(Rvcg)
#
# Import the data, check for issues ####

# folder containing the landmarks
directory_humerus_pts <- "PATH/TO/LANDMARKS" 
# file names
pts_humerus_files <- list.files(directory_humerus_pts, pattern = "\\.pts", full.names = TRUE)
# initiate list to store landmarks
pts_humerus <- list()
# import the landmarks, put them in the list
for (i in seq_along(pts_humerus_files)) {
  pts <- read.pts(pts_humerus_files[i])
  pts_humerus[[i]] <- pts
}

pts_file_names <- basename(pts_humerus_files) # name the elements of the list


# Define the directory containing the .ply files
directory_humerus_ply <- "PATH/TO/MESHES"
# Get a list of all .ply files in the directory
ply_humerus <- list.files(directory_humerus_ply, pattern = "\\.ply$", full.names = TRUE)
ply_humerus
# Extract original file names
original_file_names <- basename(ply_humerus)


# Initialize a list to store the mesh objects
meshes_humerus <- list()

# Loop through the list of .ply files and read them
for (i in seq_along(ply_humerus)) {
  mesh <- vcgPlyRead(ply_humerus[i])
  meshes_humerus[[i]] <- mesh
}

names(meshes_humerus) <- original_file_names # name the meshes

# Check the two lists are the same length
length(meshes_humerus)
length(pts_humerus)

# Checking list orders match up, both of these should return "character(0)"
setdiff(gsub("\\.ply$", "", original_file_names), gsub("\\.pts$", "", basename(pts_humerus_files)))
setdiff(gsub("\\.pts$", "", basename(pts_humerus_files)), gsub("\\.ply$", "", original_file_names))

# Put the landmarks into an array
array_humerus <- array(unlist(pts_humerus), dim = c(dim(pts_humerus[[1]])[1], dim(pts_humerus[[1]])[2], length(pts_humerus)))
concatenated_array_humerus <- abind(array_humerus, along = 1)

# This will plot the 20th mesh in the list with the 20th landmarks in the list
# Change 'fixed = 5' to the number of landmarks you have on each specimen
# Try a few different meshes to check for any issues with ordering of the meshes and landmarks
# If after alignment any specimens are clearly in the wrong orientation or position find it in your list and run this line, see if the landmarks are in the wrong order or place
plotspec(meshes_humerus[[20]], array_humerus[,,20], fixed = 5, fixed.pt.size = 8, fixed.pt.col = rainbow(5))


# Number of specimens
num_specimens_humerus <- dim(concatenated_array_humerus)[3]
# Initialize a list to store landmarks for each specimen
humerus_specimen_list <- vector("list", length = num_specimens_humerus)
# Loop through specimens and extract landmarks
for (i in 1:num_specimens_humerus) {
  humerus_specimen_list[[i]] <- concatenated_array_humerus[,,i]
}

humerus_specimen_list # landmarks in a list


##
# Align the meshes ####

# Procrustes transform the landmarks
humerus_proc <- gpagen(concatenated_array_humerus)

# Define a list to store aligned meshes
aligned_humerus_noscale <- vector("list", length = length(meshes_humerus))

# Align each mesh NOTE SCALING IS SET TO FALSE
for (i in seq_along(meshes_humerus)) {
  aligned_mesh <- rotmesh.onto(meshes_humerus[[i]], humerus_specimen_list[[i]], humerus_proc$consensus, scale = FALSE)
  aligned_humerus_noscale[[i]] <- aligned_mesh$mesh
}

aligned_scaled_humerus <- vector("list", length = length(meshes_humerus))

# scale each aligned mesh to unit centroid size
for (i in seq_along(meshes_humerus)) {
  scale_mesh <- scalemesh(aligned_humerus_noscale[[i]], size = (1/cSize(aligned_humerus_noscale[[i]]))*1000, center = "none")
  aligned_scaled_humerus[[i]] <- scale_mesh
}

aligned_scaled_humerus

#
# Export the aligned and scaled meshes ####

# change this to where you want to meshes to end up
output_directory <- c("OUTPUT/PATH/PAMS")
setwd(output_directory)

basename(ply_humerus) # the aligned meshes will have the same name as the original ones

# Loop through aligned meshes, save them by their original file names in binary format
for (i in seq_along(meshes_humerus)) {
  original_name <- gsub("\\.ply$", "", basename(ply_humerus)[i])  # remove the .ply extension
  output_file <- paste0(original_name, ".ply")
  # Export the mesh in binary format (choose either BE or LE)
  vcgPlyWrite(aligned_scaled_humerus[[i]], output_file, format = "PLY_BINARY_LE")  # Use PLY_BINARY_BE for big-endian
}

##



