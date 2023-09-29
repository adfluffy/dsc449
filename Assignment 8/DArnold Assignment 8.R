library(ggmap)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(sf)

# Sets my working directory to my folder for the assignment. Can be commented out to allow
# for any working directory to be assigned by the reviewer
setwd("F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 8")

# Registers my Google Maps API key
register_google(key="AIzaSyAP8UnKOEWXDWGtwaR6jkWe9YagOQUmaZc")

# Creates a data frame from the MaineHighSchool CSV found in my working directory
high_schools <- read.csv("MaineHighSchools.csv",header=TRUE)

# Geocodes the high schools based on their addresses. The longitude and latitude coordinates
# are then applied back to the original data frame for records keeping, and a new data frame
# names 'hs_locations' is created as a copy of the original frame.
hs_locations <- geocode(high_schools$Mailing.Address)
high_schools <- cbind(high_schools,hs_locations)
hs_locations <- high_schools
# Converts the hs_locations data frame into a spatial frame using longitude and latitude 
# coordinates
coordinates(hs_locations) <- ~ lon + lat
# Creates a variable to contain the projection string for later use
projection <- CRS("+proj=longlat +datum=WGS84")
# Applies the projection above to the hs_locations spatial frame
proj4string(hs_locations) <- projection

# Creates a spatial frame from the provided school district files
school_districts <- readOGR("./tl_2017_23_unsd","tl_2017_23_unsd")
# Applies the same projection as before to the school_district spatial frame
school_districts <- spTransform(school_districts,projection)

# Creates a spatial frame from the provided US county files
county_info <- readOGR("./tl_2017_us_county","tl_2017_us_county")
# Reduces the above spatial frame to a subset only referring to Maine counties
county_info <- subset(county_info,county_info$STATEFP=="23")
# Applies the same projection as before to the county_info spatial frame
county_info <- spTransform(county_info,projection)


# The below code determines if a certain high school (j) belongs to a certain school
# district (i). If the school is in the district, the code stores the district name, 
# county name, school address, and school name in temporary variables. Those values
# then are added to the school_districts (county name, high school address, school name)
# spatial frame or the hs_locations (district name) spatial frame. 
for (i in 1:nrow(school_districts)){
  for (j in 1:nrow(hs_locations)){
    # pacman is the larger spatial object, in this case the district. Pellet refers to the smaller
    # spatial object I am trying to fit within the pacman object. 
    pacman <- school_districts[i,]
    pellet <- hs_locations[j,]
    # Determines if the pellet object is within the pacman object
    if (gContains(pacman,pellet)){
      # Fills the temporary variables with values from the school_districts and 
      # hs_locations spatial frames
      district <- school_districts[i,]$NAME
      county <- hs_locations[j,]$County
      address <- hs_locations[j,]$Mailing.Address
      school <- hs_locations[j,]$School.Name
      # Applies the information to the appropriate row/column of each target spatial
      # frame
      school_districts[i,'County'] <- county
      school_districts[i,'Address'] <- address
      school_districts[i,'SCHOOL'] <- school
      hs_locations[j,'District'] <- district
    }
  }
}
# Completes the county locations for any school districts unaccounted for in the above code
# by performing the same process as above. In this case, the pellet is the school
# district and the pacman is the county. Otherwise, the process is the same with only
# the county name being applied to the school_districts spatial frame.
for (i in 1:nrow(county_info)){
  for (j in 1:nrow(school_districts)){
    
    pacman <- county_info[i,]
    pellet <- school_districts[j,]
    
    if (gContains(pacman,pellet)){
      
      county <- county_info[i,]$NAME

      school_districts[j,'County'] <- county
    }
  }
}

# Determines the centroid position of each school district and stores the
# results in the district_centroids spatial object. 
district_centroids <- gCentroid(school_districts,byid=TRUE)
# Applies the name of each district to the district_centroids spatial frame
district_centroids$Name <- school_districts$NAME
# Applies the same projection as before to the district_centroids spatial frame
proj4string(district_centroids) <- projection

# Creates a blank data frame for storing results of centroid distance analysis
hs_cent_dist <- data.frame()

# The below code iterates over the hs_locations data frame, extracting the district
# information for each school as well as the school's name and storing them in 
# the temporary variables 'district' and 'school'. If the district for the school
# was able to be determined, then temporary variables 'a' and 'b' are populated with 
# information from the row of interest in the district_centroids spatial frame (a) and
# the row of interest in the hs_locations spatial frame (b). Another temporary variable,
# 'distance', is then defined to be the result of the gDistance function on 'a' and 'b'.
# The school name, district name, and distance between the high school and the district
# centroid are added as a row to the hs_cent_dist data frame. 
for(i in 1:nrow(hs_locations)){
  # Populates the district and school temporary variables
  district <- hs_locations[i,]$District
  school <- hs_locations[i,]$School.Name
  # Determines if the district field is blank
  if (!is.na(district)){
    # Populates the temporary spatial variables a and b
    a <- district_centroids[district_centroids$Name==district,]
    b <- hs_locations[i,]
    # Calculates the distance between a and b and stores the result in temp variable distance
    distance <- gDistance(a,b)
  } else {
    # If the district is blank, assigns a blank distance between the school and district centroid
    distance <- NA 
  }
  # Adds a row to the hs_cent_dist data frame with the school name, district name, and 
  # centroid/school distance
  hs_cent_dist <- rbind(hs_cent_dist,c(school,district,distance))
}
# Names the hs_cent_dist data frame columns and converts the distance data type to double
colnames(hs_cent_dist) <- c("School","District","Distance")
hs_cent_dist$Distance <- type.convert(hs_cent_dist$Distance)

# Creates min_hs_cent_dist data frame to store information on the distance between
# a school and its nearest district centroid
min_hs_cent_dist <- data.frame()
# Creates a data frame with the distance between each high school and each district centroid
hs_centroid_distances <- gDistance(hs_locations,district_centroids,byid=TRUE)

# The below code iterates over the hs_centroid_distances data frame and find the smallest
# distance for each column. This corresponds to the nearest district centroid (row) for each
# high school (col). Since no row or column names were assigned, R resued the row identifiers
# from the originating spatial frames, and that knowledge is then used to match the 
# school back to the district it is nearest to. This information, as above, is then
# added as a row to the min_hs_centroid data frame. 
for (i in 1:ncol(hs_centroid_distances)){
  # Determines the row for column i that has the least value and stores that in the
  # temporary variable x
  x <- which.min(hs_centroid_distances[,i])
  # Pulls the numeric distance vale from the hs_centroid_distances data frame and stores
  # it in the temporary variable distance
  distance <- hs_centroid_distances[x,i]
  # Determines the district name from the school_districts spatial frame
  district <- school_districts[x,]$NAME
  # Determines the school name from the school_districts spatial frame
  school <- hs_locations[i,]$School.Name
  
  # Creates a temporary list of the information of interest
  bindingrow <- c(school,district,distance)
  # Adds the temporary list as a row to the min_hs_cent_dist data frame
  min_hs_cent_dist <- rbind(min_hs_cent_dist,bindingrow)
}
# Applies appropriate column names to the min_hs_cet_dist data frame
colnames(min_hs_cent_dist) <- c("School","District","Distance")
# Converts the distance column to the double data type
min_hs_cent_dist$Distance <- type.convert(min_hs_cent_dist$Distance)

# Creates a data frame of both the owned district centroid/high school distance information
# and the nearest district centroid/high school distance information
combined_hs_cent <- cbind(hs_cent_dist,min_hs_cent_dist)
# Applies appropriate column names to the data frame
colnames(combined_hs_cent) <- c("School","District","District.Distance","School","Closest.District", "Closest.Distance")
# Removes the repeated information (School name, column 'School') from the combined data frame
combined_hs_cent <- combined_hs_cent[-4]

# Compares the owned district to the nearest district. If these values are not the same,
# adds TRUE to a new data frame column "Closest.Not.District" to represent that the
# closest district centroid is NOT that of the school's own district. 
for (i in 1:nrow(combined_hs_cent)){
  # Determines if the district information for the school was available
  if (is.na(combined_hs_cent[i,2])){
    # Sets the Closest.Not.District value to FALSE for undetermined districts
    combined_hs_cent[i,"Closest.Not.District"] <- FALSE
  } else if (combined_hs_cent[i,2]!=combined_hs_cent[i,4]) {
    # If the school's district is determined, but the school's nearest district centroid
    # is not the same as the school's district then the Closest.Not.District value is
    # set to TRUE
    combined_hs_cent[i,"Closest.Not.District"] <- TRUE
  } else {
    # If the above conditions are not satisfied then a default value of FALSE is 
    # Assigned to the Closest.Not.District column for the school
    combined_hs_cent[i,"Closest.Not.District"] <- FALSE
  }
}
# Displays the school names of the schools that are closer to another district's centroid
# than their own
combined_hs_cent$School[combined_hs_cent$Closest.Not.District]

# Creates a list of a few Maine addresses pulled totally from random and not from my personal life
a_few_addresses <- c("21 Winter St Buxton, ME 04093", "160 Main Rd Milford, ME 04461", "61 Congress St Bangor, ME 04401")
# Geocodes the above addresses and stores the results in the address_locations spatial object
address_locations <- geocode(a_few_addresses)
# Creates longitude and latitude coordinates from the address_location information
coordinates(address_locations) <- ~ lon + lat
# Applies the same projection as before to the address_locations spatial frame
proj4string(address_locations) <- projection

# Creates a data frame of boolean values describing if the address location is within
# a school district
new_address_school <- gContains(school_districts,address_locations,byid=TRUE)
# Creates a blank 3 row by 3 column data frame to store information about the address' school
address_schools <- data.frame(matrix(nrow=3,ncol=3))

# For each of the addresses I totally got at random assigns their value to the first
# column of the address_schools data frame
for (i in 1:length(a_few_addresses)){
  address_schools[i,1] <- a_few_addresses[i]
}
# Names the columns of the address_schools data frame
colnames(address_schools) <- c("Address","School","District")

# The below code iterates over the new_address_school data frame to determine which
# school district the address belongs to. As with previous iterations of this method
# that I have employed, the column numbers are taken directly from the school_districts
# spatial frame's row numbers, so the value of i corresponds to the district that the
# address resides in. Temporary variables 'district' and 'school' are again utilized,
# and if a row in column i returns a TRUE value then the District and School columns
# of the address_schools data frame is populated with the respective temporary variable's
# value. 
for (i in 1:ncol(new_address_school)){
  # Assigns the school and district values to their corresponding temporary variables
  district <- school_districts[i,]$NAME
  school <- school_districts[i,]$SCHOOL
  # Determines for each column i if address 1, 2, or 3 resides within the district
  if(new_address_school[1,i]){
    # If the address does reside in district i, the District and School columns are
    # populated
    address_schools[1,]$District <- district
    address_schools[1,]$School <- school
  }
  # Repeats above code for address 2
  if (new_address_school[2,i]){
    address_schools[2,]$District <- district
    address_schools[2,]$School <- school
  }
  # Repeats above code for address 3
  if (new_address_school[3,i]){
    address_schools[3,]$District <- district
    address_schools[3,]$School <- school
  }
}
# Prints the results of above computation to the console
address_schools