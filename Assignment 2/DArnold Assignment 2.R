# Read data from provided TXT file containing euclidean coordinates
# Store data in variable 'data'
xyz_coor <- read.table("F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 2/Coordinates.txt",header=TRUE)

# Creates a for loop that iterates over each of the items from the coordinates text file
# by number of rows
for (i in 1:nrow(xyz_coor)){
  
  # Below code takes the absolute value of the x and y coordinates for calculation
  # of theta later
  absolute_x <- abs(xyz_coor[i,1])
  absolute_y <- abs(xyz_coor[i,2])
  
  # Determines if the denominator in the arc tangent function would be zero, resulting
  # in an undefined value of theta 
  if (absolute_x!=0){
    # If the x coordinate is non-zero, calculates the arc tangent of theta in quadrant 1
    absolute_theta <- atan(absolute_y/absolute_x)
  }
  
  # Below series of If-Else statements instruct the code how to adjust the 
  # quadrant 1 value of theta to represent the appropriate coordinate location
  if (xyz_coor[i,1]>0 & xyz_coor[i,2]>=0){
    # If the (x,y) pair is in Q1, absolute theta is the same as the actual theta
    # value necessary
    cylindrical_theta <- absolute_theta
    
  } else if (xyz_coor[i,1]<0 & xyz_coor[i,2]>=0){
    # If the (x,y) pair is in Q2, absolute theta must be subtracted from pi to obtain
    # the value necessary for (r,theta) to match (x,y) position in space
    cylindrical_theta <- pi-absolute_theta
    
  } else if (xyz_coor[i,1]<0 & xyz_coor[i,2]<0){
    # If the (x,y) pair is in Q3, pi must be subtracted from the value of theta so
    # that the (r,theta) coordinate pair matches the (x,y) position in space
    cylindrical_theta <- absolute_theta-pi
    
  } else if (xyz_coor[i,1]>0 & xyz_coor[i,2]<0){
    # If the (x,y) pair is in Q4, quadrant 1 theta must be multiplied by -1
    cylindrical_theta <- absolute_theta*-1
    
  } else if (xyz_coor[i,1]==0 & xyz_coor[i,2]>0){
    # Handles case of x=0 and positive y coordinate. In this case, theta is equal
    # to positive pi/2
    cylindrical_theta <- (pi/2)
    
  } else if (xyz_coor[i,1]==0 & xyz_coor[i,2]<0){
    # Handles the case of =0 and negative y coordinate. In this case theta is equal 
    # to negative pi/2
    cylindrical_theta <- (-pi/2)
    
  }
  
  # Calculates the radius (r) value for the polar coordinates from the provided
  # cartesian (x,y) pair
  cylindrical_r <- sqrt((xyz_coor[i,1]^2)+(xyz_coor[i,2]^2))
  # Cartesian Z value is equal to cylindrical z value
  cylindrical_z <- xyz_coor[i,3]
  
  # Calculates rho from the (x,y,z) cartesian coordinates
  spherical_rho <- sqrt((xyz_coor[i,1]^2)+(xyz_coor[i,2]^2)+(xyz_coor[i,3]^2))
  # Spherical and Cylindrical theta values are the same
  spherical_theta <- cylindrical_theta
  # Calculates phi value based on calculated rho and provided z values
  spherical_phi <- acos(xyz_coor[i,3]/spherical_rho)
  
  # If this is the first iteration of the loop, creates the matrices for the
  # cylindrical and spherical coordinates
  if (i==1){
    # Initializes the cylindrical coordinate matrix
    cylindrical_converted <- matrix(data=c(cylindrical_r,cylindrical_theta,cylindrical_z),nrow = 1,ncol = 3)
    # Names the matrix columns
    colnames(cylindrical_converted) <- c("r","theta","z")
    
    # Initializes the spherical coordinate matrix
    spherical_converted <- matrix(data=c(spherical_rho,spherical_theta,spherical_phi),nrow = 1,ncol = 3)
    # Names the matrix columns
    colnames(spherical_converted) <- c("rho","theta","phi")
    
  } else{
    # If not the first iteration, adds a new row to the cylindrical coordinate matrix 
    # with the r, theta, z coordinates
    cylindrical_converted <- rbind(cylindrical_converted,c(cylindrical_r,cylindrical_theta,cylindrical_z))
    # Adds a row to the spherical coordinate matrix with the converted rho, theta, and
    # phi values
    spherical_converted <- rbind(spherical_converted,c(spherical_rho,spherical_theta,spherical_phi))
    
  }
}

# Writes both spherical and cylindrical coordinate conversions to independent csv
# files
write.csv(x=cylindrical_converted,file="F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 2/Cylindrical Coordinates.txt",row.names = FALSE)
write.csv(x=spherical_converted,file="F:/Users/Devan/Documents/Education/DSC449/Assignments/Assignment 2/Spherical Coordinates.txt",row.names = FALSE)