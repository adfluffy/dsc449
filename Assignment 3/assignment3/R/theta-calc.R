#' @title Theta Angle Calculator
#' @description
#' Calculates the angle theta given a pair of (x,y) Cartesian coordinates
#' @param x x value of the point of interest
#' @param y y value of the point of interest
#' @export
#' @keywords theta
#' @return theta_rad
#' @examples \dontrun{
#' thetacalc(x,y)
#' }

thetacalc <- function(x,y){
  # Error handling (Lines 14-77)
  # Determines if any arguments sent by the user are non-numeric
  nonnumeric <- c(typeof(x),typeof(y))
  # Determines if any arguments sent by the user are blank
  missing_vars <- c(missing(x),missing(y))

  # sets the error_mess and error_var string for the upcoming error evaluations
  error_mess <- "No value was given for arguments"
  error_var <- ""
  error_counter <- 0

  # iterates through each item in the missing_vars vector
  for (i in 1:length(missing_vars)) {
    #proceeds if true
    if (missing_vars[i]){
      # adds a conjunction to the error_var list if not the first time in the loop
      if (error_counter > 0){error_var <- paste(error_var,"and")}

      # Determines which argument the missing value is for
      if (i == 1){error_var <- paste(error_var,"X")}
      if (i == 2){error_var <- paste(error_var,"Y")}

      # iterates the error_counter variable
      error_counter <- error_counter + 1
    }
  }
  #adds the error_var string to the error_mess string for reporting
  error_mess <- paste(error_mess,error_var)
  error_mess <- paste(error_mess,". Please review user inputs and run again.")

  # If errors in the arguments were found, stops execution and notifies the user
  if (error_counter > 0){
    stop(error_mess)
  }

  # sets the error_mess and error_var string for the upcoming error evaluations
  error_mess <- "You have input a non-numeric value for argument"
  error_var <- ""
  error_counter <- 0

  # Iterates through the nonnumeric vector to determine if there is an issue with
  # user inputs
  for (i in 1:length(nonnumeric)){
    # Errors occur when an argument is not of the 'double' data type
    if (nonnumeric[i]!="double"){
      # adds a conjunction to the error_var list if not the first time in the loop
      if (error_counter > 0){error_var <- paste(error_var,"and")}
      # Determines which argument(s) are non-numeric
      if (i == 1){error_var <- paste(error_var,"X")}
      if (i == 2){error_var <- paste(error_var,"Y")}

      # iterates the error_counter variable
      error_counter <- error_counter + 1
    }
  }
  # adds the error_var string to the error_mess string for reporting
  error_mess <- paste(error_mess,error_var)
  error_mess <- paste(error_mess,". Please review the information passed to the function and try again.")

  # if non-numeric arguments were found, stops the function and reports the error
  # to the user
  if (error_counter > 0){
    stop(error_mess)
  }

  # Calculates the absolute values of provided (x,y) coordinates
  absolute_x <- abs(x)
  absolute_y <- abs(y)

  # Determines if the denominator in the arc tangent function would be zero, resulting
  # in an undefined value of theta
  if (absolute_x!=0){
    # If the x coordinate is non-zero, calculates the arc tangent of theta in quadrant 1
    absolute_theta <- atan(absolute_y/absolute_x)
  }

  # Below series of If-Else statements instruct the code how to adjust the
  # quadrant 1 value of theta to represent the appropriate coordinate location
  if (x>0 & y>=0){
    # If the (x,y) pair is in Q1, absolute theta is the same as the actual theta
    # value necessary
    theta_rad <- absolute_theta

  } else if (x<0 & y>=0){
    # If the (x,y) pair is in Q2, absolute theta must be subtracted from pi to obtain
    # the value necessary for (r,theta) to match (x,y) position in space
    theta_rad <- pi-absolute_theta

  } else if (x<0 & y<0){
    # If the (x,y) pair is in Q3, pi must be subtracted from the value of theta so
    # that the (r,theta) coordinate pair matches the (x,y) position in space
    theta_rad <- absolute_theta-pi

  } else if (x>0 & y<0){
    # If the (x,y) pair is in Q4, quadrant 1 theta must be multiplied by -1
    theta_rad <- absolute_theta*-1

  } else if (x==0 & y>0){
    # Handles case of x=0 and positive y coordinate. In this case, theta is equal
    # to positive pi/2
    theta_rad <- (pi/2)

  } else if (x==0 & y<0){
    # Handles the case of x=0 and negative y coordinate. In this case theta is equal
    # to negative pi/2
    theta_rad <- (-pi/2)

  } else if (x==0 & y==0){
    # Handles the case of x=0 and y=0. In this case theta is 0
    theta_rad <- 0
  }

  # Returns the calculated value of theta in radians
  return(theta_rad)
}
