#' @title Spherical to Rectangular Coordinate Converter
#' @description
#' Converts spherical (rho,theta,phi) coordinates to (x,y,z) rectangular coordinates
#' and returns coordinates as a vector
#' @param rho rho value for the spherical coordinate of interest
#' @param theta theta value for the spherical coordinate of interest
#' @param phi phi value for the spherical coordinate of interest
#' @export
#' @return rectangular
#' @examples \dontrun{
#' sphericaltorecangular(rho,theta,phi)
#' }

sphericaltorectangular <- function (rho,theta,phi){
  # Error Handling (Lines 15-79)
  # Determines if any arguments sent by the user are non-numeric
  nonnumeric <- c(typeof(rho),typeof(theta),typeof(phi))
  # Determines if any arguments sent by the user are blank
  missing_vars <- c(missing(rho),missing(theta),missing(phi))

  # sets the error_mess and error_var string for the upcoming error evaluations
  error_mess <- "No value was given for argument(s)"
  error_var <- ""
  error_counter <- 0

  # iterates through each item in the missing_vars vector
  for (i in 1:length(missing_vars)) {
    #proceeds if true
    if (missing_vars[i]){
      # adds a conjunction to the error_var list if not the first time in the loop
      if (error_counter > 0){error_var <- paste(error_var,"and")}

      # Determines which argument the missing value is for
      if (i == 1){error_var <- paste(error_var,"Rho")}
      if (i == 2){error_var <- paste(error_var,"Theta")}
      if (i == 3){error_var <- paste(error_var,"Phi")}

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
      if (i == 1){error_var <- paste(error_var,"Rho")}
      if (i == 2){error_var <- paste(error_var,"Theta")}
      if (i == 3){error_var <- paste(error_var,"Phi")}
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
  # Calculate the conversions to (x,y,z) coordinates and store them in corresponding variables
  x <- rho*sin(phi)*cos(theta)
  y <- rho*sin(phi)*sin(theta)
  z <- rho*cos(phi)
  # Assign (x,y,z) to the vector 'rectangular' for reporting to the user
  rectangular <- c(x,y,z)
  # Return resulting vector
  return(rectangular)
}
