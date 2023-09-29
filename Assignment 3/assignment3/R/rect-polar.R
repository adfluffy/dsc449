#' @title Rectangular to Polar Coordinate Converter
#' @description
#' Converts rectangular (x,y) ordered pairs to (r,theta) polar coordinates and
#' returns as a vector
#' @param x x value of the point of interest
#' @param y y value of the point of interest
#' @export
#' @return polar
#' @examples \dontrun{
#' rectangulartopolar(x,y)
#' }

rectangulartopolar <- function (x,y){
  # Error handling (Lines 14-76)
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

  # Call the magnitude function to determine the scalar value of r
  r <- magnitude(x,y)
  # call the thetacalc function to determine the value of angle theta
  theta <- thetacalc(x,y)
  # Assign r, theta to the polar vector for returning values to user
  polar <- c(r,theta)
  # Returns the converted polar coordinates
  return(polar)
}
