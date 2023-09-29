#' @title Vector Magnitude
#' @description
#' Calculates the magnitude of vectors in 2 or 3 dimensional space
#' @param x x value of point of interest
#' @param y y value of point of interest
#' @param z z value of point of interest (optional)
#' @export
#' @keywords magnitude
#' @return r
#' @examples \dontrun{
#' magnitude(x,y,z)
#' }

magnitude <- function(x,y,z=0){
  # Error handling (Lines 15-78)
  # Determines if any arguments sent by the user are non-numeric
  nonnumeric <- c(typeof(x),typeof(y),typeof(z))
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
      if (i == 3){error_var <- paste(error_var,"Z")}
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

  # Calculates the magnitude of the vector based on argument values
  r <- sqrt(x^2 + y^2 + z^2)
  # Returns r
  return(r)
}
