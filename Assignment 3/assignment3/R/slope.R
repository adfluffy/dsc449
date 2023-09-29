#' @title Slope value
#' @description
#' Calculates the slope between two points in Cartesian space
#' @param x1 X value of the first point of interest
#' @param y1 y value of the first point of interest
#' @param x2 x value of the second point of interest (optional)
#' @param y2 y value of the second point of interest (optional)
#' @param format Determines the format of the returned slope (optional, 0 = scalar, 1 = angular)
#' @export
#' @keywords slope
#' @return slope_value
#' @examples \dontrun{
#' slope(x1,y1,x2,y2,format)
#' }

slope = function(x1,y1,x2=0,y2=0,format=0){
  #Error Handling (Lines 17-92)
  # Determines if any arguments sent by the user are non-numeric
  nonnumeric <- c(typeof(x1),typeof(y1),typeof(x2),typeof(y2))
  # Determines if any arguments sent by the user are blank
  missing_vars <- c(missing(x1),missing(y1),missing(x2),missing(y2))

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
      if (i == 1){error_var <- paste(error_var,"X1")}
      if (i == 2){error_var <- paste(error_var,"Y1")}
      if (missing_vars[3] & missing_vars[4]){
        error_counter <- error_counter - 1
      } else if (i == 3){error_var <- paste(error_var,"X2")
      } else if (i == 4){error_var <- paste(error_var,"Y2")}

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
      if (i == 1){error_var <- paste(error_var,"X1")}
      if (i == 2){error_var <- paste(error_var,"Y1")}
      if (i == 3){error_var <- paste(error_var,"X2")}
      if (i == 4){error_var <- paste(error_var,"Y2")}
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

  # Determines if the slope between (x1,y1) and (x2,y2) is undefined
  if ((x1-x2)==0){
    error_mess <- "The slope is undefined (X1=X2)"
    # Stops function and reports error to user
    stop(error_mess)
  }

  # Calculates the slope (scalar)
  slope_value = (y2-y1)/(x2-x1)

  if (format == 0){
    # Does nothing
  } else if (format == 1){
    # formats the slope value to an angle in radians
    slope_value = atan(slope_value)

  } else {
    # if the format argument is passed a value other than 0 or 1 returns an error to the user
    error_mess <- "Slope format could not be determined. Please review function arguments and run again."
    stop(error_mess)

  }

  # Returns the slope value to user
  return(slope_value)
}
