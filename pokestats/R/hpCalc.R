#' @title hit points calculator
#' @description Calculates the hit point (HP) stat for a given pokemon
#' @param poke string - the identifier for the pokemon upon the the calculation will be performed
#' @param hpIV integer - the hp individual value for the pokemon in the range [0,31]. Default value is 31
#' @param hpEV integer - the hp effort value for the pokemon, in the range [0,252]. Default value is 0
#' @param lvl integer - the level of the pokemon, in the range [1,100]. Default value is 50
#' @export
#' @keywords hp,hit points,calculate,stats
#' @return description
#' @examples \dontrun{
#' hpCalc(poke,hpIV,hpEV,lvl)
#' }


hpCalc <- function(poke,hpIV=31,hpEV=0,lvl=50){
  # Initializes the error message variable
  errorMSG <- "An error has occurred in the function 'hpCalc'."

  # Cleans the arguments passed to the function
  poke <- tolower(poke) #Ensures that the identifier is lower case
  lvl <- as.integer(lvl) #Ensures that the level is an integer
  hpIV <- as.integer(hpIV) #Ensures that the hpIV is an integer
  hpEV <- as.integer(hpEV) # Ensures that the hpEV is an integer

  # Calculates boolean values for later use in error handling
  unknownPoke <- !(poke %in% pokemonData$Identifier)
  outtarangelvl <- !(lvl %in% 1:100)
  outtarangeiv <- !(hpIV %in% 0:31)
  outtarangeev <- !(hpEV %in% 0:252)

  # Error handling (lines 32-54)
  if(unknownPoke){
    # Error - the pokemon identifier passed cannot be found in pokemonData
    errorMSG <- paste(errorMSG," The pokemon identifier cannot be matched against the pokemonData data set. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(outtarangelvl){
    # Error - the level argument is outside of the range [1,100] or is inside that range but not an integer. Earlier data conversion
    # should remove that as a possibility though
    errorMSG <- paste(errorMSG," The pokemon level is outside of the range [1,100]. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(outtarangeiv){
    # Error - the hpIV argument is outside of the range [0,31] or is inside that range but not an integer. Earlier data conversion
    # should remove that as a possibility though
    errorMSG <- paste(errorMSG," The HP individual value supplied is outside of the range [0,31]. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(outtarangeev){
    # Error - the hpEV argument is outside of the range [0,252] or is inside that range but not an integer. Earlier data conversion
    # should remove that as a possibility though
    errorMSG <- paste(errorMSG," The HP effort value supplied is outside of the range [0,252]. Please review the argument and try again.")
    stop(errorMSG)
  }
  # End error handling section

  # Determines the pokemon species' base value for its hit points stat. This is species specific and used
  # in the stat calculation
  baseHP <- pokemonData$HP[pokemonData$Identifier==poke]

  # Performs the stat calculation for the pokemon's hit points
  hp <- floor((((2 * baseHP) + hpIV + (hpEV/4)) * lvl)/100) + lvl + 10
  #Returns the calculated hits point stat value
  return(hp)
}
