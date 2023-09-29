#' @title pokemon stat calculator
#' @description Calculates the stat value for a given pokemon based on their individual value for the stat, their effort value points for the stat, their level, and their nature
#' @param poke string - the identifier for the desire pokemon calculation
#' @param stat string - the stat for which the calculation will be performed
#' @param statIV integer - the individual value of the stat for the desired pokemon. Range is [0,31], default is 31
#' @param statEV integer - the effort value points for the desired stat. Range is [0,252], default is 0
#' @param lvl integer - the level of the desired pokemon. Range is [1,100], default is 50
#' @param nature string - the nature of the desired pokemon. Default is the stat neutral nature 'Serious'
#' @export
#' @keywords stat,calc,calculate
#' @return statValue
#' @examples \dontrun{
#' statCalc(poke,stat,statIV,statEV,lvl,nature)
#' }


statCalc <- function(poke,stat,statIV=31,statEV=0,lvl=50,nature="Serious"){
  # Intializes the error message
  errorMSG <- "An error has occurred in function 'statCalc'."

  # Converts the string arguments to all upper or lower case, depending on usage
  poke <- tolower(poke)
  nature <- tolower(nature)
  stat <- toupper(stat)
  # Converts all integer arguments to the integer data type
  statIV <- as.integer(statIV)
  statEV <- as.integer(statEV)
  lvl <- as.integer(lvl)

  # Creates a vector with the possible stats to calculate
  possibleStats <- c("HP","ATK","DEF","SPA","SPD","SPE")

  # Creates boolean values for later error handling
  invalidPoke <- !(poke %in% pokemonData$Identifier)
  invalidNature <- !(nature %in% natureData$Nature)
  invalidStat <- !(stat %in% possibleStats)
  ivOutOfRange <- !(statIV %in% 0:31)
  evOutOfRange <- !(statEV %in% 0:252)
  lvlOutOfRange <- !(lvl %in% 1:100)

  # Begin error handling section (lines 42-71)
  if(invalidPoke){
    # Error - poke argument cannot be found in pokemonData database
    errorMSG <- paste(errorMSG," Cannot locate the desired pokemon in the pokemonData database. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(invalidNature){
    # Error - nature argument cannot be found in the natureData database
    errorMSG <- paste(errorMSG," Cannot verify the nature argument as valid. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(invalidStat){
    # Error - calculation requested for unkown stat argument
    errorMSG <- paste(errorMSG," The value of the stat argument is unexpected and calculation cannot proceed. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(ivOutOfRange){
    # Error - Individual value for the requested stat is not in the range [0,31]
    errorMSG <- paste(errorMSG," The value of the statIV argument is not in the range [0,31]. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(evOutOfRange){
    # Error - Effort value points for the requested stat is not in the range [0,252]
    errorMSG <- paste(errorMSG," The value of the statEV argument is not in the range [0,252]. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(lvlOutOfRange){
    # Error - the lvl argument is outside of the range [1,100]
    errorMSG <- paste(errorMSG," The lvl argument is outside of the range [1,100]. Please review the argument and try again.")
    stop(errorMSG)
  }
  # End error handling section

  # Determines if the HP stat is the target of the function's use
  if(stat=="HP"){
    # If the stat desired is HP, callcs the hpCalc function to determine the stat value
    statValue <- hpCalc(poke = poke,hpIV = statIV,hpEV = statEV,lvl = lvl)
  } else{
    # All other stats use the formula below, and are handled within this function
    # Determines the baseStat value for the desired stat of the desired pokemon
    baseStat <- pokemonData[pokemonData$Identifier==poke,stat]
    # Determines the pokemon nature's effect on the desired stat
    natureEffect <- natureData[natureData$Nature==nature,stat]
    # Calculates the value of the desired stat and stores the value in StatValue
    statValue <- floor((floor((((2 * baseStat) + statIV + (statEV/4)) * lvl)/100) + 5) * natureEffect)
  }
  # Returns the value of the stat
  return(statValue)
}
