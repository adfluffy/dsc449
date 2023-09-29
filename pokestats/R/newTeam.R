#' @title new pokemon team
#' @description Creates a new pokemon team data frame
#' @param slot1 string - the pokemon identifier desired for the team slot 1 pokemon. Random pokemon will be chosen if left blank.
#' @param slot2 string - the pokemon identifier desired for the team slot 2 pokemon. Random pokemon will be chosen if left blank.
#' @param slot3 string - the pokemon identifier desired for the team slot 3 pokemon. Random pokemon will be chosen if left blank.
#' @param slot4 string - the pokemon identifier desired for the team slot 4 pokemon. Random pokemon will be chosen if left blank.
#' @param slot5 string - the pokemon identifier desired for the team slot 5 pokemon. Random pokemon will be chosen if left blank.
#' @param slot6 string - the pokemon identifier desired for the team slot 6 pokemon. Random pokemon will be chosen if left blank.
#' @export
#' @keywords create,team,new
#' @return teamFrame
#' @examples \dontrun{
#' newTeam(slot1,slot2,slot3,slot4,slot5,slot6)
#' }


newTeam <- function(slot1="",slot2="",slot3="",slot4="",slot5="",slot6=""){
  # Initializes the error message variable
  errorMSG <- "An error has occurred in the function 'newTeam'."

  # Creates a vector containing the identifiers of the pokemon in slots 1-6
  targetPokemon <- c(slot1,slot2,slot3,slot4,slot5,slot6)
  # Sets all pokemon identifiers to lower case
  targetPokemon <- tolower(targetPokemon)
  # Creates a vector of all unique pokemon values from the target pokemon vector
  uniqueTargets <- unique(targetPokemon)

  # Begin error handling section (lines 29-43)
  if(any(targetPokemon!="")){ #Determines if the user defined any arguments, and proceeds if true
    # Determines if any arguments passed by the user are not found in pokemonData
    if (slot1!=""){
      is1NotPokemon <- !(slot1 %in% pokemonData$Identifier)
    } else {
      is1NotPokemon <- FALSE
    }
    if (slot2!=""){
      is1NotPokemon <- !(slot2 %in% pokemonData$Identifier)
    }else{
      is2NotPokemon <- FALSE
    }
    if (slot3!=""){
      is3NotPokemon <- !(slot3 %in% pokemonData$Identifier)
    } else {
      is3NotPokemon <- FALSE
    }
    if (slot4!=""){
      is4NotPokemon <- !(slot4 %in% pokemonData$Identifier)
    } else {
      is4NotPokemon <- FALSE
    }
    if (slot5!=""){
      is5NotPokemon <- !(slot5 %in% pokemonData$Identifier)
    } else {
      is5NotPokemon <- FALSE
    }
    if (slot6!=""){
      is6NotPokemon <- !(slot6 %in% pokemonData$Identifier)
    } else {
      is6NotPokemon <- FALSE
    }
    # Sums the boolean values (TRUE = 1, FALSE = 0)
    inputsNotPokemon <- sum(c(is1NotPokemon,is2NotPokemon,is3NotPokemon,is4NotPokemon,is5NotPokemon,is6NotPokemon))
  } else {
    # If all default values are used, assigns the value of 0 to the variable
    inputsNotPokemon <- 0
  }
  # If any true values were returned for inputsNotPokemon the error handling executes
  if(inputsNotPokemon!=0){
    # Error - one or more argument passed by the user is not a valid pokemon identifier
    errorMSG <- paste(errorMSG," One or more of the desired pokemon cannot be found in the pokemonData database. Please review the arguments and try again.")
    stop(errorMSG)
  }
  # End error handling section

  # Creates vector with appropriate pokemon frame row names
  names4Rows <- c("Name","Identifier","Number","Type1","Type2","Ability.Name","Ability.Number","Nature","Level","Tera.Type","HP","ATK","DEF","SPA","SPD","SPE","HP.IV","ATK.IV","DEF.IV","SPA.IV","SPD.IV","SPE.IV","HP.EV","ATK.EV","DEF.EV","SPA.EV","SPD.EV","SPE.EV","EV.Total","Move1","Move2","Move3","Move4")

  # Creates the balnk team data frame for returning data to the user
  teamFrame <- data.frame(matrix(nrow=33,ncol=6))
  # Assigns appropriate row and column names
  rownames(teamFrame) <- names4Rows
  colnames(teamFrame) <- c("slot1","slot2","slot3","slot4","slot5","slot6")

  # Creates a randomly chosen team of 6 pokemon by their national dex numbers
  randgenTeam <- sample(regD$Number,size = 6,replace = FALSE)

  # Pokemon teams can only have one pokemon of each national pokedex number. The below code takes the team
  # of randomly generated pokemon and reconciles those values with the possible values passed
  # by the user to ensure that the randomly selected portions of a team, if any, obey the national
  # dex number rule, regardless of form the pokemon takes.
  repeat{
    # Creates a vector with only the unique members of the randomly selected team
    uniqueTeam <- unique(randgenTeam)
    # Creates a vector with the unique random and unique user passed pokemon
    comboTeam <- c(uniqueTargets,randgenTeam)
    # Creates a vector with the unique members of the comboTeam vector
    uniqueComboTeam <- unique(comboTeam)

    # If reducing the randomly selected team to its unique members results in no change to the vector
    # AND if reducing the user passed unique arguments plus the randomly selected team members to only
    # unique values results in not vector change, the loop breaks and the code proceeds.
    if((length(uniqueTeam)==length(randgenTeam))  & (length(comboTeam)==length(uniqueComboTeam))){
      break
    } else {
      # If there is a change in vector length in either case, a new randomly selected team is
      # generated and the process is repeated until the user selection and the random selection
      # contain none of the same pokemon
      randgenTeam <- sample(regD$Number,size = 6,replace = FALSE)
    }
  }

  # Converts the randomly selected pokemon by national id number into their pokemon
  # identifiers. The form of the pokemon is chosen randomly from all availible forms.
  for(i in 1:6){
    randgenTeam[i] <- sample(regD$identifier[regD$Number==randgenTeam[i]],size=1)
  }

  # Assigns the randomly selected pokemon to the  final team by iterating over each team
  # member and determining if the argument has its default value.
  for(i in 1:6){
    if(targetPokemon[i]==""){
      # Assigns pokemon identifiers to the default arguments in the targetPokemon vector
      targetPokemon[i] <- randgenTeam[i]
    }
  }

  # Calls the generatePokemon function for each member of the new team
  teamFrame$slot1 <- generatePokemon(targetPokemon[1])
  teamFrame$slot2 <- generatePokemon(targetPokemon[2])
  teamFrame$slot3 <- generatePokemon(targetPokemon[3])
  teamFrame$slot4 <- generatePokemon(targetPokemon[4])
  teamFrame$slot5 <- generatePokemon(targetPokemon[5])
  teamFrame$slot6 <- generatePokemon(targetPokemon[6])
  # Assigns the appropriate names to the teamFrame columns
  colnames(teamFrame) <- c("slot1","slot2","slot3","slot4","slot5","slot6")
  # Returns the team frame
  return(teamFrame)

}
