#' @title generate pokemon data frame
#' @description Generates a pokemon data frame based on the arguments supplied by the user
#' @param targetPokemon string - the identifier string of the pokemon to generate
#' @param targetIVs List of integers - the desired set of individual values for the target pokemon. Default is all IVs set to 31
#' @param targetEvs list of integers - the desired set of effort values for the target pokemon. Defulat is all EVs set to 0
#' @param targetNature string - the desired nature for the target pokemon. Default nature is Serious
#' @param targetMoveset List of integers - the desired moveset for the target pokemon. Moves should be in listed by their numerical identifiers. Default is all moves set to 1
#' @param targetAbility Integer - the desired ability for the target pokemon. This should be in the format 1, 2, or 3. Default is 1
#' @param targetLvl Integer - the desired level for the target pokemon. Should be in the range [1,100]. Default is 50
#' @param targetTera String - the desired terastallization type for the target pokemon. Default value is "" and returns a random type from among the target pokemon's types
#' @export
#' @keywords new,generate,pokemon
#' @return pokemonDataFrame
#' @examples \dontrun{
#' generatePokemon(targetPokemon,targetIVs=c(31,31,31,31,31,31),targetEVs=c(0,0,0,0,0,0),targetNature="serious",targetMoveset=c(1,1,1,1),targetAbility=1,targetLvl=50,targetTera="")
#' }

generatePokemon <- function(targetPokemon,targetIVs=c(31,31,31,31,31,31),targetEVs=c(0,0,0,0,0,0),targetNature="serious",targetMoveset=c(1,1,1,1),targetAbility=1,targetLvl=50,targetTera=""){
  # Initializes the error message
  errorMSG <- "An error has occurred in the function 'generatePokemon'."

  # Converts the targetPokemon, targetTera, and targetNature string to lower case
  targetPokemon <- tolower(targetPokemon)
  targetNature <- tolower(targetNature)
  targetTera <- tolower(targetTera)

  # Converts the expected integer values to the integer data type
  targetIVs <- as.integer(targetIVs)
  targetEVs <- as.integer(targetEVs)
  targetMoveset <- as.integer(targetMoveset)
  targetLvl <- as.integer(targetLvl)
  targetAbility <- as.integer(targetAbility)

  # Creates boolean values for error handling under different conditions outlined in the variable names
  notsixIVs <- length(targetIVs)!=6
  notsixEVs <- length(targetEVs)!=6
  unknownNature <- !(targetNature %in% natureData$Nature)
  notfourMoves <- length(targetMoveset)!=4
  unexpectedTera <- if(targetTera!=""){!(targetTera %in% pokemonData$Type1)}else{FALSE}
  lvloutofrange <- (targetLvl<1) | (targetLvl>100)
  ivsoutofrange <- any(targetIVs>31) | any(targetIVs<0)
  evsoutofrange <- any(targetEVs>252) | any(targetEVs<0)
  unknownpokemon <- !(targetPokemon %in% pokemonData$Identifier)
  unknownAbility <- !(targetAbility %in% 1:3)

  # Error handling (Lines 42-96)
  if(notsixIVs){
    # Error - the targetIVs vector does not contain exactly 6 values
    errorMSG <- paste(errorMSG," The target Individual Value set is not a list of 6 items. Please review the arguments and try again.")
    stop(errorMSG)
  }
  if(notsixEVs){
    # Error - the targetEVs vector does not contain exactly 6 values
    errorMSG <- paste(errorMSG," The target Effort Value set is not a list of 6 items. Please review the arguments and try again.")
    stop(errorMSG)
  }
  if(unknownNature){
    # Error - the targetNature value is not in the natureData data frame
    errorMSG <- paste(errorMSG," The target nature is not unknown or otherwise invalid. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(notfourMoves){
    # Error - the targetMoves vector does not contain exactly 4 values
    errorMSG <- paste(errorMSG," The desired move set is not a list of four items. Please review the argument and try again")
    stop(errorMSG)
  }
  if(unexpectedTera){
    # Error - the targetTera argument is not default or a recognized pokemon type
    errorMSG <- paste(errorMSG," The desired terastallization type is unknown or otherwise invalid. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(lvloutofrange){
    # Error - the targetLvl argument is outside of the range [1,100]
    errorMSG <- paste(errorMSG," The desired level is outside of the range [1,100]/ Pelase review the argument and try again.")
    stop(errorMSG)
  }
  if(ivsoutofrange){
    # Error - the targetIVs are not all in the range [0,31]
    errorMSG <- paste(errorMSG," One or more of the desired individual values is outside of the range [0,31]. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(evsoutofrange){
    # Error - the targetEVs are not all in the range [0,252]
    errorMSG <- paste(errorMSG," One or more of the desired effort values is outside of the range [0,252]. Please review the arguement and try again.")
    stop(errorMSG)
  }
  if(unknownpokemon){
    # Error - the targetPokemon identifier is found in the pokemonData data frame
    errorMSG <- paste(errorMSG," The targetPokemon argument is unknown or otherwise invalid. Please review the argument and try again.")
    stop(errorMSG)
  }
  if(unknownAbility){
    # Error - the targetAbility value is not in the range [1,3]
    errorMSG <- paste(errorMSG," The targetAbility argument is outside of the range [1,3]. Please review the argument and try again.")
    stop(errorMSG)
  }
  # End of error handling section

  # Creates the pokemon data frame for returning to the user
  pokemonDataFrame <- data.frame(matrix(nrow=33,ncol=1))

  # Creates a vector containing the names for the pokemon data frame
  names4Rows <- c("Name","Identifier","Number","Type1","Type2","Ability.Name","Ability.Number","Nature","Level","Tera.Type","HP","ATK","DEF","SPA","SPD","SPE","HP.IV","ATK.IV","DEF.IV","SPA.IV","SPD.IV","SPE.IV","HP.EV","ATK.EV","DEF.EV","SPA.EV","SPD.EV","SPE.EV","EV.Total","Move1","Move2","Move3","Move4")
  # Applies the row names to the pokemon data frame
  rownames(pokemonDataFrame) <- names4Rows
  # Applies the column name to the pokemon data frame
  colnames(pokemonDataFrame) <- c("Pokemon")

  # Determines the types of the pokemon based on the pokemonData data frame and the
  # targetPokemon identifier argument
  type1 <- pokemonData$Type1[pokemonData$Identifier==targetPokemon]
  type2 <- pokemonData$Type2[pokemonData$Identifier==targetPokemon]

  # Assigns the pokemon a default tera type based on the target pokemon's type 1
  # and tpye 2 (where applicable). only executes if targetTera is left as the default "" value
  if (targetTera==""){
    if (!(type2=="none")){ # Gives the new pokemon one of its up to 2 types as its tera type
      targetTera <- sample(c(type1,type2),size = 1,replace = FALSE)
    } else {
      targetTera <- type1
    }
  }

  # Finds the pokemon's ability name based on the targetAbility argument and the data
  # in the abilityData dataset and the pokemonData dataset
  if (targetAbility==1){
    abilNum <- pokemonData$Ability1[pokemonData$Identifier==targetPokemon]
  } else if (targetAbility==2){
    abilNum <- pokemonData$Ability2[pokemonData$Identifier==targetPokemon]
  } else if (targetAbility==3){
    abilNum <- pokemonData$HA[pokemonData$Identifier==targetPokemon]
  }

  # Error handling - if the ability number is returned as a 0, this indicates that the
  # pokemon does NOT have an ability in the requested slot. Execution halts and the
  # error message is displayed.
  if(abilNum==0){
    errorMSG <- paste(errorMSG," The desired pokemon does not have an ability in the chosen ability slot. Please review the argument and try again.")
    stop(errorMSG)
  }
  # Determines the ability's name from its number
  abilName <- toString(abilityData$Name[abilityData$Number==abilNum])

  # Calls the statCalc function to determine the value of each stat for the pokemon based on its species, individual values,
  # effort values, level, and nature. Stores the results in variables for later application.
  calcHP <- statCalc(poke = targetPokemon,stat = "HP", statIV = targetIVs[2],statEV = targetEVs[2],lvl = targetLvl,nature = targetNature)
  calcATK <- statCalc(poke = targetPokemon,stat = "ATK", statIV = targetIVs[2],statEV = targetEVs[2],lvl = targetLvl,nature = targetNature)
  calcDEF <- statCalc(poke = targetPokemon,stat = "DEF", statIV = targetIVs[3],statEV = targetEVs[3],lvl = targetLvl,nature = targetNature)
  calcSPA <- statCalc(poke = targetPokemon,stat = "SPA", statIV = targetIVs[4],statEV = targetEVs[4],lvl = targetLvl,nature = targetNature)
  calcSPD <- statCalc(poke = targetPokemon,stat = "SPD", statIV = targetIVs[5],statEV = targetEVs[5],lvl = targetLvl,nature = targetNature)
  calcSPE <- statCalc(poke = targetPokemon,stat = "SPE", statIV = targetIVs[6],statEV = targetEVs[6],lvl = targetLvl,nature = targetNature)

  # Calculates the total effort value points the pokemon has
  totalEV <- sum(targetEVs)

  # Determines if the desired effort values are outside of the aggregate maximum of 510
  if(totalEV > 510){
    # In the case that the requested effort value points exceed 510, sets the effort
    # value points all to 0. The user can later change these values.
    targetEVs <- c(0,0,0,0,0,0)
    # Updates the totalEV variable for later  application
    totalEV <- sum(targetEVs)
  }

  # Assigns the appropriate values to the pokemon data frame
  pokemonDataFrame["Name",1] <- pokemonData$Name[pokemonData$Identifier==targetPokemon] # Pulls pokemon name from the pokemonData data set base on the identifier
  pokemonDataFrame["Identifier",1] <- targetPokemon # Adds the pokemon identifier
  pokemonDataFrame["Number",1] <- pokemonData$Number[pokemonData$Identifier==targetPokemon] # Adds the national dex pokemon number
  pokemonDataFrame["Type1",1] <- type1 # Adds the pokemon's primary type
  pokemonDataFrame["Type2",1] <- type2 # Adds  the pokemon's secondary type (could include a value of "none")
  pokemonDataFrame["Ability.Name",1] <- abilName # Adds the name of the pokemon's ability
  pokemonDataFrame["Ability.Number",1] <- abilNum # Adds the ability number for the pokemon
  pokemonDataFrame["Nature",1] <- targetNature # Adds the nature of the pokemon
  pokemonDataFrame["Level",1] <- targetLvl # Adds the level of the pokemon
  pokemonDataFrame["Tera.Type",1] <- targetTera # Adds the terastallization type of the pokemon
  pokemonDataFrame["HP",1] <- calcHP # Adds the pokemon's Hit Points stat based on pokemon's data
  pokemonDataFrame["ATK",1] <- calcATK # Adds the pokemon's Attack stat based on pokemon's data
  pokemonDataFrame["DEF",1] <- calcDEF # Adds the pokemon's Defense stat based on pokemon's data
  pokemonDataFrame["SPA",1] <- calcSPA # Adds the pokemon's Special Attack stat based on pokemon's data
  pokemonDataFrame["SPD",1] <- calcSPD # Adds the pokemon's Special Defense stat based on pokemon's data
  pokemonDataFrame["SPE",1] <- calcSPE # Adds the pokemon's Speed stat based on pokemon's data
  pokemonDataFrame["HP.IV",1] <- targetIVs[1] # Adds the pokemon's HP individual value for later use
  pokemonDataFrame["ATK.IV",1] <- targetIVs[2] # Adds the pokemon's ATK individual value for later use
  pokemonDataFrame["DEF.IV",1] <- targetIVs[3] # Adds the pokemon's DEF individual value for later use
  pokemonDataFrame["SPA.IV",1] <- targetIVs[4] # Adds the pokemon's SPA individual value for later use
  pokemonDataFrame["SPD.IV",1] <- targetIVs[5] # Adds the pokemon's SPD individual value for later use
  pokemonDataFrame["SPE.IV",1] <- targetIVs[6] # Adds the pokemon's SPE individual value for later use
  pokemonDataFrame["HP.EV",1] <- targetEVs[1] # Adds the pokemon's HP effort value points for later use
  pokemonDataFrame["ATK.EV",1] <- targetEVs[2] # Adds the pokemon's ATK effort value points for later use
  pokemonDataFrame["DEF.EV",1] <- targetEVs[3] # Adds the pokemon's DEF effort value points for later use
  pokemonDataFrame["SPA.EV",1] <- targetEVs[4] # Adds the pokemon's SPA effort value points for later use
  pokemonDataFrame["SPD.EV",1] <- targetEVs[5] # Adds the pokemon's SPD effort value points for later use
  pokemonDataFrame["SPE.EV",1] <- targetEVs[6] # Adds the pokemon's SPE effort value points for later use
  pokemonDataFrame["EV.Total",1] <- totalEV # Adds the sum total of all effort value points for later use
  pokemonDataFrame["Move1",1] <- targetMoveset[1] # Adds the move for move slot 1
  pokemonDataFrame["Move2",1] <- targetMoveset[1] # Adds the move for move slot 2
  pokemonDataFrame["Move3",1] <- targetMoveset[1] # Adds the move for move slot 3
  pokemonDataFrame["Move4",1] <- targetMoveset[1] # Adds the move for move slot 4

  # Returns the now populated pokemonDataFrame
  return(pokemonDataFrame)
}
