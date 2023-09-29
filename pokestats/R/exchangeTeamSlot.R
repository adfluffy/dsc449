#' @title exchange team slot
#' @description Removes or replaces a pokemon on a pokemon team data frame
#' @param team Data Frame, the team that has a pokemon the user would like to have removed or replaced
#' @param replace integer, the team slot to be altered. Default is slot 1
#' @param name Data frame, the pokemon that should be placed in slot 'replace' on the team. Default is blank
#' @export
#' @keywords team,remove,replace
#' @return team
#' @examples \dontrun{
#' exchangeTeamSlot(team,replace,newPokemon)
#' }


exchangeTeamSlot <- function(team,replace=1,newPokemon=data.frame(matrix(nrow = 33,ncol = 1))){
  # initialize the error handling message
  errorMSG <- "An error has occurred in function 'exchangeTeamSlot'."

  # Determines if there are any concerns with the arguments passed to the function
  teamType <- typeof(team)!="list"
  newPokemonType <- typeof(newPokemon)!="list"
  teamSlots <- ncol(team)!=6
  teamRows <- nrow(team)!=33
  newPokemonRows <- nrow(newPokemon)!=33
  replaceInRange <- (replace>6) | (replace<1)

  # Evaluates the above determinations and terminates execution of the function with a custom
  # error message depending on the concern.
  if(teamType){
    # Error - team data frame was not passed to the function
    errorMSG <- paste(errorMSG," The team argument is of the incorrect data type. Please review this argument and try again.")
    stop(errorMSG)
  }
  if(newPokemonType){
    # Error- pokemon data frame was not passed to the function
    errorMSG <- paste(errorMSG," The newPokemon argument is of the incorrect data type. Please review this argument and try again.")
    stop(errorMSG)
  }
  if(teamSlots){
    # Error - team data frame is not of the correct dimensions (columns)
    errorMSG <- paste(errorMSG," The team data frame has an unexpected number of columns. Please review and try again.")
    stop(errorMSG)
  }
  if(teamRows){
    # Error - team data frame is not of the correct dimensions (rows)
    errorMSG <- paste(errorMSG," The team data frame has an unexpected number of rows. Please review and try again")
    stop(errorMSG)
  }
  if(newPokemonRows){
    # Error - newPokemon data frame is not of the correct size (rows)
    errorMSG <- paste(errorMSG," The newPokemon data frame has an unexpected number of columns. Please review and try again.")
    stop(errorMSG)
  }
  if(replaceInRange){
    # Error - replace argument is outside of the slot 1 to slot 6 range
    errorMSG <- paste(errorMSG," The replace argument is ouside of the integer range [1,6]. Please review this argument and try again.")
    stop(errorMSG)
  }

  # Creates a vector is the appropriate names for the rows of a pokemon data frame
  names4Rows <- c("Name","Identifier","Number","Type1","Type2","Ability.Name","Ability.Number","Nature","Level","Tera.Type","HP","ATK","DEF","SPA","SPD","SPE","HP.IV","ATK.IV","DEF.IV","SPA.IV","SPD.IV","SPE.IV","HP.EV","ATK.EV","DEF.EV","SPA.EV","SPD.EV","SPE.EV","EV.Total","Move1","Move2","Move3","Move4")
  # Assigns the appropriate row and column names to the newPokemon data frame. A frame passed by the
  # user should already be named accordingly, but this covers for the blank frame for removal as well.
  rownames(newPokemon) <- names4Rows
  colnames(newPokemon) <- c("Pokemon")

  # Replaces the 'replace' column in the team data frame with the newPokemon argument
  # (default is blank)
  team[,replace] <- newPokemon
  # Returns the adjusted team
  return(team)
}
