#' @title effort values training
#' @description applies effort values to the target pokemon and returns the pokemon frame with calculated final stat and the ev fields filled out
#' @param targetPoke pokemon data frame to perform effort value training on
#' @param hp integer value for HP stat effort value. Default is 0
#' @param hpFill Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the HP stat. Default is FALSE
#' @param hpMax Boolean, indicates that the user wants to apply 252 effort value points to the HP stat. Default is FALSE
#' @param atk integer value for ATK stat effort value. Default is 0
#' @param atkFill Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the ATK stat. Default is FALSE
#' @param atkMax Boolean, indicates that the user wants to apply 252 effort value points to the ATK stat. Default is FALSE
#' @param def integer value for DEF stat effort value. Default is 0
#' @param defFill Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the DEF stat. Default is FALSE
#' @param defMax Boolean, indicates that the user wants to apply 252 effort value points to the DEF stat. Default is FALSE
#' @param spa integer value for SPA stat effort value. Default is 0
#' @param spaFill Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the SPA stat. Default is FALSE
#' @param spaMax Boolean, indicates that the user wants to apply 252 effort value points to the SPA stat. Default is FALSE
#' @param spd integer value for SPD stat effort value. Default is 0
#' @param spdFill Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the SPD stat. Default is FALSE
#' @param spdMax Boolean, indicates that the user wants to apply 252 effort value points to the SPD stat. Default is FALSE
#' @param spe integer value for SPE stat effort value. Default is 0
#' @param speFill Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the SPE stat. Default is FALSE
#' @param speMax Boolean, indicates that the user wants to apply 252 effort value points to the SPE stat. Default is FALSE
#' @export
#' @keywords ev,train
#' @return targetPoke
#' @examples \dontrun{
#' evTrain(targetPoke,hp=0,hpFill=FALSE,hpMax=FALSE,atk=0,atkFill=FALSE,atkMax=FALSE,def=0,defFill=FALSE,defMax=FALSE,spa=0,spaFill=FALSE,spaMax=FALSE,spd=0,spdFill=FALSE,spdMax=FALSE,spe=0,speFill=FALSE,speMax=FALSE)
#' }

evTrain <- function(targetPoke,hp=0,hpFill=FALSE,hpMax=FALSE,atk=0,atkFill=FALSE,atkMax=FALSE,def=0,defFill=FALSE,defMax=FALSE,spa=0,spaFill=FALSE,spaMax=FALSE,spd=0,spdFill=FALSE,spdMax=FALSE,spe=0,speFill=FALSE,speMax=FALSE){
  # Initializes the error handling message
  errorMSG <- "An error has occurred in function evTrain."

  if(typeof(targetPoke)!="list"){
    # Error - targetPoke is not a pokemon frame
    errorMSG <- paste(errorMSG," Unexpected data type for the targetPoke argument. Please pass a data frame or list to this argument.")
    stop(errorMSG)
  }

  if(nrow(targetPoke)!=33){
    # Error - pokemon frame is of the improper length
    errorMSG <- paste(errorMSG," Unexpected length for the targetPoke argument. Please pass a data frame of length 33 to the function.")
    stop(errorMSG)
  }

  # Defines the max single stat EV value
  maxEV <- 252
  # Defines the sum total of all ev points a single pokemon can have
  maxTotalEV <- 510
  # Creates an ordered list of stats, ev points per stat, and the positions of both data type in the pokemon frames
  statList <- c("HP","ATK","DEF","SPA","SPD","SPE")
  statPositions <- c(11:16)
  evList <- c("HP.EV","ATK.EV","DEF.EV","SPA.EV","SPD.EV","SPE.EV")
  evPositions <- c(23:28)
  # Creates vectors for the target ev point application, the calling of the evMax protocol, and the calling of the evFill protocol
  targetEVs <- c(hp,atk,def,spa,spd,spe)
  evMax <- c(hpMax,atkMax,defMax,spaMax,spdMax,speMax)
  evFill <- c(hpFill,atkFill,defFill,spaFill,spdFill,speFill)

  # Creates aggregate values for the requested EV points total, the number of evMax protocols called, and the number of evFill
  # protocols called.
  evTotal <- sum(targetEVs)
  maxCount <- sum(evMax)
  fillCount <- sum(evFill)

  # Creates a vector of the target pokemon's stats for alteration and final reporting
  finalStats <- c(targetPoke[11,1],targetPoke[12,1],targetPoke[13,1],targetPoke[14,1],targetPoke[15,1],targetPoke[16,1])
  # Creates a vecotr of the target pokemon's individual values for stat caluclation
  pokeIVs <- c(targetPoke[17,1],targetPoke[18,1],targetPoke[19,1],targetPoke[20,1],targetPoke[21,1],targetPoke[22,1])
  # Ensures that the IV data type is integer for proper calculation later
  pokeIVs <- strtoi(pokeIVs)

  # Error handling on the calculated values above (lines 72-119)
  for(i in 1:length(targetEVs)){
    if(targetEVs[i]>maxEV){
      # Error - requested ev points above 252 for one stat
      errorMSG <- paste(errorMSG," One of the EV values is above the EV limit of 252. Please correct this error and try again.")
      stop(errorMSG)
    } else if (targetEVs[i]<0){
      # Error - requested negative EV points
      errorMSG <- paste(errorMSG, " One of the EV values passed is less than 0. Please correct this error and try again.")
      stop(errorMSG)
    }
  }

  if((evTotal+252*maxCount)>maxTotalEV){
    # Error - total requested EV points exceed the 510 point limit
    errorMSG <- paste(errorMSG," Requested EV values total more than 510. This exceeds the allowable EV limit - please correct this and try again.")
    stop(errorMSG)
  }

  if(maxCount>2){
    # Error - more than 2 stats are requested to be set at 252
    errorMSG <- paste(errorMSG," More than 2 EV values have been requested at the 252 level. This exceeds the allowable EV limit - please correct this error and try again.")
    stop(errorMSG)
  }
  if(fillCount+maxCount > 6){
    # Error - more stats are set to fill or max than there are stats. Since filling and maxing a stat are
    # exclusive, this sum cannot exceed 6
    errorMSG <- paste(errorMSG," More EV values have been instructed to be filled/maxed than their are stats. Please review passed arguements and try again.")
    stop(errorMSG)
  }

  for(i in 1:6){
    if(evMax[i] & evFill[i]){
      # Error - the same stat is requested to be filled and maxed. These operations are exclusive, and thus cannot both be performed.
      errorMSG <- paste(errorMSG," You have requested to both fill and max one stat. These functions are unable to be combined - please review your arguments and try again.")
      stop(errorMSG)
    }
    if(evMax[i] & targetEVs[i]!=0){
      # Error - an ev point value was provided for a stat that the evMax protocol was requested on.
      errorMSG <- paste(errorMSG," You have provided a numeric value for a stat that you also requested be maximized. Please review arguments and try again")
      stop(errorMSG)
    }
    if(evFill[i] & targetEVs[i]!=0){
      # Error - an ev point value was provided for a stat that the evFill protocol was requested on.
      errorMSG <- paste(errorMSG," You have provided a numeric value for a stat that you also requested be filled with remaining effort value points. Please review arguments and try again")
      stop(errorMSG)
    }
  }


  # The below code executes the evMax protocol. The maxEV value is assigned to the
  # appropriate stats in the targetEVs vector for later calculation
  for(i in 1:length(evMax)){
    if(evMax[i]){ # executes for stat where evMax = TRUE
      targetEVs[i] <- maxEV
    }
  }
  # Recalculates teh evTotal value after updating with the max evs
  evTotal <- sum(targetEVs)

  # The below code performs the evFill protocol. The number of requested fill stats
  # is used to determine the value for the fill, and each filled stat is assigned that value
  for(i in 1:length(evFill)){
    if(evFill[i]){
      targetEVs[i] <- min(c(floor((maxTotalEV-evTotal)/fillCount),maxEV))
    }
  }


  # Calls the statCalc function to determine the effort value trained stats for the
  # target pokemon. This value is then applied to the finalStats vector in the appropriate
  # slot
  for(i in 1:length(targetEVs)){
    newStat <- statCalc(poke=targetPoke[2,1],stat = statList[i],statIV = pokeIVs[i],statEV = targetEVs[i],lvl = strtoi(targetPoke[9,1]),nature = targetPoke[8,1])
    finalStats[i] <- newStat
  }

  # Assigns the effort value points total for each stat to the target pokemon data frame
  # as well as the newly calculated values of those stats.
  for(i in 1:length(finalStats)){
    targetPoke[evPositions[i],1] <- targetEVs[i]
    targetPoke[statPositions[i],1] <- finalStats[i]
  }

  # Records the effort value points total in the targetPoke pokemon frame after performing a
  # final update to the evTotal variable
  evTotal <- sum(targetEVs)
  targetPoke[29,1] <- evTotal
  # Returns the updated target pokemon
  return(targetPoke)
}
