## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pokestats)

## -----------------------------------------------------------------------------
my_first_pokemon <- generatePokemon(targetPokemon = 'pikachu')
my_first_pokemon

## -----------------------------------------------------------------------------
newTeam(slot1 = 'pikachu')

## -----------------------------------------------------------------------------
pikachu <- generatePokemon('pikachu')
myTeam <- newTeam(slot1 = 'raichu')

myTeam <- exchangeTeamSlot(team = myTeam,replace = 1,newPokemon = pikachu)

## -----------------------------------------------------------------------------
statCalc(poke = 'pikachu', stat = 'SPE', statIV = 10, statEV = 252,nature = 'timid')

## -----------------------------------------------------------------------------
my_pika <- generatePokemon('pikachu')
my_pika

my_pika <- evTrain(targetPoke = my_pika,spaMax = TRUE,speMax = TRUE,hpFill = TRUE)
my_pika

