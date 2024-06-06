#Exploring the time series 5/14/24

#install.packages("Hmisc")
library(Hmisc)
library(mdbtools)
library(tidyverse)
library(GPEDM)


###Load access database###

vin <- mdb.get(file="Beam_Trawl_DBASE.mdb")

#This is not working on my work machine