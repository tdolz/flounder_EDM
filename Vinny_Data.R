#Exploring the time series 5/14/24
# halted 6/6/24 because it's only 3 years of data? 

#install.packages("Hmisc")
library(Hmisc)
library(mdbtools)
library(tidyverse)
library(GPEDM)


###########################################Load access database#########################################################################

vin <- mdb.get(file="Beam_Trawl_DBASE.mdb")


########################################### Data formatting and exploration ####################################################################
#The different tables
head(vin$catch_details) #fish.id, catch.id, bag.id, length, weight, maturity. sex, ambi?
head(vin$catches) #haul id, catch.id, species.code, total weight, total n. 
head(vin$hauls) #station ID, date, vessel, haul.id, depth, time, bottom temp, salinity, sediment grain, habitat description, lat/lon, 
head(vin$stations) #station.id, station.name, ecosystem code
head(vin$species) #species code, common name and taxonomic name. 

#get winter flounder species code
sp.code <- vin$species[vin$species$common.name == "WINTER FLOUNDER_AGE_0", "species.code"][1]

#filter the catches that contain winter flounder. 
wf.catches <-filter(vin$catches, species.code==sp.code)
#seems that yoy wf were not weighed. 

#join to catch/haul/bag info, not sure which is the base unit. - seems like haul id
# then join to station info so we can make sense of station.id
enviro.data <-left_join(wf.catches,vin$hauls, by="haul.id")%>% left_join(vin$stations, by="station.id")%>%
  mutate(year=format(as.Date(date,format="%Y-%m-%d"),"%Y"), month=format(as.Date(date,format="%Y-%m-%d"),"%m"),
         day=format(as.Date(date,format="%Y-%m-%d"),"%d"), avDepth=(start.depth+end.depth)/2)

#what of this info is actually useful?
unique(enviro.data$grainsize) #no info here
unique(enviro.data$salinity) #no info here
unique(enviro.data$total.weight) #no info here
unique(enviro.data$month) #weirdly looks like some sampling in winter? 
  enviro.data %>% count(month)
  
summary(enviro.data)

#how many hauls per day
hauls <-vin$hauls%>%mutate(year=format(as.Date(date,format="%Y-%m-%d"),"%Y"), month=format(as.Date(date,format="%Y-%m-%d"),"%m"),
                           day=format(as.Date(date,format="%Y-%m-%d"),"%d"))
hauls%>%group_by(date)%>%summarize(hpd=n_distinct(haul.id))
#how many hauls per year
#how many hauls did not contain winter flounder? 

#we now have total number along with date, time and bottom temperature and location info. 

#preliminary data exploration. 


#make analysis units
wf.year <-enviro.data %>% group_by(year)%>%
  summarize(total_n=sum(total.number),mean_n=mean(total.number), avBT=mean(Btemp), avDepth=mean(avDepth))
wf.ecosystem <-enviro.data %>% group_by(year,ecosystem)%>%
  summarize(total_n=sum(total.number),mean_n=mean(total.number), avBT=mean(Btemp), avDepth=mean(avDepth), .groups="drop")
wf.station <-enviro.data %>% group_by(year,station.name)%>%
  summarize(total_n=sum(total.number),mean_n=mean(total.number), avBT=mean(Btemp), avDepth=mean(avDepth), .groups="drop")
  
#we might also want some other species that could be competitors/predators/prey
# we'll start with the obvious ones: Striped searobin, Winter flounder (not YOY), 
#Summer flounder, Striped Bass,blue crab, green crab, winter skate, cod , dogfish, monkfish, toadfish,

preds <-c("BLUEFISH","WINTER FLOUNDER","SUMMER FLOUNDER", "STRIPED SEAROBIN","SPINY DOGFISH","WINTER SKATE", "ATLANTIC COD",
          "STRIPED BASS", "NORTHERN SEAROBIN", "OYSTER TOADFISH", "BLUE CRAB", "GREEN CRAB")

#Based on previous research, we're also interested in late and early season bottom temperatures, late and early season abundance. 
early.szn <-mutate(enviro.data, month=format(as.Date(date,format="%Y-%m-%d"),"%m"))   

