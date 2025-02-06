#NOW THE ENVIRONMENTAL DATA! 

#extracting griddap data 

#There are a number of ways we could do this. But I am going to do a 50km radius around the individual and get it's data from the time it was caught because these are all adults here. Another version might be to integrate across the lifetime of the fish or some longer timescale.  

# **Getting data from erdapp**
#https://docs.ropensci.org/rerddap/articles/Using_rerddap.html
#https://github.com/ropensci/rerddap

library("rerddap")
library("akima")
library("dplyr")
library("tidyr")
library("ggplot2")
library("mapdata")
library("ncdf4")
library("rgdal")
library("raster") 
library('spatialrisk')
library('lubridate')
#library(devtools)
#devtools::install_github("cfree14/wcfish", force=T, ref= "main")
library("wcfish")
library("sf")


#I can't find longer time series for temperature, so for now, NOAA pathfinder. 
#https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst2Agg.html

info('ncdcOisst2Agg')


#We need to think about how we will aggregate the data. We have to think about the subset of the data that will be useful based on what data we are trying to eventually join it to. So first figure out the spatial and temporal extent of our data. 

N<-max(Mig_set$Lat) %>% conv_unit(from="dec_deg", to="deg_min_sec")
S <-min(Mig_set$Lat) %>% conv_unit(from="dec_deg", to="deg_min_sec")
E <- max(Mig_set$Long)%>% conv_unit(from="dec_deg", to="deg_min_sec")
W <-min(Mig_set$Long)%>% conv_unit(from="dec_deg", to="deg_min_sec")

min(Mig_set$Date,na.rm=T)
max(Mig_set$Date,na.rm=T)

#North bound: 44.34769, "44 20 51.6912000000011"
#South bound: 40.28612, "40 17 10.0200119999936"
#East bound: -67.29326, "-67 17 35.7402119999751"
#West bound: -73.87955,  "-73 52 46.3800000000047"

#the longitude used in this projection is weirdly 0deg to 360deg, so because our longitude values are negative, we must add 360 to them. 
newlon1 = -73.88 + 360
newlon2 = -67.29 + 360
sst_mon <-info('ncdcOisst2Agg')
SSTCA <-griddap(sst_mon, latitude=c(40.2, 44.4),longitude=c(newlon1,newlon2),  
                time=c("2016-01-01", "2019-01-01"), stride=c(7,1,1,1))

SSTCAdat <-mutate(SSTCA$data, longitude = longitude-360)
#unique(SSTCAdat$time)
SSTCAdat <- filter(SSTCAdat, !is.na(sst))%>%dplyr::select(-ice)

#It's taking one value every 7 days
# **TO CHANGE IT SO THAT IT TAKES A DAILY VALUE, SET STRIDE TO C(1,1,1,1)** but it will be much longer to run. 
# April 2024 update, try changing it to every 7 days. 

#what are the mean max min values? are they realistic?
max(SSTCAdat$sst)
min(SSTCAdat$sst)
mean(SSTCAdat$sst)

################################# TEST ###################################################
#Extract all temperature values that fall within a 10km radius of the animal. 
points <-spatialrisk::points_in_circle(SSTCAdat, lon_center = strata3$LON[1],
                                       lat_center = strata3$LAT[1],
                                       lon=longitude, lat=latitude, #use long instead of lon so they're on the same scale. 
                                       radius = 10000)#[,c(1,9)] # specify radius around the centroid. 
points
#Works but there is a really wide temperature range contained within this timeframe. 

##########################################################################################################################################
############## GOAL: Try to extract all temperature from within 10km radius and 1 month time of when individual was caught.################# 

#inputs 
strata3$catch.date=as.Date(strata3$catch.date,format="%m/%d/%y")
strata3$catch.date=format(strata3$catch.date, "%m-%d-%Y")
strata3$Date <- as.POSIXct(strata3$catch.date, format =  "%m-%d-%Y")
#strata5 <-strata3[!is.na(strata3$LAT),]
#strata5 <-strata5[!is.na(strata5$catch.date),]
strata5 <-strata3

#outputs
strata5$sst <-rep(NA,length(strata5$id.save))


for (i in 1:length(strata5$id.save)){
  #first all the measurements. 
  points<-spatialrisk::points_in_circle(SSTCAdat, lon_center = strata5$LON[i],
                                        lat_center = strata5$LAT[i],
                                        lon=longitude, lat=latitude, #use long instead of lon so they're on the same scale. 
                                        radius = 50000)#[,c(1,9)] # specify radius around the centroid. 
  #filter the dates to one month around the date the animal was caught.
  points<-filter(points, time >=strata5$Date[i]-days(15) & time <= strata5$Date[i]+days(15))
  strata5$sst[i] <-mean(points$sst,na.rm=T) #append the sst data to the original data frame. 
}

