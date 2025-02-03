rm(list=ls(all=TRUE))
#****************Calculates stratum statistics for Seine Survey**


#****************Calculates stratified statistics for all estuaries

#*************************************************************************
# READS IN DATA FROM EXCEL FILE:  ALL STATIONS, YEARS 76- ONWARD 

data<-read.csv("DMF_seine/Dolan/1060_catch_76-23.csv", header=TRUE)

data
names (data)
min1<- min (data$year)
max1 <- max (data$year) 

#creates vector for years (1976-2023) 

 minyear<- min (data$year)
 maxyear<- max (data$year)
 year <- (minyear:maxyear)
 year

#*************************************************************************
#Table of weights based on perimeter (nautical miles )of estuary

greatpondw <- (7.31)
waquoitw   <- (17.38)
cotuitw    <- (24.08)
lewisw     <- (9.37)
bassrw     <- (20.05)
stagew     <- (11.66)

greatpondsqw <- greatpondw^2
waquoitsqw <- waquoitw^2
cotuitsqw <- cotuitw^2
lewissqw <- lewisw^2
bassrsqw <- bassrw^2
stagesqw <- stagew^2

totalw <- greatpondw+waquoitw+cotuitw+lewisw+bassrw+stagew
totalsqw <- totalw^2

# potential number of area sampling units
A1 <- (823)
A2 <- (1956)
A3 <- (2710)
A4 <- (1054)
A5 <- (2256)
A6 <- (1312)



#***********************************************************************************
#CALCULATES STRATUM STATISTICS FOR EACH ESTUARY NUMBER PER TOW

names (data)

# CALCULATES STRATUM MEANS

	datamw<-as.data.frame(tapply(X=data$density,list(data$year,data$estuary),mean))
          names (datamw) <-c ("md1", "md2", "md3", "md4", "md5", "md6")
	
	
# CALCULATES STRATUM VARIANCES (sample)

	datamwvar<-as.data.frame (tapply (X=data$density, list(data$year,data$estuary),var))
	names (datamwvar) <-c ("var1", "var2", "var3", "var4", "var5", "var6")
	datamwvar       
      names (datamwvar)

 
# SQUARES THE SAMPLE VARIANCES  


       datavarsq<- datamwvar*datamwvar
	 names (datavarsq) <-c ("var1sq", "var2sq", "var3sq", "var4sq", "var5sq", "var6sq")
	 datavarsq
        
       
# MAKES A LIST OF THE NUMBER OF STATIONS SAMPLED IN EACH ESTUARY EACH YEAR


	ntows<-as.data.frame (tapply (X=data$density, list(data$year,data$estuary),length))
	names (ntows)<-c ("N1", "N2", "N3", "N4", "N5", "N6")                   
	names (ntows)
      ntows

#  CALCULATES N-1 FOR EACH STRATUM (ESTUARY)

      ntows_1 <- as.data.frame(ntows)-1
      names (ntows_1) <- c ("NM1", "NM2", "NM3", "NM4", "NM5", "NM6")  	
	ntows_1

# CALCULATES THE VARIANCE OF THE MEAN DENSITY AT EACH STRATUM (ESTUARY)
	
	datamwvom<- datamwvar/ntows
	names (datamwvom) <-c ("vom1", "vom2", "vom3", "vom4", "vom5", "vom6")
  	datamwvom                 

#********************************************************************************
#****** BE SURE TO CHANGE THE NUMBER TO THE TERMINAL YEAR !!!!!!!!!!*************
#********************************************************************************

#  CALCULATES THE STANDARD ERROR AT EACH ESTUARY     
	
	datamwse<- sqrt(datamwvom)
	names (datamwse)<-c ("se1", "se2", "se3", "se4", "se5", "se6")
      length (datamwse[length(datamwse[,1]),])  
	datamwse[1:48,]        

#***** DID YOU CHANGE THE NUMBER OF YEARS IN THE TIMESERIES ??????? ***********

#  Calculates the t values for 95% confidence limits for each stratum

	tv1 <- qt (0.975, ntows_1$NM1)
      tv2 <- qt (0.975, ntows_1$NM2)
      tv3 <- qt (0.975, ntows_1$NM3)
      tv4 <- qt (0.975, ntows_1$NM4)
      tv5 <- qt (0.975, ntows_1$NM5)
      tv6 <- qt (0.975, ntows_1$NM6)


	tv.est <- cbind (tv1, tv2, tv3, tv4, tv5, tv6)
      tv.est <- as.data.frame (tv.est)
	names (tv.est)<- c ("tv1", "tv2", "tv3", "tv4", "tv5", "tv6")

   
#**************************************************************************
#   CALCULATES + AND - 2 STANDARD ERRORS FOR EACH ESTUARY

	datahigh <- datamw + 2*datamwse
      names (datahigh)<-c ("shi1", "shi2", "shi3", "shi4", "shi5", "shi6")
      datahigh
  
      datalow <- datamw - 2*datamwse
      names (datalow)<-c ("slo1", "slo2", "slo3", "slo4", "slo5", "slo6")
      datalow

#**************************************************************************
#CREATES A DATA SET WITH ALL STRATUM STATISTICS
      lab1 <- "Great Pond"
	lab2 <- "Waquoit Bay"
	lab3 <- "Cotuit Bay"
	lab4 <- "Lewis Bay"
	lab5 <- "Bass River"
	lab6 <- "Stage Harbor"
	lab7 <- "All Estuaries"
	blank.row      <-  ""

	label <- c(lab1, lab2, lab3, lab4, lab5, lab6, lab7)

	dset <- cbind (year, ntows, datamw, datamwvar, datamwvom, datamwse, datalow, datahigh)
	dset
	names (dset)

	
	GP <- cbind (dset$year, dset$N1, dset$md1, dset$var1, 
             dset$se1, dset$slo1, dset$shi1)
      GP <- as.data.frame (GP)
	names (GP)<- c("year","N", "mean" , "Var", "SE", "lowCL", "HiCL")   

	
      WQ <- cbind (dset$year, dset$N2, dset$md2, dset$var2, 
              dset$se2, dset$slo2, dset$shi2)
      WQ <- as.data.frame (WQ)
	names (WQ)<- c("year", "N", "mean" , "Var", "SE", "lowCL", "HiCL")   


	CB <- cbind (dset$year, dset$N3, dset$md3, dset$var3, 
             dset$se3, dset$slo3, dset$shi3)
      CB <- as.data.frame (CB)
	names (CB)<- c("year","N", "mean" , "Var", "SE", "lowCL", "HiCL")   

	
      LB <- cbind (dset$year, dset$N4, dset$md4, dset$var4, 
              dset$se4, dset$slo4, dset$shi4)
      LB <- as.data.frame (LB)
	names (LB)<- c("year", "N", "mean" , "Var", "SE", "lowCL", "HiCL")   


	BR <- cbind (dset$year, dset$N5, dset$md5, dset$var5, 
             dset$se5, dset$slo5, dset$shi5)
      BR <- as.data.frame (BR)
	names (BR)<- c("year","N", "mean" , "Var", "SE", "lowCL", "HiCL")   

	
      SH <- cbind (dset$year, dset$N6, dset$md6, dset$var6, 
              dset$se6, dset$slo6, dset$shi6)
      SH <- as.data.frame (SH)
	names (SH)<- c("year", "N", "mean" , "Var",  "SE", "lowCL", "HiCL")   

#***************************************************************************
#CORRELATIONS AMONG ESTUARIES

correlations <-  cor(datamw)
correlations <- as.data.frame (correlations)
names (correlations) <- c("Great Pond", "Waquoit Bay", "Cotuit", 
				 "Lewis Bay", "Bass River", "Stage Harbor")

rownames (correlations)<-c("Great Pond", "Waquoit Bay", "Cotuit", 
				 "Lewis Bay", "Bass River", "Stage Harbor")

correlations

#*******************************************************************************
#SUMMARY STATISTICS FOR MEAN DENSITY FOR EACH ESTUARY
summary_GP <- cbind (
min(GP$mean),
quantile (GP$mean, c(0.25)),
mean (GP$mean),
quantile (GP$mean, c(0.50)),
quantile (GP$mean, c(0.75)),
max (GP$mean))

summary_GP <- as.data.frame (summary_GP)
names (summary_GP)<- c ("min","25th", "median", "mean", "75th", "max") 
rownames (summary_GP) <-("Great Pond")
summary_GP


summary_WQ <- cbind (
min(WQ$mean),
quantile (WQ$mean, c(0.25)),
mean (WQ$mean),
quantile (WQ$mean, c(0.50)),
quantile (WQ$mean, c(0.75)),
max (WQ$mean))

summary_WQ <- as.data.frame (summary_WQ)
names (summary_WQ)<- c ("min","25th", "median", "mean", "75th", "max") 
rownames (summary_WQ) <-("Waquoit Bay")
summary_WQ

summary_CB <- cbind (
min(CB$mean),
quantile (CB$mean, c(0.25)),
mean (CB$mean),
quantile (CB$mean, c(0.50)),
quantile (CB$mean, c(0.75)),
max (CB$mean))

summary_CB <- as.data.frame (summary_CB)
names (summary_CB)<- c ("min","25th", "median", "mean", "75th", "max") 
rownames (summary_CB) <-("Cotuit Bay")
summary_CB


summary_LB <- cbind (
min(LB$mean),
quantile (LB$mean, c(0.25)),
mean (LB$mean),
quantile (LB$mean, c(0.50)),
quantile (LB$mean, c(0.75)),
max (LB$mean))

summary_LB <- as.data.frame (summary_LB)
names (summary_LB)<- c ("min","25th", "median", "mean", "75th", "max") 
rownames (summary_LB) <-("Lewis Bay")
summary_LB

summary_BR <- cbind (
min(BR$mean),
quantile (BR$mean, c(0.25)),
mean (BR$mean),
quantile (BR$mean, c(0.50)),
quantile (BR$mean, c(0.75)),
max (BR$mean))

summary_BR <- as.data.frame (summary_BR)
names (summary_BR)<- c ("min","25th", "median", "mean", "75th", "max") 
rownames (summary_BR) <-("Bass River")
summary_BR

summary_SH <- cbind (
min(SH$mean),
quantile (SH$mean, c(0.25)),
mean (SH$mean),
quantile (SH$mean, c(0.50)),
quantile (SH$mean, c(0.75)),
max (SH$mean))

summary_SH <- as.data.frame (summary_SH)
names (summary_SH)<- c ("min","25th", "median", "mean", "75th", "max") 
rownames (summary_SH) <-("Stage Harbor")
summary_SH

#  PRINTS ESTUARY STATISTICAL SUMMARY TO FILE 


	sink ("C://Users/vmanfredi/Desktop/DAILY_FILES_FOR_UPLOAD/SEINE 2023/1060_summary_2023.csv")

		lab1
            blank.row
		GP
		blank.row
            summary_GP
		blank.row

		lab2
		WQ
		blank.row
		summary_WQ
            blank.row

		lab3
		CB
		blank.row
		summary_CB
		blank.row

		lab4
            blank.row
		LB
		blank.row
            summary_LB
            blank.row
		
		lab5
		BR
		blank.row
		summary_BR
		blank.row

		lab6
		SH
		blank.row
		summary_SH
            blank.row
      
	#  Correlations coefficients among stations 1976-2010
	#  WARNING:  station set is inconsistent before and after 1982
      
            blank.row
		correlations
sink ()

#*************************************************************************

# THESE SECTIONS CALCULATE STRATIFIED STATISTICS

#*************************************************************************
 #CALCULATES STRATIFIED MEAN DENSITY FOR 6 ESTUARIES


	stratmean<-(dset$md1*greatpondw+dset$md2*waquoitw+dset$md3*cotuitw+ dset$md4*lewisw+dset$md5*bassrw
	+dset$md6*stagew)/totalw

	stratmean

#****************************************************************************
#CALCULATES STRATIFIED VARIANCE OF THE MEAN FOR MEAN DENSITY

     	stratvom<-(dset$vom1*greatpondsqw+dset$vom2*waquoitsqw+dset$vom3*cotuitsqw+ dset$vom4*lewissqw
           +dset$vom5*bassrsqw +dset$vom6*stagesqw)/totalsqw

	stratvom

#**************************************************************************

# CALCULATES THE STRATIFIED STANDARD ERROR FOR MEAN DENSISTY

stratSE=sqrt(stratvom)

stratSE

#************************************************************************

#  Calculates G values
names (dset)
	G1<- (A1*(A1-dset$N1)/dset$N1)
	g1 <- G1*G1
	     
	G2<-  (A2*(A2-dset$N2)/dset$N2)
      g2 <- G2*G2
	
	G3<-  (A3*(A3-dset$N3)/dset$N3)
	g3 <- G3*G3

	G4<-  (A4*(A4-dset$N4)/dset$N4)
	g4 <- G4*G4

	G5 <- (A5*(A5-dset$N5)/dset$N5) 
      g5 <- G5* G5
      
	G6<- (A6*(A6-dset$N6)/dset$N6)
      g6 <- G6*G6

  	G<- cbind (G1, G2, G3, G4, G5, G6)
  		G <- as.data.frame (G)
	names (G) <- c ("G1", "G2", "G3", "G4", "G5", "G6")
G
	g<- cbind (g1, g2, g3, g4, g5, g6)
	g <- as.data.frame (g)  
  	names (g) <-c ("g1", "g2", "g3", "g4", "g5", "g6")
g
#*******************************************************************
 #  Calculates Effective Degrees of freedom for each year

   	s1 <- as.data.frame (G)*(datamwvar)
	names (s1)<- c("s1", "s2", "s3", "s4", "s5", "s6")
	
  #  sums variance weighted by G for each estuary by year 	
     	stot <- apply (s1, 1, sum)
 	stot <- as.data.frame (stot)	   

	stotsq <- stot*stot
	stotsq

   	totstot <- cbind (stotsq) 
   	totstot<- as.data.frame (stotsq)
   	totstot
	
  #  Sum of the squared variances weighted by G squared

      s2<- as.data.frame (g)*(datavarsq)/(ntows_1)
      names (s2)<- c("t1", "t2", "t3", "t4", "t5", "t6")
     
	s2tot <- apply (s2,1,sum)
      s2tot <- as.data.frame (s2tot)
   
 #  Calculates the effective degrees of freedom   
     df <- totstot/s2tot
     df <- cbind (df)
     names (df)<- c("df")
     
#********************************************************************
# Computes critical values from the T distribution at P=0.95 

# NOTE THAT R CALCULATES AREA FOR 1 TAIL DISTRIBUTION 
  
   tv <- qt(0.975, df$df)
   tv <- as.data.frame (tv)
   names (tv)<- c("tvalue")

#**************************************************************************
#CALCULATES 95% CONFIDENCE LIMITS FOR STRATIFIED MEAN DENSITY
	
	stratlow <- as.data.frame (stratmean - tv*stratSE)
	strathigh<- as.data.frame (stratmean + tv*stratSE)

# **********************************************************************
# CREATES OUTPUT FILE

# CREATES A DATAFILE WITH 
output <- cbind (year, stratmean, stratvom, stratSE, df, tv, stratlow, strathigh)

output <- as.data.frame (output)
names (output) <- c("year", "stratmean", "stratvom", "stratSE", "df", "tvalue", "lowCI", "upperCI")
output

#**************************************************************************************************************************

#  CALCULATES MINIMUM VALUE, 25, 50 AND 75TH PERCENTILE, MEAN, MAXIMUN VALUE
summary_est2 <- rbind (
min(output$stratmean),
quantile (output$stratmean, c(0.25)),
mean (output$stratmean),
quantile (output$stratmean, c(0.50)),
quantile (output$stratmean, c(0.75)),
max (output$stratmean))

summary_est2 <- as.data.frame (summary_est2)
rownames (summary_est2)<- c("min","25th", "median", "mean", "75th", "max") 
names (summary_est2) <-("Summary Stratified Mean")

#****************************************************************************
# CREATES PLOTS FOR INDIVIDUAL ESTUARIES

# calls the lattice program for doing xyplot
    	library(lattice)

# calls for graphics to be spooled and displayed in R
	graphics.off()
  	windows(record=TRUE, height=10, width=20)
      .SavedPlots <- NULL

#sets paramater for graphic to plot out 4 graphs to a page, oma sets margins

par(mar=c(2,2.6,2,.5), oma=c(1.6,1.6,1,1), mfrow=c(2,3), xaxp= c(x1=minyear, x2=maxyear+2, n=2))

# calls for the length of the timeseries and uses it as a vector to draw 
# the appropriate number of median dashes in the graph

	length (year)

# calculates the median for an individual estuary

	median.meanw.md1 <- c (rep (median(datamw$md1), length(year)))
	median.meanw.md2 <- c (rep (median(datamw$md2), length(year)))
	median.meanw.md3 <- c (rep (median(datamw$md3), length(year)))
	median.meanw.md5 <- c (rep (median(datamw$md5), length(year)))
	median.meanw.md4 <- c (rep (median(datamw$md4), length(year)))
	median.meanw.md6 <- c (rep (median(datamw$md6), length(year)))

# plots the individual estuary index with 95% CI and median line

	plot(year,datahigh$shi1,type="l",lty=c(2),col="blue", 
	ylab="", xaxt="n",yaxt="s",
	xlab="year",main="Great Pond", cex.main=1.5)
	axis(1,seq(minyear,maxyear,1),tcl=-0.3,cex.axis=1.1,mgp=c(1.2,0.8,0)) 
	lines(year, datamw$md1 ,type="l", lty=c(1), col="black")
	lines(year, datalow$slo1,type="l", lty=c(2), col="blue") 
	lines(year, median.meanw.md1 ,type="l", lty=c(1), col="red")

# X-AXIS LABELS IN OUTER MARGIN
mtext("Year", 1, line=.4, adj=.5, cex=1.0, col="black", outer=TRUE) 

# Y-AXIS LABELS IN OUTER MARGIN
mtext("Number per Square Meter", 2, line=.4, adj=.5, cex=1.2, col="black"
	, outer=TRUE)

	plot(year,datahigh$shi2,type="l",lty=c(2),col="blue", 
	ylab="", xaxt="n",yaxt="s",
	xlab="",main="Waquoit Bay",cex.main=1.5) 
	axis(1,seq(minyear,maxyear,1),tcl=-0.3,cex.axis=1.1,mgp=c(1.2,0.8,0)) 
	lines(year, datamw$md2 ,type="l", lty=c(1), col="black")
	lines(year, datalow$slo2,type="l", lty=c(2), col="blue") 
	lines(year, median.meanw.md2 ,type="l", lty=c(1), col="red")

	plot(year,datahigh$shi3,type="l",lty=c(2),col="blue", 
	ylab="", xaxt="n",yaxt="s",
	xlab="",main="Cotuit Bay",cex.main=1.5) 
	axis(1,seq(minyear,maxyear,1),tcl=-0.3,cex.axis=1.1,mgp=c(1.2,0.8,0))   
	lines(year, datamw$md3 ,type="l", lty=c(1), col="black")
	lines(year, datalow$slo3,type="l", lty=c(2), col="blue") 
	lines(year, median.meanw.md3 ,type="l", lty=c(1), col="red")

	plot(year,datahigh$shi4,type="l",lty=c(2),col="blue", 
	ylab="", xaxt="n",yaxt="s",
	xlab="",main="Lewis Bay",cex.main=1.5) 
	axis(1,seq(minyear,maxyear,1),tcl=-0.3,cex.axis=1.1,mgp=c(1.2,0.8,0))   
	lines(year, datamw$md4 ,type="l", lty=c(1), col="black")
	lines(year, datalow$slo4,type="l", lty=c(2), col="blue") 
	lines(year, median.meanw.md4 ,type="l", lty=c(1), col="red")

	plot(year,datahigh$shi5,type="l",lty=c(2),col="blue", 
	ylab="", xaxt="n",yaxt="s",
	xlab="",main="Bass River",cex.main=1.5) 
	axis(1,seq(minyear,maxyear,1),tcl=-0.3,cex.axis=1.1,mgp=c(1.2,0.8,0))   
	lines(year, datamw$md5 ,type="l", lty=c(1), col="black")
	lines(year, datalow$slo5,type="l", lty=c(2), col="blue") 
	lines(year, median.meanw.md5 ,type="l", lty=c(1), col="red")

	plot(year,datahigh$shi6,type="l",lty=c(2),col="blue", 
	ylab="", xaxt="n",yaxt="s",
	xlab="", main="Stage Harbor",cex.main=1.5) 
	axis(1,seq(minyear,maxyear,1),tcl=-0.3,cex.axis=1.1,mgp=c(1.2,0.8,0))   
	lines(year, datamw$md6 ,type="l", lty=c(1), col="black")
	lines(year, datalow$slo6,type="l", lty=c(2), col="blue") 
	lines(year, median.meanw.md6 ,type="l", lty=c(1), col="red")
     
savePlot(file="C://Users/vmanfredi/Desktop/DAILY_FILES_FOR_UPLOAD/SEINE 2023/Trends for Individual Estuaries_1060_2023",type="bmp")
#*************************************************************************************************************
#		GAM INDEX PLOT

library(latticeExtra)	
library (lattice)
library(mgcv)

	# calls for graphics to be spooled and displayed in R
	graphics.off()
  	windows(record=TRUE, height=4, width=6)
      .SavedPlots <- NULL
	library (lattice)

	bottomnGAM <- 	"Black line: GAM fit.
				Grey line: timeseries median."

xyplot (output$stratmean ~ output$year,
      data=output,
      panel=function (x,y) {
            panel.grid (v=-1, h=-1)
            panel.abline (h=median (y), col="grey", lwd=3)
	     	panel.smoother(y~s(x), method="gam", col="black", lwd=2)
            panel.xyplot (x,y,col="blue", cex=.8)
	panel.rug(data$year, side=1, col="black")},
	scales=list(cex=1,1),
      layout= c (1,1),
      xlab=list("Year",cex=1),
      ylab= list("Stratified Mean Number Per Square M",cex=1),
	main=list ("Figure 2. YOY Winter Flounder Abundance
			MDMF Seine Survey: 1976-2023", cex=1.1),
	sub=list (label=bottomnGAM, cex=.8)
	)


savePlot(file="C://Users/vmanfredi/Desktop/DAILY_FILES_FOR_UPLOAD/SEINE 2023/ANNRPT_1060_index wGAM_2023",type="bmp")

# DMFNEWS_Q3n4_2022_YOY106_SEINE
rm(list=ls(all=TRUE))



