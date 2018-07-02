rm(list = ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(prodlim)
library(matrixStats)

source("ReadBounds.R")            #Reads "data/boundaries.txt" & "data/BoundsHypoxicArea.txt"
source("ReadBathymetry.R")        #Reads "data/hypsograph.txt"
source("ReadIndicatorType.R")     #Reads "data/IndicatorList.txt"
source("ReadVariances.R")         #Reads "data/varcomp.txt"
source("IndicatorFunctions.R")

# ------------load monitoring data for testing ------------ 
load("dftest.Rda")

WBselected<-"SE554500-125001"
typology<-"6"
Ind<-"CoastOxygen"
MeasuredVar<-"Oxygen"
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)

# ------------ call functions to load boundary, indicator and variance data ------------
df.bounds<-ReadBounds()
df.bounds.hypox<-ReadBoundsHypoxicArea()
df.bathy<-ReadBathymetry()
df.indicators<-ReadIndicatorType()
df.variances<-ReadVariances()

# ------------  Get variance list for the selected Waterbody / Measurement variable ------------ 
df.variances<-df.variances %>% filter(Type==typology, Measurement==MeasuredVar)
variance_list <- list(V_station=df.variances$V_station[1],
                      V_obspoint=df.variances$V_station[1],
                      V_year=df.variances$V_year[1],
                      V_yearmonth=df.variances$V_yearmonth[1],
                      V_stationdate=df.variances$V_stationdate[1],
                      V_stationyear=df.variances$V_stationyear[1],
                      V_stationmonth=df.variances$V_stationmonth[1],
                      V_institution=df.variances$V_institution[1],
                      V_replication=df.variances$V_replication[1])
variance_list <- lapply(variance_list, function(x) ifelse(is.na(x),0,x))
cat("\nvariance_list:\n")
print(unlist(variance_list))

# ------------  Get boundaries according to type & indicator ------------  
BoundsList<-df.bounds %>% filter(Type==typology,Indicator==Ind)

# ------------  Get hypox boundaries for selected WB, if available ------------ 
#               otherwise use those from SE582000-115270 Byfjorden 
if(!nrow(df.bounds.hypox %>% filter(WB==WBselected))>0){
  cat(paste0("\nNo Hypoxic Area boundaries found for ",WBselected," - Using those from SE582000-115270"))
  BoundariesHypoxicArea <<- df.bounds.hypox %>% filter(WB=="SE582000-115270") %>% select(Worst,P.B,M.P,G.M,H.G,RefCond) %>% as.list()
}else{
  BoundariesHypoxicArea <- df.bounds.hypox %>% filter(WB==WBselected) %>% select(Worst,P.B,M.P,G.M,H.G,RefCond) %>% as.list()
}
cat("\nBoundariesHypoxicArea:\n")
print(unlist(BoundariesHypoxicArea))


# ------------ Get hypsograph data ------------ 
WB_bathymetry <- df.bathy %>% filter(WB==WBselected) %>% select(area_pct,depth)


# ------------  Get RefCond_Sali ------------ 
RefCond_sali<-filter(df.bounds,Type==typology,Indicator==Ind) 
RefCond_sali<-RefCond_sali[,grep("Sali_", names(RefCond_sali), value=TRUE)]
RefCond_sali<-as.numeric(RefCond_sali[1,])
cat("\nRefCond_sali:\n")
print(RefCond_sali)


# ------------ Call Indicator calculations ------------ 
CalculateIndicator("CoastOxygen",dftest,RefCond_sali,variance_list,MonthInclude,2010,2015,n_iter=100)
