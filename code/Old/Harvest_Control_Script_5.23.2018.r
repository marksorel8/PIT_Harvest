rm(list=ls(all=TRUE)) #clears work environment (deletes old junk!)
#---------------------------------------------------

#set controls
stratafile<-"data/pop_strata_2011-2020.csv"
functionsfile<-"Harvest_Functions_5.23.2018.r"
#databasename<-"PIT_Harvest_Database V3.1 - 04.04.2018_230pm.accdb"#"PIT_Harvest_DatabaseV_03_10_2017_5pm.accdb"
#data.dir<-"C:\\data\\BPA Projects\\BPA CWT\\PIT Tag Columbia Harvest\\data"
#script.dir="C:\\data\\BPA Projects\\BPA CWT\\PIT Tag Columbia Harvest\\Code\\2012-2015\\"
#results.dir<-paste("C:\\data\\BPA Projects\\BPA CWT\\PIT Tag Columbia Harvest\\results\\",Sys.Date(),"\\",sep="")
db_path <- "C:/data/PIT_Harvest_DBs"
databasename<-file.path(db_path, "PIT_Harvest_Database V3.1 - 03.24.2021_5pm.accdb")#"PIT_Harvest_DatabaseV_03_10_2017_5pm.accdb"
data.dir<-"data/"
script.dir <- "code/"
results.dir<-"results/"

Types=c(#"A_B_groups",
  #"A_B_groups_Rear",
  #"ESU.DPS_groups",
  "ESU.DPS_groups_Rear",
  #"MPG_groups",
  "MPG_groups_Rear",	
  #"DIP_groups",
  "DIP_groups_Rear"
)


#----------------
#load packages
#----------------
if(!require(plyr))install.packages("plyr") #installs Rmark if not already!
library(plyr) 
if(!require(dplyr))install.packages("dplyr") #installs Rmark if not already!
library(dplyr) 
if(!require(reshape))install.packages("reshape") #installs Rmark if not already!
library(reshape) 
if(!require(tidyr))install.packages("tidyr") #installs Rmark if not already!
library(tidyr) 
if(!require(RODBC))install.packages("RODBC") #installs Rmark if not already!
library(RODBC) 
#--------------------
#source functions
#--------------------
#setwd("../")
source(paste(script.dir,functionsfile,sep=""))
#---------------------------
#run functions to load load and organize data
#---------------------------
HarvestReportingGroups<-MakeHarvestReportingGroups(data.dir=data.dir,stratafile=stratafile)
MarkSamples<-MakeMarkSamples() #doesn't include season
SportCatch<-MakeSportCatch()
TicketedCatch<-MakeTicketedCatch()
TotalTreatyCatch<-MakeTotalTreatyCatch() #doesn't include fishery
NonTicketedCatch<-MakeNonTicketedCatch() #doesn't include fishery 
Weights<-MakeWeights() 
BONtag<-MakeBONtag(data.dir = data.dir,stratafile = stratafile) #doesn't include harvest group category
Catchtag<-MakeCatchtag(data.dir = data.dir,stratafile = stratafile) #doesn't include harvest group category
BONefficiency<-MakeBONefficiency()
#getwd()
#------------------------------
#generate harvest model list
#------------------------------
#sport models
Fishery=data.frame(c("Sport"))
Season=data.frame(c("All"))
Year=data.frame(c(2016:2019))
Species=data.frame(c(1:3))
SportModels<-data.frame(merge(Fishery,Year))
SportModels<-merge(Season,SportModels)
SportModels<-merge(Species,SportModels)
colnames(SportModels)<-c("Species","Season","Fishery","Year")

#commercial models
Fishery=data.frame(c("Commercial"))
Season=data.frame(c("Spring","Summer","Fall"))
Year=data.frame(c(2016:2019))
Species=data.frame(c(1:2))
CommercialModels<-data.frame(merge(Fishery,Year))
CommercialModels<-merge(Season,CommercialModels)
CommercialModels<-merge(Species,CommercialModels)
colnames(CommercialModels)<-c("Species","Season","Fishery","Year")

#treaty models
Fishery=data.frame(c("Treaty"))
Season=data.frame(c("Summer","Fall"))
Year=data.frame(c(2016:2019))
Species=data.frame(c(1:4))
TreatyModels<-data.frame(merge(Fishery,Year))
TreatyModels<-merge(Season,TreatyModels)
TreatyModels<-merge(Species,TreatyModels)
colnames(TreatyModels)<-c("Species","Season","Fishery","Year")

Models<-data.frame(bind_rows(CommercialModels,SportModels,TreatyModels))
Models<-Models[!Models$Species==2 | Models$Species==2 & Models$Season%in%c("Fall","All") ,]
Models<-Models[!Models$Species==4 | Models$Species==4 & Models$Season%in%c("Summer","Fall","All") & Models$Fishery=="Treaty",]
#------------------------------
#run harvest model list
#------------------------------
for(i in 1:nrow(Models)){
  print(Models[i,])
  trymodel<-try(EstimateHarvestRate(Year=Models$Year[i],Season=as.character(Models$Season[i]),Fishery=as.character(Models$Fishery[i]),Types=Types,Species=Models$Species[i],Write="True"), silent=F)
  if ('try-error' %in% class(trymodel)){next
  }else(next)
}

trymodel
#a<-MakeJAGSdata(Year=2015,Season="Summer",Fishery="Commercial",Species=1,Types=Types,Write="True")
  
