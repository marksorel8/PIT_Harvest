
#connect to database ####
odbc <- function(dsn, sqtable, fields){
  #setwd(data.dir)
  package.already.there <- any(search()=="package:RODBC")
  if(!package.already.there)
    require(RODBC, quietly=TRUE)
  channel <- odbcConnectAccess2007(databasename)
  #channel <- odbcConnect(dsn)
  query <- paste("SELECT ", fields, " FROM [", sqtable, "]", sep="")
  imported <- sqlQuery(channel, query)
  odbcClose(channel)
  if(!package.already.there)
    detach("package:RODBC")
  return(imported)
}

connect_db <- function(db_name){
  
  # The driver name (can find these in your ODBC connections)
  driver <- "{Microsoft Access Driver (*.mdb, *.accdb)}"
  
  # Paste together the connection string
  con_str <- paste0("Driver=", driver, ";", "Dbq=", db_name, ";")
  
  # returns the connection object
  DBI::dbConnect(odbc::odbc(), .connection_string=con_str )
}



#make harvest reporting groups from BON Pop Strata ####
# This only needs to be run once. Remove calls in other fn's, read_csv instead.
MakeHarvestReportingGroups <- function(data.dir, stratafile){
  #setwd(data.dir)
 # dat <- data.frame(read.csv(file.path(dir=data.dir, file=stratafile), fill=T))
  dat <- read_xlsx(file.path(data.dir,stratafile),sheet=1)
 # dat[dat==""] <- NA
  
  #make harvest reporting groups
  #ESU H&W
  dat$ESU.DPS_groups_Rear <- NA
  dat$ESU.DPS_groups_Rear[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & dat$Species!=3] <- 
    paste(dat$ESU.DPS[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & dat$Species!=3],
          "ESU",
          dat$Rear[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & dat$Species!=3],
          dat$Species[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & dat$Species!=3],
          sep="_")
  dat$ESU.DPS_groups_Rear[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ] <- 
    paste(dat$ESU.DPS[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          "ESU",
          dat$Rear[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          dat$Species[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          dat$A_B[!is.na(dat$ESU.DPS) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          sep="_")
  ESU.DPS_groups_Rear <- unique(dat$ESU.DPS_groups_Rear)[!is.na(unique(dat$ESU.DPS_groups_Rear))]
  #ESU
  dat$ESU.DPS_groups <- NA
  dat$ESU.DPS_groups[!is.na(dat$ESU.DPS)  & dat$Species!=3] <- 
    paste(dat$ESU.DPS[!is.na(dat$ESU.DPS) & dat$Species!=3],
          "ESU",
          dat$Species[!is.na(dat$ESU.DPS) & dat$Species!=3],
          sep="_")
  dat$ESU.DPS_groups[!is.na(dat$ESU.DPS) & !is.na(dat$A_B) ] <- 
    paste(dat$ESU.DPS[!is.na(dat$ESU.DPS) & !is.na(dat$A_B) ],
          "ESU",
          dat$Species[!is.na(dat$ESU.DPS) & !is.na(dat$A_B) ],
          dat$A_B[!is.na(dat$ESU.DPS) & !is.na(dat$A_B) ],
          sep="_")
  ESU.DPS_groups <- unique(dat$ESU.DPS_groups)[!is.na(unique(dat$ESU.DPS_groups))]
  #MPG H&W
  dat$MPG_groups_Rear <- NA
  dat$MPG_groups_Rear[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & dat$Species!=3] <- 
    paste(dat$MPG[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & dat$Species!=3],
          "MPG",
          dat$Rear[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & dat$Species!=3],
          dat$Species[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & dat$Species!=3],
          sep="_")
  dat$MPG_groups_Rear[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & !is.na(dat$A_B)] <- 
    paste(dat$MPG[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          "MPG",
          dat$Rear[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          dat$Species[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & !is.na(dat$A_B)],
          dat$A_B[!is.na(dat$MPG) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          sep="_")
  MPG_groups_Rear <- unique(dat$MPG_groups_Rear)[!is.na(unique(dat$MPG_groups_Rear))]
  #MPG
  dat$MPG_groups <- NA
  dat$MPG_groups[!is.na(dat$MPG)  & dat$Species!=3] <- 
    paste(dat$MPG[!is.na(dat$MPG) & dat$Species!=3],
          "MPG",
          dat$Species[!is.na(dat$MPG) & dat$Species!=3],
          sep="_")
  dat$MPG_groups[!is.na(dat$MPG) & !is.na(dat$A_B)] <- 
    paste(dat$MPG[!is.na(dat$MPG)  & !is.na(dat$A_B)],
          dat$Species[!is.na(dat$MPG)  & !is.na(dat$A_B)],
          dat$A_B[!is.na(dat$MPG) & !is.na(dat$A_B) ],
          sep="_")
  MPG_groups <- unique(dat$MPG_groups)[!is.na(unique(dat$MPG_groups))]
  #DIP H&W
  dat$DIP_groups_Rear <- NA
  dat$DIP_groups_Rear[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & dat$Species!=3] <- 
    paste(dat$DIP[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & dat$Species!=3],
          "DIP",
          dat$Rear[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & dat$Species!=3],
          dat$Species[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & dat$Species!=3],
          sep="_")
  dat$DIP_groups_Rear[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & !is.na(dat$A_B)] <- 
    paste(dat$DIP[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          "DIP",
          dat$Rear[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          dat$Species[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & !is.na(dat$A_B)],
          dat$A_B[!is.na(dat$DIP) & dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          sep="_")
  DIP_groups_Rear <- unique(dat$DIP_groups_Rear)[!is.na(unique(dat$DIP_groups_Rear))]
  #DIP
  dat$DIP_groups <- NA
  dat$DIP_groups[!is.na(dat$DIP)  & dat$Species!=3] <- 
    paste(dat$DIP[!is.na(dat$DIP) & dat$Species!=3],
          "DIP",
          dat$Species[!is.na(dat$DIP) & dat$Species!=3],
          sep="_")
  dat$DIP_groups[!is.na(dat$DIP) & !is.na(dat$A_B)] <- 
    paste( dat$DIP[!is.na(dat$DIP)  & !is.na(dat$A_B)],
           "DIP",
          dat$Species[!is.na(dat$DIP)  & !is.na(dat$A_B)],
          dat$A_B[!is.na(dat$DIP) & !is.na(dat$A_B) ],
          sep="_")
  DIP_groups <- unique(dat$DIP_groups)[!is.na(unique(dat$DIP_groups))]
  #A_B H&W
  dat$A_B_groups_Rear <- NA
  dat$A_B_groups_Rear[dat$Rear%in%c("W","H") & !is.na(dat$A_B)] <- 
    paste(dat$Rear[dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          dat$Species[ dat$Rear%in%c("W","H") & !is.na(dat$A_B)],
          dat$A_B[dat$Rear%in%c("W","H") & !is.na(dat$A_B) ],
          sep="_")
  A_B_groups_Rear <- unique(dat$A_B_groups_Rear)[!is.na(unique(dat$A_B_groups_Rear))]
  #A_B
  dat$A_B_groups <- NA
  dat$A_B_groups[!is.na(dat$A_B)] <- 
    paste(dat$Species[ !is.na(dat$A_B)],
          dat$A_B[!is.na(dat$A_B) ],
          sep="_")
  A_B_groups <- unique(dat$A_B_groups)[!is.na(unique(dat$A_B_groups))]
  if (file.exists(results.dir)){
    #setwd(file.path(results.dir))
  } else {
    dir.create(file.path(results.dir))
    #setwd(file.path(results.dir))
  }
  write.csv(dat,file.path(results.dir,paste("Harvest_Reporting_Groups_",Sys.Date(),".csv",sep="")),row.names = F)
  HarvestReportingGroups_summary <- as.data.frame(dat %>% group_by(A_B_groups,A_B_groups_Rear,ESU.DPS_groups,ESU.DPS_groups_Rear,MPG_groups,MPG_groups_Rear,DIP_groups,DIP_groups_Rear)
                                                %>% summarise(RelSite_SpRRT=length(unique(RelSite_SpRRT)))
                                                %>% arrange(-desc(A_B_groups_Rear),-desc(ESU.DPS_groups_Rear),-desc(MPG_groups_Rear),-desc(DIP_groups_Rear)))
  write.csv(HarvestReportingGroups_summary,file.path(results.dir,"HarvestReportingGroups_summary.csv"),row.names=F)
  return(dat)
}


#mark samples ####
MakeMarkSamples <- function(db_con){
  
  #dat1 <- odbc(dsn = "MS Access Database", sqtable = "qry_MarkSample_ByDay", fields ="*")
  dat1 <- DBI::dbReadTable(db_con, "qry_MarkSample_ByDay")
  
  #all(colnames(d1) %in% colnames(dat1))
  #which(!(unlist(sapply(d1,class))==unlist(sapply(dat1,class))))

#  ERROR #### 
    #THROWING AN ERROR ABOUT UNDEFINED FUNCTION but it is in VBA of DB
  dat2 <- DBI::dbReadTable(db_con, "qry_FisherySeasons") %>% 
    mutate(Season=stringr::str_extract(FisherySeasonID,"Winter|Spring|Summer|Fall")) 
  
  # THIS DOESNT THROW AN ERROR, but fails for same reason. DB returns the error msg as dat2.
    #dat2 <- odbc(dsn = "MS Access Database", sqtable = "qry_FisherySeasons", fields ="*")
  MSdat <- dat1 %>% 
    filter(SampledForPIT==1) %>% 
    group_by(Year=SampleYear, Month=SampleMonth, Week=SampleWeek, Fishery=FisheryType, Species=SpeciesID) %>% 
    summarise(MS=sum(MKSamplecount)) %>% 
    mutate(SportID=paste(Year, Month, Fishery, sep="."),
           CommTreatID=paste(Year, Week, Fishery, sep="."))
  
  dat2$SportID <- paste(dat2$Year,dat2$Month,dat2$FisheryType,sep=".")
  dat2$CommTreatID <- paste(dat2$Year,dat2$StatWeek,dat2$FisheryType,sep=".")
  
  MSdat_CommTreat <- merge(dat2[,colnames(dat2)%in%c("CommTreatID","Season")],
                         MSdat[MSdat$Fishery%in%c("Treaty","Commercial"),],
                         by="CommTreatID")
  MSdat_Sport <- merge(dat2[colnames(dat2)%in%c("SportID","Season")],
                     MSdat[MSdat$Fishery%in%c("Sport"),],
                     by="SportID")
  MSdat_Sport <- as.data.frame(MSdat_Sport %>% 
                                 group_by(SportID=SportID,Year=Year,Month=Month,Fishery=Fishery, Species=Species)
                %>% summarise(MS=sum(MS)))
  
  MS <- list(MSdat_CommTreat,MSdat_Sport)
  names(MS) <- c("CommercialTreaty","Sport")
  return(MS)
}

# SportCatch ####

MakeSportCatch <- function(db_con){
#  dat1 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_SportCatch", fields ="*") 
  dat1 <- DBI::dbReadTable(db_con, "tbl_SportCatch")
  SportCatchdat <- dat1 %>% 
    group_by(Year=SportYear,Month=SportMonth,Species=Species)%>% 
    summarise(Catch=sum(Catch))
  #return(SportCatchdat)
  SportCatchdat
}

# Ticketed Catch ####
MakeTicketedCatch <- function(db_con){
  #dat1 <-  odbc(dsn = "MS Access Database", sqtable = "qry_TicketedCatch_CommercialTribal", fields ="*")
  dat1 <- DBI::dbReadTable(db_con, "tbl_TicketedCatch_CommercialTribal")
 # dat2 <-  odbc(dsn = "MS Access Database", sqtable = "qry_FisherySeasons", fields ="*") 
  #  ERROR #### 
  dat2 <- DBI::dbReadTable(db_con, "qry_FisherySeasons") %>% 
    mutate(Season=stringr::str_extract(FisherySeasonID,"Winter|Spring|Summer|Fall"))  %>% 
    mutate(CommTreatID=paste(Year,StatWeek,FisheryType,sep="."))
  
  TCdat <- dat1 %>% 
    group_by(Year=TicketedCatchYear, Week=TicketedCatchWeek, Fishery=FisheryType, Species=TicketedSpecies) %>% 
    summarise(LW=sum(ReportedWeight)) %>% 
    mutate(CommTreatID=paste(Year,Week,Fishery,sep="."))
  
  #TCdat$CommTreatID <- paste(TCdat$Year,TCdat$Week,TCdat$Fishery,sep=".")
  
  #dat2$CommTreatID <- paste(dat2$Year,dat2$StatWeek,dat2$FisheryType,sep=".")
  
  TCdat <- merge(dat2[,colnames(dat2)%in%c("CommTreatID","Season")],
                TCdat[TCdat$Fishery%in%c("Treaty","Commercial"),],
                by="CommTreatID")
  #return(TCdat)
  TCdat
}

#Total Treaty Catch ####

MakeTotalTreatyCatch <- function(db_con){
  #dat1 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_Total_Reported_Treaty_Catch", fields ="*")
  dat1 <- DBI::dbReadTable(db_con, "tbl_Total_Reported_Treaty_Catch")
  
  TTCdat <- dat1 %>% 
    group_by(Year=Year,Season=Season, Species=Species) %>% 
    summarise(TTC=ReportedCatch)
  #return(TTCdat)
  TTCdat
}

#Reported Non-Ticketed Catch ####
MakeNonTicketedCatch <- function(db_con){
  #dat1 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_NonTicketedCatch", fields ="*")
  dat1 <-   DBI::dbReadTable(db_con, "tbl_NonTicketedCatch") %>% 
    mutate(CommTreatID=paste(NTCatchYear, StatWeek, FisheryType,sep=".")) %>% 
    mutate(NonTicketedCatch=parse_number(NonTicketedCatch)) %>% 
    as_tibble()
  
  #dat2 <-  odbc(dsn = "MS Access Database", sqtable = "qry_FisherySeasons", fields ="*")
#  ERROR #### 
  dat2 <- DBI::dbReadTable(db_con, "tbl_FisherySeasons") %>% 
    mutate(Season=stringr::str_extract(FisherySeasonID,"Winter|Spring|Summer|Fall"))  %>% 
    mutate(CommTreatID=paste(FisheryYear,FisheryWeek,FisheryType,sep="."))
  
 # dat1$CommTreatID <- paste(dat1$NTCatchYear,dat1$StatWeek,dat1$FisheryType,sep=".")
 # dat2$CommTreatID <- paste(dat2$Year,dat2$StatWeek,dat2$FisheryType,sep=".")
  
  NTCdat <- merge(dat2,dat1[,!colnames(dat1)%in%c("StatWeek","FisheryType")],by="CommTreatID")
  NTCdat <- NTCdat %>% 
    filter(Season=="Fall") %>% 
    group_by(Year=NTCatchYear,Season=Season,Week=StatWeek, Species=Species) %>% 
    summarise(NTC=NonTicketedCatch)
  #  return(NTCdat)
  NTCdat
}

#Weights ####
MakeWeights <- function(db_con){
  #need to merge in sampled for PITS from mark sample based on sample number
  #Wdat <-  odbc(dsn = "MS Access Database", sqtable = "tbl_Weights", fields ="*") 
  Wdat <- DBI::dbReadTable(db_con, "tbl_Weights") %>% 
    mutate(CommTreatID=paste(SampleYear, SampleWeek,FisheryType,sep="."))
    #Wdat$CommTreatID <- paste(Wdat$SampleYear,Wdat$SampleWeek,Wdat$FisheryType,sep=".")
  
# ERROR ####
  #dat2 <-  odbc(dsn = "MS Access Database", sqtable = "qry_FisherySeasons", fields ="*") 
  dat2 <- DBI::dbReadTable(db_con, "qry_FisherySeasons") %>% 
    mutate(Season=stringr::str_extract(FisherySeasonID,"Winter|Spring|Summer|Fall"))  %>% 
    mutate(CommTreatID=paste(Year,StatWeek,FisheryType,sep="."))
  
#dat2$CommTreatID <- paste(dat2$Year,dat2$StatWeek,dat2$FisheryType,sep=".")
  
  Wdat <- merge(dat2[, colnames(dat2) %in% c("CommTreatID", "Season")],
              Wdat[Wdat$FisheryType %in% c("Commercial", "Treaty"),],
              by="CommTreatID") %>% 
    dplyr::rename(Fishery=FisheryType,
           Year=SampleYear,
           Month=SampleMonth,
           Week=SampleWeek,
           Species=SpeciesID)
  
# colnames(Wdat)[colnames(Wdat)=="FisheryType"] <- "Fishery"
#  colnames(Wdat)[colnames(Wdat)=="SampleYear"] <- "Year"
#  colnames(Wdat)[colnames(Wdat)=="SampleMonth"] <- "Month"
#  colnames(Wdat)[colnames(Wdat)=="SampleWeek"] <- "Week"
#  colnames(Wdat)[colnames(Wdat)=="SpeciesID"] <- "Species"

  Wdat
  #return(Wdat)
}

#BON_tag ####
MakeBONtag <- function(data.dir, stratafile, db_con){
 # browser()
  #dat1 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_Annual_BON_PIT_Passage", fields ="*")
  dat1 <- DBI::dbReadTable(db_con, "tbl_Annual_BON_PIT_Passage") %>% 
    mutate(RelSite_SpRRT=paste0(ReleaseSite, SpRRT)) %>% as_tibble() %>% 
    filter(!(Tag_File=="ORPHAN"))#%>%
  #mutate(TravelDays=stringr::str_remove(TravelDays,"'")) %>%   
 # mutate(TravelDays=as.numeric(TravelDays))
  #dat1$RelSite_SpRRT <- paste(dat1$ReleaseSite,dat1$SpRRT,sep="")
  dat1 %>% filter(is.na(ReleaseDate)) %>% select(Tag_File,PTAGIS_Query_Date) %>% distinct()
  # SHOULD CHANGE THIS TO READ IN EXISTING
  dat2 <- MakeHarvestReportingGroups(data.dir,stratafile) %>% 
    select("RelSite_SpRRT",
           "ESU.DPS_groups_Rear",   
           "ESU.DPS_groups",     
           "MPG_groups_Rear",     
           "MPG_groups",            
           "DIP_groups_Rear",       
           "DIP_groups",            
           "A_B_groups_Rear",       
           "A_B_groups")
  
  
 # TAGdat <-dat1 %>% left_join(dat2, by="RelSite_SpRRT") %>%  
 #   mutate(RelMonth=as.numeric(format(as.Date(ReleaseDate),format="%m")))
  TAGdat<-merge(dat1,dat2[,colnames(dat2)%in%c("RelSite_SpRRT",
                                               "ESU.DPS_groups_Rear",   
                                               "ESU.DPS_groups",     
                                               "MPG_groups_Rear",     
                                               "MPG_groups",            
                                               "DIP_groups_Rear",       
                                               "DIP_groups",            
                                               "A_B_groups_Rear",       
                                               "A_B_groups")],
                by="RelSite_SpRRT")
  TAGdat$RelMonth<-as.numeric(format(as.Date(TAGdat$ReleaseDate),format="%m"))
  #TAGdat %>% filter(SpeciesID==2, TravelDays>450,TravelDays<1500) %>% select(TravelDays)
  
  #Filter Out Jacks and MiniJacks
  #Coho
  TAGdat <- TAGdat[TAGdat$SpeciesID!=2 | TAGdat$TravelDays>450 & TAGdat$TravelDays<1500,]
  #spring & Summer Chinook
  TAGdat <- TAGdat[!TAGdat$SpRRT%in%c("11H","11W","11U","12W","12H","12U") | TAGdat$TravelDays>700 & TAGdat$TravelDays<2200,]
  #fall chinook
  TAGdat <- TAGdat[!TAGdat$SpRRT%in%c("13H","13W","13U") | TAGdat$TravelDays>715 & TAGdat$TravelDays<2200,]
  #unknown chinook
  TAGdat <- TAGdat[!TAGdat$SpRRT%in%c("15H","15W","15U") | TAGdat$RelMonth < 7 & TAGdat$TravelDays>700 & TAGdat$TravelDays<2200 | 
                                                         TAGdat$RelMonth >= 7 & TAGdat$TravelDays>715 & TAGdat$TravelDays<2200,]  

# TAGdat %>%
#   filter(PassageYear==2019, SpeciesID==1) %>% 
#   group_by(PassageYear, ESU.DPS_groups_Rear) %>%
#   count()
# 
# TAGdat %>% 
#   group_by(PassageYear, ESU.DPS_groups) %>% 
#   count()
# 
# TAGdat %>% 
#   group_by(PassageYear, MPG_groups) %>% 
#   count()
  
# TAGdat %>% 
#   group_by(PassageYear, MPG_groups_Rear) %>% 
#   count()

#COUNT BY GROUP
  BON_tag_dat <- rbind.fill(
  as.data.frame(TAGdat %>% filter(!is.na(ESU.DPS_groups_Rear))
                %>% group_by(Type="ESU.DPS_groups_Rear",Year=PassageYear,Group=ESU.DPS_groups_Rear)
                %>% summarise(Tags=length(unique(PITTag_ID)))),
  as.data.frame(TAGdat %>% filter(!is.na(ESU.DPS_groups))
                %>% group_by(Type="ESU.DPS_groups",Year=PassageYear,Group=ESU.DPS_groups)
                %>% summarise(Tags=length(unique(PITTag_ID)))),
  as.data.frame(TAGdat %>% filter(!is.na(MPG_groups_Rear))
                %>% group_by(Type="MPG_groups_Rear",Year=PassageYear,Group=MPG_groups_Rear)
                %>% summarise(Tags=length(unique(PITTag_ID)))),
  as.data.frame(TAGdat %>% filter(!is.na(MPG_groups))
                %>% group_by(Type="MPG_groups",Year=PassageYear,Group=MPG_groups)
                %>% summarise(Tags=length(unique(PITTag_ID)))),
  as.data.frame(TAGdat %>% filter(!is.na(DIP_groups_Rear))
                %>% group_by(Type="DIP_groups_Rear",Year=PassageYear,Group=DIP_groups_Rear)
                %>% summarise(Tags=length(unique(PITTag_ID)))),
  as.data.frame(TAGdat %>% filter(!is.na(DIP_groups))
                %>% group_by(Type="DIP_groups",Year=PassageYear,Group=DIP_groups)
                %>% summarise(Tags=length(unique(PITTag_ID)))),
  as.data.frame(TAGdat %>% filter(!is.na(A_B_groups_Rear))
                %>% group_by(Type="A_B_groups_Rear",Year=PassageYear,Group=A_B_groups_Rear)
                %>% summarise(Tags=length(unique(PITTag_ID)))),
  as.data.frame(TAGdat %>% filter(!is.na(A_B_groups))
                %>% group_by(Type="A_B_groups",Year=PassageYear,Group=A_B_groups)
                %>% summarise(Tags=length(unique(PITTag_ID))))
  )
  return(BON_tag_dat)
}

#Catch_tag ####
MakeCatchtag <- function(data.dir, stratafile, db_con){
  #dat1 <-  odbc(dsn = "MS Access Database", sqtable = "qry_PITRecoveries", fields ="*")
  dat1 <- DBI::dbReadTable(db_con, "qry_PITRecoveries")
  #dat2 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_TaggingDetails", fields ="*")
  dat2 <- DBI::dbReadTable(db_con, "tbl_TaggingDetails")
  
  TAGdat <- merge(dat1,dat2,by="PITTag_ID",suffixes=c(".Mort",".Tag" ))
  
  # AGAIN- should just read this in in setup
  dat3 <- MakeHarvestReportingGroups(data.dir, stratafile)
  
  TAGdat <- merge(TAGdat, dat3[, colnames(dat3) %in% c("RelSite_SpRRT",
                                               "ESU.DPS_groups_Rear",   
                                               "ESU.DPS_groups",     
                                               "MPG_groups_Rear",     
                                               "MPG_groups",            
                                               "DIP_groups_Rear",       
                                               "DIP_groups",            
                                               "A_B_groups_Rear",       
                                               "A_B_groups")],
                by="RelSite_SpRRT") %>% 
    mutate(TravelDays=as.numeric(as.Date(SampleDate)-as.Date(ReleaseDate)),
           RelMonth=as.numeric(format(as.Date(ReleaseDate), format="%m")))
  
  #Filter Out Jacks and MiniJacks
  #Coho
  TAGdat <- TAGdat[TAGdat$SpeciesID.Mort!=2 | TAGdat$TravelDays>450 & TAGdat$TravelDays<1500,]
  #spring & Summer Chinook
  TAGdat <- TAGdat[!TAGdat$SpRRT%in%c("11H","11W","11U","12W","12H","12U") | TAGdat$TravelDays>700 & TAGdat$TravelDays<2200,]
  #fall chinook
  TAGdat <- TAGdat[!TAGdat$SpRRT%in%c("13H","13W","13U") | TAGdat$TravelDays>715 & TAGdat$TravelDays<2200,]
  #unknown chinook
  TAGdat <- TAGdat[!TAGdat$SpRRT%in%c("15H","15W","15U") | TAGdat$RelMonth < 7 & TAGdat$TravelDays>700 & TAGdat$TravelDays<2200 | 
                                                       TAGdat$RelMonth >= 7 & TAGdat$TravelDays>715 & TAGdat$TravelDays<2200,]  
  #COMM/TREATY COUNT BY GROUP
  CommTreat <- rbind.fill(
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(ESU.DPS_groups_Rear))
                  %>% group_by(Type="ESU.DPS_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=ESU.DPS_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(ESU.DPS_groups))
                  %>% group_by(Type="ESU.DPS_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=ESU.DPS_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(MPG_groups_Rear))
                  %>% group_by(Type="MPG_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=MPG_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(MPG_groups))
                  %>% group_by(Type="MPG_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=MPG_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(DIP_groups_Rear))
                  %>% group_by(Type="DIP_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=DIP_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(DIP_groups))
                  %>% group_by(Type="DIP_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=DIP_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(A_B_groups_Rear))
                  %>% group_by(Type="A_B_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=A_B_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Commercial","Treaty"),!is.na(A_B_groups))
                  %>% group_by(Type="A_B_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Week=SampleWeek,Group=A_B_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Week,Tags,fill=0))
  )
  #SPORT COUNT BY GROUP
  Sport <- rbind.fill(
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(ESU.DPS_groups_Rear))
                  %>% group_by(Type="ESU.DPS_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=ESU.DPS_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(ESU.DPS_groups))
                  %>% group_by(Type="ESU.DPS_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=ESU.DPS_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(MPG_groups_Rear))
                  %>% group_by(Type="MPG_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=MPG_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(MPG_groups))
                  %>% group_by(Type="MPG_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=MPG_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(DIP_groups_Rear))
                  %>% group_by(Type="DIP_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=DIP_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(DIP_groups))
                  %>% group_by(Type="DIP_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=DIP_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(A_B_groups_Rear))
                  %>% group_by(Type="A_B_groups_Rear",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=A_B_groups_Rear)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0)),
    as.data.frame(TAGdat %>% filter(FisheryType%in%c("Sport"),!is.na(A_B_groups))
                  %>% group_by(Type="A_B_groups",Fishery=FisheryType,Year=SampleYear,Species=SpeciesID.Mort,Month=SampleMonth,Group=A_B_groups)
                  %>% summarise(Tags=length(unique(PITTag_ID)))
                  %>% spread(Month,Tags,fill=0))
  )
  Sport[is.na(Sport)] <- 0
  CommTreat[is.na(CommTreat)] <- 0
  Catch_tag_dat <- list(Sport=Sport,CommTreat=CommTreat)
  return(Catch_tag_dat)
}

# BON Efficiency ####
MakeBONefficiency <- function(db_con){
  #uses a May 1 date to avoid delayed overwintering steelhead
  
  #dat1 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_Annual_BON_PIT_Passage", fields ="*")
  dat1 <- DBI::dbReadTable(db_con, "tbl_Annual_BON_PIT_Passage") %>% 
    select(PITTag_ID,Obs_DateTime,PassageYear)
  
  #dat2 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_Annual_MCN_PIT_Passage", fields ="*")
  dat2 <- DBI::dbReadTable(db_con, "tbl_Annual_MCN_PIT_Passage") %>% 
    select(PITTag_ID, SpeciesID, Obs_DateTime, PassageYear)
  
  #dat1 <- dat1[,colnames(dat1)%in%c("PITTag_ID","Obs_DateTime","PassageYear")] # CHANGE TO SELECT
  #dat2 <- dat2[,colnames(dat2)%in%c("PITTag_ID","SpeciesID","Obs_DateTime","PassageYear")] # CHANGE TO SELECT
  
  BON_eff <- merge(dat1, dat2,by="PITTag_ID",all.y=T,suffixes=c(".BON",".MCN" )) %>% 
    mutate(DOY=as.numeric(format(as.Date(Obs_DateTime.MCN),"%j"))) %>% 
    filter(DOY > 120, SpeciesID %in% c(1:4)) %>% 
    group_by(Year=PassageYear.MCN, Species=SpeciesID) %>% 
    #count()
    summarise(MCNTags=length(Obs_DateTime.MCN[!is.na(Obs_DateTime.MCN)]), 
              BONTags=length(Obs_DateTime.BON[!is.na(Obs_DateTime.BON)]))
  
  #BON_eff_dat$DOY <- as.numeric(format(as.Date(BON_eff_dat$Obs_DateTime.MCN),"%j"))
  
 # BON_eff <- as.data.frame(BON_eff_dat %>% filter(DOY>120, SpeciesID %in% c(1:4))
#                %>% group_by(Year=PassageYear.MCN, Species=SpeciesID)
#                %>% summarise(MCNTags=length(Obs_DateTime.MCN[!is.na(Obs_DateTime.MCN)]),BONTags=length(Obs_DateTime.BON[!is.na(Obs_DateTime.BON)])))
  return(BON_eff)
}

# MakeJAGSdata ####
MakeJAGSdata <- function(Year,Season,Fishery,Species,Types,Write=FALSE){
  Yr=Year
  Sn=Season
  Fy=Fishery
  Sp=Species
  # 
  # Yr=2019
  #  Sn="Fall"
  #  Fy="Treaty"
  #  Sp=1
 # browser()
  TypeOrder=data.frame(Type=c("A_B_groups","A_B_groups_Rear","ESU.DPS_groups","ESU.DPS_groups_Rear","MPG_groups","MPG_groups_Rear","DIP_groups","DIP_groups_Rear"),
                       TypeNumber=1:8)
  #colnames(TypeOrder) <- c("Type","TypeNumber")

  #sport fishery
  # ERROR WITH SEASON FUNCTION IN ACCESS ####
   # FisherySeasons <- odbc(dsn = "MS Access Database", sqtable = "qry_FisherySeasons", fields ="*")
  
 FisherySeasons <-  DBI::dbReadTable(db_con, "qry_FisherySeasons") %>% 
    mutate(Season=stringr::str_extract(FisherySeasonID,"Winter|Spring|Summer|Fall")) 
    
  if(Fy=="Sport"){
     #MS, Catch, Periods
     
     FisherySeasons <- as.data.frame(FisherySeasons %>% filter(Year==Yr,FisheryType==Fy)
                            %>% select(Year,Fishery=FisheryType,Month))
     Catch <- as.data.frame(SportCatch %>% filter(Year==Yr,Species==Sp)
                          %>% group_by(Month=Month)
                          %>% summarise(Catch=sum(Catch)))
     MSdat <- as.data.frame(MarkSamples$Sport %>% filter(Year==Yr,Species==Sp)
                      %>% group_by(Month=Month)
                      %>% summarise(MS=sum(MS)))
     MS_Catch_Periods <- merge(FisherySeasons,Catch,by="Month",all.x=T)
     MS_Catch_Periods <- merge(MS_Catch_Periods,MSdat,by="Month",all.x=T) 
     MS_Catch_Periods$MS[is.na(MS_Catch_Periods$MS)] <- 0
     MS_Catch_Periods$Catch[is.na(MS_Catch_Periods$Catch)] <- 0
     periods <- data.frame(rbind(unique(MS_Catch_Periods$Week),rep(NA,length(MS_Catch_Periods$Week))))
     colnames(periods) <- unique(MS_Catch_Periods$Week)
     periods <- periods[-c(1:2),,drop=FALSE]
     #BON and Catch Tags
     CatchTagdat <- as.data.frame(Catchtag$Sport %>% filter(Fishery==Fy,Species==Sp,Year==Yr))
     CatchTagdat <- as.data.frame(rbind.fill(CatchTagdat,periods))
     CatchTagdat[is.na(CatchTagdat)] <- 0
     BONTagdat <- as.data.frame(BONtag %>% filter(Year==Yr, Group%in% CatchTagdat$Group))
     Catch_BON_Tagdat <- merge(CatchTagdat,BONTagdat[,colnames(BONTagdat)%in%c("Group","Tags")],by="Group")
     Catch_BON_Tagdat <- merge(Catch_BON_Tagdat,TypeOrder,by="Type")
     Catch_BON_Tagdat <- Catch_BON_Tagdat[order(Catch_BON_Tagdat$TypeNumber,Catch_BON_Tagdat$Group),]
     Catch_BON_Tagdat <- Catch_BON_Tagdat[Catch_BON_Tagdat$Type%in%Types,]
    #these are allflex (I think)
     if(Sp==1){det_beta=c(671.97,8.23)} #2010-11 Det study Chinook detection beta dist
     if(Sp==2){det_beta=c(2201.72,2.42)} #coho detection beta dist params
    #these are the dets for the Cheeseblock
     if(Sp==3){det_beta=c(2026.95,4.06)} #Steelhead detection beta dist
     if(Sp==4){det_beta=c(2201.72,2.42)} #Sockeye USED coho detection beta dist params (no sockeye)
     #Make JAGS data list
     JAGSdat <- list(
       periods=dim(MS_Catch_Periods)[1],
       samp=MS_Catch_Periods$MS,
       catch=MS_Catch_Periods$Catch,
       groups=dim(Catch_BON_Tagdat)[1],
       BON_tag=Catch_BON_Tagdat$Tags,
       tag=as.matrix(Catch_BON_Tagdat[,colnames(Catch_BON_Tagdat)%in%c(1:53)]),
       B_tags=BONefficiency$BONTags[BONefficiency$Species==Sp & BONefficiency$Year==Yr],
       M_tags=BONefficiency$MCNTags[BONefficiency$Species==Sp & BONefficiency$Year==Yr],
       det_beta=det_beta
     )
  }
  
  #Commercial fishery

  if(Fy=="Commercial"){
    #MS, Catch, Periods
    # ERROR####
   # FisherySeasons <- odbc(dsn = "MS Access Database", sqtable = "qry_FisherySeasons", fields ="*") 
    FisherySeasons <- as.data.frame(FisherySeasons %>% filter(Year==Yr,FisheryType==Fy, Season==Sn)
                                  %>% select(Year,Fishery=FisheryType,Week=StatWeek))
    Weightdat <- as.data.frame(Weights %>% filter(Year==Yr,Species==Sp, Fishery==Fy, Season==Sn)
                         %>% group_by(Week=Week))
    Weightdat <- merge(FisherySeasons,Weightdat[,colnames(Weightdat)%in%c("Week","Weight")],by="Week",all.x=T)
    Catch <- as.data.frame(TicketedCatch %>% filter(Year==Yr,Species==Sp, Fishery==Fy, Season==Sn)
                           %>% group_by(Week=Week))
    Catch <- merge(FisherySeasons,Catch[,!colnames(Catch)%in%c("Fishery","Year")],by="Week",all.x=T)
    MSdat <- as.data.frame(MarkSamples$CommercialTreaty %>% filter(Year==Yr,Species==Sp, Season==Sn, Fishery==Fy)
                         %>% group_by(Week=Week)
                         %>% summarise(MS=sum(MS)))
    MS_Catch_Periods <- merge(Catch,MSdat,by="Week",all.x=T) 
    MS_Catch_Periods$MS[is.na(MS_Catch_Periods$MS)] <- 0
    MS_Catch_Periods$LW[is.na(MS_Catch_Periods$LW)] <- 0
    MS_Catch_Periods$period <- as.numeric(as.factor(MS_Catch_Periods$Week))
    periods <- data.frame(rbind(unique(MS_Catch_Periods$Week),rep(NA,length(MS_Catch_Periods$Week))))
    colnames(periods) <- unique(MS_Catch_Periods$Week)
    periods <- periods[-c(1:2),,drop=FALSE] #WTF
    
    #BON and Catch Tags
    CatchTagdat <- as.data.frame(Catchtag$CommTreat %>% filter(Fishery==Fy,Species==Sp,Year==Yr))
    CatchTagdat <- CatchTagdat[,colnames(CatchTagdat)%in%c("Fishery","Year","Species","Type","Group",FisherySeasons$Week)]
    CatchTagdat <- CatchTagdat[rowSums(CatchTagdat[,colnames(CatchTagdat)%in%c(1:53),drop=FALSE])>0,]#
    CatchTagdat <- as.data.frame(rbind.fill(CatchTagdat,periods))
    CatchTagdat[is.na(CatchTagdat)] <- 0
    BONTagdat <- as.data.frame(BONtag %>% filter(Year==Yr,Group%in%CatchTagdat$Group))
    Catch_BON_Tagdat <- merge(CatchTagdat,BONTagdat[,colnames(BONTagdat)%in%c("Group","Tags")],by="Group")
    Catch_BON_Tagdat <- merge(Catch_BON_Tagdat,TypeOrder,by="Type")
    Catch_BON_Tagdat <- Catch_BON_Tagdat[order(Catch_BON_Tagdat$TypeNumber,Catch_BON_Tagdat$Group),]
    Catch_BON_Tagdat <- Catch_BON_Tagdat[Catch_BON_Tagdat$Type%in%Types,]
    #these are allflex (I think)
    if(Sp==1){det_beta=c(671.97,8.23)} #2010-11 Det study Chinook detection beta dist
    if(Sp==2){det_beta=c(2201.72,2.42)} #coho detection beta dist params
    #these are the dets for the Cheeseblock
    if(Sp==3){det_beta=c(2026.95,4.06)} #Steelhead detection beta dist
    if(Sp==4){det_beta=c(2201.72,2.42)} #Sockeye USED coho detection beta dist params (no sockeye)
    #Make JAGS data list
    JAGSdat <- list(
      periods=length(FisherySeasons$Week),
      samp=MS_Catch_Periods$MS,
      samp_period=MS_Catch_Periods$period[MS_Catch_Periods$MS>0],
      samp_periods=length(MS_Catch_Periods$period[MS_Catch_Periods$MS>0]),
      non_samp_period=MS_Catch_Periods$period[MS_Catch_Periods$MS==0],
      non_samp_periods=length(MS_Catch_Periods$period[MS_Catch_Periods$MS==0]),
      T_lbs=MS_Catch_Periods$LW,
      logweight=log(Weightdat$Weight),
      N_bio=length(Weightdat$Weight),
      period=as.numeric(as.factor(Weightdat$Week)),
      groups=dim(Catch_BON_Tagdat)[1],
      BON_tag=Catch_BON_Tagdat$Tags,
      tag=as.matrix(Catch_BON_Tagdat[,colnames(Catch_BON_Tagdat)%in%c(1:53)]),
      B_tags=BONefficiency$BONTags[BONefficiency$Species==Sp & BONefficiency$Year==Yr],
      M_tags=BONefficiency$MCNTags[BONefficiency$Species==Sp & BONefficiency$Year==Yr],
      det_beta=det_beta
    )
  }

  #Treaty fishery

  if(Fy=="Treaty"){
    #browser()
    #MS, Catch, Periods
   # FisherySeasons <- odbc(dsn = "MS Access Database", sqtable = "qry_FisherySeasons", fields ="*") 
    FisherySeasons <- FisherySeasons %>% filter(Year==Yr,FisheryType==Fy, Season==Sn) %>% 
      select(Year,Fishery=FisheryType,Week=StatWeek)
     
    TotCatch <- dplyr::filter(TotalTreatyCatch, Year==Yr,Species==Sp, Season==Sn) # DELETED FISHERY TO SLEEP AT NIGHT
 
    Weightdat <- as.data.frame(Weights %>% filter(Year==Yr,Species==Sp, Fishery==Fy, Season==Sn)
                             %>% group_by(Week=Week))
    Weightdat <- merge(FisherySeasons,Weightdat[,colnames(Weightdat)%in%c("Week","Weight")],by="Week",all.x=T)
    Catch <- as.data.frame(TicketedCatch %>% filter(Year==Yr,Species==Sp, Fishery==Fy, Season==Sn)
                         %>% group_by(Week=Week))
    Catch <- merge(FisherySeasons,Catch[,!colnames(Catch)%in%c("Fishery","Year")],by="Week",all.x=T)
    NTCatch <- as.data.frame(NonTicketedCatch %>% filter(Year==Yr,Species==Sp, Season==Sn) #Fishery==Fy
                           %>% group_by(Week=Week))
    Catch <- merge(Catch,NTCatch[,colnames(NTCatch)%in%c("Week","NTC")],by="Week",all.x=T)
    MSdat <- as.data.frame(MarkSamples$CommercialTreaty %>% filter(Year==Yr,Species==Sp, Season==Sn, Fishery==Fy)
                         %>% group_by(Week=Week)
                         %>% summarise(MS=sum(MS)))
    MS_Catch_Periods <- merge(Catch,MSdat,by="Week",all.x=T) 
    MS_Catch_Periods$MS[is.na(MS_Catch_Periods$MS)] <- 0
    MS_Catch_Periods$LW[is.na(MS_Catch_Periods$LW)] <- 0
    MS_Catch_Periods$NTC[is.na(MS_Catch_Periods$NTC)] <- 0
    MS_Catch_Periods$period <- as.numeric(as.factor(MS_Catch_Periods$Week))
    periods <- data.frame(rbind(unique(MS_Catch_Periods$Week),rep(NA,length(MS_Catch_Periods$Week))))
    colnames(periods) <- unique(MS_Catch_Periods$Week)
    periods <- periods[-c(1:2), , drop=FALSE]
    #BON and Catch Tags
    CatchTagdat <- as.data.frame(Catchtag$CommTreat %>% filter(Fishery==Fy,Species==Sp,Year==Yr))
    CatchTagdat <- CatchTagdat[,colnames(CatchTagdat)%in%c("Fishery","Year","Species","Type","Group",FisherySeasons$Week)]
    CatchTagdat <- CatchTagdat[rowSums(CatchTagdat[,colnames(CatchTagdat)%in%c(1:53)])>0,]
    CatchTagdat <- as.data.frame(rbind.fill(CatchTagdat,periods))
    CatchTagdat[is.na(CatchTagdat)] <- 0
    BONTagdat <- as.data.frame(BONtag %>% filter(Year==Yr,Group%in%CatchTagdat$Group))
    
    #BONTagdat %>% left_join(CatchTagdat)
    
    Catch_BON_Tagdat <- merge(CatchTagdat,BONTagdat[,colnames(BONTagdat)%in%c("Group","Tags")],by="Group")
    Catch_BON_Tagdat <- merge(Catch_BON_Tagdat,TypeOrder,by="Type")
    Catch_BON_Tagdat <- Catch_BON_Tagdat[order(Catch_BON_Tagdat$TypeNumber,Catch_BON_Tagdat$Group),]
    Catch_BON_Tagdat <- Catch_BON_Tagdat[Catch_BON_Tagdat$Type%in%Types,]
    #these are allflex (I think)
    if(Sp==1){det_beta=c(671.97,8.23)} #2010-11 Det study Chinook detection beta dist
    if(Sp==2){det_beta=c(2201.72,2.42)} #coho detection beta dist params
    #these are the dets for the Cheeseblock
    if(Sp==3){det_beta=c(2026.95,4.06)} #Steelhead detection beta dist
    if(Sp==4){det_beta=c(2201.72,2.42)} #Sockeye USED coho detection beta dist params (no sockeye)
    #Make JAGS data list
    JAGSdat <- list(
      periods=length(FisherySeasons$Week),
      samp=MS_Catch_Periods$MS,
      T_lbs=MS_Catch_Periods$LW,
      NT_catch=as.numeric(MS_Catch_Periods$NTC),
      samp_period=MS_Catch_Periods$period[MS_Catch_Periods$MS>0],
      samp_periods=length(MS_Catch_Periods$period[MS_Catch_Periods$MS>0]),
      non_samp_period=MS_Catch_Periods$period[MS_Catch_Periods$MS==0],
      non_samp_periods=length(MS_Catch_Periods$period[MS_Catch_Periods$MS==0]),
      period=as.numeric(as.factor(Weightdat$Week)),
      total_catch_reported=as.numeric(TotCatch$TTC),
      logweight=log(Weightdat$Weight),
      N_bio=length(Weightdat$Weight),
      groups=dim(Catch_BON_Tagdat)[1],
      BON_tag=Catch_BON_Tagdat$Tags,
      tag=as.matrix(Catch_BON_Tagdat[,colnames(Catch_BON_Tagdat)%in%c(1:53)]),
      B_tags=BONefficiency$BONTags[BONefficiency$Species==Sp & BONefficiency$Year==Yr],
      M_tags=BONefficiency$MCNTags[BONefficiency$Species==Sp & BONefficiency$Year==Yr],
      det_beta=det_beta
    )
  }
 
  #write out Jags data formatted
 
  # subDir <- paste(Year,"_",Fishery,sep="")
  # if (file.exists(subDir)){
  #   setwd(file.path(results.dir, subDir))
  # } else {
  #   dir.create(file.path(results.dir, subDir))
  #   setwd(file.path(results.dir, subDir))
  # }
  # if(Write==TRUE){
  #   outfile  <- paste(Year,"_",Season,"_",Fishery,"_",Species,"_JAGSDATA_",Sys.Date(),".txt",sep="")
  #   write(paste("##",outfile,collapse=""),outfile)
  #   for(i in 1: length(JAGSdat))
  #   {
  #     if(is.null(dim(JAGSdat[[i]]))){Dimension <- length(JAGSdat[[i]])}else(Dimension <- paste(dim(JAGSdat[[i]]),collapse=","))
  #     write(paste("##",names(JAGSdat)[i]," ","dim = c(",Dimension,")",sep=""),outfile,append=T);write(JAGSdat[[i]],outfile,append=T)
  #   }
  # }
  JAGSdat <- list(JAGSdat=JAGSdat,Catch_BON_Tagdat=Catch_BON_Tagdat,MS_Catch_Periods=MS_Catch_Periods)
  return(JAGSdat$JAGSdat)
}

# JAGS plotting function ####
# JAGSplot <- function(JAGS.sims.list, Year, Season, Fishery, Species){
#   dat <- JAGS.sims.list
#   subDir <- paste(Year,"_",Fishery,sep="")
#   out_dir <- file.path(results.dir, subDir)
#   # if (file.exists(subDir)){
#   #   setwd(file.path(results.dir, subDir))
#   # } else {
#   #   dir.create(file.path(results.dir, subDir))
#   #   setwd(file.path(results.dir, subDir))
#   # }
#   pdf(file.path(out_dir, paste(Year=Year,"_",Season=Season,"_",Fishery=Fishery,"_",Species=Species,"_",Sys.Date(),".pdf",sep="")))
#   par(oma=c(3,8,0,0))
#   for(i in 1:length(names(dat))){
#     tdat <- dat[[i]]
#     if(length(dim(tdat))==3){
#       for(j in 1:dim(tdat)[3]){
#         plotdat <- as.matrix(tdat[,,j])
#         index <- j
#         bp <- boxplot.matrix(plotdat,outline=F,plot=F)
#         bp$stats[1,] <- apply(plotdat,2,function(x) quantile(x,0.025))
#         bp$stats[2,] <- apply(plotdat,2,function(x) quantile(x,0.25))
#         bp$stats[3,] <- apply(plotdat,2,function(x) quantile(x,0.5))
#         bp$stats[4,] <- apply(plotdat,2,function(x) quantile(x,0.75))
#         bp$stats[5,] <- apply(plotdat,2,function(x) quantile(x,0.975))
#         bp$out <- NULL
#         if(names(dat[i])%in%c("det_prob","hr","p_samp","pB_det")){limits=c(0,1)}else(limits=c(min(bp$stats[1,])*0.8,max(bp$stats[5,])*1.2))
#         bxp(bp,
#             outline=F,
#             horiz=T,
#             las=2,
#             boxfill="grey",
#             cex.axis=0.6,
#             yaxs="i",
#             xaxs="i",
#             ylim=limits
#             )
#         mtext(1,line=3,text=paste(names(dat)[[i]],index))
#       }
#     }else{plotdat <- tdat;index <- ""
#     bp <- boxplot.matrix(plotdat,outline=F,plot=F)
#     bp$stats[1,] <- apply(plotdat,2,function(x) quantile(x,0.025))
#     bp$stats[2,] <- apply(plotdat,2,function(x) quantile(x,0.25))
#     bp$stats[3,] <- apply(plotdat,2,function(x) quantile(x,0.5))
#     bp$stats[4,] <- apply(plotdat,2,function(x) quantile(x,0.75))
#     bp$stats[5,] <- apply(plotdat,2,function(x) quantile(x,0.975))
#     bp$out <- NULL
#     if(names(dat[i])%in%c("det_prob","hr","p_samp","pB_det")){limits=c(0,1)}else(limits=c(min(bp$stats[1,])*0.8,max(bp$stats[5,])*1.2))
#     bxp(bp,
#         outline=F,
#         horiz=T,
#         las=2,
#         boxfill="grey",
#         cex.axis=0.6,
#         yaxs="i",
#         xaxs="i",
#         ylim=limits
#         )
#     mtext(1,line=3,text=paste(names(dat)[[i]],index))
#     }
#   }
#   dev.off()
# }

#Estimate Harvest Rate ####
EstimateHarvestRate <- function(Year,Season,Fishery,Species,Types, Write=FALSE, JAGSdat){
  if(!require(R2jags))install.packages("R2jags") #installs Rmark if not already!
  library(R2jags) 
  #JAGSdat <- MakeJAGSdata(Year=Year,Season=Season,Fishery=Fishery,Species=Species,Types=Types,Write=Write)
  if(Fishery=="Sport"){
    pars <- c("BONP_tag","det_prob", "hr","pB_det","p_samp","sum_tag","xBON_tag","catch","tot_catch_season")}
  if(Fishery=="Commercial"){
    pars <- c("BONP_tag","det_prob", "hr","pB_det","p_samp","sum_tag","xBON_tag","mu_wt","sigma_logmu_wt","sigma_wt", "sigma_log_sigma_wt","T_catch","tot_catch_season")}
  if(Fishery=="Treaty"){    
    pars <- c("det_prob", "hr","pB_det","p_samp","sum_tag","xBON_tag","mu_wt","sigma_logmu_wt","sigma_wt", "sigma_log_sigma_wt","T_catch","tot_catch_season","tot_catch_season_adj","tot_catch","tot_catch_adj","report_ratio","tot_catch_sampled","sampled_prop","total_catch_reported")}
 # print(JAGSdat)
  start.time <- Sys.time()
  #print(start.time)
  jagsfit <- jags.parallel(JAGSdat, 
                           inits=NULL, 
                           model.file=file.path(model.dir, paste0(Fishery,"_psamp.txt")),
                           n.chains=3, 
                           n.thin=1, 
                           n.burnin=10000, 
                           n.iter=30000,
                           parameters.to.save=pars)
  end.time <- Sys.time()
  print(end.time-start.time)
  
  subDir <- paste(Year,"_",Fishery,sep="")
  out_dir <- file.path(results.dir, subDir)
  
  if(!dir.exists(out_dir)){dir.create(out_dir,recursive=TRUE)}

 # }
  save(jagsfit, file=file.path(out_dir,paste(Year,"_",Season,"_",Fishery,"_",Species,"_",Sys.Date(),".rda",sep="")))
  #write.csv(jagsfit$BUGSoutput$summary,paste(Year,"_",Season,"_",Fishery,"_",Species,"_","ALLRESULTS_",Sys.Date(),".csv",sep=""))
 
  
  # #write results tables and figs
  # if(Fishery=="Treaty"){
  #   #period variables
  #   colnames(jagsfit$BUGSoutput$sims.list$p_samp) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$mu_wt) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$T_catch) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$tot_catch) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$tot_catch_adj) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$sigma_wt) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   #harvest group variables
  #   colnames(jagsfit$BUGSoutput$sims.list$sum_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$hr) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$xBON_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   JAGSplot(JAGS.sims.list=jagsfit$BUGSoutput$sims.list,Year=Year,Season=Season,Fishery=Fishery,Species=Species)
  #   # tables <- c("p_samp","mu_wt","T_catch","tot_catch","tot_catch_adj","sum_tag","hr","xBON_tag")
  #   # quants <- c(0.025,0.5,0.975)
  #   # for(i in 1:length(tables)){
  #   #   tdat <- as.data.frame(jagsfit$BUGSoutput$sims.list[names(jagsfit$BUGSoutput$sims.list)==tables[i]])
  #   #   if(tables[i]%in%c("p_samp","mu_wt","T_catch","tot_catch","tot_catch_adj")){
  #   #     names <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   #   }
  #   #   if(tables[i]%in%c("sum_tag","hr","xBON_tag")){
  #   #     names <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   #   }
  #   #   stats <- t(apply(tdat,2,function(x) quantile(x,quants)))
  #   #   row.names(stats) <- names
  #   #   write.csv(stats,paste(Year,"_",Season,"_",Fishery,"_",Species,"_",tables[i],"_",Sys.Date(),".csv",sep=""))
  #   # }
  # }
  # if(Fishery=="Commercial"){
  #   #period variables
  #   colnames(jagsfit$BUGSoutput$sims.list$p_samp) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$mu_wt) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$T_catch) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$sigma_wt) <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   #harvest group variables
  #   colnames(jagsfit$BUGSoutput$sims.list$sum_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$hr) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$xBON_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   colnames(jagsfit$BUGSoutput$sims.list$BONP_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #  
  #   JAGSplot(JAGS.sims.list=jagsfit$BUGSoutput$sims.list,Year=Year,Season=Season,Fishery=Fishery,Species=Species)
  #   #tables <- c("p_samp","mu_wt","T_catch","sum_tag","hr","xBON_tag","BONP_tag")
  #   #quants <- c(0.025,0.5,0.975)
  #   # for(i in 1:length(tables)){
  #   #   tdat <- as.data.frame(jagsfit$BUGSoutput$sims.list[names(jagsfit$BUGSoutput$sims.list)==tables[i]])
  #   #   if(tables[i]%in%c("p_samp","mu_wt","T_catch")){
  #   #     names <- paste("Week ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   #   }
  #   #   if(tables[i]%in%c("sum_tag","hr","xBON_tag","BONP_tag")){
  #   #     names <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   #   }
  #   #   #stats <- t(apply(tdat,2,function(x) quantile(x,quants)))
  #   #   #row.names(stats) <- names
  #   #   #write.csv(stats,paste(Year,"_",Season,"_",Fishery,"_",Species,"_",tables[i],"_",Sys.Date(),".csv",sep=""))
  #   # }
  # }
  # if(Fishery=="Sport"){
  #   #period variables
  #   #colnames(jagsfit$BUGSoutput$sims.list$p_samp) <- paste("Month ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:12)],sep="")
  #   #colnames(jagsfit$BUGSoutput$sims.list$catch) <- paste("Month ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:12)],sep="")
  #   #harvest group variables
  #   #colnames(jagsfit$BUGSoutput$sims.list$sum_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   #colnames(jagsfit$BUGSoutput$sims.list$hr) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   #colnames(jagsfit$BUGSoutput$sims.list$xBON_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   #colnames(jagsfit$BUGSoutput$sims.list$BONP_tag) <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   #JAGSplot(JAGS.sims.list=jagsfit$BUGSoutput$sims.list,Year=Year,Season=Season,Fishery=Fishery,Species=Species)
  #   # tables <- c("p_samp","catch","sum_tag","hr","xBON_tag","BONP_tag")
  #   # quants <- c(0.025,0.5,0.975)
  #   # for(i in 1:length(tables)){
  #   #   tdat <- as.data.frame(jagsfit$BUGSoutput$sims.list[names(jagsfit$BUGSoutput$sims.list)==tables[i]])
  #   #   if(tables[i]%in%c("p_samp","mu_wt","catch")){
  #   #     names <- paste("Month ",colnames(JAGSdat$Catch_BON_Tagdat)[colnames(JAGSdat$Catch_BON_Tagdat)%in%c(1:53)],sep="")
  #   #   }
  #   #   if(tables[i]%in%c("sum_tag","hr","xBON_tag","BONP_tag")){
  #   #     names <- paste(JAGSdat$Catch_BON_Tagdat$Group,sep="")
  #   #   }
  #   #   stats <- t(apply(tdat,2,function(x) quantile(x,quants)))
  #   #   row.names(stats) <- names
  #   #   write.csv(stats,paste(Year,"_",Season,"_",Fishery,"_",Species,"_",tables[i],"_",Sys.Date(),".csv",sep=""))
  #   # }
  # }
  return(jagsfit)
}

# #wt/length vs travel days and age
# MakeWt_Length_TravelDaysPlots <- function(data.dir,stratafile)
# {
#   dat1 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_PITRecoveries", fields ="*")
#   dat2 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_TaggingDetails", fields ="*")
#   TAGdat <- merge(dat1,dat2,by="PITTag_ID",suffixes=c(".Mort",".Tag" ))
#   dat3 <-  odbc(dsn = "MS Access Database", sqtable = "tbl_MortalityDetails", fields ="*")
#   TAGdat <- merge(TAGdat,dat3,by="PITTag_ID",suffixes=c("",".Mort" ))
#   dat4 <- MakeHarvestReportingGroups(data.dir,stratafile)
#   TAGdat <- merge(TAGdat,dat4[,colnames(dat4)%in%c("RelSite_SpRRT",
#                                                  "ESU.DPS_groups_Rear",   
#                                                  "ESU.DPS_groups",     
#                                                  "MPG_groups_Rear",     
#                                                  "MPG_groups",            
#                                                  "DIP_groups_Rear",       
#                                                  "DIP_groups",            
#                                                  "A_B_groups_Rear",       
#                                                  "A_B_groups")],
#                 by="RelSite_SpRRT")
#   TAGdat$TravelDays <- as.numeric(as.Date(TAGdat$SampleDate)-as.Date(TAGdat$ReleaseDate))
#   TAGdat$RelMonth <- as.numeric(format(as.Date(TAGdat$ReleaseDate),format="%m"))
#   dat <- TAGdat[TAGdat$TravelDays>0 & TAGdat$SpeciesID==1,]
#   plot(dat$ForkLength.Mort~dat$TravelDays,col=as.factor(dat$SpeciesID.Mort))
#   hist(dat$ForkLength.Mort)
#   hist(dat$TravelDays,breaks=100)
#   legend("bottomright",legend=unique(dat$SpeciesID.Mort),pch=20,col=as.factor(unique(dat$SpeciesID.Mort)))
#   
#   w <- MakeWeights()
#   w <- w[w$Species%in%c(1),]
#   plot(w$Weight~w$ForkLength,col=as.factor(w$Species),ylim=c(0,40),xlim=c(0,1200))
#   legend("topleft",legend=unique(w$Species),pch=20,col=as.factor(unique(w$Species)))
#   hist(w$Weight[w$Species==1],breaks=200,xlim=c(0,50))
#   hist(w$ForkLength[w$Species==1 & w$Month<7],breaks=200,xlim=c(0,1000))
#   
#   hist(w$Weight[w$Species==1& w$Month>8],breaks=500,xlim=c(0,50))

fix_species <- function(df) {
  lut <- tibble(Species=c("1","2","3","4","B"),
         Name=c("Chinook","Coho","Steelhead","Sockeye","Sturgeon"))
  if(is.numeric(df$Species)){df$Species <- as.character(df$Species)}
  
  df %>% left_join(lut,by="Species") %>% 
    dplyr::rename(SpeciesID=Species,Species=Name)
}

det_beta <- function(Sp){
   if(Sp==1){det_beta=c(671.97,8.23)} #2010-11 Det study Chinook detection beta dist
     if(Sp==2){det_beta=c(2201.72,2.42)} #coho detection beta dist params
    #these are the dets for the Cheeseblock
     if(Sp==3){det_beta=c(2026.95,4.06)} #Steelhead detection beta dist
     if(Sp==4){det_beta=c(2201.72,2.42)}
  det_beta
}


JAGSdat_sport <- function(Sp, Yr){
catch_samp <- MarkSamples$Sport %>% 
  select(Month,Year,Species,samp=MS) %>%  
  left_join(SportCatch %>% filter(Catch>0), by=c("Year","Month","Species")) %>% 
  arrange(Year,Month) %>% 
  filter(Year==Yr, Species==Sp)

tag_dat <- Catchtag$Sport %>% 
  left_join(BONtag, by=c("Type","Group","Year")) %>% 
  filter(!is.na(Tags)) %>% 
  filter(Year==Yr, Species==Sp, Type %in% Types)

if(nrow(catch_samp)==0 & nrow(tag_dat)==0){stop("No tag or catch data found.")}
if(nrow(catch_samp)==0){stop(paste("No catch data found for Species",Sp,"in",Yr))}
if(nrow(tag_dat)==0){stop(paste("No tag data found for Species",Sp,"in",Yr))}

list(periods=length(catch_samp$Catch),
     samp=catch_samp$samp,
     catch=catch_samp$Catch,
     groups=nrow(tag_dat),
     BON_tag=tag_dat$Tags,
     tag=tag_dat %>% select_if(!is.na(suppressWarnings(as.numeric(colnames(.))))) %>% as.matrix(),
     B_tags=BONefficiency %>% filter(Species==Sp,Year==yr) %>% pull(BONTags),
     M_tags=BONefficiency %>% filter(Species==Sp,Year==yr) %>% pull(MCNTags),
     det_bet=det_beta(Sp=Sp))
}

