#load packages ####
if(!require(plyr))install.packages("plyr"); library(plyr)  
if(!require(dplyr))install.packages("dplyr"); library(dplyr) 
if(!require(reshape))install.packages("reshape"); library(reshape) 
if(!require(tidyr))install.packages("tidyr"); library(tidyr) 
if(!require(RODBC))install.packages("RODBC"); library(RODBC) # ideally remove RODBC dependency
if(!require(DBI))install.packages("DBI"); library(DBI)
if(!require(odbc))install.packages("odbc"); library(odbc)
library(purrr)
library(readxl)

#set controls ####
data.dir <- "data"
script.dir <- "code"
results.dir <- "results"
model.dir <- "models"

#stratafile <- "BON_Pop_Strata_2011_to_2015_2.28.2017_AB - Copy.csv"
stratafile <- "pop_strata_2011-2020.xlsx"

functionsfile <- "Harvest_Functions_BC.r"

db_path <- "C:/data/PIT_Harvest_DBs/"
#db_name <- "DCamp_exmpl.accdb"
#db_name <- "PIT_Harvest_Database V3.1 - 05.23.2018_445pm.accdb"
db_name <- "PIT_Harvest_Database V3.1 - 03.24.2021_5pm.accdb"
#db_name <- "PIT_Harvest_Database V3.1 - 01.14.2021_530pm.accdb"
database <- file.path(db_path, db_name)

#source functions ####
source(file.path(script.dir, functionsfile))

# connect to db
db_con <- connect_db(database)

Types=c(#"A_B_groups",
  #"A_B_groups_Rear",
  #"ESU.DPS_groups",
  "ESU.DPS_groups_Rear",
  #"MPG_groups",
  "MPG_groups_Rear",	
  #"DIP_groups",
  "DIP_groups_Rear"
)
#HarvestReportingGroups %>% select(RelSite_SpRRT,Types)
#run functions to load and organize data ####
HarvestReportingGroups <- MakeHarvestReportingGroups(data.dir=data.dir, stratafile=stratafile) %>% as_tibble()
MarkSamples <- MakeMarkSamples(db_con) %>% map(as_tibble) #Doesn't include season
SportCatch <- MakeSportCatch(db_con) %>% as_tibble()
TicketedCatch <- MakeTicketedCatch(db_con) %>% as_tibble()
TotalTreatyCatch <- MakeTotalTreatyCatch(db_con) %>%  as_tibble()#%>% mutate(Fishery="Treaty") 
NonTicketedCatch <- MakeNonTicketedCatch(db_con) %>% as_tibble()
Weights <- MakeWeights(db_con) %>% as_tibble()
BONtag <- MakeBONtag(data.dir = data.dir,stratafile = stratafile,db_con=db_con) %>% as_tibble() %>% mutate(Year=as.numeric(Year)) #doesn't include harvest group category
Catchtag <- MakeCatchtag(data.dir = data.dir,stratafile = stratafile,db_con=db_con) %>% map(as_tibble)  #doesn't include harvest group category
BONefficiency <- MakeBONefficiency(db_con) %>% as_tibble()

#debugonce(MakeJAGSdata)
MakeJAGSdata(2019,"Fall","Treaty",1,Types)
# Disconnect from db
dbDisconnect(db_con)

g2g[2,] %>% select(Fishery,Season,Year,SpeciesID)
g2g$JAGSdat[[2]]
Sp=3
Yr=2016
Sn="Fall"
Fy="Treaty"

mark_samp <- MarkSamples$CommercialTreaty %>% 
  filter(Species==Sp,Fishery=="Treaty",Season=="Fall",Year==2016) %>% 
  group_by(Season,Year,Week,Species) %>% 
  summarize(MS=sum(MS,na.rm=TRUE)) %>% 
  mutate(Species=as.numeric(Species))

ticketed <- TicketedCatch %>% filter(Species==Sp, Season==Sn, Year==Yr) %>%  
  left_join(mark_samp,c("Season", "Year", "Week", "Species")) %>%
  left_join(NonTicketedCatch %>% mutate(Species=as.numeric(Species)) %>% filter(Species==Sp, Season==Sn,Year==Yr), 
            by=c("Season", "Year", "Week", "Species")) %>% 
  mutate_at(vars(MS,NTC), ~replace_na(.x,0)) 
 
Weightdat <-  Weights %>% 
  filter(Species==Sp,Fishery==Fy,Season==Sn,Year==Yr) %>% 
  right_join(ticketed,by=c("Year","Week","Fishery")) %>% 
  select(Week,Weight)  %>% 
   mutate(period=as.numeric(as.factor(Week)))

 logweight <- log(Weightdat$Weight)
 N_bio <- nrow(Weightdat)
 period <- Weightdat$period
 samp_period <- which(ticketed$MS>0)
 non_samp_period <- which(ticketed$MS==0)
 samp_periods <- length(samp_period)
 non_samp_periods <- length(non_samp_period)
 
# FisherySeasons <- FisherySeasons %>% 
#   filter(Year==Yr,FisheryType==Fy, Season==Sn) %>% 
#       select(Year,Fishery=FisheryType,Week=StatWeek)
#  

# Catchtag$CommTreat%>% 
#   group_by(Type, Fishery,Species,Year) %>% 
#   left_join(BONtag %>% mutate(Year=as.numeric(Year)),by=c("Type","Group","Year")) %>% filter(is.na(Tags))
#   filter(Fishery=="Sport",Year==2016,Species==1)
#   nest() %>% 
#   mutate(mtrx=map(data, ~.x %>% select(-Group) %>% as.matrix())) %>% 
#   

#generate harvest model list ####
yr <- 2016:2019

SportModels <- mods(Fishery="Sport", 
                    Season="All", 
                    Year=yr, 
                    Species=1:3)

CommercialModels <- mods(Fishery="Commercial", 
                         Season=c("Spring","Summer","Fall"), 
                         Year=yr, 
                         Species=1:2)

TreatyModels <- mods(Fishery="Treaty",
                     Season=c("Summer","Fall"),
                     Year=yr,
                     Species=1:4)

Models <- bind_rows(CommercialModels,SportModels,TreatyModels)
Models <- Models[!Models$Species==2 | Models$Species==2 & Models$Season%in%c("Fall","All") ,]
Models <- Models[!Models$Species==4 | Models$Species==4 & Models$Season%in%c("Summer","Fall","All") & Models$Fishery=="Treaty",]

Models <- Models %>% 
  crossing(Types=list(Types)) %>% 
  mutate(safeDat=pmap(list(Year, Season, Fishery, Species, Types, Write=FALSE),
                      safely(MakeJAGSdata))) %>% 
  mutate(datErrs=map(safeDat,"error"),
         JAGSdat=map(safeDat,"result"),
         data_error=map_lgl(datErrs,~!is.null(.x))) %>%
  mutate(lengths=map(JAGSdat,~which(lengths(.x)==0)),
         data_missing=map_lgl(lengths,~length(.x)> 0)) %>% 
  mutate(missing_vars=map2(JAGSdat, lengths, ~names(.x[.y]))) %>%
  fix_species()

g2g <- Models %>% filter(!data_error,!data_missing)
dat_ers <- Models %>% filter(data_error)
dat_msng <- Models %>% filter(data_missing)
dat_msng %>% print(n=Inf)
dat_msng %>% unnest(missing_vars) %>% 
  select(Fishery,Season,Year,SpeciesID,missing_vars) %>% 
  distinct() %>% 
  print(n=Inf)

message(paste(nrow(g2g),"models ready.\n",
              nrow(dat_ers),"models have data errors.\n",
              nrow(dat_msng),"models have missing data."))

out <- Models %>% 
  mutate(safe_run=pmap(list(Year, Season, Fishery, Species=SpeciesID, Types, Write=FALSE,JAGSdat), 
                      safely(EstimateHarvestRate)),
         mod=map(safe_run,"result"),
         err=map(safe_run,"error"),
         is_error=map_lgl(mod, is.null))

out %>% filter(!is_error) %>% pluck("mod", 21)
  
  print(n=Inf)
  select(Fishery,Season,Year,SpeciesID,Species,JAGSdat,missing_vars,mod) %>%
  mutate(n_missing_vars=map_int(missing_vars,length)) %>%
  pluck("JAGSdat",1)
  #unnest(missing_vars)


out %>% filter(is_error)%>% select(Fishery,Season,Year,SpeciesID,Species,JAGSdat,missing_vars,mod)
# # try data missing 
# out_w_missing <- dat_msng %>% 
#   mutate(safe_run=pmap(list(Year, Season, Fishery, Species=SpeciesID, Types, Write=FALSE,JAGSdat), 
#                       safely(EstimateHarvestRate)),
#          mod=map(safe_run,"result"),
#          err=map(safe_run,"error"),
#          is_error=map_lgl(mod, is.null)) %>% 
#   select(datErrs,JAGSdat,mod,err,is_error)


out %>% filter(!is_error)
res <- out %>% filter(!is_error) %>% pluck("mod",1)

as.mcmc(res) %>% summary()
# #run harvest model list ####
# for(i in 1:nrow(Models)){
#   i <- 14
#   print(Models[i,])
#   trymodel <- try(EstimateHarvestRate(Year=Models$Year[i],Season=as.character(Models$Season[i]),Fishery=as.character(Models$Fishery[i]),Types=Types,Species=Models$Species[i],Write=FALSE), silent=F)
#   if ('try-error' %in% class(trymodel)){next} else(next)
# }

#a <- MakeJAGSdata(Year=2015,Season="Summer",Fishery="Commercial",Species=1,Types=Types,Write="True")
  
