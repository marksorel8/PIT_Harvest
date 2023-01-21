library(tidyverse)
library(lubridate)
library(readxl)
library(DBI)
library(odbc)

#set controls ####
data.dir <- "data"
script.dir <- "code"
results.dir <- "results"
model.dir <- "models"


stratafile <- "pop_strata_2011-2020.xlsx"

functionsfile <- "Harvest_Functions_BC.r"

db_path <- "C:/data/PIT_Harvest_DBs/"
db_name <- "PIT_Harvest_Database V3.1 - 03.24.2021_5pm.accdb"
database <- file.path(db_path, db_name)

#source functions ####
# source(file.path(script.dir, functionsfile))

Types <- c(#"A_B_groups",
  #"A_B_groups_Rear",
  #"ESU.DPS_groups",
  "ESU.DPS_groups_Rear",
  #"MPG_groups",
  "MPG_groups_Rear",
  #"DIP_groups",
  "DIP_groups_Rear"
    )

connect_db <- function(db_name){
  
  # The driver name (can find these in your ODBC connections)
  driver <- "{Microsoft Access Driver (*.mdb, *.accdb)}"
  
  # Paste together the connection string
  con_str <- paste0("Driver=", driver, ";", "Dbq=", db_name, ";")
  
  # returns the connection object
  DBI::dbConnect(odbc::odbc(), .connection_string=con_str )
}

# connect to db
db_con <- connect_db(database)

#DBI::dbDisconnect(db_con)

# function to make fishery x season x year x species table
mods <- function(Fishery, Season, Year, Species){
  crossing(Fishery,Season,Year,Species)
}

# Tables ####
pop_lut <- read_xlsx(file.path(data.dir,stratafile),sheet="pop_lut") %>% select(RelSite_SpRRT,ESU.DPS,MPG,DIP,A_B,Year_Added)

get_dam_tags <- function(tbl){
 getd(tbl) %>% 
  mutate(RelSite_SpRRT=paste0(ReleaseSite, SpRRT),
         TravelDays=parse_number(TravelDays)) %>% 
  filter(!(Tag_File=="ORPHAN"),SpeciesID!="0") %>% 
  select(RelSite_SpRRT, PassageYear, PassDate=Obs_DateTime, PIT=PITTag_ID,Species=SpeciesID,SpRRT,ReleaseSite,Rel_RKM,TravelDays,ReleaseDate) %>% 
# there was a release with release year=2069 ,tagfile JAR08281.WS2. Looked up info, fish were released from Oct 2008 - April 2009. Set arbitrary Jan 1 2009 release date
  mutate(ReleaseDate=case_when(year(ReleaseDate)==2069 ~ date("2009-01-01"),
                               TRUE ~ date(ReleaseDate)),
         PassDate=date(PassDate))
}

tags_BON <- get_dam_tags("tbl_Annual_BON_PIT_Passage") %>% mutate(Dam="BON")
tags_MCN <- get_dam_tags("tbl_Annual_MCN_PIT_Passage") %>% mutate(Dam="MCN")

rel_site_lut <- getd("LUT_Validation_Codes_MRRSiteID")
rel_site_gis <- getd("LUT_MRRSite_GIS_Metadata") %>% 
  select(MRRSiteCode,SiteName=MRRSiteName, RKM_Mask=MRRSiteRKMMask,StreamName,TribToName,HUC8,HUC8_Name)


tags_Dam <- tags_BON %>% 
  bind_rows(tags_MCN) %>% 
  filter((Species==2 & between(TravelDays,450,1500))|
                    (SpRRT %in% c("11H","11W","11U","12W","12H","12U") & between(TravelDays, 700, 2200)) |
                     (SpRRT %in% c("13H","13W","13U") & between(TravelDays, 715, 2200)) |
                     (SpRRT %in% c("15H","15W","15U") & 
                        ((month(ReleaseDate)<7 & between(TravelDays,700,2200)) | 
                           (month(ReleaseDate)>=7 & between(TravelDays,715,2200))))) %>% 
  left_join(pop_lut,by="RelSite_SpRRT")


# UNDEFINED ESUs- need to check.
DAMtags %>% 
  filter(is.na(Year_Added)) %>% 
  select(ReleaseSite,RelSite_SpRRT,Year_Added) %>% distinct() %>% 
  left_join(rel_site_lut,by=c("ReleaseSite"="Code")) %>% 
  filter(!str_detect(ReleaseSite,"COLR"), # FILTER FISH TAGGED IN MAINSTEM 
         !str_detect(Site.Type.Name,"Intra-Dam"))%>% # and IntraDam tagging sites 
  print(n=Inf) %>% 
  left_join(rel_site_gis %>% select(MRRSiteCode,TribToName,HUC8_Name),by=c("ReleaseSite"="MRRSiteCode")) 
#%>% 
  #write_excel_csv("data/sites_to_add.csv")

# Function to calculate "Calendar Week" ####
# Starting sunday (week_start=7), requires lubridate
  calweek <- function(dates, week_start=1, type=c("Jan1","First4")){
  dates <- date(dates)
    #dates <- fisheryTAGS %>% filter(year(SampleDate)==2012) %>% pull(SampleDate) %>% unique()
  type <- match.arg(type)
 # type <- "First4"
  #browser()
    # Jan 1st of year for each date in d
    jan1 <- lubridate::floor_date(dates, unit = "year") 
    jan1start <- lubridate::floor_date(jan1, unit="week",week_start=week_start)
    jan1end <- lubridate::ceiling_date(jan1, unit="week",week_start=week_start)
    
    daysinweekofjan1 <- jan1end-jan1
    
  if(type=="First4"){
       day1_week1 <- if_else(daysinweekofjan1<4, jan1end, jan1start)
    }else{ 
       day1_week1 <- jan1start
    }

    as.numeric(dates - day1_week1) %/% 7 + 1
}   
    
FISHERYtags <- getd("qry_PitRecoveries") %>% 
  mutate(Week=case_when(year(SampleDate) < 2014~calweek(SampleDate, week_start=1, type="First4"),
                        year(SampleDate) >= 2014~calweek(SampleDate, week_start=7, type="Jan1")))


FISHERYdetail <- getd("qry_TaggingDetails")


FISHERYtags %>%    
  left_join(FISHERYdetail, by="PITTag_ID", suffix=c("_recovery","_tagging")) %>% 
  filter(SpeciesID_recovery %in% c("1", "2", "3", "4"),
         !TagFile %in% c("ORPHAN", "DISOWN")) %>%   
  mutate(ReleaseDate=case_when(year(ReleaseDate)==2069 ~ date("2009-01-01"),
                               TRUE ~ date(ReleaseDate))) %>% 
  mutate(TravelDays=as.numeric(date(SampleDate) - ReleaseDate)) %>%  
  select(TagFile,ReleaseDate,SampleDate,TravelDays) %>% 
  arrange((TravelDays)) %>% 
  filter((SpeciesID_tagging==2 & between(TravelDays,450,1500))|
                    (SpRRT %in% c("11H","11W","11U","12W","12H","12U") & between(TravelDays, 700, 2200)) |
                     (SpRRT %in% c("13H","13W","13U") & between(TravelDays, 715, 2200)) |
                     (SpRRT %in% c("15H","15W","15U") & 
                        ((month(ReleaseDate)<7 & between(TravelDays,700,2200)) | 
                           (month(ReleaseDate)>=7 & between(TravelDays,715,2200))))) %>%   
 left_join(pop_lut,by="RelSite_SpRRT") 
  #filter(is.na(Year_Added)) 

NTC <- getd("tbl_NonTicketedCatch") %>% 
  mutate(NonTicketedCatch=parse_number(NonTicketedCatch))


