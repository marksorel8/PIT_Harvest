# SETUP ####
 source("code/packages.r")

# connection to db
connect_db <- function(db_path="C:/data/PIT_Harvest_DBs", 
                       db_file="PIT_Harvest_Database V3.1 - 03.24.2021_5pm.accdb"){

  db_name <- file.path(db_path, db_file)
  # The driver name (can find these in your ODBC connections)
  driver <- "{Microsoft Access Driver (*.mdb, *.accdb)}"
  
  # Paste together the connection string
  con_str <- paste0("Driver=", driver, ";", "Dbq=", db_name, ";")
  
  # returns the connection object
  DBI::dbConnect(odbc::odbc(), .connection_string=con_str )
}

# fn to read in table from db convert to tibble
getd <- purrr::compose(tidyr::as_tibble, purrr::partial(DBI::dbReadTable, con=db_con))

# READ TABLES ####
get_pop_lut <- function(data.dir="data", stratafile="pop_strata_2011-2020.xlsx"){
  read_xlsx(file.path(data.dir, stratafile), sheet="pop_lut") %>% 
  select(RelSite_SpRRT, ESU.DPS, MPG, DIP, A_B, Year_Added)
}

get_ticketed_catch <- function(tblTicketedCatch="tbl_TicketedCatch_CommercialTribal"){
  getd(tblTicketedCatch) %>%
    filter(FisheryType %in% c("Treaty", "Commercial"), TicketedSpecies %in% 1:4) %>% 
    rename(Date=TicketedCatchDate) %>% 
    fix_weeks(Date) %>% 
    rename(Fishery=FisheryType,
           Species=TicketedSpecies,
           Year=TicketedCatchYear,
           Period=Week) %>% 
    mutate(Month=month(Date)) %>% 
    add_season(Month) %>% 
    mutate(RoundWeight=case_when(CatchType==1 ~ ReportedWeight,
                                 CatchType==2 ~ as.numeric(RoundWeight),
                                 CatchType==3 ~ as.numeric(ReportedWeight))) %>%  
    group_by(Year, Period, Season, Fishery, Species) %>% 
    summarise(LW=sum(RoundWeight)) %>% 
    ungroup()
}

get_sport_catch <- function(tblSportCatch="tbl_SportCatch"){
  getd(tblSportCatch) %>% 
    filter(Species %in% 1:4) %>% 
    mutate(Fishery="Sport",
           Year=SportYear,
           Period=SportMonth) %>% 
    add_season(SportMonth) %>% 
    group_by(Fishery,Year, Season, Period, Species) %>%
    summarize(Catch=sum(Catch,na.rm=TRUE)) %>% 
    filter(Catch>0) %>% 
    ungroup()
}

get_nonticket_catch <- function(tblNonTicket="tbl_NonTicketedCatch"){
  getd(tblNonTicket) %>% 
    mutate(NonTicketedCatch=parse_number(NonTicketedCatch)) %>% 
    mutate(Fishery="Treaty",
           Period=StatWeek,
           Year=NTCatchYear)%>% 
    select(Year, Fishery, Species, Period, NT=NonTicketedCatch) %>% 
    filter(!is.na(NT), Species %in% 1:4)
}

get_wts <- function(tblWts="tbl_Weights"){
  getd(tblWts) %>% 
    filter(SpeciesID %in% 1:4) %>% 
    fix_weeks(SampleDate) %>% 
    add_season(SampleMonth) %>% 
    select(Fishery=FisheryType, 
           Year=SampleYear, 
           Season, 
           Period=Week, 
           Species=SpeciesID, 
           Weight)%>% 
    nest(wts=c(Weight))
}

get_marksamples <- function(MSbyDay="qry_MarkSample_ByDay"){
  getd(MSbyDay) %>% 
    filter(FisheryType %in% c("Sport", "Treaty", "Commercial")) %>% 
    fix_weeks(SampleDate) %>% 
    add_season(SampleMonth) %>% 
    mutate(Period=if_else(FisheryType=="Sport", SampleMonth, as.integer(Week))) %>% 
    group_by(Species=SpeciesID, Fishery=FisheryType, Season, Period, Year=year(SampleDate)) %>% 
    summarize(MS=sum(MKSamplecount, na.rm=TRUE))
}

# Dam tag data
get_dam_tags <- function(BON="tbl_Annual_BON_PIT_Passage", MCN="tbl_Annual_MCN_PIT_Passage"){
  
  tags_BON <- getd(BON) %>% mutate(Dam="BON",TravelDays=parse_number(TravelDays))
  tags_MCN <- getd(MCN) %>% mutate(Dam="MCN",TravelDays=parse_number(TravelDays))
  
  tags_BON %>% bind_rows(tags_MCN) %>% 
    filter_jacks(pop_lut) %>% #FILTER OUT JACKS
    filter(yday(Obs_DateTime)>60) %>% #Filter fish that passed before Mar1 (removes a couple Coho that pass MCN in jan of following year)
    select(PITTag_ID,RelSite_SpRRT,ReleaseSite, SpeciesID, Dam,ESU.DPS,MPG,DIP,A_B, PassageYear,Year_Added) %>% 
    distinct()
  
}

# Recoveries
get_fishery_tags <- function(recs="qry_PitRecoveries", detail="qry_TaggingDetails"){
  
  tags_FISHERY <- getd(recs) %>% fix_weeks(SampleDate)
  
  weeks_match <- tags_FISHERY %>% summarise(check=all(SampleWeek==Week)) %>% pull(check)
  if(!weeks_match){warning("Weeks from Access don't match calweek.")}
  
  tags_FISHERY_detail <- getd(detail) %>% 
    mutate(HatcheryID=if_else(HatcheryID=="",NA_character_, HatcheryID),
           ReleaseDate=date(ReleaseDate))
  
  tags_FISHERY %>% 
    left_join(tags_FISHERY_detail, by="PITTag_ID", suffix=c("_rec","")) %>% 
    dplyr::rename(ReleaseSite=ReleaseSiteID,
                  Tag_File=TagFile) %>% 
    mutate(TravelDays=SampleDate - ReleaseDate) %>% 
    filter(FisheryType %in% c("Sport","Commercial","Treaty")) %>% 
    filter_jacks(pop_lut) %>% 
    mutate(Period=if_else(FisheryType=="Sport", as.numeric(SampleMonth), Week)) %>% #
    add_season(SampleMonth) %>% 
    group_by(Fishery=FisheryType, 
             Year=as.numeric(RecoveryYear), 
             Species=SpeciesID,
             Rear=RearTypeID,
             Season,
             Period, 
             ESU.DPS,
             MPG,
             DIP,
             A_B) %>% 
    count() %>% 
    filter(ESU.DPS!="UWR_Sp")%>% 
    pivot_longer(cols=c(ESU.DPS, MPG, DIP, A_B), names_to="GroupType", values_to="Group", values_drop_na=TRUE) %>% 
    group_by_at(vars(-n)) %>% 
    summarize(n=sum(n)) %>%
    ungroup() %>% 
    mutate(Group=paste0(Group,"_",Rear))
}

# Data manip ####

# filters applied to all tags (recovered or detected at BON), does population lookup
filter_jacks <- function(tags, pop_lut){
  tags %>%
  mutate(RelSite_SpRRT=paste0(ReleaseSite, SpRRT)) %>% 
    mutate_at(vars(TravelDays), .funs=as.numeric) %>% 
      #   TravelDays=if_else(is.character(TravelDays),as.numeric(TravelDays),TravelDays)) %>% 
  filter(!(Tag_File=="ORPHAN"), 
         SpeciesID%in% c("1","2","3","4")) %>% 
# there was a release with release year=2069 ,tagfile JAR08281.WS2. Looked up info, fish were released from Oct 2008 - April 2009. Set arbitrary Jan 1 2009 release date
  mutate(ReleaseDate=case_when(year(ReleaseDate) == 2069 ~ date("2009-01-01"),
                               TRUE ~ date(ReleaseDate))) %>% 
  filter((SpeciesID==2 & between(TravelDays,450,1500))| #COHO
                    (SpRRT %in% c("11H","11W","11U","12W","12H","12U") & between(TravelDays, 700, 2200)) | #Spring/Summer Chk
                     (SpRRT %in% c("13H","13W","13U") & between(TravelDays, 715, 2200)) | # Fall Chk
                     #Unknown Chk
                     (SpRRT %in% c("15H","15W","15U") &
                        ((month(ReleaseDate)<7 & between(TravelDays,700,2200)) | 
                           (month(ReleaseDate)>=7 & between(TravelDays,715,2200))))|
           (SpeciesID== 3 & between(TravelDays, 400, 2200 ))|
           SpeciesID==4 & between(TravelDays, 400, 2200)) %>% 
  left_join(pop_lut, by="RelSite_SpRRT")
}

fix_weeks <- function(data, date_field){
  date <- enquo(date_field)
  data %>%
    mutate(Week=case_when(year(!!date) < 2014 ~ calweek(!!date, week_start=1, type="First4"),
                          year(!!date) >= 2014 ~ calweek(!!date, week_start=7, type="Jan1")),
           !!date:=date(!!date))
}

check_missing_pops <- function(filtered_tags){

rel_site_lut <- getd("LUT_Validation_Codes_MRRSiteID")
rel_site_gis <- getd("LUT_MRRSite_GIS_Metadata") %>% 
  select(MRRSiteCode, SiteName=MRRSiteName, RKM_Mask=MRRSiteRKMMask, StreamName, TribToName, HUC8, HUC8_Name)

  filtered_tags %>% 
      filter(is.na(Year_Added)) %>% 
  select(ReleaseSite,RelSite_SpRRT,Year_Added) %>% distinct() %>% 
  left_join(rel_site_lut,by=c("ReleaseSite"="Code")) %>% 
  filter(!str_detect(ReleaseSite,"COLR"), # FILTER FISH TAGGED IN MAINSTEM 
         !str_detect(Site.Type.Name,"Intra-Dam"))%>% # and IntraDam tagging sites 
  left_join(rel_site_gis %>% select(MRRSiteCode,TribToName,HUC8_Name),by=c("ReleaseSite"="MRRSiteCode")) #%>% 
  #print(n=Inf)
}

make_bon_tags <- function(tags_DAM, ...){
  grp <- rlang::ensyms(...)

tags_DAM %>% 
  mutate(Rear=str_sub(RelSite_SpRRT, -1, -1)) %>% 
  filter(Dam=="BON", !is.na(ESU.DPS)) %>% 
  count(Year=PassageYear, Species=SpeciesID, Rear, !!!grp) %>% 
  mutate(Year=as.numeric(Year))%>%
  filter(ESU.DPS!="UWR_Sp")%>% 
  pivot_longer(cols=c(ESU.DPS, MPG, DIP,A_B), names_to="GroupType", values_to="Group", values_drop_na=TRUE) %>% 
  group_by_at(vars(-n)) %>% 
  summarize(n=sum(n)) %>%
  ungroup() %>%
  mutate(Group=paste0(Group,"_",Rear))
}

join_marksamples <- function(catch_dat, mark_samples){
  catch_dat %>% 
    left_join(mark_samples, by=c("Fishery","Year", "Period", "Species","Season")) %>% 
    mutate(MS=replace_na(MS, 0))
}

add_season <- function(data, month_field){
  Month <- enquo(month_field)
  data %>% 
    mutate(Season=case_when(!!Month %in% 1:3 ~ "Winter",
                            !!Month %in% 4:5 ~ "Spring",
                            !!Month %in% 6:7 ~ "Summer",
                            !!Month %in% 8:12 ~ "Fall"))
}

# Statweek for dates, requires lubridate 
# week_start = 1 = Monday, week_start = 7 = Sunday
# Jan1: Week one is the first week with jan1 in it as determined by week_start
# First4: Week one is the first week with >4 days as determined by week_start
calweek <- function(dates, week_start=7, type=c("Jan1", "First4")){
  
  dates <- date(dates)
  type <- match.arg(type)
  
# Jan 1st of year for dates
  jan1 <- lubridate::floor_date(dates, unit = "year")
    
# First day of week with Jan1 in it.
  jan1start <- lubridate::floor_date(jan1, unit="week", week_start=week_start)
    
  if(type=="First4"){  
# Start of next week following jan1
      jan1end <- lubridate::ceiling_date(jan1, unit="week", week_start=week_start)
# Number of days in week with jan1 in it
      daysinweekofjan1 <- jan1end - jan1
# If its less than 4, start week 1 on next week_start
      day1_week1 <- if_else(daysinweekofjan1<4, jan1end, jan1start)
    }else{ 
       day1_week1 <- jan1start
    }
  
    as.numeric(dates - day1_week1) %/% 7 + 1
}

#Prep for JAGS ####
BON_EFF <- function(tags_DAM){ 
  tags_DAM %>% 
    select(PITTag_ID, SpeciesID, Dam, PassageYear) %>%  
    mutate(Obs=1) %>% 
    pivot_wider(names_from=Dam, values_from=Obs, values_fill=list(Obs=0)) %>% 
    group_by(PassageYear,SpeciesID,BON,MCN) %>% 
    count() %>% 
    filter(MCN==1) %>% 
    group_by(Year=as.numeric(PassageYear),SpeciesID) %>% 
    mutate(Tot=sum(n)) %>% 
    mutate(p=n/Tot) %>% 
    filter(BON==1) %>% 
    select(Year,Species=SpeciesID, B_tags=n,M_tags=Tot)
}

make_tagdat <- function(ms_table, tags_FISHERY, BONtags){
  ms_table %>% 
  #filter(Fishery=="Commercial") %>% 
  select(Fishery, Year, Season, Species, Period) %>% 
  left_join(tags_FISHERY, by = c("Fishery", "Year", "Season", "Species", "Period")) %>% 
  left_join(BONtags, by=c("Year","Species","GroupType","Group","Rear"),suffix=c("_fishery","_BON")) %>% 
  ungroup() %>% 
  nest(data_tags=c(Period, Rear, GroupType, Group, n_fishery, n_BON)) %>%
  mutate(recs=map(data_tags, 
                  ~.x %>% pivot_wider(names_from=c(Period), values_from=n_fishery, values_fill=(list(n_fishery=0)),names_prefix="p") %>% 
                    filter(!is.na(GroupType)))) %>% 
  filter(map_lgl(recs, ~nrow(.x)>0)) %>% 
  mutate(tag=map(recs, ~.x %>% select(starts_with("p")) %>% as.matrix()))
}

add_sport_detbetas <- function(data, species_field){
  sp <- enquo(species_field)
  data %>% mutate(detbetas=map(!!sp,
                      ~case_when(.x == 1 ~ c(671.97, 8.23),
                                 .x %in% 2:4 ~ c(2201.72, 2.42),
                                 .x == 3 ~ c(2026.95, 4.06))))
}

add_comm_detbetas <- function(data){
  
  data %>% 
    # FROM 2020 detection study for chinook in commercial setting
    # Beta params fit to posterior of mu_sampler from model 
    # using quantile matching at IQR c(.25,.75)
    mutate(detbetas=list(c(123.0, 2.8)) )
}

add_tags <- function(data, BONeff, tag_dat){
 data %>%  
    left_join(BONeff,by=c("Species","Year")) %>%
    left_join(tag_dat,c("Fishery", "Year", "Season", "Species"))
}

# Run Model ####
run_mod <-function(data , model, monitor, ...){
  run.jags(data=data,
           model=model,
           monitor=monitor,
           burnin=10000,
           thin=10,
           adapt=2000,
           sample=10000,
           n.chains=3,
           method="rjparallel")
           
  }
