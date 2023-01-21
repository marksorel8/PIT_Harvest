library(tidyverse)
library(readxl)
library(lubridate)

d <- read_xlsx("data/Interrogation Summary.xlsx",1)  %>% 
  mutate(PassYear=year(`Last Obs Date Max`)) %>% 
  select(PIT= `Tag Code`,
         SpRRT=`SRR Code`,
         Site=`Site Code Value`,
         ReleaseSite=`Release Site Code Value`,
         PassYear,
         ObsDate=`Last Obs Date Max`,
         TravelDays=`Travel Time Days`,
         ReleaseDate=`Release Date Max`) %>% 
  mutate(Species=str_sub(SpRRT,1,1),
         Run=str_sub(SpRRT,2,2),
         RelSite_SpRRT=paste0(ReleaseSite,SpRRT)) #%>% 
  #filter(yday(ObsDate)>120,Species %in% 1:4)


pop_lut <- read_xlsx(file.path(data.dir, stratafile), sheet="pop_lut") %>% 
  select(RelSite_SpRRT, ESU.DPS, MPG, DIP, A_B, Year_Added)

  recs <- read_xlsx("data/PIT recoveries.xlsx") %>% 
    select(PIT=`Tag Code`,RelSpecies=Species, MortDate,RelSite,SpRRT=SRR,RecSpecies=MortSpecies,RecSite=`Mort Site` ,Year=MortYear,TravelDays) %>% 
    mutate(Year=as.numeric(Year)) %>%
    mutate(RelSite_SpRRT=paste0(RelSite,SpRRT)) %>% 
    left_join(pop_lut,by="RelSite_SpRRT") %>% 
    filter(!RelSite %in% c("TEANMF", "COLR2")) %>%
    filter(RecSite!="COLR") #%>% 
    #filter(is.na(Year_Added))
    #filter(Project=="CRF")
  
d_ch <- d %>% #filter(PIT=="384.3B2395F754") %>% 
    filter(Species!=3) %>% 
    select(PIT,Site,PassYear) %>% 
    distinct() %>% 
    group_by(PIT,Site) %>% 
    filter(PassYear==max(PassYear)) %>% # 1 Coho and 1 Chinook came to BON 1 year after first detection
    ungroup() %>% 
    arrange(match(Site, c("PD7","BON","MCN"))) %>% 
    pivot_wider(id_cols=c(PIT),names_from=Site, values_from=PassYear,values_fn=max) %>% 
    #filter(MCN>BON|(is.na(BON)&PD7>MCN)) %>% 
    mutate(MCN=case_when(!is.na(MCN)&MCN > BON ~ BON,
                         is.na(MCN)~NA_real_,
                         TRUE ~ MCN)) %>% 
    pivot_longer(names_to="Site",values_to="PassYear",cols=c(PD7,BON,MCN)) %>%
    filter(!is.na(PassYear)) %>% 
    mutate(Obs=1) %>% 
    pivot_wider(names_from=Site, values_from=Obs, values_fill=list(Obs=0)) %>% 
    left_join(d %>% select(PIT,Species,Run,RelSite_SpRRT) %>% distinct(),by="PIT") %>% 
    left_join(pop_lut,by="RelSite_SpRRT")

 dam_tags <-  d_ch %>% 
    filter(MCN==1) %>%   
    group_by(Species,PassYear, MCN, BON) %>% 
    count() %>% 
    group_by(Species,PassYear) %>% 
    mutate(Tot=sum(n)) %>%
    filter(BON==1) %>% 
    select(Species,PassYear,
           B_tags=n,
           M_tags=Tot) %>% print(n=Inf)
    
fishery_lut <- getd("qry_PitRecoveries") %>% select(PIT=PITTag_ID,Year=SampleYear, Month=SampleMonth, Week=SampleWeek,FisheryType)
season_lut <- getd("qry_FisherySeasons") %>% 
  mutate(Period=if_else(is.na(StatWeek) | StatWeek==0,as.numeric(Month),StatWeek)) %>% 
  select(Year,Season,FisheryType,Period)

mark_samples <- getd("qry_MarkSample_ByDay") %>% 
  mutate(Period=if_else(FisheryType=="Sport",SampleMonth,SampleWeek)) %>% 
  group_by(Species=SpeciesID,Fishery=FisheryType, Period,Year=year(SampleDate)) %>% 
  summarize(MS=sum(MKSamplecount,na.rm=TRUE)) #%>% 
  #left_join(season_lut) #%>% 
  #filter(is.na(MS))

#..  Ticketed ####
  catch_TICKET <- getd("tbl_TicketedCatch_CommercialTribal") %>% 
  rename(Fishery=FisheryType,
         SpeciesID=TicketedSpecies)
    
#..  Sport ####
  catch_SPORT <- getd("tbl_SportCatch") %>% 
  rename(SpeciesID=Species) %>% 
  filter(SpeciesID %in% 1:4) %>% 
  mutate(Fishery="Sport") 
    
#..  Non-ticketed ####
  catch_NONTICKET <- getd("tbl_NonTicketedCatch") %>% 
  mutate(NonTicketedCatch=parse_number(NonTicketedCatch)) %>% 
  mutate(Fishery="Treaty")

catch_SPORT %>% 
  group_by(Year=SportYear, Period=SportMonth,Species=SpeciesID,Fishery) %>% 
  summarize(Catch=sum(Catch)) %>% 
  filter(Catch>0) %>% 
  left_join(mark_samples, c("Year","Fishery","Period","Species")) %>% 
  mutate(MS=replace_na(MS, 0)) %>% 
  left_join(season_lut, by=c("Year","Fishery"="FisheryType","Period")) %>% 
  arrange(Year, Species, Period)


# MISSING PIT Recoveries in qry_PITRecoveries ####
recs1 <- recs %>% 
  mutate(Run=str_sub(SpRRT,2,2)) %>% 
  select(PIT, Species=RelSpecies, Run, RelSite_SpRRT, ESU.DPS, MPG, DIP, Year, MortDate, Year_Added)%>%
  left_join(fishery_lut, by=c("PIT","Year")) %>%
  #filter(!is.na(FisheryType)) %>% 
  filter(is.na(FisheryType)) %>% # MISSING TAGS IN RECOVERIES 
  mutate(Period=case_when(FisheryType=="Sport" ~ Month,
                          TRUE ~ Week)) %>% 
  left_join(season_lut, by=c("Year","FisheryType","Period"))
recs1 %>% filter(is.na(Season),Species!=3,!is.na(ESU.DPS))
  recs1 %>% 
  #filter(FisheryType=="Sport", Species!=3) %>% 
  select(PIT,Species,Run,RelSite_SpRRT,ESU.DPS,MPG,DIP,Year,Period,FisheryType,Season) %>% 
  distinct() %>% 
  group_by(Species,Run,ESU.DPS,MPG,DIP,Year,Period,FisheryType,Season) %>% 
  count(name="Obs") %>%  
  group_by(ESU.DPS,Species,Run,Year,Period,FisheryType,Season) %>% 
    summarize(Obs=sum(Obs)) %>%
    ungroup() %>% 
    arrange(Period) %>%
  pivot_wider(names_from=Period, values_from=Obs, values_fill=list(Obs=0),names_prefix="p") %>% 
    arrange(Year,Species) %>% 
    filter(!is.na(ESU.DPS)) %>%
    filter(is.na(Season))
    #select(Year,starts_with("p"))
    

