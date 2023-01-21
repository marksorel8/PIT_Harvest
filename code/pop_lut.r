library(tidyverse)

getd <- compose(partial(dbReadTable, conn=db_con), as_tibble, .dir="forward")

#run through the db connection in Harvest Control Script
bon_psg <- getd("tbl_Annual_BON_PIT_Passage") 
mcn_psg <- getd("tbl_Annual_MCN_PIT_Passage")
fshry_recs <- getd("tbl_PITRecoveries") 
site_lut <- getd("LUT_MRRSite_GIS_Metadata") 
run_lut <- getd("LUT_Validation_Codes_RunTypeID")
sp_lut <- getd("LUT_Validation_Codes_SpeciesID")
colnames(bon_psg)==colnames(mcn_psg)


tag_info <- dbReadTable(db_con, "tbl_TaggingDetails") %>% 
  as_tibble() %>% 
  select(PITTag_ID, SpeciesID, RunTypeID, RearTypeID, HatcheryID, ReleaseSiteID, RiverKilometerMask)


  
d <- bind_rows(bon_psg,mcn_psg,fshry_recs) %>% filter(SpeciesID!=0) %>% 
  select(PITTag_ID,SpeciesID,SpRRT,ReleaseSite,Rel_RKM) %>% distinct()

all_d <- d %>% 
  left_join(site_lut,by=c("ReleaseSite"="MRRSiteCode")) %>% 
  select(SpeciesID,
         SpRRT,
         ReleaseSite,
         Rel_RKM,
         StreamName, 
         SiteName=MRRSiteName,
         TribTo=TribToName,
         RKM=MRRSiteRKMMask,
         SiteType=MRRSiteType,
         SiteCode=MRRSiteHUC4Code,
         HUC8_Name) %>% 
  distinct() %>% 
  mutate(Species=SpeciesID,
    Run=str_sub(SpRRT,2,2),
         Rear=str_sub(SpRRT,3,3)) %>%  
  left_join(run_lut,by=c("Run"="Code")) %>% 
  left_join(sp_lut,by=c("Species"="Harvest_Code_Value")) %>% 
  select(Run=Value,Species=Name,Rear,SpRRT,StreamName,TribTo,ReleaseSite,SiteName,SiteType,RKM,SiteCode,HUC8_Name) %>% 
  filter(!str_detect(SiteType,"Intra-Dam"))

strata <- read_csv(file.path(data.dir,stratafile))


all_d %>% 
  left_join(strata %>% select(ReleaseSite,SpRRT,`ESU/DPS`, MPG, DIP, A_B),by=c("ReleaseSite","SpRRT")) %>% 
  filter(is.na(`ESU/DPS`))

d %>% colnames()
d %>% select(PITTag_ID,SpeciesID,SpRRT, ReleaseSite) %>% filter(SpeciesID!=0)
