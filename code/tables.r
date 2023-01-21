source("code/functions.r")

# connect to db
db_con <- connect_db()

#1. LUTs ####
pop_lut <- get_pop_lut()

#2. PIT data ####
#..  Dams ####
tags_DAM <- get_dam_tags() 

#.. Bon tags ####
BONtags <- make_bon_tags(tags_DAM, ESU.DPS, MPG, DIP, A_B) 

BONeff <- BON_EFF(tags_DAM)

#..  Fisheries ####
tags_FISHERY <- get_fishery_tags()
  
#..  CHECKS ####
# missing ESU/MPG/DIPs

# tags_DAM %>% check_missing_pops() %>% 
#     select(RelSite_SpRRT,Subbasin.Name,TribToName) %>% 
#     distinct() %>%  
#     print(n=Inf)

# tags_FISHERY %>% check_missing_pops()%>% 
#     select(RelSite_SpRRT,Subbasin.Name,TribToName) %>% 
#     distinct() %>%  
#     print(n=Inf)

# repeat_sthd <- tags_DAM %>% filter(SpeciesID==3, Dam=="BON") %>% group_by(PITTag_ID) %>% count() #%>% 
#   repeat_sthd %>% filter(n>1) %>% print(n=Inf)

# recovery/tagging species mismatch
 #tags_FISHERY %>% filter(!SpeciesID_rec == SpeciesID)


#3. Catch data ####

#..  Ticketed ####
  catch_TICKET <- get_ticketed_catch()

#..  Sport ####
  catch_SPORT <- get_sport_catch()

#..  Non-ticketed ####
  catch_NONTICKET <- get_nonticket_catch()

#..  Biodata ####
  weights <- get_wts()

#..  Mark samples ####
  mark_samples <- get_marksamples()

ms_sport <- catch_SPORT %>% join_marksamples(mark_samples)
ms_comm <- catch_TICKET %>% join_marksamples(mark_samples)

