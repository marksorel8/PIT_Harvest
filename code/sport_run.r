source("code/tables.r")

 tag_dat <- make_tagdat(ms_sport, tags_FISHERY, BONtags)
 
 run_tab <-  ms_sport %>% 
  arrange(Period) %>% 
  group_by(Fishery,Year,Season,Species) %>% 
  mutate(Period_id=row_number()) %>% 
  ungroup() %>%
  nest(data=c(Period,Period_id, Catch, MS)) %>% 
  add_tags(BONeff, tag_dat) %>% 
  filter(!map_lgl(recs, is.null)) %>%
  add_sport_detbetas(Species) %>% 
  mutate(ms=map(data, ~pluck(.x) %>% select(Period, Catch, MS))) %>% 
  mutate(JagsDat=pmap(list(ms, tag, detbetas, B_tags, M_tags, recs),
                      ~list(
                        periods=nrow(..1),
                        catch=..1$Catch,
                        samp=..1$MS,
                        tag=..2,
                        groups=nrow(..2),
                        det_beta=..3,
                        B_tags=..4,
                        M_tags=..5,
                       BON_tag=..6$n_BON
                      ))) 
  
     
out <- run_tab %>% 
  mutate(run=map(JagsDat,
                 safely(~run_mod(data=.x, 
                                 model="models/Sport_psamp.txt", 
                                 monitor=c("hr","xBON_tag","p_samp","pB_det"))))) %>% 
  mutate(res=map(run,"result"), 
         err=map(run,"error"))

# look for failed runs
out %>% filter(!map_lgl(err, is.null)) #%>% pluck("res",1)


out <-  out %>% 
  mutate(ngroup=map_lgl(JagsDat, ~pluck(.x,"groups")>1)) %>% 
  mutate(smry=case_when(ngroup ~ map_if(res, ngroup, ~gather_draws(.x, hr[group]) %>% median_qi %>% ungroup()),
                       !ngroup ~ map_if(res, !ngroup, ~gather_draws(.x, hr) %>% median_qi %>% mutate(group=1))),
          output_sum=map2(smry, recs,  ~.x %>% bind_cols(.y %>% select(GroupType, Group)))) 

# SAVE THE RESULTS LIST COLUMN DATAFRAME AS AN RDS
 #out %>% saveRDS(file="results/sport_results.rds")

out %>% select(Season,Fishery,Year,Species,output_sum) %>% 
  unnest(output_sum) %>% 
  arrange(Year,Species) %>%
  filter(Species==1,GroupType=="ESU.DPS") %>% 
  ggplot(aes(x=Year,y=.value,col=Group))+
  geom_point()+
  geom_line(show.legend=FALSE,lty=2)+
  geom_linerange(aes(ymin=.lower, ymax=.upper))+
  facet_grid(rows=vars(Group),cols=vars(Season))+
  scale_x_continuous(breaks=seq(2010,2019,1))

