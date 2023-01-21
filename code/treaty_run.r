source("code/tables.r")

wts <- weights %>%
  filter(Fishery=="Treaty") 

tag_dat <- make_tagdat(ms_comm, tags_FISHERY, BONtags) %>% 
  filter(Fishery=="Treaty")

run_tab <- ms_comm %>% 
  filter(Fishery=="Treaty") %>% 
  left_join(catch_NONTICKET ,c("Fishery", "Year", "Species", "Period")) %>%
  mutate(NT=replace_na(NT,0)) %>%
  nest(ms=c(LW,MS,NT)) %>% 
  left_join(wts,by=c("Fishery","Year","Species","Season","Period")) %>% 
  unnest_wider(ms) %>% 
  arrange(Period) %>% 
  group_by(Fishery,Year,Season,Species) %>% 
  mutate(Period_id=row_number()) %>% 
  ungroup() %>% 
  nest(data=c(Period,Period_id,LW,MS,NT,wts)) %>% 
  add_tags(BONeff, tag_dat) %>% 
  filter(!map_lgl(recs, is.null)) %>% 
  add_comm_detbetas() %>%
  mutate(ms=map(data, ~pluck(.x) %>% select(Period,LW,MS,NT)),
         wts=map(data,~pluck(.x) %>% unnest(wts) %>% select(Period_id,Weight))) %>% 
  mutate(JagsDat=pmap(list(wts, ms, tag, detbetas,B_tags,M_tags,recs),
                      ~list(
                        logweight=log(..1$Weight),
                        period=..1$Period_id,
                        N_bio=nrow(..1),
                        periods=length(..2$LW),
                        T_lbs=..2$LW,
                        NT_catch=..2$NT,
                        samp=..2$MS,
                        samp_period=which(..2$MS>0),
                        non_samp_period=which(..2$MS==0),
                        samp_periods=length(which(..2$MS>0)),
                        non_samp_periods=length(which(..2$MS==0)),
                        det_beta=..4,
                        groups=nrow(..3),
                        tag=..3,
                        M_tags=..6,
                        B_tags=..5,
                       BON_tag=..7$n_BON
                      )))

out <- run_tab %>% 
  mutate(run=map(JagsDat,
                 safely(~run_mod(data=.x,
                                 model="models/Treaty_psamp.txt",
                                 monitor=c("hr", "mu_wt", "sigma_wt", "T_catch", "p_samp", "tot_catch_season", "pB_det"),
                                 method="parallel")))) %>% 
  mutate(res=map(run,"result"), 
         err=map(run,"error"))

# Check for errors in model results
  out %>% filter(map_lgl(res, is.null)) 

out <-  out %>% 
  mutate(ngroup=map_lgl(JagsDat, ~pluck(.x,"groups")>1)) %>% 
  mutate(smry=case_when(ngroup ~ map_if(res, ngroup, ~gather_draws(.x, hr[group]) %>% median_qi %>% ungroup()),
                       !ngroup ~ map_if(res, !ngroup, ~gather_draws(.x, hr) %>% median_qi %>% mutate(group=1))),
         output_sum=map2(smry, recs,  ~.x %>% bind_cols(.y %>% select(GroupType, Group))))

# SAVE THE RESULTS LIST COLUMN DATAFRAME AS AN RDS
# out %>% saveRDS(file="results/treaty_results.rds")

out %>% select(Season,Fishery,Year,Species,output_sum) %>% 
  unnest(output_sum) %>% 
  arrange(Year,Species) %>%
  filter(Species==1,GroupType=="DIP") %>% #,str_detect(Group,"UCR_Su")) %)  
  #mutate(Rear=str_sub(Group,-1,-1)) %>% 
  separate(Group, into=c("ESU","Run","Rear"),sep="_") %>% 
  filter(Rear %in% c("H","W"), Season=="Fall") %>% 
  ggplot(aes(x=Year,y=.value))+
  geom_point(aes(shape=Run,col=Rear))+
  geom_line(aes(col=Rear),show.legend=FALSE,lty=2)+
  geom_linerange(aes(ymin=.lower, ymax=.upper,col=Rear))+
  facet_wrap(~ESU,scales="free_y")+
  scale_x_continuous(breaks=seq(2010,2019,1))

         
                     