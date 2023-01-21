source("code/packages.r")
library(patchwork)
library(lemon)
library(flextable)
source("code/tables.r")

analysis_yr <- 2019
analysis_sp <- 1

# Data summaries ####
rec_summry <- tags_FISHERY %>% 
  filter(GroupType=="ESU.DPS") %>% 
  group_by(Fishery,Year,Species) %>% 
  summarize(N=sum(n)) %>% 
  mutate(Species=fct_collapse(Species, Chinook="1",Coho="2",steelhead="3",Sockeye="4"),
         Fishery=factor(Fishery,levels=c("Sport", "Commercial", "Treaty")))

dam_summry <- tags_DAM %>% 
  select(PITTag_ID, ESU.DPS,SpeciesID,Year=PassageYear,Dam) %>% 
  distinct() %>% 
  filter(Dam=="BON", Year < (analysis_yr -1)) %>% 
  group_by(Year, Species=SpeciesID) %>% 
  count(name = "N") %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year),
         Fishery=factor(NA)) %>% 
  mutate(Species=fct_collapse(Species,Chinook="1",Coho="2",steelhead="3",Sockeye="4"))
 
  labs <- tibble(Species=unique(rec_summry$Species),
                 Lab=paste0(letters[1:4],") ", unique(rec_summry$Species)))

p <- rec_summry %>%
  left_join(labs) %>% 
  ggplot(aes(x=Year,y=N,shape=Fishery))+
   geom_line(lwd=0.1)+
  geom_point(fill="white",size=2.5)+
  scale_shape_manual(values=c(22,23,24))+
 lemon::facet_rep_wrap(~Species,ncol=1,scales="free_y")+
  geom_point(data=dam_summry,aes(x=Year, y=N/51,size="BON"),shape=16,col="black")+
  geom_line(data=dam_summry,aes(x=Year, y=N/51,lty="BON"),col="black",size=.7,inherit.aes = FALSE)+
  scale_linetype_manual(values=2)+
  scale_size_manual(values=2.5)+
  scale_y_continuous(name="Recoveries", 
                     limits=c(0,NA),
                     sec.axis = sec_axis(~.x*51,name = "Detections",
                                         labels=scales::comma_format()),
                     labels = scales::comma_format(accuracy = 1),
                     expand=expansion(c(0,.05)))+
  labs(lty="Dam",size="Dam")+
  scale_x_continuous(limits=c(2011,2019),breaks=seq(2011,analysis_yr,1),expand = c(0.01,0.01))+
  geom_hline(aes(yintercept=0))+
  guides(shape=guide_legend(order = 1))+
  #theme_bw(base_size = 12)+
  theme(legend.key = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.line.x.bottom = element_line()) +
  geom_text(aes(label=Lab), x=analysis_yr-1,y=Inf,hjust=0,vjust=1.5, check_overlap=TRUE,fontface='plain')+
  coord_capped_cart(bottom='both', left='both') 

p + 
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        strip.text = element_blank(),
        axis.line.x.bottom = element_line())

ggsave(dpi=300,filename="results/det_rec_plot.jpg")

# Model results ####

sport <- read_rds("results/sport_results.rds")%>%
mutate(nperiod=map_lgl(JagsDat, ~pluck(.x,"periods")>1)) %>%
mutate(samp_smry=case_when(nperiod ~ map_if(res, nperiod, ~gather_draws(.x, p_samp[period]) %>% median_qi %>% ungroup()),
                          !nperiod ~ map_if(res, !nperiod, ~gather_draws(.x, p_samp) %>% median_qi %>% mutate(period=1))))

# comm <- read_rds("results/comm_results.rds")%>% 
#   mutate(nperiod=map_lgl(JagsDat, ~pluck(.x,"periods")>1)) %>% 
#   mutate(samp_smry=case_when(nperiod ~ map_if(res, nperiod, ~gather_draws(.x, p_samp[period]) %>% median_qi %>% ungroup()),
#                              !nperiod ~ map_if(res, !nperiod, ~gather_draws(.x, p_samp) %>% median_qi %>% mutate(period=1))))

treaty <- read_rds("results/treaty_results.rds")%>% 
  mutate(nperiod=map_lgl(JagsDat, ~pluck(.x,"periods")>1)) %>% 
  mutate(samp_smry=case_when(nperiod ~ map_if(res, nperiod, ~gather_draws(.x, p_samp[period]) %>% median_qi %>% ungroup()),
                             !nperiod ~ map_if(res, !nperiod, ~gather_draws(.x, p_samp) %>% median_qi %>% mutate(period=1)))) 

treaty %>% 
  mutate(pBON=map(res,~gather_draws(.x, pB_det) %>% median_qi)) %>% 
  select(Year,Season,Species,Fishery,pBON) %>% 
  filter(Year==analysis_yr,Species==analysis_sp) %>% 
  unnest(pBON)


treaty %>%
  mutate(p_samp=map2(data, samp_smry, ~.x %>% left_join(.y, by=c("Period_id"="period")) %>%  select(Period,.variable,.value,.lower,.upper,.width)),
         labels=if_else(Season=="Summer", paste0("a) ",Season),paste0("b) ",Season)),
         labx=case_when(Season=="Summer"~26,
                        Season=="Fall"~31)) %>% 
  unnest(p_samp) %>% 
  filter(Year>=analysis_yr,Species==1) %>% 
  mutate(Species=case_when(Species==1~"Chinook",
                           Species==2~"Coho",
                           Species==3~"steelhead",
                           Species==4~"Sockeye")) %>% 
  arrange(match(Season,c("Summer","Fall"))) %>% 
  ggplot(aes(x=Period, y=.value))+
  facet_wrap(~factor(Season,levels=c("Summer","Fall")),ncol=2,scales="free")+
  geom_point(size=3)+
  geom_linerange(aes(ymin=.lower,ymax=.upper))+
  labs(x="Week", # CHANGE TO "MONTH" IF SPORT
       y="Sample rate")+
  geom_hline(aes(yintercept=.2),lty=2)+
  scale_linetype_manual(values=2)+
  scale_y_continuous(expand=expansion(c(0.01,.05)),labels=scales::percent_format(accuracy = 1),limits=c(0,.4)) +
  scale_x_continuous(breaks=seq(1,52,1))+
  theme_bw()+
  theme(legend.key = element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(color = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  guides(col=guide_legend(order = 1), shape=guide_legend(order=1))+
geom_text(aes(label=labels,x=labx), y=Inf, hjust=0, vjust=1.5, check_overlap=TRUE, fontface='plain', size=14/.pt)
  
  ggsave(filename = paste0("results/Treaty",analysis_yr,"_psamp.jpg"),dpi=300)

  
  treaty %>% 
    filter(Year==analysis_yr, Species==analysis_sp) %>% 
    select(Season,output_sum) %>% 
    unnest(output_sum) %>% 
    arrange(match(GroupType,c("ESU.DPS", "MPG", "DIP"))) %>% 
    mutate_at(vars(.value,.lower,.upper),~.x*100) %>% 
    select(Season,GroupType,Group,hr=.value,.lower,.upper) %>% 
    arrange(match(Season,c("Summer","Fall")),match(GroupType,c("ESU.DPS","MPG","DIP"))) %>% 
    mutate(CL=paste0(round(.lower,1)," - ",round(.upper,1))) %>% 
    rename(`Harvest rate (%)`=hr, 
           `Group type`=GroupType,
           `Reporting group`=Group,
           `Credible limits (95%)`=CL) %>% 
    select(-c(.lower,.upper)) %>% 
    flextable() %>% 
    merge_v(j=c("Season","Group type","Reporting group")) %>%
    fix_border_issues() %>% 
    flextable::colformat_double(j=4:5,digits=1) %>% 
    autofit() %>% 
    valign(j="Group type",valign='top',part='body') %>% 
    align(j="Credible limits (95%)",align = 'center',part='all') %>% 
  
  flextable::save_as_docx(path = paste0("results/HR_table_",analysis_yr,"_Species-",analysis_sp,".docx"))  
  
grp_ids <- treaty %>%
  filter(Year==analysis_yr,Species==analysis_sp) %>%
  select(Season,recs) %>% 
  unnest(recs) %>% 
  select(Season,Rear,GroupType,Group) %>% 
  group_by(Season) %>% 
  mutate(group_id=row_number())

treaty %>%
  filter(Year==analysis_yr,Species==analysis_sp) %>% 
  select(Season,res) %>% 
  mutate(draws=map(res,~gather_draws(.x,hr[group_id]))) %>% 
  unnest(draws) %>% 
  left_join(grp_ids, by=c("Season", "group_id")) %>% 
  group_by(Season,Group,GroupType) %>% 
  pivot_wider(names_from = Season, values_from = .value,values_fill = list(.value=0)) %>% 
  mutate(hr=Summer+Fall-(Summer*Fall))
  
treaty %>%
  filter(Year==analysis_yr,Species==analysis_sp) %>%  
  select(Season,output_sum) %>% 
  unnest(output_sum) %>% 
  arrange(match(GroupType,c("ESU.DPS","MPG","DIP"))) %>% 
  ggplot(aes(x=Group,y=.value))+
  geom_point()+
  geom_linerange(aes(ymin=.lower,ymax=.upper))+
  facet_grid(rows=vars(GroupType),cols=vars(Season))
