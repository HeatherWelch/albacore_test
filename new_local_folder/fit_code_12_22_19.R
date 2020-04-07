## fitting brts for gaps data from 12 hour 100nm gaps created in code_11_14_19.R
source("utilities/load_libraries.R")
source("utilities/data_exploration.R")

# ----------------------------------------------------> poisson gaps ---------------------------------------------------####
# all gaps ----
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/Gaps_data/extracto/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_xtra_poles.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% .[complete.cases(.),] %>% mutate(sum_gaps=log(sum_gaps+.0001))%>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% filter(sum_gaps>0)

#P1 8050 trees, writing out
gbm.x=c("sci","loitering_hours","dist_NTA","dist_eez","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
family="poisson"
lr=0.01
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_xtra_poles.rds")
write_rds(gap_brt_poiss_step,name)

# ----------------------------------------------------> bernoulli gaps ---------------------------------------------------####
# all gaps ----

### 4 playing around with tolerance, wrote out, 2000 trees; THIS IS THE BEST MODEL FROM THE 12.12.19 work ####
# na tolerance is fixed at .001 as opposed to .1
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/Gaps_data/extracto/gaps_classA_sat_fracD0.2_100nm_12hr_12_12_19.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]
master0=master %>% filter(sum_gaps==0)
master1=master %>% filter(sum_gaps>0) %>% mutate(sum_gaps=1)
master=rbind(master0,master1)

gbm.x=c("sci","loitering_hours","dist_NTA","dist_eez","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}.rds")
write_rds(gap_brt_poiss_step,name)

### 5 playing around with tolerance, wrote out, 2350 trees; THIS IS THE BEST MODEL FROM THE 12.12.19 work, this is the same as above but extra poles data for absences  ####
# na tolerance is fixed at .001 as opposed to .1
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/Gaps_data/extracto/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_xtra_poles.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]
master0=master %>% filter(sum_gaps==0)
master1=master %>% filter(sum_gaps>0) %>% mutate(sum_gaps=1)
master=rbind(master0,master1)

gbm.x=c("sci","loitering_hours","dist_NTA","dist_eez","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_xtra_poles.rds")
write_rds(gap_brt_poiss_step,name)

# all gaps, juridictional vs environmental predictors ----
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/Gaps_data/extracto/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_xtra_poles.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]
master0=master %>% filter(sum_gaps==0)
master1=master %>% filter(sum_gaps>0) %>% mutate(sum_gaps=1)
master=rbind(master0,master1)

### 6, jurisdicion only, writting out, 2950 trees
gbm.x=c("loitering_hours","dist_NTA","dist_eez","random")
family="bernoulli"
lr=0.01
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_xtra_poles_jurisdiction.rds")
write_rds(gap_brt_poiss_step,name)

### 7, envrionment only, writting out, 1900 trees
gbm.x=c("sci","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_xtra_poles_environment.rds")
write_rds(gap_brt_poiss_step,name)

### 8, envrionment, top 3
gbm.x=c("l.chl","sst","sla_sd","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_xtra_poles_environment_top3.rds")
write_rds(gap_brt_poiss_step,name)

# ----------------------------------------------------> bernoulli fishing activity ---------------------------------------------------####
# #no_sst ## failed all around, trying again with >2 >=2 FAILED
# master1=read.csv("/Volumes/SeaGate/IUU_GRW/Fishing_data/fishing_activity_modeling/fishing_activity_presAbs_564630_12_12_19.csv") %>% .[complete.cases(.),]
# master=master1%>% mutate(random=sample(1:nrow(master1)))
# m1=master %>% filter(sum_gaps==1) %>% .[sample(nrow(.),31474),]
# m0=master %>% filter(sum_gaps==0)%>% .[sample(nrow(.),31474),]
# master=rbind(m1,m0)
# gbm.x=c("dist_shore","dist_eez","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
# family="bernoulli"
# lr=0.001
# tc=5
# bf=0.6
# tolerance = "fixed"
# gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
# # name=glue("/Volumes/SeaGate/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_12_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_no_SST_fishing_activity.rds")
# # write_rds(gap_brt_poiss_step,name)

# 9 ####
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/fishing_activity_modeling/extracto/fishing_activity_presAbs_30000_12_22_19.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]

hrs=master$sum_gaps  # hours not fishing hours
master_r=master %>% dplyr::select(x,y) 
# master_r=master_r[,2:1]
test=rasterize(master_r,template,field=hrs)
plot(test,colNA="grey")

gbm.x=c("sci","loitering_hours","dist_NTA","dist_eez","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/fishing_activity_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}.rds")
write_rds(gap_brt_poiss_step,name)

# 10, 3800 trees, writing out ####
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/fishing_activity_modeling/extracto/fishing_activity_presAbs_15000_12_22_19.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]

hrs=master$sum_gaps  # hours not fishing hours
master_r=master %>% dplyr::select(x,y) 
# master_r=master_r[,2:1]
test=rasterize(master_r,template,field=hrs)
plot(test,colNA="grey")

gbm.x=c("sci","loitering_hours","dist_NTA","dist_eez","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/fishing_activity_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_15000.rds")
write_rds(gap_brt_poiss_step,name)

# 11, juridsictional predictors only 3100 trees, writing out ####
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/fishing_activity_modeling/extracto/fishing_activity_presAbs_15000_12_22_19.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]

gbm.x=c("loitering_hours","dist_NTA","dist_eez","random")
family="bernoulli"
lr=0.01
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/fishing_activity_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_15000_jurisdiction.rds")
write_rds(gap_brt_poiss_step,name)

# 12, environmental predictors only, 3900 treeswriting out ####
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/fishing_activity_modeling/extracto/fishing_activity_presAbs_15000_12_22_19.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]

gbm.x=c("sci","bathy","bathy_sd","l.chl","l.chl_temporal_sd","sst","sst_sd","sst_temporal_sd","l.eke_temporal_sd","l.eke","sla_sd","sla_temporal_sd","sla","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/fishing_activity_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_15000_environment.rds")
write_rds(gap_brt_poiss_step,name)

# 13, environmental predictors TOP 3 only, writing out ####
gaps=read.csv("/Users/heatherwelch/Dropbox/IUU_GRW/fishing_activity_modeling/extracto/fishing_activity_presAbs_15000_12_22_19.csv")
master=gaps %>% mutate(random=sample(1:nrow(gaps)))
master=master %>% mutate(sum_gaps=as.integer(round(sum_gaps))) %>% .[complete.cases(.),]

gbm.x=c("l.chl","sst","sst_temporal_sd","random")
family="bernoulli"
lr=0.1
tc=3
bf=0.6
tolerance = "fixed"
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,tolerance.method ="fixed") 
name=glue("/Users/heatherwelch/Dropbox/IUU_GRW/SDMs/brt/fishing_activity_classA_sat_fracD0.2_100nm_12hr_12_22_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_15000_environment_top3.rds")
write_rds(gap_brt_poiss_step,name)


