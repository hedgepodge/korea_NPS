library(data.table)
library(dplyr)
library(reshape2)

import_nps("~/NPS-2014")
import_nps("~/NPS-2015")
import_nps("~/NPS-2016")
rm(nps_14_4, nps_14_5)
rm(nps_15_4, nps_15_5)
rm(nps_16_4, nps_16_5)

# remove patient having both sex types
nps_14_2_sex <- nps_14_2 %>%
  group_by(JID) %>%
  filter(any(max(SEX_TP_CD)-min(SEX_TP_CD)<1))

# remove patient having more than one age
nps_14_2_age <- nps_14_2 %>%
  group_by(JID) %>%
  filter(any(max(PAT_AGE)-min(PAT_AGE)<2))

# check removed data
nps_14_2_age_not <- nps_14_2 %>%
  group_by(JID) %>%
  filter(any(max(PAT_AGE)-min(PAT_AGE)>=2))

# merge datasets to have no errors
nps_14_2_fin<-inner_join(nps_14_2_sex, nps_14_2_age)



# remove patient having both sex types
nps_15_2_sex <- nps_15_2 %>%
  group_by(JID) %>%
  filter(any(max(SEX_TP_CD)-min(SEX_TP_CD)<1))

# remove patient having more than one age
nps_15_2_age <- nps_15_2 %>%
  group_by(JID) %>%
  filter(any(max(PAT_AGE)-min(PAT_AGE)<2))

# check removed data
nps_15_2_age_not <- nps_15_2 %>%
  group_by(JID) %>%
  filter(any(max(PAT_AGE)-min(PAT_AGE)>=2))

# merge datasets to have no errors
nps_15_2_fin<-inner_join(nps_15_2_sex, nps_15_2_age)



# remove patient having both sex types
nps_16_2_sex <- nps_16_2 %>%
  group_by(JID) %>%
  filter(any(max(SEX_TP_CD)-min(SEX_TP_CD)<1))

# remove patient having more than one age
nps_16_2_age <- nps_16_2 %>%
  group_by(JID) %>%
  filter(any(max(PAT_AGE)-min(PAT_AGE)<2))

# check removed data
nps_16_2_age_not <- nps_16_2 %>%
  group_by(JID) %>%
  filter(any(max(PAT_AGE)-min(PAT_AGE)>=2))

# merge datasets to have no errors
nps_16_2_fin<-inner_join(nps_16_2_sex, nps_16_2_age)


# select ICU codes from 300 table
nps_14_3_icu<-filter(select(nps_14_3,
                            SPEC_ID_SNO,
                            DIV_CD,
                            TOT_USE_QTY_EXEC_FQ),
                     (substr(DIV_CD,1,2)=="AJ" & substr(DIV_CD,3,3) %in% c(1,2,3,4,5) & !(substr(DIV_CD,6,6) %in% c("1","2"))))
                     

nps_15_3_icu<-filter(select(nps_15_3,
                            SPEC_ID_SNO,
                            DIV_CD,
                            TOT_USE_QTY_OR_EXEC_FQ),
                     (substr(DIV_CD,1,2)=="AJ" & substr(DIV_CD,3,3) %in% c(1,2,3,4,5) & !(substr(DIV_CD,6,6) %in% c("1","2"))))
                     

nps_16_3_icu<-filter(select(nps_16_3,
                            SPEC_ID_SNO,
                            DIV_CD,
                            TOT_USE_QTY_OR_EXEC_FQ),
                     (substr(DIV_CD,1,2)=="AJ" & substr(DIV_CD,3,3) %in% c(1,2,3,4,5) & !(substr(DIV_CD,6,6) %in% c("1","2"))))

rm(nps_14_2,nps_14_3,nps_14_2_age,nps_14_2_age_not,nps_14_2_sex)
rm(nps_15_2,nps_15_3,nps_15_2_age,nps_15_2_age_not,nps_15_2_sex)
rm(nps_16_2,nps_16_3,nps_16_2_age,nps_16_2_age_not,nps_16_2_sex)


kcd_list<-function(vector){
  for (i in 1:5){
    a<- c()
    assign(paste0("code",i),
           data.frame())
    for (j in 1:length(nchar(vector))){
      if(nchar(vector[j])==i) {
        a<- c(a,
              vector[j])
      }
    }
    assign(paste0("code",i),
           a,
           envir = .GlobalEnv)
  }
}




# select Particular Matter disease codes from 200 table
kcd_list(c("J00","J01","J02","J03","J04","J05","J06","J12","J13","J14","J15","J16","J17","J18","J20","J21","J30","J32","J399","J40","J41","J42","J45","J46","J6","J70","A481","B052","L20","H65","H66","H67","N14","R78","T56"))

nps_14_2_fin_pm <- nps_14_2_fin %>% filter(MSICK_CD != "" &
                                             (substr(MSICK_CD,1,1) %in% code1 |
                                                substr(MSICK_CD,1,2) %in% code2 |
                                                substr(MSICK_CD,1,3) %in% code3 |
                                                substr(MSICK_CD,1,4) %in% code4 |
                                                substr(MSICK_CD,1,5) %in% code5)
                                           & FOM_TP_CD %in% c(21,41,71,72,73,101,121)
                                           )

nps_14_2_fin_pm$gage<-ifelse(nps_14_2_fin_pm$PAT_AGE<1,0,floor(nps_14_2_fin_pm$PAT_AGE/5)*5+2)

# VST_DDCNT는 N박으로 입원하는 일자를 나타내는거니까, +1-3 안하고 -3만 해줌
nps_14_2_fin_pm$limit<-apply(cbind(apply(cbind(nps_14_2_fin_pm$VST_DDCNT-3,
                                               min=0),
                                         1,
                                         max),
                                   max=60),
                             1,
                             min)

nps_14_2_fin_pm$limitweight<-nps_14_2_fin_pm$limit * nps_14_2_fin_pm$SamplingWeight

nps_14_2_fin_pm_pv<-dcast(nps_14_2_fin_pm,
                          gage~SEX_TP_CD,
                          value.var="limitweight",
                          sum)
write.table(nps_14_2_fin_pm_pv,
            "/home/2016805/icu/nps_14_2_fin_pm_pv.txt",
            sep="\t",
            row.names=F)
write.table(nps_14_2_fin_pm,
            "/home/2016805/icu/nps_14_2_fin_pm.txt",
            sep="\t",
            row.names=F)

# round up decimal of ICU day counts
nps_14_3_icu_sum<-summarise(group_by(nps_14_3_icu,
                                     SPEC_ID_SNO),
                            sum=sum(ceiling(TOT_USE_QTY_EXEC_FQ))
                            )

# merge 200 table and 300 table with above results
nps_14_2_3_icu_pm<-merge(nps_14_2_fin_pm,
                         nps_14_3_icu_sum,
                         by="SPEC_ID_SNO")

nps_14_2_3_icu_pm$limit<-apply(cbind(apply(cbind(nps_14_2_3_icu_pm$sum+1-3,
                                                 min=0),
                                           1,
                                           max),
                                     max=60),
                               1,
                               min)

nps_14_2_3_icu_pm$limitweight<-nps_14_2_3_icu_pm$limit * nps_14_2_3_icu_pm$SamplingWeight

nps_14_2_3_icu_pm_pv<-dcast(nps_14_2_3_icu_pm,
                            gage~SEX_TP_CD,
                            value.var="limitweight",
                            sum)
write.table(nps_14_2_3_icu_pm_pv,
            "/home/2016805/icu/nps_14_2_3_icu_pm_pv.txt",
            sep="\t",
            row.names=F)
write.table(nps_14_2_3_icu_pm,
            "/home/2016805/icu/nps_14_2_3_icu_pm.txt",
            sep="\t",
            row.names=F)







nps_15_2_fin_pm <- nps_15_2_fin %>% filter(MSICK_CD != "" &
                                             (substr(MSICK_CD,1,1) %in% code1 |
                                                substr(MSICK_CD,1,2) %in% code2 |
                                                substr(MSICK_CD,1,3) %in% code3 |
                                                substr(MSICK_CD,1,4) %in% code4 |
                                                substr(MSICK_CD,1,5) %in% code5)
                                           & FOM_TP_CD %in% c(21,41,71,72,73,101,121)
                                           )

nps_15_2_fin_pm$gage<-ifelse(nps_15_2_fin_pm$PAT_AGE<1,0,floor(nps_15_2_fin_pm$PAT_AGE/5)*5+2)

nps_15_2_fin_pm$limit<-apply(cbind(apply(cbind(nps_15_2_fin_pm$VST_DDCNT-3,
                                               min=0),
                                         1,
                                         max),
                                   max=60),
                             1,
                             min)

nps_15_2_fin_pm$limitweight<-nps_15_2_fin_pm$limit * nps_15_2_fin_pm$SAMPLINGWEIGHT

nps_15_2_fin_pm_pv<-dcast(nps_15_2_fin_pm,
                          gage~SEX_TP_CD,
                          value.var="limitweight",
                          sum)
write.table(nps_15_2_fin_pm_pv,
            "/home/2016805/icu/nps_15_2_fin_pm_pv.txt",
            sep="\t",
            row.names=F)
write.table(nps_15_2_fin_pm,
            "/home/2016805/icu/nps_15_2_fin_pm.txt",
            sep="\t",
            row.names=F)

# round up decimal of ICU day counts
nps_15_3_icu_sum<-summarise(group_by(nps_15_3_icu,
                                     SPEC_ID_SNO),
                            sum=sum(ceiling(TOT_USE_QTY_OR_EXEC_FQ))
                            )

# merge 200 table and 300 table with above results
nps_15_2_3_icu_pm<-merge(nps_15_2_fin_pm,
                         nps_15_3_icu_sum,
                         by="SPEC_ID_SNO")

nps_15_2_3_icu_pm$limit<-apply(cbind(apply(cbind(nps_15_2_3_icu_pm$sum+1-3,
                                                 min=0),
                                           1,
                                           max),
                                     max=60),
                               1,
                               min)

nps_15_2_3_icu_pm$limitweight<-nps_15_2_3_icu_pm$limit * nps_15_2_3_icu_pm$SAMPLINGWEIGHT

nps_15_2_3_icu_pm_pv<-dcast(nps_15_2_3_icu_pm,
                            gage~SEX_TP_CD,
                            value.var="limitweight",
                            sum)
write.table(nps_15_2_3_icu_pm_pv,
            "/home/2016805/icu/nps_15_2_3_icu_pm_pv.txt",
            sep="\t",
            row.names=F)
write.table(nps_15_2_3_icu_pm,
            "/home/2016805/icu/nps_15_2_3_icu_pm.txt",
            sep="\t",
            row.names=F)







nps_16_2_fin_pm <- nps_16_2_fin %>% filter(MSICK_CD != "" &
                                             (substr(MSICK_CD,1,1) %in% code1 |
                                                substr(MSICK_CD,1,2) %in% code2 |
                                                substr(MSICK_CD,1,3) %in% code3 |
                                                substr(MSICK_CD,1,4) %in% code4 |
                                                substr(MSICK_CD,1,5) %in% code5)
                                           & FOM_TP_CD %in% c(21,41,71,72,73,101,121)
                                           )

nps_16_2_fin_pm$gage<-ifelse(nps_16_2_fin_pm$PAT_AGE<1,0,floor(nps_16_2_fin_pm$PAT_AGE/5)*5+2)

nps_16_2_fin_pm$limit<-apply(cbind(apply(cbind(nps_16_2_fin_pm$VST_DDCNT-3,
                                               min=0),
                                         1,
                                         max),
                                   max=60),
                             1,
                             min)

nps_16_2_fin_pm$limitweight<-nps_16_2_fin_pm$limit * nps_16_2_fin_pm$SamplingWeight

nps_16_2_fin_pm_pv<-dcast(nps_16_2_fin_pm,
                          gage~SEX_TP_CD,
                          value.var="limitweight",
                          sum)
write.table(nps_16_2_fin_pm_pv,
            "/home/2016805/icu/nps_16_2_fin_pm_pv.txt",
            sep="\t",
            row.names=F)
write.table(nps_16_2_fin_pm,
            "/home/2016805/icu/nps_16_2_fin_pm.txt",
            sep="\t",
            row.names=F)

# round up decimal of ICU day counts
nps_16_3_icu_sum<-summarise(group_by(nps_16_3_icu,
                                     SPEC_ID_SNO),
                            sum=sum(ceiling(TOT_USE_QTY_OR_EXEC_FQ))
                            )

# merge 200 table and 300 table with above results
nps_16_2_3_icu_pm<-merge(nps_16_2_fin_pm,
                         nps_16_3_icu_sum,
                         by="SPEC_ID_SNO")

nps_16_2_3_icu_pm$limit<-apply(cbind(apply(cbind(nps_16_2_3_icu_pm$sum+1-3,
                                                 min=0),
                                           1,
                                           max),
                                     max=60),
                               1,
                               min)

nps_16_2_3_icu_pm$limitweight<-nps_16_2_3_icu_pm$limit * nps_16_2_3_icu_pm$SamplingWeight

nps_16_2_3_icu_pm_pv<-dcast(nps_16_2_3_icu_pm,
                            gage~SEX_TP_CD,
                            value.var="limitweight",
                            sum)
write.table(nps_16_2_3_icu_pm_pv,
            "/home/2016805/icu/nps_16_2_3_icu_pm_pv.txt",
            sep="\t",
            row.names=F)
write.table(nps_16_2_3_icu_pm,
            "/home/2016805/icu/nps_16_2_3_icu_pm.txt",
            sep="\t",
            row.names=F)
