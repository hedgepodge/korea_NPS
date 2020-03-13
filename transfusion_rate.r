########## NPS anesthesia, transfusion disease inpatient ##########

library(data.table)
library(dplyr)
library(reshape2)

import_nps("~/NPS-2014")
import_nps("~/NPS-2015")
import_nps("~/NPS-2016")
rm(nps_14_5)
rm(nps_15_5)
rm(nps_16_5)

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


# select transfusion codes from 300 table
nps_14_3_tr<-filter(select(nps_14_3,
                           SPEC_ID_SNO, DIV_CD, TOT_USE_QTY_EXEC_FQ),
                    (substr(DIV_CD,1,5) %in% c("X1001","X1002","X2011","X2012","X2021","X2022","X2031","X2032","X2131","X2132","X2041","X2042","X2051","X2052","X2061","X2062","X2141","X2142","X2071","X2072","X2081","X2082","X2091","X2092","X2101","X2102","X2111","X2112","X2121","X2122","X2515","X2501","X2502","X2504","X2516","X2511","X2512","X2513","X2514")))

nps_15_3_tr<-filter(select(nps_15_3,
                           SPEC_ID_SNO, DIV_CD, TOT_USE_QTY_OR_EXEC_FQ),
                    (substr(DIV_CD,1,5) %in% c("X1001","X1002","X2011","X2012","X2021","X2022","X2031","X2032","X2131","X2132","X2041","X2042","X2051","X2052","X2061","X2062","X2141","X2142","X2071","X2072","X2081","X2082","X2091","X2092","X2101","X2102","X2111","X2112","X2121","X2122","X2515","X2501","X2502","X2504","X2516","X2511","X2512","X2513","X2514")))

nps_16_3_tr<-filter(select(nps_16_3,
                           SPEC_ID_SNO, DIV_CD, TOT_USE_QTY_OR_EXEC_FQ),
                    (substr(DIV_CD,1,5) %in% c("X1001","X1002","X2011","X2012","X2021","X2022","X2031","X2032","X2131","X2132","X2041","X2042","X2051","X2052","X2061","X2062","X2141","X2142","X2071","X2072","X2081","X2082","X2091","X2092","X2101","X2102","X2111","X2112","X2121","X2122","X2515","X2501","X2502","X2504","X2516","X2511","X2512","X2513","X2514")))

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

# Select all matter disease codes from 200 table
no<-c("O","Q","U","Z","F1","F2","F3","F4","F5","F6","F7","F8","F9","F04","F05","F06","F07","F08","F09","N96","N97","N98","E66","R32","K60","K61","K62","I84","K00","K01","K02","K03","K04","K05","K06","K07","K08","N393","N394")
kcd_list(no)

for (i in 1:5) {
  assign(paste0("nocode",i),
         get(paste0("code",i)))
}

kcd_list(c("D5", "D60", "D61", "D62", "D63", "D64"))

nps_14_2_fin_tr <- nps_14_2_fin %>% filter(MSICK_CD != "" &
                                             (substr(MSICK_CD,1,1) %in% code1 |
                                                substr(MSICK_CD,1,2) %in% code2 |
                                                substr(MSICK_CD,1,3) %in% code3 |
                                                substr(MSICK_CD,1,4) %in% code4 |
                                                substr(MSICK_CD,1,5) %in% code5)&
                                             !(substr(MSICK_CD,1,1) %in% nocode1 |
                                                 substr(MSICK_CD,1,2) %in% nocode2 |
                                                 substr(MSICK_CD,1,3) %in% nocode3 |
                                                 substr(MSICK_CD,1,4) %in% nocode4 |
                                                 substr(MSICK_CD,1,5) %in% nocode5))

# select anemia from 400 table
nps_14_4_tr<-filter(select(nps_14_4,
                           SPEC_ID_SNO, SICK_SNO, SICK_CD),
                    (substr(SICK_CD,1,1) %in% code1 |
                       substr(SICK_CD,1,2) %in% code2 |
                       substr(SICK_CD,1,3) %in% code3 |
                       substr(SICK_CD,1,4) %in% code4 |
                       substr(SICK_CD,1,5) %in% code5))

nps_14_2_fin_tr_sub <- semi_join(nps_14_2_fin,
                                 nps_14_4_tr,
                                 by="SPEC_ID_SNO")

nps_14_2_fin_tr_3<-merge(nps_14_2_fin_tr,
                         nps_14_3_tr,
                         by="SPEC_ID_SNO")

nps_14_2_fin_tr_3$gage<-if_else(nps_14_2_fin_tr_3$PAT_AGE<1,0,floor(nps_14_2_fin_tr_3$PAT_AGE/5)*5+2)

nps_14_2_fin_tr_3_pivot<-dcast(nps_14_2_fin_tr_3,
                               gage~SEX_TP_CD,
                               value.var="SamplingWeight",
                               sum)

write.table(nps_14_2_fin_tr_3_pivot,
            "~/anesth,transfu/nps_14_2_fin_tr_3_pivot.txt",
            sep="\t",
            row.names=F)

nps_14_2_fin_tr_sub_3<-merge(nps_14_2_fin_tr_sub,
                             nps_14_3_tr,
                             by="SPEC_ID_SNO")

nps_14_2_fin_tr_sub_3$gage<-if_else(nps_14_2_fin_tr_sub_3$PAT_AGE<1,0,floor(nps_14_2_fin_tr_sub_3$PAT_AGE/5)*5+2)

nps_14_2_fin_tr_sub_3_pivot<-dcast(nps_14_2_fin_tr_sub_3,
                               gage~SEX_TP_CD,
                               value.var="SamplingWeight",
                               sum)

write.table(nps_14_2_fin_tr_sub_3_pivot,
            "~/anesth,transfu/nps_14_2_fin_tr_sub_3_pivot.txt",
            sep="\t",
            row.names=F)








nps_15_2_fin_tr <- nps_15_2_fin %>% filter(MSICK_CD != "" &
                                             (substr(MSICK_CD,1,1) %in% code1 |
                                                substr(MSICK_CD,1,2) %in% code2 |
                                                substr(MSICK_CD,1,3) %in% code3 |
                                                substr(MSICK_CD,1,4) %in% code4 |
                                                substr(MSICK_CD,1,5) %in% code5)&
                                             !(substr(MSICK_CD,1,1) %in% nocode1 |
                                                 substr(MSICK_CD,1,2) %in% nocode2 |
                                                 substr(MSICK_CD,1,3) %in% nocode3 |
                                                 substr(MSICK_CD,1,4) %in% nocode4 |
                                                 substr(MSICK_CD,1,5) %in% nocode5))

# select anemia from 400 table
nps_15_4_tr<-filter(select(nps_15_4,
                           SPEC_ID_SNO, SICK_SNO, SICK_CD),
                    (substr(SICK_CD,1,1) %in% code1 |
                       substr(SICK_CD,1,2) %in% code2 |
                       substr(SICK_CD,1,3) %in% code3 |
                       substr(SICK_CD,1,4) %in% code4 |
                       substr(SICK_CD,1,5) %in% code5))

nps_15_2_fin_tr_sub <- semi_join(nps_15_2_fin,
                                 nps_15_4_tr,
                                 by="SPEC_ID_SNO")

nps_15_2_fin_tr_3<-merge(nps_15_2_fin_tr,
                         nps_15_3_tr,
                         by="SPEC_ID_SNO")

nps_15_2_fin_tr_3$gage<-if_else(nps_15_2_fin_tr_3$PAT_AGE<1,0,floor(nps_15_2_fin_tr_3$PAT_AGE/5)*5+2)

nps_15_2_fin_tr_3_pivot<-dcast(nps_15_2_fin_tr_3,
                               gage~SEX_TP_CD,
                               value.var="SAMPLINGWEIGHT",
                               sum)

write.table(nps_15_2_fin_tr_3_pivot,
            "~/anesth,transfu/nps_15_2_fin_tr_3_pivot.txt",
            sep="\t",
            row.names=F)

nps_15_2_fin_tr_sub_3<-merge(nps_15_2_fin_tr_sub,
                             nps_15_3_tr,
                             by="SPEC_ID_SNO")

nps_15_2_fin_tr_sub_3$gage<-if_else(nps_15_2_fin_tr_sub_3$PAT_AGE<1,0,floor(nps_15_2_fin_tr_sub_3$PAT_AGE/5)*5+2)

nps_15_2_fin_tr_sub_3_pivot<-dcast(nps_15_2_fin_tr_sub_3,
                               gage~SEX_TP_CD,
                               value.var="SAMPLINGWEIGHT",
                               sum)

write.table(nps_15_2_fin_tr_sub_3_pivot,
            "~/anesth,transfu/nps_15_2_fin_tr_sub_3_pivot.txt",
            sep="\t",
            row.names=F)








nps_16_2_fin_tr <- nps_16_2_fin %>% filter(MSICK_CD != "" &
                                             (substr(MSICK_CD,1,1) %in% code1 |
                                                substr(MSICK_CD,1,2) %in% code2 |
                                                substr(MSICK_CD,1,3) %in% code3 |
                                                substr(MSICK_CD,1,4) %in% code4 |
                                                substr(MSICK_CD,1,5) %in% code5)&
                                             !(substr(MSICK_CD,1,1) %in% nocode1 |
                                                 substr(MSICK_CD,1,2) %in% nocode2 |
                                                 substr(MSICK_CD,1,3) %in% nocode3 |
                                                 substr(MSICK_CD,1,4) %in% nocode4 |
                                                 substr(MSICK_CD,1,5) %in% nocode5))

# select anemia from 400 table
nps_16_4_tr<-filter(select(nps_16_4,
                           SPEC_ID_SNO, SICK_SNO, SICK_CD),
                    (substr(SICK_CD,1,1) %in% code1 |
                       substr(SICK_CD,1,2) %in% code2 |
                       substr(SICK_CD,1,3) %in% code3 |
                       substr(SICK_CD,1,4) %in% code4 |
                       substr(SICK_CD,1,5) %in% code5))

nps_16_2_fin_tr_sub <- semi_join(nps_16_2_fin,
                                 nps_16_4_tr,
                                 by="SPEC_ID_SNO")

nps_16_2_fin_tr_3<-merge(nps_16_2_fin_tr,
                         nps_16_3_tr,
                         by="SPEC_ID_SNO")

nps_16_2_fin_tr_3$gage<-if_else(nps_16_2_fin_tr_3$PAT_AGE<1,0,floor(nps_16_2_fin_tr_3$PAT_AGE/5)*5+2)

nps_16_2_fin_tr_3_pivot<-dcast(nps_16_2_fin_tr_3,
                               gage~SEX_TP_CD,
                               value.var="SamplingWeight",
                               sum)

write.table(nps_16_2_fin_tr_3_pivot,
            "~/anesth,transfu/nps_16_2_fin_tr_3_pivot.txt",
            sep="\t",
            row.names=F)

nps_16_2_fin_tr_sub_3<-merge(nps_16_2_fin_tr_sub,
                             nps_16_3_tr,
                             by="SPEC_ID_SNO")

nps_16_2_fin_tr_sub_3$gage<-if_else(nps_16_2_fin_tr_sub_3$PAT_AGE<1,0,floor(nps_16_2_fin_tr_sub_3$PAT_AGE/5)*5+2)

nps_16_2_fin_tr_sub_3_pivot<-dcast(nps_16_2_fin_tr_sub_3,
                               gage~SEX_TP_CD,
                               value.var="SamplingWeight",
                               sum)

write.table(nps_16_2_fin_tr_sub_3_pivot,
            "~/anesth,transfu/nps_16_2_fin_tr_sub_3_pivot.txt",
            sep="\t",
            row.names=F)
