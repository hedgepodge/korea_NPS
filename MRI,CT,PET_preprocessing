require(data.table)
require(dplyr)
require(reshape2)

radio_code<-read.csv("/home/2016805/hwsb_ct,mri,pet/radio_code.csv", header=TRUE)
colnames(radio_code)[2]<-"name"
radio_code$EDI<-as.character(radio_code$EDI)

DIV_CD_RVS<-read.csv("/home/2016805/hwsb_ct,mri,pet/DIV_CD_RVS_all.csv", header=TRUE, fileEncoding="CP949", encoding="UTF-8")
DIV_CD<-subset(DIV_CD_RVS, select=c(div_cd, name_kor, or_yn, danga_byungwon, janggoo))

colnames(DIV_CD)<-c("DIV_CD", "name_kor", "or_yn", "danga_byungwon", "janggoo")

import_nps("~/NPS-2016")

# streamline 300 table
nps_16_3<-nps_16_3[,c(1,5,11)]
nps_16_3$EDI<-substr(nps_16_3$DIV_CD,1,5)

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

# 200 table for accident rate
no<-c("O","Q","U","Z","F1","F2","F3","F4","F5","F6","F7","F8","F9","F04","F05","F06","F07","F08","F09","N96","N97","N98","E66","R32","K60","K61","K62","I84","K00","K01","K02","K03","K04","K05","K06","K07","K08","N393","N394")
kcd_list(no)

for (i in 1:5) {
  assign(paste0("nocode",i),
         get(paste0("code",i)))
}

kcd_list(c("C","D45","D46","D471","D473","D474","D475","D0","D37","D38","D39","D40","D41","D42","D43","D44","D470","D472","D477","D479","D48","I6","I20","I21","I22","I23","I24","I25"))

nps_16_2_fin_3ci <- nps_16_2_fin %>% filter(MSICK_CD != "" &
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

# 200 table for distinct JID, respectively
nps_16_2_fin_3ci_distinct<- nps_16_2_fin_3ci %>% distinct(JID, .keep_all=TRUE)
