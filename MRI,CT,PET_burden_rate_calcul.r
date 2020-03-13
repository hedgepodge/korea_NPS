aa<-cut(nps_16_2_fin$RVD_SLF_BRDN_AMT, breaks=30)
aaa<-table(aa)
breaks<-c(seq(0,50000,by=5000),seq(100000,5000000,by=50000),seq(10000000,80000000,by=10000000))

nps_16_3_2_ct_burden<-merge(nps_16_2_fin,
                            distinct(nps_16_3_2_ct,SPEC_ID_SNO),
                            by="SPEC_ID_SNO",
                            all=FALSE)
nps_16_3_2_mri_burden<-merge(nps_16_2_fin,
                             distinct(nps_16_3_2_mri,SPEC_ID_SNO),
                             by="SPEC_ID_SNO",
                             all=FALSE)
nps_16_3_2_pet_burden<-merge(nps_16_2_fin,
                             distinct(nps_16_3_2_pet,SPEC_ID_SNO),
                             by="SPEC_ID_SNO",
                             all=FALSE)

cbind(table(cut(nps_16_2_fin$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))
cbind(table(cut(nps_16_3_2_ct_burden$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))
cbind(table(cut(nps_16_3_2_mri_burden$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))
cbind(table(cut(nps_16_3_2_pet_burden$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))

a<-nps_16_3_2_ct_burden[nps_16_3_2_ct_burden$RVD_SLF_BRDN_AMT<5000,]

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
      if(grep("-",vector[j]==1)){
        
      }
    }
    assign(paste0("code",i),
           a,
           envir = .GlobalEnv)
  }
}
grep("-",b[6])

b<-c("C","D0","D32","D33", "D37", "D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48", "I6", "I20", "I21", "I22", "I23", "I24", "I25")
kcd_list(b)

example <- filter(nps_16_2_fin,
                  (substr(MSICK_CD,1,1) %in% code1 |
                     substr(MSICK_CD,1,2) %in% code2 |
                     substr(MSICK_CD,1,3) %in% code3 |
                     substr(MSICK_CD,1,4) %in% code4 |
                     substr(MSICK_CD,1,5) %in% code5))

nps_16_3_2_ct_burden_kcd<-merge(example,
                            distinct(nps_16_3_2_ct,SPEC_ID_SNO),
                            by="SPEC_ID_SNO",
                            all=FALSE)
nps_16_3_2_mri_burden_kcd<-merge(example,
                             distinct(nps_16_3_2_mri,SPEC_ID_SNO),
                             by="SPEC_ID_SNO",
                             all=FALSE)
nps_16_3_2_pet_burden_kcd<-merge(example,
                             distinct(nps_16_3_2_pet,SPEC_ID_SNO),
                             by="SPEC_ID_SNO",
                             all=FALSE)

cbind(table(cut(nps_16_2_fin$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))
cbind(table(cut(nps_16_3_2_ct_burden_kcd$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))
cbind(table(cut(nps_16_3_2_mri_burden_kcd$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))
cbind(table(cut(nps_16_3_2_pet_burden_kcd$RVD_SLF_BRDN_AMT, breaks, right=FALSE)))

nps_16_2_3_ct<-semi_join(example,
                         nps_16_3_ct,
                         by="SPEC_ID_SNO")
nps_16_2_3_ct_count<-summarise(group_by(nps_16_2_3_ct,
                                              JID),
                                     SEX_min=min(SEX_TP_CD),
                                     AGE_min=min(age),
                                     Weight=min(SamplingWeight),
                                     cnt=n())
nps_16_2_3_ct_count$weight_cnt<-nps_16_2_3_ct_count$Weight*nps_16_2_3_ct_count$cnt
nps_16_2_3_ct_count$gage<-if_else(nps_16_2_3_ct_count$AGE_min==0,0,floor(nps_16_2_3_ct_count$AGE_min/5)*5+2)
nps_16_2_3_ct_pv<-dcast(nps_16_2_3_ct_count,
                        gage~SEX_min,
                        value.var="weight_cnt",
                        sum)
nps_16_2_3_mri<-semi_join(example,
                         nps_16_3_mri,
                         by="SPEC_ID_SNO")
nps_16_2_3_mri_count<-summarise(group_by(nps_16_2_3_mri,
                                        JID),
                               SEX_min=min(SEX_TP_CD),
                               AGE_min=min(age),
                               Weight=min(SamplingWeight),
                               cnt=n())
nps_16_2_3_mri_count$weight_cnt<-nps_16_2_3_mri_count$Weight*nps_16_2_3_mri_count$cnt
nps_16_2_3_mri_count$gage<-if_else(nps_16_2_3_mri_count$AGE_min==0,0,floor(nps_16_2_3_mri_count$AGE_min/5)*5+2)
nps_16_2_3_mri_pv<-dcast(nps_16_2_3_mri_count,
                        gage~SEX_min,
                        value.var="weight_cnt",
                        sum)
nps_16_2_3_pet<-semi_join(example,
                         nps_16_3_pet,
                         by="SPEC_ID_SNO")
nps_16_2_3_pet_count<-summarise(group_by(nps_16_2_3_pet,
                                        JID),
                               SEX_min=min(SEX_TP_CD),
                               AGE_min=min(age),
                               Weight=min(SamplingWeight),
                               cnt=n())
nps_16_2_3_pet_count$weight_cnt<-nps_16_2_3_pet_count$Weight*nps_16_2_3_pet_count$cnt
nps_16_2_3_pet_count$gage<-if_else(nps_16_2_3_pet_count$AGE_min==0,0,floor(nps_16_2_3_pet_count$AGE_min/5)*5+2)
nps_16_2_3_pet_pv<-dcast(nps_16_2_3_pet_count,
                        gage~SEX_min,
                        value.var="weight_cnt",
                        sum)
example_distinct<-summarise(group_by(example,
                                     JID),
                            SEX_min=min(SEX_TP_CD),
                            AGE_min=min(age),
                            Weight=min(SamplingWeight))

example_distinct$gage<-if_else(example_distinct$AGE_min==0,0,floor(example_distinct$AGE_min/5)*5+2)
nps_16_2_3_pv<-dcast(example_distinct,
                     gage~SEX_min,
                     value.var="Weight",
                     sum)

write.table(example,
            "/home/2016805/example.txt",
            sep="\t",
            row.names=F)
