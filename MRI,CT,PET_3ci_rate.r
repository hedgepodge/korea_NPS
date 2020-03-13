#############################
########## CT rate ##########
#############################

nps_16_3_ct<-semi_join(nps_16_3,
                       subset(radio_code,(name=="CT")),
                       by="EDI")

# merge JID in 200 table with CT EDI codes in 300table
nps_16_3_2_ct<-merge(subset(nps_16_3_ct,
                            select=c("SPEC_ID_SNO", "DIV_CD", "TOT_USE_QTY_OR_EXEC_FQ")),
                     subset(nps_16_2_fin_3ci,
                            select=c("SPEC_ID_SNO", "DMD_TP_CD", "JID", "SamplingWeight", "SEX_TP_CD", "PAT_AGE", "MSICK_CD", "SSICK_CD", "FST_IPAT_DD", "RECU_FR_DD", "RECU_TO_DD", "YID")),
                     by="SPEC_ID_SNO",
                     all.x=TRUE)

# cut decimal points
nps_16_3_2_ct$TOT_USE_QTY_OR_EXEC_FQ_up<-nps_16_3_2_ct$TOT_USE_QTY_OR_EXEC_FQ%/%1

# count number of CT EDI codes by JID
nps_16_3_2_ct_check<-summarise(group_by(nps_16_3_2_ct,
                                        SPEC_ID_SNO),
                               x=sum(TOT_USE_QTY_OR_EXEC_FQ_up),
                               DMD_TP_CD=min(DMD_TP_CD),
                               JID=min(JID),
                               SamplingWeight=min(SamplingWeight),
                               SEX_TP_CD=min(SEX_TP_CD),
                               age=min(PAT_AGE),
                               MSICK_CD=min(MSICK_CD),
                               SSICK_CD=min(SSICK_CD),
                               FST_IPAT_DD=min(FST_IPAT_DD),
                               RECU_FR_DD=min(RECU_FR_DD),
                               RECU_TO_DD=min(RECU_TO_DD),
                               YID=min(YID)
                               )

# remove duplicated Bo Hoon hospitalization cases
nps_16_3_2_ct_merge<-summarise(group_by(nps_16_3_2_ct_check,
                                        DMD_TP_CD, JID, SamplingWeight, SEX_TP_CD, age, MSICK_CD, SSICK_CD, FST_IPAT_DD, RECU_FR_DD, RECU_TO_DD, YID),
                               x=max(x)
                               )

# check the removability of Bo Hoon hospitalization cases (see if the row numbers of above and below dataframes are the same)
check<-nps_16_3_2_ct_check %>% 
  distinct(DMD_TP_CD, JID, SamplingWeight, SEX_TP_CD, age, MSICK_CD, SSICK_CD, FST_IPAT_DD, RECU_FR_DD, RECU_TO_DD, YID)

nps_16_3_2_ct_merge$x<-apply(cbind(nps_16_3_2_ct_merge$x, max=2),
                             1,
                             min)

# make rate
nps_16_3_2_ct_merge$ct_weight<-nps_16_3_2_ct_merge$x * nps_16_3_2_ct_merge$SamplingWeight
nps_16_3_2_ct_merge$gage<-if_else(nps_16_3_2_ct_merge$age<1,0,floor(nps_16_3_2_ct_merge$age/5)*5+2)

nps_16_3_2_ct_pivot<-dcast(nps_16_3_2_ct_merge,
                                    gage~SEX_TP_CD,
                                    value.var="ct_weight",
                                    sum)

write.table(nps_16_3_2_ct_pivot,
            "/home/2016805/nps_16_3_2_ct_pivot.txt",
            sep="\t",
            row.names=F)





##############################
########## PET rate ##########
##############################

nps_16_3_pet<-semi_join(nps_16_3,
                        subset(radio_code,(name=="PET")),
                        by="EDI")

# merge JID in 200 table with PET EDI codes in 300table
nps_16_3_2_pet<-merge(subset(nps_16_3_pet,
                             select=c("SPEC_ID_SNO", "DIV_CD", "TOT_USE_QTY_OR_EXEC_FQ")),
                      subset(nps_16_2_fin_3ci,
                             select=c("SPEC_ID_SNO", "DMD_TP_CD", "JID", "SamplingWeight", "SEX_TP_CD", "PAT_AGE", "MSICK_CD", "SSICK_CD", "FST_IPAT_DD", "RECU_FR_DD", "RECU_TO_DD", "YID")),
                      by="SPEC_ID_SNO",
                      all.x=TRUE)

# cut decimal points
nps_16_3_2_pet$TOT_USE_QTY_OR_EXEC_FQ_up<-nps_16_3_2_pet$TOT_USE_QTY_OR_EXEC_FQ%/%1

# count number of PET EDI codes by JID
nps_16_3_2_pet_check<-summarise(group_by(nps_16_3_2_pet,
                                         SPEC_ID_SNO),
                                x=sum(TOT_USE_QTY_OR_EXEC_FQ_up),
                                DMD_TP_CD=min(DMD_TP_CD),
                                JID=min(JID),
                                SamplingWeight=min(SamplingWeight),
                                SEX_TP_CD=min(SEX_TP_CD),
                                age=min(PAT_AGE),
                                MSICK_CD=min(MSICK_CD),
                                SSICK_CD=min(SSICK_CD),
                                FST_IPAT_DD=min(FST_IPAT_DD),
                                RECU_FR_DD=min(RECU_FR_DD),
                                RECU_TO_DD=min(RECU_TO_DD),
                                YID=min(YID)
                                )

# remove duplicated Bo Hoon hospitalization cases
nps_16_3_2_pet_merge<-summarise(group_by(nps_16_3_2_pet_check,
                                         DMD_TP_CD, JID, SamplingWeight, SEX_TP_CD, age, MSICK_CD, SSICK_CD, FST_IPAT_DD, RECU_FR_DD, RECU_TO_DD, YID),
                                x=max(x)
                                )

# check the removability of Bo Hoon hospitalization cases (see if the row numbers of above and below dataframes are the same)
check<-nps_16_3_2_pet_check %>% 
  distinct(DMD_TP_CD, JID, SamplingWeight, SEX_TP_CD, age, MSICK_CD, SSICK_CD, FST_IPAT_DD, RECU_FR_DD, RECU_TO_DD, YID)

nps_16_3_2_pet_merge$x<-apply(cbind(nps_16_3_2_pet_merge$x, max=2),
                              1,
                              min)

# make rate
nps_16_3_2_pet_merge$pet_weight<-nps_16_3_2_pet_merge$x * nps_16_3_2_pet_merge$SamplingWeight
nps_16_3_2_pet_merge$gage<-if_else(nps_16_3_2_pet_merge$age<1,0,floor(nps_16_3_2_pet_merge$age/5)*5+2)

nps_16_3_2_pet_pivot<-dcast(nps_16_3_2_pet_merge,
                            gage~SEX_TP_CD,
                            value.var="pet_weight",
                            sum)

write.table(nps_16_3_2_pet_pivot,
            "/home/2016805/nps_16_3_2_pet_pivot.txt",
            sep="\t",
            row.names=F)





##############################
########## MRI rate ##########
##############################

nps_16_3_MRI<-semi_join(nps_16_3,
                        subset(radio_code,(name=="MRI")),
                        by="EDI")

# merge JID in 200 table with MRI EDI codes in 300table
nps_16_3_2_MRI<-merge(subset(nps_16_3_MRI,
                             select=c("SPEC_ID_SNO", "DIV_CD", "TOT_USE_QTY_OR_EXEC_FQ")),
                      subset(nps_16_2_fin_3ci,
                             select=c("SPEC_ID_SNO", "DMD_TP_CD", "JID", "SamplingWeight", "SEX_TP_CD", "PAT_AGE", "MSICK_CD", "SSICK_CD", "FST_IPAT_DD", "RECU_FR_DD", "RECU_TO_DD", "YID")),
                      by="SPEC_ID_SNO",
                      all.x=TRUE)

# cut decimal points
nps_16_3_2_MRI$TOT_USE_QTY_OR_EXEC_FQ_up<-nps_16_3_2_MRI$TOT_USE_QTY_OR_EXEC_FQ%/%1

# count number of MRI EDI codes by JID
nps_16_3_2_MRI_check<-summarise(group_by(nps_16_3_2_MRI,
                                         SPEC_ID_SNO),
                                x=sum(TOT_USE_QTY_OR_EXEC_FQ_up),
                                DMD_TP_CD=min(DMD_TP_CD),
                                JID=min(JID),
                                SamplingWeight=min(SamplingWeight),
                                SEX_TP_CD=min(SEX_TP_CD),
                                age=min(PAT_AGE),
                                MSICK_CD=min(MSICK_CD),
                                SSICK_CD=min(SSICK_CD),
                                FST_IPAT_DD=min(FST_IPAT_DD),
                                RECU_FR_DD=min(RECU_FR_DD),
                                RECU_TO_DD=min(RECU_TO_DD),
                                YID=min(YID)
                                )

# remove duplicated Bo Hoon hospitalization cases
nps_16_3_2_MRI_merge<-summarise(group_by(nps_16_3_2_MRI_check,
                                         DMD_TP_CD, JID, SamplingWeight, SEX_TP_CD, age, MSICK_CD, SSICK_CD, FST_IPAT_DD, RECU_FR_DD, RECU_TO_DD, YID),
                                x=max(x)
)

# check the removability of Bo Hoon hospitalization cases (see if the row numbers of above and below dataframes are the same)
check<-nps_16_3_2_MRI_check %>% 
  distinct(DMD_TP_CD, JID, SamplingWeight, SEX_TP_CD, age, MSICK_CD, SSICK_CD, FST_IPAT_DD, RECU_FR_DD, RECU_TO_DD, YID)

nps_16_3_2_MRI_merge$x<-apply(cbind(nps_16_3_2_MRI_merge$x, max=2),
                              1,
                              min)

# make rate
nps_16_3_2_MRI_merge$MRI_weight<-nps_16_3_2_MRI_merge$x * nps_16_3_2_MRI_merge$SamplingWeight
nps_16_3_2_MRI_merge$gage<-if_else(nps_16_3_2_MRI_merge$age<1,0,floor(nps_16_3_2_MRI_merge$age/5)*5+2)

nps_16_3_2_MRI_pivot<-dcast(nps_16_3_2_MRI_merge,
                            gage~SEX_TP_CD,
                            value.var="MRI_weight",
                            sum)

write.table(nps_16_3_2_MRI_pivot,
            "/home/2016805/nps_16_3_2_MRI_pivot.txt",
            sep="\t",
            row.names=F)
