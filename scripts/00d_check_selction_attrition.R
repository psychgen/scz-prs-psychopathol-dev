#install.packages("N:/data/durable/common/software/knitr_1.26.tar.gz", repos = NULL, type = "source")

library(tidyverse)


alldata <- read.table('./scratch_data/all_scales_prs.txt', header = T)

seldata <- alldata %>%
  select(PREG_ID_2306, BARN_NR, sex, IID, matches("growth"),mfq_8yr, rsdbd_cd_8yr,rsdbd_hyp_8yr,
         rsdbd_innat_8yr,rsdbd_od_8yr,scared_8yr) %>%
  mutate(label = case_when(!is.na(IID) ~ "Geno",
                           is.na(IID) ~"noGeno")) 

# Testing selection on key vars

ttests <- lapply(seldata[-c(1:4,17)], function(x) t.test(x ~ seldata$label))

selection <-transpose(ttests) %>%
  map(bind_rows) %>%
  map(t) %>%
  as.data.frame()%>%
  rownames_to_column() %>%
  select(variable= rowname,tstatistic = statistic, df = parameter, p.value, 
         mean_geno = estimate.1, mean_rest = estimate.2,lci = conf.int.1, uci = conf.int.2 ) %>%
  mutate(diff = mean_geno-mean_rest,
         mtc_sig= ifelse(p.value<0.05/12,1,0)) %>%
  mutate_at(vars(-variable), funs(round(.,3))) 

# Testing selective attrition

seldata_sa <- seldata %>%
  select(-label) %>%
  gather(key=variable, value=score, -c(PREG_ID_2306, BARN_NR, sex, IID)) %>%
  mutate(age=str_sub(variable, -3))%>%
  group_by(PREG_ID_2306,BARN_NR, age) %>%
  summarise(missing_by_age =sum(is.na(score)),
            n = n()) %>%
  mutate(hasno_data = ifelse(n-missing_by_age == 0, 1,0))
 head(seldata_sa)

 alldata_sa <- seldata_sa %>%
   left_join(alldata %>%
               select(PREG_ID_2306, BARN_NR, X0.05_resid, matches("18m"))) %>%
   drop_na(X0.05_resid)
 
 sa_18m <- alldata_sa %>%
   filter(age=="18m")
 sa_3yr <- alldata_sa %>%
   filter(age=="3yr")
 sa_5yr <- alldata_sa %>%
   filter(age=="5yr")
 sa_8yr <- alldata_sa %>%
   filter(age=="8yr")
 
sa <- list()
sa[["PRS_18m"]] <- t.test(sa_18m$X0.05_resid ~ sa_18m$hasno_data)
sa[["PRS_3yr"]] <- t.test(sa_3yr$X0.05_resid ~ sa_3yr$hasno_data)
sa[["PRS_5yr"]] <- t.test(sa_5yr$X0.05_resid ~ sa_5yr$hasno_data)
sa[["PRS_8yr"]] <- t.test(sa_8yr$X0.05_resid ~ sa_8yr$hasno_data)
#sa[["eas_soc_5yr"]] <- t.test(sa_5yr$eas_soc_18m ~ sa_5yr$hasno_data)
#sa[["eas_emo_5yr"]] <- t.test(sa_5yr$eas_emo_18m ~ sa_5yr$hasno_data)
#sa[["eas_act_5yr"]] <- t.test(sa_5yr$eas_act_18m ~ sa_5yr$hasno_data)
#sa[["eas_shy_5yr"]] <- t.test(sa_5yr$eas_shy_18m ~ sa_5yr$hasno_data)
sa[["cbclint18m_5yr"]] <- t.test(sa_5yr$cbcl_int_growth_18m ~ sa_5yr$hasno_data)
sa[["cbclext18m_5yr"]] <- t.test(sa_5yr$cbcl_ext_growth_18m ~ sa_5yr$hasno_data)


sa_t<-transpose(sa) %>%
  map(bind_rows) %>%
  map(t) %>%
  as.data.frame()%>%
  rownames_to_column() %>%
  select(variable= rowname,tstatistic = statistic, df = parameter, p.value, 
         mean_data = estimate.1,mean_nodata = estimate.2, lci = conf.int.1, uci = conf.int.2 ) %>%
  mutate(diff = mean_data-mean_nodata,
         mtc_sig= ifelse(p.value<0.05/6,1,0)) %>%
  mutate_at(vars(-variable), funs(round(.,3))) 

##Save out


save(selection, sa_t,file = "./output/selection_sa.RData")

write.table(selection, "./output/selection.txt", quote = F, row.names = F)
write.table(sa_t, "./output/selective_attrition.txt", quote = F, row.names = F)

