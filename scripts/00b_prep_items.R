# Read in and prepare data for scz prs analysis

library(foreign)
library(tidyverse)


q4 <- read.spss("./data/pheno/PDB2306_Q4_6months_v12.sav", to.data.frame = TRUE) # read in SPSS file with IDs for genetic and phenotypic (i.e. "the key")

q8 <- read.spss("./data/pheno/PDB2306_Q8yrs_v12.sav", to.data.frame = TRUE) # read in SPSS file with IDs for genetic and phenotypic (i.e. "the key")


 
 ############################################################
 
 # Q8 year pfactor scales
 
 q8sel <- q8 %>%
   select(PREG_ID_2306, BARN_NR,
          NN119, NN120, NN121, NN122, NN123, NN124, NN125, NN126, NN127, #Innattention
          NN128, NN129, NN130, NN131, NN132, NN133, NN134, NN135, NN136, #Hyperactivity
          NN68, NN69, NN70, NN71, NN72, NN73, NN74, NN75, NN76, NN77, NN78, NN79, NN80, #mfq
          NN145, NN146, NN147, NN148, NN149, #SCARED
          NN137, NN138, NN139, NN140, NN141, NN142, NN143, NN144, #SCQ-OD
          NN111, NN112, NN113, NN114, NN115, NN116, NN117, NN118) #SCQ-CD
 
 
 innat <- c('NN119', 'NN120', 'NN121', 'NN122', 'NN123', 'NN124', 'NN125', 'NN126', 'NN127')
 hyp <- c('NN128', 'NN129', 'NN130', 'NN131', 'NN132', 'NN133', 'NN134', 'NN135', 'NN136')
 mfq <- c('NN68', 'NN69', 'NN70', 'NN71', 'NN72', 'NN73', 'NN74', 'NN75', 'NN76', 'NN77', 'NN78', 'NN79', 'NN80')
 scared <- c('NN145', 'NN146', 'NN147', 'NN148', 'NN149')
 od <-  c( 'NN137', 'NN138', 'NN139', 'NN140', 'NN141', 'NN142', 'NN143', 'NN144')
 cd <- c('NN111', 'NN112', 'NN113', 'NN114', 'NN115', 'NN116', 'NN117', 'NN118')
 
 rvrsd <- c('NN152', 'NN153', 'NN154', 'NN155', 'NN156', 'NN157', 'NN159', 'NN160', 'NN161', 'NN162', 'NN163', 'NN164', 'NN165', 'NN167')  
 
 lkp <- data.frame(cbind(innat, seq(1:length(innat)))) %>%
   rename(item = innat) %>%
   bind_rows(data.frame(cbind(hyp, seq(1:length(hyp)))) %>%
               rename(item = hyp)) %>%
   bind_rows(data.frame(cbind(mfq, seq(1:length(mfq)))) %>%
               rename(item = mfq)) %>%
   bind_rows(data.frame(cbind(scared, seq(1:length(scared)))) %>%
               rename(item = scared)) %>%
   bind_rows(data.frame(cbind(od, seq(1:length(od)))) %>%
               rename(item = od)) %>%
   bind_rows(data.frame(cbind(cd, seq(1:length(cd))))%>%
               rename(item = cd)) 
 
 save(lkp, file= "./scratch_data/lookup_table_pfact_items.RData")
 
# Gather data to long format and recode responses to numeric, reversing certain itmes from the SCQ 
 
 q8sel_lng <- q8sel %>%
   filter(BARN_NR == 1) %>%
   gather(item,val,-PREG_ID_2306, -BARN_NR) %>%
   mutate(true_val = NA,
          sscale = NA,
          reversed = 'no') %>%
   mutate(true_val = ifelse(val %in% c("Never / seldom", "Yes", "Disagree","Never" ,"Is not or nearly not correct"), 0,true_val),
          true_val = ifelse(val %in% c("Sometimes", "Sometimes correct", "No","Seldom" ), 1,true_val),
          true_val = ifelse(val %in% c("Often", "Correct", "Often true" ), 2,true_val),
          true_val = ifelse(val %in% c("Very often" ), 3,true_val),
          sscale = ifelse(item %in% innat,"rsdbd_innat",sscale),
          sscale = ifelse(item %in% hyp,"rsdbd_hyp",sscale),
          sscale = ifelse(item %in% mfq,"mfq",sscale),
          sscale = ifelse(item %in% scared,"scared",sscale),
          sscale = ifelse(item %in% od,"rsdbd_od",sscale),
          sscale = ifelse(item %in% cd,"rsdbd_cd",sscale),
          reversed = ifelse(item %in% rvrsd,"yes",reversed))%>%
   mutate(true_val = ifelse(reversed == "yes", (true_val-1)*-1, true_val),
          true_val = ifelse(sscale == "rsdbd_cd" & val =="Sometimes", 2, true_val),
          true_val = ifelse(sscale == "rsdbd_cd" & val =="Often", 3, true_val)) %>%
   select(-reversed, -val) %>%
   distinct() 
 
 q8_scale <- q8sel_lng %>% 
   left_join(lkp) %>%
   unite(item2, sscale, V2) %>%
   select(-item) %>%
   spread(key = item2, value = true_val) %>%
   mutate_at(vars(-PREG_ID_2306, -BARN_NR), funs(ordered(.)))
   
   
 
 
 ############################################################
 ############################################################
 
save(q8_scale, file = './data/00_pfact_items.RData')
  
