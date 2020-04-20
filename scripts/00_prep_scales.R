# Read in and prepare data for scz prs analysis

library(foreign)
library(tidyverse)


q4 <- read.spss("N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q4_6months_v12.sav", to.data.frame = TRUE) # read in SPSS file with IDs for genetic and phenotypic (i.e. "the key")

q5 <- read.spss("N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q5_18months_v12.sav", to.data.frame = TRUE) # read in SPSS file with IDs for genetic and phenotypic (i.e. "the key")

q6 <- read.spss("N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q6_3yrs_v12.sav", to.data.frame = TRUE) # read in SPSS file with IDs for genetic and phenotypic (i.e. "the key")

q5yrs <- read.spss("N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q5yrs_v12.sav", to.data.frame = TRUE) # read in SPSS file with IDs for genetic and phenotypic (i.e. "the key")

q8 <- read.spss("N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q8yrs_v12.sav", to.data.frame = TRUE) # read in SPSS file with IDs for genetic and phenotypic (i.e. "the key")


# CBCL 5/6/5yr

q5sel <- q5 %>%
  select(PREG_ID_2306, BARN_NR, EE908, EE438, EE439, EE909,EE963,
         EE435,EE903,EE961,EE446,EE447,EE962,EE442,EE448)

q6sel <- q6 %>%
  select(PREG_ID_2306, BARN_NR, GG321,GG335,GG317,GG328,GG336,GG318,GG323,GG334,GG337,
         GG314,GG315,GG330, GG332, GG316,GG319,GG320 ,GG324, GG326 ,GG329 ,GG331)

q5yrsel <- q5yrs %>%
  select(PREG_ID_2306,BARN_NR, LL309,LL305,LL504,LL317,LL315,LL505,LL321,LL322,LL310,LL320,LL323,
         LL303,LL302,LL324,LL319,LL304,LL307,LL308,LL311,LL313, LL316, LL318)


int <- c("EE908", "EE438", "EE439", "EE909", "EE963",
         "GG321", "GG335", "GG317", "GG328", "GG336","GG318", "GG323", "GG334", "GG337",
         "LL309", "LL305", "LL504", "LL317", "LL315","LL505", "LL321", "LL322", "LL310", "LL320", "LL323")

int_growth <- c("EE908", "EE438", "EE439", "EE909", "EE963",
         "GG321", "GG317", "GG328", "GG336", "GG323",
         "LL309", "LL305", "LL315", "LL321", "LL310" )

ext <- c("EE435", "EE903", "EE961", "EE446", "EE447", "EE962", "EE442","EE448",
         "GG314", "GG315", "GG330","GG332", "GG316", "GG319", "GG320", "GG324", "GG326", "GG329", "GG331",
         "LL303", "LL302", "LL324", "LL319", "LL304", "LL307", "LL308", "LL311","LL313", "LL316", "LL318")

ext_growth <- c("EE435", "EE903", "EE961", "EE446", "EE447", "EE962", "EE442","EE448",
         "GG314", "GG315", "GG332", "GG319", "GG324", "GG326", "GG329", "GG331",
         "LL303", "LL302", "LL319", "LL307", "LL311","LL313", "LL316", "LL318")

#Re-shape to long and recode items (and assign to subscales)

q565yr <- q5sel %>% 
  full_join(q6sel) %>%
  full_join(q5yrsel) %>%
  gather(item,val,-PREG_ID_2306, -BARN_NR) %>%
  mutate(true_val = ifelse(val %in% c("Not true","Rarely/never"),0,
                           ifelse(val %in% c("Somewhat or sometimes true","Sometimes"),1,
                                  ifelse(val %in% c("Very true or often true","Often/ typical"),2, NA))),
         age = ifelse(str_detect(item, "EE"),"18mo",
                      ifelse(str_detect(item, "GG"),"3yr",
                             ifelse(str_detect(item, "LL"),"5yr",NA))),
         sscale = ifelse(item %in% int_growth,"cbcl_int_growth",
                          ifelse(item %in% ext_growth,"cbcl_ext_growth",NA))) %>% 
  drop_na(sscale)

head(q565yr)

q565yr_item <- q565yr %>% 
  select(PREG_ID_2306,BARN_NR,item,true_val) %>% 
  spread(item, true_val)

#Calculate alphas

ordinal_alpha <- function(x){
  psych::alpha(psych::polychoric(x)$rho)
}

int18 <- int_growth[1:5]
int36 <- int_growth[6:10]
int60 <- int_growth[11:15]
ext18 <- ext_growth[1:8]
ext36 <- ext_growth[9:16]
ext60 <- ext_growth[17:24]
allsets <- list(int18,int36,int60,ext18,ext36,ext60)
cbclalphas <- data.frame()
for(i in allsets){
a <-ordinal_alpha(q565yr_item[,i])
cbclalphas<-rbind(cbclalphas,a$total$std.alpha)
}

cbclalphas <- cbclalphas %>%
  `colnames<-`("ord_alpha") %>% 
  mutate(pheno= rep(c("int","ext"),each=3),
         age = rep(c("18m","3yr","5yr"),2))

#Summarize to get components of scale score computation

q565yr_scale <- q565yr %>% 
  group_by( age, sscale) %>%
  summarize(items_scale = length(unique(item))) %>%
  right_join(q565yr) %>%
  group_by(PREG_ID_2306,BARN_NR,  age, sscale, items_scale) %>%
  summarize(items_present = sum(!is.na(val)),
            score = mean(true_val, na.rm=T)) %>%
  ungroup()

#Compute scale score

q565yr_scale <- q565yr_scale %>%
  mutate(sc_score = ifelse(items_present >= (items_scale/2), round(score*items_scale,0), NA))

#Back to wide

 cbcl565yr <- q565yr_scale %>% 
  unite(age_sscale,  sscale, age) %>%
  select(-c(items_scale, items_present, score)) %>%
  spread(key=age_sscale, value = sc_score) 
 cor(cbcl565yr[,-c(1:2)], use = "pairwise.complete.obs")
 
 
 head(cbcl565yr[,-1])
 
 ############################################################
 
 # Q8 year pfactor scales
 
 q8sel <- q8 %>%
   select(PREG_ID_2306,BARN_NR,
          NN119, NN120, NN121, NN122, NN123, NN124, NN125, NN126, NN127, #Innattention
          NN128, NN129, NN130, NN131, NN132, NN133, NN134, NN135, NN136, #Hyperactivity
          NN151, NN158, NN168, NN169, NN170, NN171, NN172, NN173, NN174, NN175, NN176, NN177, #Asd items where scoring is in correct direction
          NN178, NN179, NN180, NN181, NN182, NN183, NN184, NN185, NN186, NN187, NN188, NN189, #Asd items where scoring is in correct direction
          NN152, NN153, NN154, NN155, NN156, NN157, NN159, NN160, NN161, NN162, NN163, NN164, NN165, NN166, NN167, #Asd, REVERSE SCORED
          NN68, NN69, NN70, NN71, NN72, NN73, NN74, NN75, NN76, NN77, NN78, NN79, NN80, #mfq
          NN145, NN146, NN147, NN148, NN149, #SCARED
          NN137, NN138, NN139, NN140, NN141, NN142, NN143, NN144, #SCQ-OD
          NN111, NN112, NN113, NN114, NN115, NN116, NN117, NN118) #SCQ-CD
 
 
 innat <- c('NN119', 'NN120', 'NN121', 'NN122', 'NN123', 'NN124', 'NN125', 'NN126', 'NN127')
 hyp <- c('NN128', 'NN129', 'NN130', 'NN131', 'NN132', 'NN133', 'NN134', 'NN135', 'NN136')
 asd_soc <- c('NN151', 'NN153', 'NN158','NN159',
              'NN168' ,'NN169', 'NN170',
              'NN171', 'NN172', 'NN173', 'NN174', 'NN175', 'NN176', 'NN177', 'NN178', 'NN179', 'NN180',
              'NN181', 'NN182', 'NN183', 'NN185', 'NN186', 'NN187', 'NN188', 'NN189' )
 asd_rrb <- c('NN152', 'NN154', 'NN155', 'NN156',  'NN157',  'NN160', 'NN161', 'NN162', 'NN163', 'NN164', 'NN165', 'NN167')
 mfq <- c('NN68', 'NN69', 'NN70', 'NN71', 'NN72', 'NN73', 'NN74', 'NN75', 'NN76', 'NN77', 'NN78', 'NN79', 'NN80')
 scared <- c('NN145', 'NN146', 'NN147', 'NN148', 'NN149')
 od <-  c( 'NN137', 'NN138', 'NN139', 'NN140', 'NN141', 'NN142', 'NN143', 'NN144')
 cd <- c('NN111', 'NN112', 'NN113', 'NN114', 'NN115', 'NN116', 'NN117', 'NN118')
 
 rvrsd <- c('NN152', 'NN153', 'NN154', 'NN155', 'NN156', 'NN157', 'NN159', 'NN160', 'NN161', 'NN162', 'NN163', 'NN164', 'NN165', 'NN167')  
 
 
 # Gather data to long format and recode responses to numeric, reversing certain itmes from the SCQ 
 
 q8sel_lng <- q8sel %>%
   gather(item,val,-PREG_ID_2306,-BARN_NR) %>%
   mutate(true_val = NA,
          sscale = NA,
          reversed = 'no') %>%
   mutate(true_val = ifelse(val %in% c("Never / seldom", "Yes", "Disagree","Never" ,"Is not or nearly not correct"), 0,true_val),
          true_val = ifelse(val %in% c("Sometimes", "Sometimes correct", "No","Seldom" ), 1,true_val),
          true_val = ifelse(val %in% c("Often", "Correct", "Often true" ), 2,true_val),
          true_val = ifelse(val %in% c("Very often" ), 3,true_val),
          sscale = ifelse(item %in% innat,"rsdbd_innat",sscale),
          sscale = ifelse(item %in% hyp,"rsdbd_hyp",sscale),
          sscale = ifelse(item %in% asd_soc,"scq_soc",sscale),
          sscale = ifelse(item %in% asd_rrb,"scq_rrb",sscale),
          sscale = ifelse(item %in% mfq,"mfq",sscale),
          sscale = ifelse(item %in% scared,"scared",sscale),
          sscale = ifelse(item %in% od,"rsdbd_od",sscale),
          sscale = ifelse(item %in% cd,"rsdbd_cd",sscale),
          reversed = ifelse(item %in% rvrsd,"yes",reversed))%>%
   mutate(true_val = ifelse(reversed == "yes", (true_val-1)*-1, true_val),
          true_val = ifelse(sscale == "rsdbd_cd" & val =="Sometimes", 2, true_val),
          true_val = ifelse(sscale == "rsdbd_cd" & val =="Often", 3, true_val),
          age = "8yr")
 
 q8_item <- q8sel_lng %>% 
   select(PREG_ID_2306,BARN_NR,item,true_val) %>% 
   spread(item, true_val)
 
 #Calculate alphas
 
 
 allsets2 <- list(scared,mfq,hyp,innat,od,cd)
 pfactalphas <- data.frame()
 for(i in allsets2){
   a <-ordinal_alpha(q8_item[,i])
   pfactalphas<-rbind(pfactalphas,a$total$std.alpha)
 }
 
 pfactalphas <- pfactalphas %>%
   `colnames<-`("ord_alpha") %>% 
   mutate(pheno= c("anx","dep","hyp","inatt","od","cd"))
 
 save(cbclalphas,pfactalphas, file="./tables/alphas.RData")
 
 q8_scale <- q8sel_lng %>% 
   group_by( age, sscale) %>%
   summarize(items_scale = length(unique(item))) %>%
   right_join(q8sel_lng) %>%
   group_by(PREG_ID_2306, age, sscale, items_scale) %>%
   summarize(items_present = sum(!is.na(val)),
             score = mean(true_val, na.rm=T) ) %>%
   ungroup()
 
 
 q8_scale <- q8_scale %>%
   mutate(sc_score = ifelse(items_present >= (items_scale/2), round(score*items_scale,0), NA))
 
 a <- q8_scale %>%
   filter(sscale %in% 'mfq')
 table(a$sc_score)
 hist(a$sc_score)
 
 b <- q8_scale %>%
   filter(sscale %in% 'rsdbd_innat')
 hist(b$sc_score)
 table(b$sc_score)
 
 pfact8_scales <- q8_scale %>% 
   drop_na(age,sscale) %>%
   unite(age_sscale,  sscale,age) %>%
   select(-c(items_scale, items_present, score)) %>%
   spread(key=age_sscale, value = sc_score)  

 

 ############################################################
 ############################################################
 
save(cbcl565yr, eas565yr, pfact8_scales, file = './data/00_scales.RData')
  
