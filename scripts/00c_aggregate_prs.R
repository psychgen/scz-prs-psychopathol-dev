# Read in prepped  data 

# install.packages("FAMT", repos = "file://tsd-evs/shared/R/cran")

require('tidyverse')
require('psych')
require('foreign')

# Read in prepped scales - LH

load('./data/00_scales.RData')

cbcl565yr <- cbcl565yr %>%
  rename_at(vars(matches("18mo")), funs( gsub("18mo", "18m", .) ))

cbcl565yr2 <- cbcl565yr %>%
  group_by(PREG_ID_2306, BARN_NR) %>%
  gather(key=var, value = val, -PREG_ID_2306, -BARN_NR)%>%
  summarise(missing = sum(is.na(val))) %>%
  ungroup() %>%
  left_join(cbcl565yr) %>%
  mutate(has18mdata = ifelse(missing<11,1,NA))

pfact8_scales <- pfact8_scales %>%
  mutate(has8yrdata = 1) 


# Need to restrict to unrelated sample for child-only analysis

incl <- read.table('N:/data/durable/data/genetic/qcd_genetic_data/relatedness_exclusion_flag_list.txt', header= T) %>% 
  filter(children_only_analysis==0)

# Need file that matches ID numbers and PREG_IDs

IDS <- read.table('N:/data/durable/data/Linkage files/core_IDs&covars_hrv_njl_v2.txt', header= T) %>%
  filter(IID %in% incl$IID,
         Role=="Child") %>%
  select(IID, PREG_ID_2306)
  


# Read in scz PRS - we only need those individuals who have PRSs so can drop_na on this

scz_prs <- read.table('./scratch_data/prs_processed/offspring_scz.prs', header=T) %>% 
  select(IID, matches("resid"))


mbrn <- read.spss("N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_MBRN_541_v12.sav", to.data.frame = TRUE) %>%
  select(PREG_ID_2306,BARN_NR,sex = KJONN)# Get sex variable from mbrn


alldata <- mbrn %>%
  full_join(IDS) %>%
  full_join(cbcl565yr2) %>%
  full_join(pfact8_scales) %>%
  full_join(scz_prs) %>% 
  filter(sex %in% c("Male", "Female")) %>%
  droplevels()

describe(alldata)


#write.csv(round(cor(alldata[,-c(1:2)], use = "pairwise.complete.obs"),3), './scratch_data/all_corrs_raw.csv')


write.table(alldata, './scratch_data/all_scales_PRS.txt', quote=F,row.names = F)


###ITEM-LEVEL data + PRS

load('./data/00_pfact_items.RData')
allitemdata <- q8_scale


allitemdata2 <- allitemdata %>%
  left_join(mbrn) %>% 
  filter(PREG_ID_2306 %in% IDS$PREG_ID_2306)


itemdat <- allitemdata2 %>%
  left_join(IDS) %>%
  left_join(scz_prs) %>%
  filter(sex %in% c("Male", "Female")) %>%
  droplevels() 

write.table(itemdat, './scratch_data/all_items_PRS.txt', quote=F,row.names = F)

#Descriptives

dat1 <- alldata %>%
  drop_na(IID) %>% 
  select(matches("growth|rsdbd|mfq|scared"),sex) %>% 
  filter(sex %in%c("Female","Male")) %>% 
  select(-sex)

descs <- as.data.frame(describe(dat1)) %>% 
  rownames_to_column() %>% 
  select(var=rowname,n,mean,sd,min,max)

write.table(descs,"./output/descriptives.txt", row.names = FALSE, quote = FALSE)
