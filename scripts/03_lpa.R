# Script to use MplusAutomation package to run batches of mplus
# models, pull and use output 
## @knitr prep_lpa
#install.packages("BiocManager", repos = "file://tsd-evs/shared/R/cran")
#install.packages("N:/data/durable/common/software/patchwork_1.0.0.tar.gz", repos = NULL, type ="source")
require('lavaan')
require('semPlot')
require('psych')
require('tidyverse')
require('MplusAutomation')
require('patchwork')

#Scales

alldata <- read.table('./scratch_data/all_scales_prs.txt', header = T) %>%
  drop_na(IID, has18mdata)
alldata_raw <- alldata %>%
  select(PREG_ID_2306,IID,sex,  matches("eas"), matches("growth"))

dat1 <- alldata %>%
  select(PREG_ID_2306,IID,matches("growth|rsdbd|mfq|scared|resid"),sex) %>% 
  filter(sex %in%c("Female","Male"))

describe(dat1)

dat1 <- dat1 %>%
  rename_at(vars(matches("cbcl_")), list(~str_replace(.,"cbcl_",""))) %>% 
  rename_at(vars(matches("growth_")), list(~str_replace(.,"growth_","")))%>%
  rename_at(vars(matches("rsdbd_")), list(~str_replace(.,"rsdbd_","")))  


prepareMplusData(dat1, "./data/scales_for_mplus.dat")


## @knitr run_lpa 

filepath1 <- "N:/data/durable/projects/scz_prs_general/scripts/mplus/lpa"
runModels(filepath1, logFile="allGMM.txt", recursive =T,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )
writeLines(readLines(paste(filepath1,"/allGMM.txt",sep="")))


 file.copy(from=paste0(filepath1),
           to="./output",
           overwrite = TRUE, recursive = TRUE,
           copy.mode = TRUE)

junk <- dir(path=filepath1, pattern=".out|.gh5") 
file.remove(paste0(filepath1,"/",junk))

## @knitr process_lpa 

mplusOutput <- readModels("./output/lpa", recursive=TRUE)
mySummaries <- readModels("./output/lpa", recursive=TRUE, what="summaries")

# condense results and summaries into one single df each

summaries <-
  lapply(mySummaries, function(x) {
    x$summaries
  }) %>%  bind_rows() %>% as.data.frame()


row.names(summaries) <- NULL

summaries <- summaries %>% 
  select(Model = Title, Parameters, LL, AIC,BIC,aBIC,AICC, Entropy,T11_VLMR_2xLLDiff,T11_VLMR_Mean,T11_VLMR_SD,T11_VLMR_PValue) %>% 
  mutate(Model = str_sub(Model,start=-2)) %>% 
  filter(Model %in% c("2c","3c","4c","5c","6c"))


summaries

write.table(summaries, "./output/lpa_model_fitting.txt", quote = F, row.names = F)


classcounts<- cbind(data.frame(mplusOutput$..output.lpa.lpa_4c.out$class_counts$mostLikely),
                    data.frame(mplusOutput$..output.lpa.lpa_4c.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4)
write.table(classcounts, "./output/lpa_classcounts.txt", quote = F, row.names = F)


## @knitr extract_ORs 
### ORS from 4class LPA at all thresholds

models <- c("4c_0001","4c_001","4c_01","4c_05","4c_10","4c_20","4c_50","4c")
res <- data.frame()
for(i in models){
  
  b<- readLines(paste0("./output/lpa/lpa_",i,".out"))
  
  c <- b[grep("X0_0001_|X0_001_|X0_01_|X0_05_|X0_1_|X0_2_|X0_5_|X1_",b)] %>% 
    as.data.frame() %>% 
    `colnames<-`(c("var")) %>%
    mutate(var = gsub("\\s+", " ", str_trim(var))) %>%
    separate(var, into= c("var", "Lowerp5%", "lci","Lower5%","est","Upper5%","uci","Upperp5%"), sep= " ") %>% 
    drop_na("Upperp5%") %>% 
    select( est, lci,uci) %>%
    mutate(model=i,
           comp = c(rep("drop",4), 
                    "Ref4_C1",
                    "Ref4_C2",
                    "Ref4_C3",
                    rep("drop",9), 
                    "Ref1_C2",
                    "Ref1_C3",
                    "Ref1_C4",
                    "Ref2_C1",
                    "Ref2_C3",
                    "Ref2_C4",
                    "Ref3_C1",
                    "Ref3_C2",
                    "Ref3_C4")) %>% 
    filter(!comp == "drop") 
  
  res <- bind_rows(res,c)
}

write.table(res, "./output/LPA_ORs.txt", row.names = F, quote = F)


## @knitr plot_lpa 

res_plt <- res %>% 
  filter(comp%in%c("Ref3_C1","Ref3_C2","Ref3_C4")) %>% 
  mutate(profile = case_when(comp=="Ref3_C1" ~ "Profile 1 v. 3",
                             comp=="Ref3_C2" ~ "Profile 2 v. 3",
                             comp=="Ref3_C4" ~ "Profile 4 v. 3")) %>% 
  select(-comp)%>% 
  mutate(model = str_replace(model, "4c_", "0.")) %>% 
  mutate(prs_thresh = str_replace(model, "4c", "1")) %>% 
  mutate_at(vars(est, lci,uci), as.numeric)


p1 <-ggplot(data=res_plt)+
  geom_errorbar(aes(x=prs_thresh,ymin=lci,ymax=uci, group=profile),colour= "steelblue4", alpha=0.2, size=1.2,width=0, position = position_dodge(0.4))+
  geom_point( aes(x=prs_thresh, y=est, group=profile),colour= "steelblue4",size=3,alpha=1, position = position_dodge(0.4))+
  facet_grid(profile~.)+
  geom_hline(yintercept = 1, linetype=2, colour="grey70", size=1)+
#  scale_colour_manual(labels=c("1","2","4"), values=c("purple","orange","#009E73"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = , l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = , l = 0)),
        axis.text = element_text(angle=0,size=13),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70",size=0.7),
        text =element_text(size = 16.5),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size =14, face="bold"),
        legend.key= element_rect(fill=NA),
        strip.background = element_blank(),
        strip.text.x = element_text(angle=0,colour="slateblue4", face="bold", size=14.5))+
  scale_x_discrete(name="PRS threshold (p < ...)")+
  scale_y_continuous(name="Odds ratio with 95% CIs \nof profile membership on PRS ", breaks=c(1.0,1.1,1.2,1.3,1.4,1.5))+ 
  coord_flip()+
  guides(colour= guide_legend(title="Symptom profile",override.aes = list(size=5), reverse=F))

p1

tiff("figures/lpaORs.tiff", res = 600, compression = "lzw", unit = "in",
     height = 7, width =7)

p1 
dev.off()

write.table(res, "./output/lpa_ORs.txt", quote = F, row.names = F)

###Traj plot all trajectories


source("./scripts/adapted_functions.R")
ext <- MyplotGrowthMixtures(mplusOutput, bw = FALSE, rawdata = FALSE,
                            estimated = TRUE, poly = FALSE, alpha_range = c(0, 0.1),
                            growth_variables = c("i1","s1"), time_scale = c(1.5,3,5), jitter_lines = NULL,
                            coefficients = "unstandardized") %>% 
  mutate(Pheno = "ext") %>% 
  filter(Title =="lpa_4c")
int <- MyplotGrowthMixtures(mplusOutput, bw = FALSE, rawdata = FALSE,
                            estimated = TRUE, poly = FALSE, alpha_range = c(0, 0.1),
                            growth_variables = c("i2","s2"), time_scale = c(1.5,3,5), jitter_lines = NULL,
                            coefficients = "unstandardized") %>% 
  mutate(Pheno = "int")%>% 
  filter(Title =="lpa_4c")

trajs <- ext %>% 
  bind_rows(int)%>%
  mutate_at(vars(Value,Lci,Uci), list(~case_when(Pheno=="ext" ~ ./16,
                                                 Pheno=="int" ~ ./10))) %>% 
  mutate(Pheno=recode(Pheno, "ext"="Behavioural","int"="Emotional"))  %>% 
  mutate(Pheno=factor(Pheno, levels=c("Behavioural","Emotional")))
mutate_at(vars(Value,Lci,Uci,Time), list(~as.numeric(.))) %>% 
  droplevels() 

profile_names <- list(
  '1'="Profile 1\n(5.7%)",
  '2'="Profile 2\n(7.9%)",
  '3'="Profile 3\n(84.7%)",
  '4'="Profile 4\n(1.8%)"
)
profile_labeller <- function(variable,value){
  return(profile_names[value])
}

p2 <- ggplot(data=trajs, aes(x=Time, y=Value, group=interaction(Class,Pheno)))+
  geom_point(aes( colour=Pheno),size=3.5,alpha=1)+
  geom_line( aes( colour=Pheno),size=1.8,alpha=1)+
  geom_ribbon(aes(ymin=Lci,ymax=Uci, fill= Pheno), alpha=0.4)+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  facet_grid(.~Class, labeller=profile_labeller)+
  theme(axis.title.y = element_text(margin = margin(t = -20, r = 30, b = 0, l = 0)),
        axis.title.x = element_blank(),
        text =element_text(size = 19.5),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=20),
        axis.text.x.top = element_text(angle= 0,size=12, margin = margin(t = 0, r = 0, b = , l = 0), hjust=0.5),
        panel.background = element_rect(fill = "grey95", colour = "white"),
        #      panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size =18, face="bold"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.placement = "outside",
        strip.text.x = element_text(angle=0,colour="slateblue4", face="bold", size=14.5))+
  coord_cartesian(xlim=c(1.2,5.3), ylim=c(-0.05,0.6))+
  scale_x_continuous(name="Age (yrs)", breaks=c(1,2,3,4,5), position = "top")+
  scale_y_continuous(name="Mean score on scale\n(scaled 0-1)", breaks=c(0.0,0.2,0.4,0.6))+
  guides(fill= FALSE,
         shape= FALSE, colour= guide_legend(title="Domain",override.aes = list(size=6))) 

p2

ests <- data.frame(mplusOutput$..output.lpa.lpa_4c.out$parameters$ci.unstandardized) %>% 
  filter(paramHeader %in% "Intercepts",
         param %in% c("HYP_8YR","INNAT_8YR","OD_8YR","CD_8YR","MFQ_8YR","SCARED_8YR"),
         LatentClass %in% c(1,2,3,4)) %>%
  mutate_at(vars(est,low2.5,up2.5), list(~case_when(param=="MFQ_8YR" ~ ./(2*13),
                                                    param=="SCARED_8YR" ~ ./(2*5),
                                                    param=="CD_8YR" ~ ./(3*8),
                                                    param=="OD_8YR" ~ ./(3*8),
                                                    param=="HYP_8YR" ~ ./(3*9),
                                                    param=="INNAT_8YR" ~ ./(3*9)))) %>% 
  mutate(var=factor(param, levels=c("MFQ_8YR",
                                    "SCARED_8YR",
                                    "CD_8YR",
                                    "OD_8YR",
                                    "HYP_8YR",
                                    "INNAT_8YR"),
                    labels = c("DEP","ANX","CD","ODD","HYP","INAT")),
         Class=LatentClass) %>% 
  mutate(Pheno=factor(ifelse(param %in% c("MFQ_8YR","SCARED_8YR"), "Emotional", "Behavioural")))%>% 
  mutate_at(vars(est,low2.5,up2.5), list(~as.numeric(.))) %>% 
  mutate(Class=factor(Class)) %>% 
  droplevels()



p3 <- ggplot()+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  geom_point(data=ests, aes(x=var, y=est, group=Class, colour=Pheno),size=3.5,alpha=1)+
  geom_errorbar(data=ests,aes(x=var,ymin=low2.5,ymax=up2.5, colour=Pheno), alpha=0.4, size=1.8,width=0)+
  scale_shape_manual("Domain",values=c("Behavioural"=16,"Emotional"=17,"8-year (see axis)"=15))+
  facet_grid(.~Class, labeller=profile_labeller)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 30, b =10 , l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size=16),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=20),
        axis.text.x = element_text(angle=90, size=12, vjust=0.5),
        text =element_text(size = 19.5),
        panel.background = element_rect(fill = "grey95", colour = "white"),
        #     panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        legend.position = "bottom",
        # legend.justification = "right",
        legend.direction = "horizontal",
        legend.title = element_text(size =18, face="bold"),
        legend.key= element_rect(fill=NA),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  coord_cartesian( ylim=c(-0.05,0.6))+
  scale_y_continuous(name="Mean score on scale\n(scaled 0-1)", breaks=c(0.0,0.2,0.4,0.6))+
  scale_x_discrete(name="", breaks=c("DEP","ANX","CD","ODD","HYP","INAT"), position="bottom")+
  guides(fill=FALSE,colour= guide_legend(title="Domain",override.aes = list(size=6), reverse=F)) 


p3

tiff("figures/lpatrajsALLCLASSES.tiff", res = 600, compression = "lzw", unit = "in",
     height = 4.5, width =12)

p2 
dev.off()

tiff("figures/lpa8yrALLCLASSES.tiff", res = 600, compression = "lzw", unit = "in",
     height = 4.5, width =12)

p3 
dev.off()

