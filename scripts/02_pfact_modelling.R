# Read in prepped  data and scz scores
#install.packages("semPlot", repos = "file://tsd-evs/shared/R/cran")

## @knitr pf_model_setup

require('lavaan')
require('semPlot')
require('psych')
require('tidyverse')
require('foreign')


itemdat <- read.table('./scratch_data/all_items_prs.txt', header = T) 

dat <- itemdat %>% 
  mutate(sex = as.numeric(case_when(sex=="Male"~0,
                                    sex=="Female"~1)))

#  For each threshold:
  # 1. Run a scale-specific bifactor model
  # 1.1. Run PRS-on-resids models 
  # 1.2. Run PRS-on-scale-factors models 
  # 1.3. Run PRS-on-p-factor model 
  # 1.4. Run PRS-at-zero model

varnames <- dat %>%
  select(-IID, -PREG_ID_2306,-BARN_NR, -matches("resid|score|sex")) %>%
  names()

paste(varnames, collapse = " ~ sex ")

#############################################################################################
# BASE MODELS: see 02.1_specify_pfact_models.R
#############################################################################################  

source("./scripts/02.1_specify_pfact_models.R")


####################################################################################
## RUN MODELS IN LOOP ACROSS ALL P-VAL THRESHOLDS


# dataframes to hold results
basefits <- data.frame()
fitcomps <- data.frame()
ests1 <- data.frame()
ests2 <- data.frame()
ests3 <- data.frame()
rsq_vars <- data.frame()
rsq_factors <- data.frame()

xs <- c( "X0.001_resid", "X0.01_resid", "X0.05_resid", 
         "X0.1_resid", "X0.2_resid", "X0.5_resid", "X1_resid")

params <- expand.grid(xs, stringsAsFactors = FALSE)


# Test model

#dat[,"x"] <- dat[,params[1,1]]
## @knitr test_model 

#fit1 <- cfa(model1, data=dat, ordered = c(varnames),std.lv=TRUE)
#base_rsq <- as.data.frame(inspect(fit1, "r2")[1:52])

#semPaths(fit1.1, style = 'ram', intercepts = F)

#print(fit1.1)


## @knitr pf_run_models

source("./scripts/02.2_run_pfact_models.R")

forloop_pfm(params,dat)

save(basefits, fitcomps, ests1, ests2,ests3, rsq_factors, rsq_vars, file = './scratch_data/pfact_item_model_output_v3.RData')
 
## @knitr process_output

load('./scratch_data/pfact_item_model_output_v3.RData')

basefits <- basefits[1,] 

basefits2 <- basefits %>%
  select(npar, df,cfi, tli,rmsea, rmsea.ci.lower,rmsea.ci.upper,rmsea.pvalue ) 

ests_wide1 <- ests1 %>%
  mutate(score = rep(params$Var1,each=length(varnames))) %>%
  select(score, model, lhs, pvalue,est.std, ci.lower, ci.upper)  %>% 
  gather(param,val, -score, -model,-lhs) %>% 
  unite(sp_param,lhs,param) %>% 
  spread(sp_param,val) %>% 
  select(-model)
ests_wide2 <- ests2 %>%
  mutate(score = rep(params$Var1,each=6)) %>%
  select(score, model, lhs, pvalue,est.std, ci.lower, ci.upper)  %>% 
  gather(param,val, -score, -model,-lhs) %>% 
  unite(sp_param,lhs,param) %>% 
  spread(sp_param,val)%>% 
  select(-model)
ests_wide3 <- ests3 %>%
  mutate(score = rep(params$Var1,each=1)) %>%
  select(score, model, lhs, pvalue,est.std, ci.lower, ci.upper)  %>% 
  gather(param,val, -score, -model,-lhs) %>% 
  unite(sp_param,lhs,param) %>% 
  spread(sp_param,val)%>% 
  select(-model)
## @knitr model_fitting

fitcomps1 <- fitcomps %>%
  mutate(score = rep(params$Var1,each=6),
         specific_model = rep(c('prs_on_resids','prs_on_scalefs','prs_on_scalefs','prs_on_pfact','prs_on_pfact','no_prs_effect') ,dim(params)[[1]]),
         drop= case_when(specific_model !="prs_on_resids"&is.na(Df.diff) ~ 1)) %>% 
  filter(is.na(drop))
  
options(scipen=999)
fitcomps2 <- fitcomps1 %>%
  select(score,specific_model,Df,Df.diff,Chisq.diff,Chisq,Pr..Chisq.)

#Add in r-squared info
rsq_vars_procd <- rsq_vars %>% 
  rownames_to_column() %>% 
  mutate(score = rep(params$Var1,each=52),
         base_rsq = rep(base_rsq[,1],7)) %>%
  mutate_at(vars(matches("prs")), list(~.-base_rsq)) %>% 
  group_by(score) %>% 
  summarise(mnprs_on_resids =mean(prs_on_resids),
            mnprs_on_factors =mean(prs_on_factors),
            mnprs_on_pfact = mean(prs_on_pfact),
            mnno_prs_effect = mean(no_prs_effect),
            mxprs_on_resids =max(prs_on_resids),
            mxprs_on_factors =max(prs_on_factors),
            mxprs_on_pfact = max(prs_on_pfact),
            mxno_prs_effect = max(no_prs_effect))

#Add in r-squared info
rsq_factors_procd <- rsq_factors %>% 
  rownames_to_column() %>% 
  mutate(score = rep(params$Var1,each=7)) %>%
  filter(!str_detect(rowname,"pfact" )) %>% 
  group_by(score) %>% 
  summarise(prs_on_all_factors =mean(prs_on_all_factors))



fitcomps2sel <- fitcomps2[c(3,7,9,14,17,21,25),] ###Warning, hard-coded

write.table(fitcomps2, "./output/genetic_model_fitting_pfact_v3.txt", quote = F, row.names = F)

## @knitr model_fitting

bestfits_w_ests <- fitcomps2sel %>%
  group_by(score) %>%
  left_join(ests_wide3) %>% 
  left_join(ests_wide2) %>% 
  left_join(ests_wide1)

write.table(bestfits_w_ests, "./output/genetic_model_fitting_&_ests_pfact_v3.txt", quote = F, row.names = F)


ests1 <- ests1 %>%
  mutate(score = rep(params$Var1,each=length(varnames))) %>%
  select(score, model, lhs, pvalue,est.std, ci.lower, ci.upper) 
  
ests12 <- ests1 %>%  
  unite(new, score, model) %>%
  gather(key = parameter, value = val, -new, -lhs) %>%
  unite(final, lhs, parameter) %>%
  spread(key= final, value= val)


load( file= "./scratch_data/lookup_table_pfact_items.RData")


lkp2 <-read.csv('./tables/item_content_lookup.csv', header=F, sep=";") %>% 
  rename(item=V1, content=V2)
innat <- c('NN119', 'NN120', 'NN121', 'NN122', 'NN123', 'NN124', 'NN125', 'NN126', 'NN127')
hyp <- c('NN128', 'NN129', 'NN130', 'NN131', 'NN132', 'NN133', 'NN134', 'NN135', 'NN136')
mfq <- c('NN68', 'NN69', 'NN70', 'NN71', 'NN72', 'NN73', 'NN74', 'NN75', 'NN76', 'NN77', 'NN78', 'NN79', 'NN80')
scared <- c('NN145', 'NN146', 'NN147', 'NN148', 'NN149')
od <-  c( 'NN137', 'NN138', 'NN139', 'NN140', 'NN141', 'NN142', 'NN143', 'NN144')
cd <- c('NN111', 'NN112', 'NN113', 'NN114', 'NN115', 'NN116', 'NN117', 'NN118')
lkp_full <- lkp %>% 
  left_join(lkp2) %>% 
  mutate(scale = factor(case_when(item %in% innat ~"innat",
                           item %in% hyp ~"hyp",
                           item %in% mfq ~"mfq",
                           item %in% scared ~"scared",
                           item %in% od ~"od",
                           item %in% cd ~"cd"), levels=c("mfq","scared",
                                                          "cd","od",
                                                          "hyp", "innat"))) %>% 
  rename(itemno=V2) %>% 
  arrange(scale,itemno)

## @knitr output_plot_ests

ests1plt <- ests1 %>%
  mutate(lhs=str_replace(lhs,"rsdbd_","")) %>% 
  separate(lhs, c("scale","itemno")) %>% 
  select(scale, itemno, est.std,pvalue, ci.lower, ci.upper, score,model) %>%
  left_join(lkp_full) %>% 
  filter(score %in% c("X1_resid"),
         model == "prs_on_resids") %>% 
  droplevels() %>% 
  mutate(content=factor(content, levels=lkp_full$content))
  

png("figures/pfact_modelling_results_resids.png", res = 600,  unit = "in",
     height = 10, width =4, bg="transparent")
p1<-ggplot(ests1plt, aes(y=est.std, x=fct_rev(content), ymin = ci.lower, ymax = ci.upper, colour = log(pvalue) ) ) +
  geom_errorbar( size = 1,alpha=0.6,width = 0,position=position_dodge(0.3)) +
  geom_point(size=2,position=position_dodge(0.3)) +
  coord_flip()+
# facet_grid(.~score,scales="free")+
  scale_colour_distiller("p-value of\nestimate",palette = "YlOrRd", limits=c(-8,0), direction=-1,breaks=c(-0.69,-2.99,-5.29,-7.60), labels=c("0.5","0.05","0.005","0.0005"))+
  geom_hline( aes(yintercept=0), color = "grey50", linetype = 2) +
  theme(axis.text.x = element_text(angle= 90),
        axis.text.y = element_text(hjust= 0),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text =element_text(size = 12.5),
        panel.background = element_rect(fill = "transparent", colour = "grey80"),
        plot.background = element_rect(fill="transparent"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        strip.text = element_text(size=11),
        legend.title = element_text(size =11),
        legend.position="bottom",
        legend.direction= "horizontal") + 
  scale_y_continuous(name="Standardized beta coefficient from \nbest-fitting growth model") +
  guides(colour=guide_colourbar(barwidth=10,barheight=0.6, reverse=TRUE))
p1
dev.off()



