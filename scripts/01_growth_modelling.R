# Read in prepped cbcl data and scz scores

## @knitr prep_models 

#install.packages("tidyimpute", repos = "file://tsd-evs/shared/R/cran")
require('lavaan')
require('semPlot')
require('psych')
require('tidyverse')


alldata <- read.table('./scratch_data/all_scales_prs.txt', header = T) %>%
  drop_na(IID, has18mdata) %>%
  select(PREG_ID_2306,IID,sex,  matches("growth"), matches("resid"))


dat <- alldata 

#dat <- simdata


source('./scripts/01.1_specify_growth_models.R')

xs <- c( "X0.001_resid", "X0.01_resid", "X0.05_resid", 
        "X0.1_resid", "X0.2_resid", "X0.5_resid", "X1_resid")
ys <- c( "cbcl_ext_growth", "cbcl_int_growth")


params <- expand.grid(xs,ys, stringsAsFactors = FALSE)

basefits <- data.frame()
ests2 <- data.frame()
ests3 <- data.frame()
ests4 <- data.frame()
ests5 <- data.frame()
ests6 <- data.frame()
fitcomps <- data.frame()
rsq_vars <- data.frame()

# Test model

dat[,"x"] <- dat[,params[1,1]]
dat[,"ytime1"] <- dat[,paste0(params[1,2],"_18m")]
dat[,"ytime2"] <- dat[,paste0(params[1,2],"_3yr")]
dat[,"ytime3"] <- dat[,paste0(params[1,2],"_5yr")]

## @knitr test_model 

fit1 <- growth(model1, data=dat, missing="fiml.x")

semPaths(fit1, style = 'ram', intercepts = F)

print(fit1)


## @knitr run_loop 

# Loop over combinations to run models for each, saving results as additional rows in the dataframe

source("./scripts/01.2_run_growth_models.R")

forloop_gm(params,dat)

## @knitr process_output

basefits <- basefits %>%
  mutate(score = params$Var1,
         scale = params$Var2)

ests <- ests2 %>%
  mutate(score = rep(params$Var1,each=3),
         scale = rep(params$Var2,each=3),
         model= "prs_on_resid") %>%
  rbind(ests3 %>%  mutate(score = rep(params$Var1,each=2),
                          scale = rep(params$Var2,each=2),
                          model= "prs_on_growth")) %>%
  rbind(ests4 %>%  mutate(score = rep(params$Var1,each=2),
                          scale = rep(params$Var2,each=2),
                          model= "prs_on_incpt")) %>%
  rbind(ests5 %>%  mutate(score = rep(params$Var1,each=2),
                          scale = rep(params$Var2,each=2),
                          model= "prs_on_slope")) %>%
  rbind(ests6 %>%  mutate(score = rep(params$Var1,each=2),
                          scale = rep(params$Var2,each=2),
                          model= "prs_no_effect"))

fitcomps <- fitcomps %>%
  mutate(score = rep(params$Var1,each=7),
         scale = rep(params$Var2,each=7))

rsq_vars <- rsq_vars %>% 
  mutate(score = rep(params$Var1,each=5),
         scale = rep(params$Var2,each=5),
         parameter = rep(c("y1","y2","y3","int","slo"), length(params$Var1))) 

save(basefits, ests, fitcomps,rsq_vars, file = './scratch_data/growth_model_results.RData')


write.table(fitcomps, "./output/genetic_model_fitting_growth.txt", quote = F, row.names = F)


## @knitr model_fitting
load(file = './scratch_data/growth_model_results.RData')


# Find best-fitting model per phenotype + threshold combination

fitcomp_best <- fitcomps  %>%
  group_by(scale, score) %>%
  summarise(min = min(Pr..Chisq., na.rm = T)) %>%
  mutate(best = ifelse(min > 0.05, "no_prs_effect", "prs_on_resid"))

# fitcomp_int_v_slope <- fitcomps  %>%
#   group_by(scale, score) %>%
#   filter(model %in% c('prs_on_incpt_vs_resid','prs_on_slope_vs_resid')) %>%
#   summarise(AIC = max(AIC, na.rm = T)) %>%
#   mutate(ivs = "worse") 
  
best_of_rest <- fitcomp_best %>%
  left_join(fitcomps) %>%
#  anti_join(fitcomp_int_v_slope) %>%
  mutate(intok = ifelse( model == "prs_on_incpt_vs_resid" & Pr..Chisq. > 0.05  , 1,0)) %>%
  mutate(slopeok = ifelse( model == "prs_on_slope_vs_resid" & Pr..Chisq. > 0.05  , 1,0)) %>%
  mutate(growthok = ifelse(  model == "prs_on_growth_vs_resid" & Pr..Chisq. > 0.05  , 1,0))%>%
  mutate(intdrop = ifelse(  model == "prs_no_eff_vs_incpt" & Pr..Chisq. > 0.05  , 1,0))%>%
  mutate(slopedrop = ifelse(  model == "prs_no_eff_vs_slope" & Pr..Chisq. > 0.05  , 1,0))%>%
  mutate(growthdrop = ifelse(  model == "prs_no_eff_vs_growth" & Pr..Chisq. > 0.05  , 1,0))
  
  
b <- best_of_rest %>%
  group_by(scale, score) %>%
  mutate(best = ifelse(best!= "no_prs_effect"& sum(intok)>0& sum(intdrop)==0, "prs_on_incpt", best)) %>%
  mutate(best = ifelse(best!= "no_prs_effect"&  sum(slopeok)>0& sum(slopedrop)==0, "prs_on_slope", best)) %>%
  mutate(best = ifelse(best!= "no_prs_effect"&  sum(growthok)>0& sum(slopedrop)>0& sum(intdrop)==0, "prs_on_incpt", best)) %>%
  mutate(best = ifelse(best!= "no_prs_effect"&  sum(growthok)>0& sum(slopedrop)==0& sum(intdrop)>0, "prs_on_slope", best)) %>%
  mutate(best = ifelse(best!= "no_prs_effect"&  sum(growthok)>0& sum(slopedrop)==0& sum(intdrop)==0, "prs_on_growth_cannot_dist_SI", best)) %>%
  mutate(best = ifelse(best!= "no_prs_effect"&  sum(growthok)>0& sum(slopedrop)>0& sum(intdrop)>0& sum(growthdrop)==0, "prs_on_growth_cannot_dist_SI", best)) %>%
  ungroup()

b2 <- b %>%
  distinct(scale,score,best, .keep_all = T) %>%
  group_by(scale, score) %>%
  summarise(Df = max(Df)) %>%
  left_join(b) %>%
  select(scale, score, best) %>%
  ungroup()

rsq_comp <- rsq_vars%>%  
  filter(parameter %in% c("int","slo")) %>% 
  group_by(scale, score) %>% 
  summarise(sum_both = sum(both_growth_factors),
            sum_int = sum(intercept_only),
            sum_slo = sum(slope_only))

b2_rsq <- b2 %>% 
  left_join(rsq_comp) %>% 
  group_by(scale, score,best) %>% 
  summarise(max_rsq_both = max(sum_both),
            max_rsq_int = max(sum_int),
            max_rsq_slo = max(sum_slo)) %>% 
  ungroup() %>% 
  mutate(max_rsq=case_when(best == "prs_on_growth_cannot_dist_SI" ~ max_rsq_both,
                       best == "no_prs_effect" ~ 0,
                       best == "prs_on_slope" ~ max_rsq_slo,
                       best == "prs_on_int" ~ max_rsq_int,
                       best == "prs_on_resid" ~ 0)) %>% 
  group_by(scale) %>% 
  slice(which.max(max_rsq)) %>% 
  select(scale, score, best)

head(b2_rsq) %>% knitr::kable()

## @knitr output_plot_ests
fitcomps_wbest <- fitcomps %>%
  rename("comparison"="model") %>% 
  mutate(model=str_sub(comparison,end=-10)) %>% 
  left_join(b2) %>% 
  select(scale,score,comparison,Df,AIC,BIC, Chisq,Chisq.diff, Df.diff, pval= Pr..Chisq., best)

write.table(fitcomps_wbest, "./output/genetic_model_fitting_growth.txt", quote = F, row.names = F)

ests_from_bests1 <- ests %>%
  select(scale,score,best = model,lhs, est.std, ci.lower,ci.upper) %>%
  nest(est.std, ci.lower,ci.upper, .key = 'value_col') %>%
  spread(key = lhs, value = value_col) 

ref <- b2_rsq %>% 
  mutate(best = recode(best, "prs_on_growth_cannot_dist_SI" = "prs_on_growth")) %>% 
  unite(ssb, c(scale,score,best)) %>%  select(ssb)

ests_from_bests2 <- ests_from_bests1 %>%
  select(scale,score,best,i1,s1) %>%
  filter(!best %in% c("prs_on_resid","prs_no_effect")) %>%
  unnest(i1,s1, .sep = '_') %>%
  unite(ssb,c(scale,score,best)) %>%
  full_join(ests_from_bests1 %>%
          select(scale,score,best,ytime1,ytime2,ytime3) %>%
          filter(best %in% c("prs_on_resid")) %>%
          unnest(ytime1,ytime2,ytime3, .sep = '_') %>%
            unite(ssb,c(scale,score,best)))%>% 
  drop_na(i1_est.std)


plot_efb <-ests_from_bests2 %>% 
  select(ssb, intercept_est=i1_est.std,intercept_lci=i1_ci.lower,intercept_uci=i1_ci.upper,
         slope_est=s1_est.std,slope_lci=s1_ci.lower,slope_uci=s1_ci.upper) %>% 
  gather(param_type, val, -ssb) %>% 
  filter(val!=0) %>% 
  separate(param_type, c("on","what"))%>%
  select(ssb,on, what,val) %>% 
  spread(what,val) %>% 
  mutate(maximises_r2 = factor(ifelse(ssb %in% ref$ssb, "Yes", "No"))) %>% 
  separate(ssb, c("n1","pheno","n2","prsthresh","n3","n4","n5","model"), sep="_") %>% 
  mutate(prsthresh=factor(str_replace(prsthresh,"X","p<")),
         pheno=recode(pheno,ext="Behavioral problems",int="Emotional problems")) 

  

p1<-ggplot()+
  geom_errorbar(data=plot_efb,aes(x=prsthresh,ymin=lci,ymax=uci,group=interaction(model,on)), alpha=0.2, size=1,width=0, position = position_dodge(0.4))+
  geom_point(data=plot_efb, aes(x=prsthresh, y=est, shape=on, group=interaction(model,on),colour= fct_rev(maximises_r2)),size=4,alpha=1, position = position_dodge(0.4))+
 facet_grid(model ~pheno)+
  geom_hline(yintercept = 0, linetype=2, colour="grey70", size=0.3)+
  #geom_label(data=plot_efb,aes(x=pheno,y=0.05,label = prsthresh ),hjust=0.4 , vjust= -2)+
  theme(axis.ticks = element_line(colour="grey70",size=0.7),
        axis.title.x = element_text(margin = margin(t = 30, r = 0, b = , l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = , l = 0)),
        axis.line = element_line(colour="grey70", size=0.7),
        text =element_text(size = 14.5),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="white"),
        strip.text = element_text(size=14.5, face = "bold" ),
        strip.text.y = element_text(angle = 90),
        legend.title = element_text(size =11, face="bold"),
        legend.key= element_rect(fill=NA),
        strip.background = element_rect(fill = "grey90", colour = "white")) +
  coord_flip(ylim=c(-0.02,0.06), expand = c(1))+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Standardized beta coefficient from \nbest-fitting growth model") +
  guides(shape= guide_legend(title=""),colour= guide_legend(title="Maximises R2 across\n thresholds in best \nfitting model"))

#tiff("figures/growth_results.tiff", res = 600, compression = "lzw", unit = "in",
#     height = 5, width =5)

p1 






