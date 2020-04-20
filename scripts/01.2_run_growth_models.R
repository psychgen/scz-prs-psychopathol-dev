## @knitr run_loop 

# Loop over combinations to run models for each, saving results as additional rows in the dataframe

forloop_gm <- function(variable_frame,dataset){

  for(i in 1:dim(variable_frame)[[1]]){ 
  
  
  dataset[,"x"] <- dataset[,variable_frame[i,1]]
  dataset[,"ytime1"] <- dataset[,paste0(variable_frame[i,2],"_18m")]
  dataset[,"ytime2"] <- dataset[,paste0(variable_frame[i,2],"_3yr")]
  dataset[,"ytime3"] <- dataset[,paste0(variable_frame[i,2],"_5yr")]
  
  fit1 <<- growth(model1, data=dataset, missing="fiml.x")
  semPaths(fit1, style = 'ram', intercepts = F)
  semSyntax(fit1, "lavaan")
  summary(fit1,fit.measures=TRUE, standardized=TRUE)
  tmp.a <- as.data.frame(t(as.data.frame(fitMeasures(fit1))))
  
  basefits <<- rbind(basefits,tmp.a)
  
  fit2 <<- growth(model2, data=dataset, missing="fiml.x")
  semPaths(fit2, style = 'ram', intercepts = F)
  semSyntax(fit2, "lavaan")
  summary(fit2,fit.measures=TRUE, standardized=TRUE)
  tmp2 <- standardizedSolution(fit2, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~")
  
  ests2 <<- rbind(ests2,tmp2)
  
  fit3 <<- growth(model3, data=dataset, missing="fiml.x")
  semPaths(fit3, style = 'ram', intercepts = F)
  semSyntax(fit3, "lavaan")
  summary(fit3,fit.measures=TRUE, standardized=TRUE)
  tmp3 <- standardizedSolution(fit3, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~")
  
  ests3 <<- rbind(ests3,tmp3)
  
  fit4 <<- growth(model4, data=dataset, missing="fiml.x")
  semPaths(fit4, style = 'ram', intercepts = F)
  semSyntax(fit4, "lavaan")
  summary(fit4,fit.measures=TRUE, standardized=TRUE)
  tmp4 <- standardizedSolution(fit4, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~")
  
  ests4 <<- rbind(ests4,tmp4)
  
  
  fit5 <<- growth(model5, data=dataset, missing="fiml.x")
  semPaths(fit5, style = 'ram', intercepts = F)
  semSyntax(fit5, "lavaan")
  summary(fit5,fit.measures=TRUE, standardized=TRUE)
  tmp5 <- standardizedSolution(fit5, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~")
  
  ests5 <<- rbind(ests5,tmp5)
  
  fit6 <<- growth(model6, data=dataset, missing="fiml.x")
  semPaths(fit6, style = 'ram', intercepts = F)
  semSyntax(fit6, "lavaan")
  summary(fit5,fit.measures=TRUE, standardized=TRUE)
  tmp6 <- standardizedSolution(fit6, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~")
  
  ests6 <<- rbind(ests6,tmp6)
  
  #R2 values
  inspect(fit1, "r2")
  inspect(fit2, "r2")
  inspect(fit3, "r2")
  inspect(fit4, "r2")
  inspect(fit5, "r2")
  inspect(fit6, "r2")
  
  tmprsq_vars <- as.data.frame(inspect(fit3, "r2"))%>% 
    cbind(as.data.frame(inspect(fit4, "r2"))) %>% 
    cbind(as.data.frame(inspect(fit5, "r2"))) %>% 
    `colnames<-`(c("both_growth_factors", "intercept_only", "slope_only"))
  
  rsq_vars <<- rbind(rsq_vars, tmprsq_vars)
  
  tmp.b <- data.frame(anova(fit2,fit3,fit4,fit6)) %>%
    mutate(model = c('prs_on_resid','prs_on_growth_vs_resid','prs_on_incpt_vs_resid','prs_no_eff_vs_incpt'))
  tmp.c <- data.frame(anova(fit2,fit3,fit5,fit6)) %>%
    mutate(model = c('prs_on_resid','prs_on_growth_vs_resid','prs_on_slope_vs_resid','prs_no_eff_vs_slope'))
  tmp.d <- data.frame(anova(fit2,fit3,fit6)) %>%
    mutate(model = c('prs_on_resid','prs_on_growth_vs_resid','prs_no_eff_vs_growth'))
  
  tmp.f <- rbind(tmp.b,tmp.c,tmp.d) %>%
    distinct()
  
  
  fitcomps <<- rbind(fitcomps, tmp.f)
  
  # tmp.g <- as.data.frame(lavPredict(fit1))
  
  #colnames(tmp.g) <-paste(colnames(tmp.g),i,variable_frame[i,2], sep="_")
  
  #  factscores <- cbind(factscores, tmp.g)
  
}
}