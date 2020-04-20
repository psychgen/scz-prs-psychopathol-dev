

forloop_pfm <- function(variable_frame,dataset){
  
for(i in 1:dim(variable_frame)[[1]]){ 
  
  dataset[,"x"] <- dataset[,variable_frame[i,1]]
  
  # 1. Run a scale-specific bifactor model
  # 1.1. Run PRS-on-resids models 
  # 1.2. Run PRS-on-scale-factors models 
  # 1.3. Run PRS-on-p-factor model 
  # 1.4. Run PRS-at-zero model 
  
  # BASEFITS

  message(paste0("Starting baseline model fitting for run ",i," of ",dim(variable_frame)[[1]]))
  
  fit1 <<- cfa(model1, data=dataset, ordered = c(varnames),std.lv=TRUE)
  basefits <<- rbind(basefits,as.data.frame(t(as.data.frame(fitMeasures(fit1)))))
  
  base_rsq <<- as.data.frame(inspect(fit1, "r2")[1:52])
  
  message(paste0("Completed baseline model fitting for run ",i," of ",dim(variable_frame)[[1]]))
  
  
  # fit models
  message(paste0("Starting PRS-on-resids model fitting for run ",i," of ",dim(variable_frame)[[1]]))
  fit1.1 <<- cfa(model1.1, data=dataset, ordered = c(varnames),std.lv=TRUE)
  message("Completed\n")
  message(paste0("Starting PRS-on-factors model fitting for run ",i," of ",dim(variable_frame)[[1]]))
  fit1.2 <<- cfa(model1.2, data=dataset, ordered = c(varnames),std.lv=TRUE)
  message("Completed\n")
  message(paste0("Starting PRS-on-p-factor model fitting for run ",i," of ",dim(variable_frame)[[1]]))
  fit1.3 <<- cfa(model1.3, data=dataset, ordered = c(varnames),std.lv=TRUE)
  message("Completed\n")
  message(paste0("Starting no-PRS-effect model fitting for run ",i," of ",dim(variable_frame)[[1]]))
  fit1.4 <<- cfa(model1.4, data=dataset, ordered = c(varnames),std.lv=TRUE)
  message("Completed\n")
  print(paste0("Completed PRS model fitting for run ",i))
  
  #R2 values
 mean(inspect(fit1.1, "r2")[1:52]) 
 mean(inspect(fit1.2, "r2")[1:52]) 
 mean(inspect(fit1.3, "r2")[1:52]) 
 mean(inspect(fit1.4, "r2")[1:52]) 
  
 tmprsq_vars <- as.data.frame(inspect(fit1.1, "r2")[1:52])%>% 
    cbind(as.data.frame(inspect(fit1.2, "r2")[1:52])) %>% 
    cbind(as.data.frame(inspect(fit1.3, "r2")[1:52])) %>% 
    cbind(as.data.frame(inspect(fit1.4, "r2")[1:52])) %>% 
   `colnames<-`(c("prs_on_resids", "prs_on_factors", "prs_on_pfact", "no_prs_effect"))
 
 rsq_vars <<- rbind(rsq_vars, tmprsq_vars)
 
 tmprsq_factors <- as.data.frame(c(inspect(fit1.2, "r2")[53:58],inspect(fit1.3, "r2")[53]))%>% 
   `colnames<-`(c("prs_on_all_factors"))
 
 rsq_factors <<- rbind(rsq_factors, tmprsq_factors)
  
  #FITCOMPS

  
  tmp <- data.frame(rbind(anova(fit1.1,fit1.2),
                          anova(fit1.1,fit1.3),
                          anova(fit1.1,fit1.4))) 
  
  fitcomps <<- rbind(fitcomps,tmp) 
  
  # ESTS
  # PRS on resids
  tmp2 <- standardizedSolution(fit1.1, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~") %>%
    mutate(model = "prs_on_resids") 
  ests1 <<- rbind(ests1, tmp2)
  # PRS on factors 
  tmp3 <- standardizedSolution(fit1.2, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~") %>%
    mutate(model = "prs_on_scalefs") 
  ests2 <<- rbind(ests2, tmp3)
  
  # PRS on p factor 
  tmp4 <- standardizedSolution(fit1.3, ci = TRUE, level = 0.95) %>%
    filter(rhs=="x",
           op == "~") %>%
    mutate(model = "prs_on_pfact") 
  ests3 <<- rbind(ests3, tmp4)
  
  print(paste0("Completed run ",i," of ",dim(variable_frame)[[1]] ))
}
}