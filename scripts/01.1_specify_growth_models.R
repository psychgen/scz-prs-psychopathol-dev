
#  For each scale set, at each threshold:
#  1. Run a basic LGM
#  2. Run LGM with PRS on residuals
#  3. Run LGM with PRS on growth factors
#  4. Run LGM with PRS on intercept only
#  5. Run LGM with PRS effects at zero (but retaining PRS in the model)
#  6. Compare all fits and extract PRS path estimates
#  All models have sex as a covariate


# Basic  LGM

model1 <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3

#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3

#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1

#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex
ytime2 ~ 0*1 + sex
ytime3 ~ 0*1 + sex

#Growth parameter intercepts (freely estimated)
i1 ~ 1 
s1 ~ 1 

'

# Basic  LGM + PRS on residuals

model2 <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3

#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3

#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1

#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + x + sex
ytime2 ~ 0*1 + x + sex
ytime3 ~ 0*1 + x + sex

#Growth parameter intercepts (freely estimated)
i1 ~ 1 
s1 ~ 1 

'

# Basic  LGM + PRS on growth factors

model3 <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3

#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3

#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1

#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex
ytime2 ~ 0*1 + sex
ytime3 ~ 0*1 + sex 

#Growth parameter intercepts (freely estimated)
i1 ~ 1 + x 
s1 ~ 1 + x

'

# Basic  LGM + PRS on intercept only 

model4 <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3

#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3

#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1

#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex 
ytime2 ~ 0*1 + sex 
ytime3 ~ 0*1 + sex 

#Growth parameter intercepts (freely estimated)
i1 ~ 1 + x
s1 ~ 1 + 0*x

'

# Basic  LGM + PRS on slope only 

model5 <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3

#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3

#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1

#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex 
ytime2 ~ 0*1 + sex 
ytime3 ~ 0*1 + sex 

#Growth parameter intercepts (freely estimated)
i1 ~ 1 + 0*x
s1 ~ 1 + x

'

# Basic  LGM + no PRS effects


model6 <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3

#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3


#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1

#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex 
ytime2 ~ 0*1 + sex 
ytime3 ~ 0*1 + sex 

#Growth parameter intercepts (freely estimated)
i1 ~ 1 + 0*x
s1 ~ 1 + 0*x

'

