

model1 <-  
  '
#Latent variable
pfact =~  mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9 + rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8 + rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9 + rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9 + rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8 + scared_1 + scared_2 + scared_3 + scared_4 + scared_5

dep =~ mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9
cd =~ rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8
odd =~ rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8
anx =~ scared_1 + scared_2 + scared_3 + scared_4 + scared_5
innat =~ rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9
hyp =~ rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9


# Regressions
mfq_1 ~ sex 
mfq_2 ~ sex 
mfq_3 ~ sex 
mfq_4 ~ sex 
mfq_5 ~ sex 
mfq_6 ~ sex 
mfq_7 ~ sex 
mfq_8 ~ sex 
mfq_9 ~ sex
mfq_10 ~ sex 
mfq_11 ~ sex 
mfq_12 ~ sex 
mfq_13 ~ sex  
rsdbd_cd_1 ~ sex 
rsdbd_cd_2 ~ sex 
rsdbd_cd_3 ~ sex 
rsdbd_cd_4 ~ sex 
rsdbd_cd_5 ~ sex 
rsdbd_cd_6 ~ sex 
rsdbd_cd_7 ~ sex 
rsdbd_cd_8 ~ sex 
rsdbd_hyp_1 ~ sex 
rsdbd_hyp_2 ~ sex 
rsdbd_hyp_3 ~ sex 
rsdbd_hyp_4 ~ sex 
rsdbd_hyp_5 ~ sex 
rsdbd_hyp_6 ~ sex 
rsdbd_hyp_7 ~ sex 
rsdbd_hyp_8 ~ sex 
rsdbd_hyp_9 ~ sex 
rsdbd_innat_1 ~ sex 
rsdbd_innat_2 ~ sex 
rsdbd_innat_3 ~ sex 
rsdbd_innat_4 ~ sex 
rsdbd_innat_5 ~ sex 
rsdbd_innat_6 ~ sex 
rsdbd_innat_7 ~ sex 
rsdbd_innat_8 ~ sex 
rsdbd_innat_9 ~ sex 
rsdbd_od_1 ~ sex 
rsdbd_od_2 ~ sex 
rsdbd_od_3 ~ sex 
rsdbd_od_4 ~ sex 
rsdbd_od_5 ~ sex 
rsdbd_od_6 ~ sex 
rsdbd_od_7 ~ sex 
rsdbd_od_8 ~ sex 
scared_1 ~ sex 
scared_2 ~ sex 
scared_3 ~ sex 
scared_4 ~ sex 
scared_5 ~ sex

#Latent factor covariances

#Latent variable correlations @ 0
pfact ~~ 0*dep
pfact ~~ 0*anx
pfact ~~ 0*innat
pfact ~~ 0*hyp
pfact ~~ 0*cd
pfact ~~ 0*odd
dep ~~ 0*anx
dep ~~ 0*innat
dep ~~ 0*hyp
dep ~~ 0*cd
dep ~~ 0*odd
anx ~~ 0*innat
anx ~~ 0*hyp
anx ~~ 0*cd
anx ~~ 0*odd
innat ~~ 0*hyp
innat ~~ 0*cd
innat ~~ 0*odd
hyp ~~ 0*cd
hyp ~~ 0*odd
cd ~~ 0*odd

'





model1.1 <-  
  '
#Latent variable
pfact =~  mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9 + rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8 + rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9 + rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9 + rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8 + scared_1 + scared_2 + scared_3 + scared_4 + scared_5

dep =~ mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9
cd =~ rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8
odd =~ rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8
anx =~ scared_1 + scared_2 + scared_3 + scared_4 + scared_5
innat =~ rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9
hyp =~ rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9


# Regressions
mfq_1 ~ sex + x
mfq_2 ~ sex + x
mfq_3 ~ sex + x
mfq_4 ~ sex + x
mfq_5 ~ sex + x
mfq_6 ~ sex + x
mfq_7 ~ sex + x
mfq_8 ~ sex + x
mfq_9 ~ sex + x
mfq_10 ~ sex + x 
mfq_11 ~ sex + x
mfq_12 ~ sex + x
mfq_13 ~ sex + x
rsdbd_cd_1 ~ sex + x 
rsdbd_cd_2 ~ sex + x
rsdbd_cd_3 ~ sex + x
rsdbd_cd_4 ~ sex + x
rsdbd_cd_5 ~ sex + x
rsdbd_cd_6 ~ sex + x
rsdbd_cd_7 ~ sex + x
rsdbd_cd_8 ~ sex + x
rsdbd_hyp_1 ~ sex + x
rsdbd_hyp_2 ~ sex + x
rsdbd_hyp_3 ~ sex + x
rsdbd_hyp_4 ~ sex + x
rsdbd_hyp_5 ~ sex + x
rsdbd_hyp_6 ~ sex + x
rsdbd_hyp_7 ~ sex + x
rsdbd_hyp_8 ~ sex + x
rsdbd_hyp_9 ~ sex + x
rsdbd_innat_1 ~ sex + x
rsdbd_innat_2 ~ sex + x
rsdbd_innat_3 ~ sex + x
rsdbd_innat_4 ~ sex + x
rsdbd_innat_5 ~ sex + x
rsdbd_innat_6 ~ sex + x
rsdbd_innat_7 ~ sex + x
rsdbd_innat_8 ~ sex + x
rsdbd_innat_9 ~ sex + x
rsdbd_od_1 ~ sex + x
rsdbd_od_2 ~ sex + x
rsdbd_od_3 ~ sex + x
rsdbd_od_4 ~ sex + x
rsdbd_od_5 ~ sex + x
rsdbd_od_6 ~ sex + x
rsdbd_od_7 ~ sex + x
rsdbd_od_8 ~ sex + x
scared_1 ~ sex + x
scared_2 ~ sex + x
scared_3 ~ sex + x
scared_4 ~ sex + x
scared_5 ~ sex + x


#Latent factor covariances

#Latent variable correlations @ 0
pfact ~~ 0*dep
pfact ~~ 0*anx
pfact ~~ 0*innat
pfact ~~ 0*hyp
pfact ~~ 0*cd
pfact ~~ 0*odd
dep ~~ 0*anx
dep ~~ 0*innat
dep ~~ 0*hyp
dep ~~ 0*cd
dep ~~ 0*odd
anx ~~ 0*innat
anx ~~ 0*hyp
anx ~~ 0*cd
anx ~~ 0*odd
innat ~~ 0*hyp
innat ~~ 0*cd
innat ~~ 0*odd
hyp ~~ 0*cd
hyp ~~ 0*odd
cd ~~ 0*odd

'

model1.2 <-  
  '
#Latent variable
pfact =~  mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9 + rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8 + rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9 + rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9 + rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8 + scared_1 + scared_2 + scared_3 + scared_4 + scared_5

dep =~ mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9
cd =~ rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8
odd =~ rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8
anx =~ scared_1 + scared_2 + scared_3 + scared_4 + scared_5
innat =~ rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9
hyp =~ rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9


# Regressions

dep ~ x
cd ~ x
odd ~ x
anx ~ x
innat ~ x
hyp ~ x

mfq_1 ~ sex 
mfq_2 ~ sex 
mfq_3 ~ sex 
mfq_4 ~ sex 
mfq_5 ~ sex 
mfq_6 ~ sex 
mfq_7 ~ sex 
mfq_8 ~ sex 
mfq_9 ~ sex
mfq_10 ~ sex 
mfq_11 ~ sex 
mfq_12 ~ sex 
mfq_13 ~ sex  
rsdbd_cd_1 ~ sex 
rsdbd_cd_2 ~ sex 
rsdbd_cd_3 ~ sex 
rsdbd_cd_4 ~ sex 
rsdbd_cd_5 ~ sex 
rsdbd_cd_6 ~ sex 
rsdbd_cd_7 ~ sex 
rsdbd_cd_8 ~ sex 
rsdbd_hyp_1 ~ sex 
rsdbd_hyp_2 ~ sex 
rsdbd_hyp_3 ~ sex 
rsdbd_hyp_4 ~ sex 
rsdbd_hyp_5 ~ sex 
rsdbd_hyp_6 ~ sex 
rsdbd_hyp_7 ~ sex 
rsdbd_hyp_8 ~ sex 
rsdbd_hyp_9 ~ sex 
rsdbd_innat_1 ~ sex 
rsdbd_innat_2 ~ sex 
rsdbd_innat_3 ~ sex 
rsdbd_innat_4 ~ sex 
rsdbd_innat_5 ~ sex 
rsdbd_innat_6 ~ sex 
rsdbd_innat_7 ~ sex 
rsdbd_innat_8 ~ sex 
rsdbd_innat_9 ~ sex 
rsdbd_od_1 ~ sex 
rsdbd_od_2 ~ sex 
rsdbd_od_3 ~ sex 
rsdbd_od_4 ~ sex 
rsdbd_od_5 ~ sex 
rsdbd_od_6 ~ sex 
rsdbd_od_7 ~ sex 
rsdbd_od_8 ~ sex 
scared_1 ~ sex 
scared_2 ~ sex 
scared_3 ~ sex 
scared_4 ~ sex 
scared_5 ~ sex

#Latent factor covariances

#Latent variable correlations @ 0
pfact ~~ 0*dep
pfact ~~ 0*anx
pfact ~~ 0*innat
pfact ~~ 0*hyp
pfact ~~ 0*cd
pfact ~~ 0*odd
dep ~~ 0*anx
dep ~~ 0*innat
dep ~~ 0*hyp
dep ~~ 0*cd
dep ~~ 0*odd
anx ~~ 0*innat
anx ~~ 0*hyp
anx ~~ 0*cd
anx ~~ 0*odd
innat ~~ 0*hyp
innat ~~ 0*cd
innat ~~ 0*odd
hyp ~~ 0*cd
hyp ~~ 0*odd
cd ~~ 0*odd

'

model1.3 <-  
  '
#Latent variable
pfact =~  mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9 + rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8 + rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9 + rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9 + rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8 + scared_1 + scared_2 + scared_3 + scared_4 + scared_5

dep =~ mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9
cd =~ rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8
odd =~ rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8
anx =~ scared_1 + scared_2 + scared_3 + scared_4 + scared_5
innat =~ rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9
hyp =~ rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9


# Regressions

pfact ~ x


mfq_1 ~ sex 
mfq_2 ~ sex 
mfq_3 ~ sex 
mfq_4 ~ sex 
mfq_5 ~ sex 
mfq_6 ~ sex 
mfq_7 ~ sex 
mfq_8 ~ sex 
mfq_9 ~ sex
mfq_10 ~ sex 
mfq_11 ~ sex 
mfq_12 ~ sex 
mfq_13 ~ sex  
rsdbd_cd_1 ~ sex 
rsdbd_cd_2 ~ sex 
rsdbd_cd_3 ~ sex 
rsdbd_cd_4 ~ sex 
rsdbd_cd_5 ~ sex 
rsdbd_cd_6 ~ sex 
rsdbd_cd_7 ~ sex 
rsdbd_cd_8 ~ sex 
rsdbd_hyp_1 ~ sex 
rsdbd_hyp_2 ~ sex 
rsdbd_hyp_3 ~ sex 
rsdbd_hyp_4 ~ sex 
rsdbd_hyp_5 ~ sex 
rsdbd_hyp_6 ~ sex 
rsdbd_hyp_7 ~ sex 
rsdbd_hyp_8 ~ sex 
rsdbd_hyp_9 ~ sex 
rsdbd_innat_1 ~ sex 
rsdbd_innat_2 ~ sex 
rsdbd_innat_3 ~ sex 
rsdbd_innat_4 ~ sex 
rsdbd_innat_5 ~ sex 
rsdbd_innat_6 ~ sex 
rsdbd_innat_7 ~ sex 
rsdbd_innat_8 ~ sex 
rsdbd_innat_9 ~ sex 
rsdbd_od_1 ~ sex 
rsdbd_od_2 ~ sex 
rsdbd_od_3 ~ sex 
rsdbd_od_4 ~ sex 
rsdbd_od_5 ~ sex 
rsdbd_od_6 ~ sex 
rsdbd_od_7 ~ sex 
rsdbd_od_8 ~ sex 
scared_1 ~ sex 
scared_2 ~ sex 
scared_3 ~ sex 
scared_4 ~ sex 
scared_5 ~ sex

#Latent factor covariances

#Latent variable correlations @ 0
pfact ~~ 0*dep
pfact ~~ 0*anx
pfact ~~ 0*innat
pfact ~~ 0*hyp
pfact ~~ 0*cd
pfact ~~ 0*odd
dep ~~ 0*anx
dep ~~ 0*innat
dep ~~ 0*hyp
dep ~~ 0*cd
dep ~~ 0*odd
anx ~~ 0*innat
anx ~~ 0*hyp
anx ~~ 0*cd
anx ~~ 0*odd
innat ~~ 0*hyp
innat ~~ 0*cd
innat ~~ 0*odd
hyp ~~ 0*cd
hyp ~~ 0*odd
cd ~~ 0*odd

'

model1.4 <-  
  '
#Latent variable
pfact =~  mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9 + rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8 + rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9 + rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9 + rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8 + scared_1 + scared_2 + scared_3 + scared_4 + scared_5

dep =~ mfq_1 + mfq_10 + mfq_11 + mfq_12 + mfq_13 + mfq_2 + mfq_3 + mfq_4 + mfq_5 + mfq_6 + mfq_7 + mfq_8 + mfq_9
cd =~ rsdbd_cd_1 + rsdbd_cd_2 + rsdbd_cd_3 + rsdbd_cd_4 + rsdbd_cd_5 + rsdbd_cd_6 + rsdbd_cd_7 + rsdbd_cd_8
odd =~ rsdbd_od_1 + rsdbd_od_2 + rsdbd_od_3 + rsdbd_od_4 + rsdbd_od_5 + rsdbd_od_6 + rsdbd_od_7 + rsdbd_od_8
anx =~ scared_1 + scared_2 + scared_3 + scared_4 + scared_5
innat =~ rsdbd_innat_1 + rsdbd_innat_2 + rsdbd_innat_3 + rsdbd_innat_4 + rsdbd_innat_5 + rsdbd_innat_6 + rsdbd_innat_7 + rsdbd_innat_8 + rsdbd_innat_9
hyp =~ rsdbd_hyp_1 + rsdbd_hyp_2 + rsdbd_hyp_3 + rsdbd_hyp_4 + rsdbd_hyp_5 + rsdbd_hyp_6 + rsdbd_hyp_7 + rsdbd_hyp_8 + rsdbd_hyp_9


# Regressions

pfact ~ 0*x
dep ~ 0*x
cd ~ 0*x
odd ~ 0*x
anx ~ 0*x
innat ~ 0*x
hyp ~ 0*x

mfq_1 ~ sex 
mfq_2 ~ sex 
mfq_3 ~ sex 
mfq_4 ~ sex 
mfq_5 ~ sex 
mfq_6 ~ sex 
mfq_7 ~ sex 
mfq_8 ~ sex 
mfq_9 ~ sex
mfq_10 ~ sex 
mfq_11 ~ sex 
mfq_12 ~ sex 
mfq_13 ~ sex  
rsdbd_cd_1 ~ sex 
rsdbd_cd_2 ~ sex 
rsdbd_cd_3 ~ sex 
rsdbd_cd_4 ~ sex 
rsdbd_cd_5 ~ sex 
rsdbd_cd_6 ~ sex 
rsdbd_cd_7 ~ sex 
rsdbd_cd_8 ~ sex 
rsdbd_hyp_1 ~ sex 
rsdbd_hyp_2 ~ sex 
rsdbd_hyp_3 ~ sex 
rsdbd_hyp_4 ~ sex 
rsdbd_hyp_5 ~ sex 
rsdbd_hyp_6 ~ sex 
rsdbd_hyp_7 ~ sex 
rsdbd_hyp_8 ~ sex 
rsdbd_hyp_9 ~ sex 
rsdbd_innat_1 ~ sex 
rsdbd_innat_2 ~ sex 
rsdbd_innat_3 ~ sex 
rsdbd_innat_4 ~ sex 
rsdbd_innat_5 ~ sex 
rsdbd_innat_6 ~ sex 
rsdbd_innat_7 ~ sex 
rsdbd_innat_8 ~ sex 
rsdbd_innat_9 ~ sex 
rsdbd_od_1 ~ sex 
rsdbd_od_2 ~ sex 
rsdbd_od_3 ~ sex 
rsdbd_od_4 ~ sex 
rsdbd_od_5 ~ sex 
rsdbd_od_6 ~ sex 
rsdbd_od_7 ~ sex 
rsdbd_od_8 ~ sex 
scared_1 ~ sex 
scared_2 ~ sex 
scared_3 ~ sex 
scared_4 ~ sex 
scared_5 ~ sex

#Latent factor covariances

#Latent variable correlations @ 0
pfact ~~ 0*dep
pfact ~~ 0*anx
pfact ~~ 0*innat
pfact ~~ 0*hyp
pfact ~~ 0*cd
pfact ~~ 0*odd
dep ~~ 0*anx
dep ~~ 0*innat
dep ~~ 0*hyp
dep ~~ 0*cd
dep ~~ 0*odd
anx ~~ 0*innat
anx ~~ 0*hyp
anx ~~ 0*cd
anx ~~ 0*odd
innat ~~ 0*hyp
innat ~~ 0*cd
innat ~~ 0*odd
hyp ~~ 0*cd
hyp ~~ 0*odd
cd ~~ 0*odd

'