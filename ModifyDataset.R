library(dplyr)

#Demo data
Demo <- read.csv("~/Path/Demo.csv", header=TRUE)  #opens file
Demo2 <- select(Demo, SEQN, SDDSRVYR, RIDSTATR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDMARTL, DMDEDUC2, WTINT2YR, WTMEC2YR, INDFMIN2, 
SDMVSTRA, SDMVPSU, INDFMPIR, INDHHIN2)   #keeps only columns which will be used
write.csv(Demo2, file="~/Path/Demo2.csv") #saves

#Alcohol use data
Alq <- read.csv("~/Path/Alq.csv", header=TRUE) #opens file
Alq2 <- select(Alq, SEQN, ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130) #keeps only columns which will be used
Alq3 <- Alq2 %>% rowwise() %>% mutate(AlcoholYN = ifelse(ALQ101==1, 1, ifelse(ALQ101==2, 0, NA))) #binary: 1 if person drinks alcohol or 0 if not
write.csv(Alq3, file="~/Path/Alq3.csv") #saves

#Smoking data
Smq <- read.csv("~/Path/Smq.csv", header=TRUE)  #opens file
Smq2 <- select(Smq, SEQN, SMQ020, SMD030, SMQ040, SMD641, SMD650) #keeps only columns which will be used
Smq3 <- Smq2 %>% rowwise() %>% mutate(SmokeYN = ifelse(SMQ020==1 & SMQ040<3, “current”, ifelse(SMQ020==1 & SMQ040==3, “former”, 
ifelse(SMQ020==2, “never”, NA))) #creates new labels: "current" if smoker, "former" if former smoker and "never" if never smoked
write.csv(Smq3, file="~/Path/Smq3.csv") #saves

#Exercise data
Paq <- read.csv("~/Path/Smq.csv", header=TRUE)  #opens file
Paq2 <- select(Paq, SEQ, PAQ706, PAQ605, PAQ610, PAD615, PAQ620, PAQ625, PAD630, PAQ635, PAQ640, PAD645, PAQ650, PAQ655, 
PAD660, PAQ665, PAQ670, PAD675)  #keeps only columns which will be used
Paq3 <- Paq %>% rowwise() %>% mutate(VigWork = ifelse(any((PAQ610 > 7),(PAD615> 780)), NA, Reduce('*', c(PAQ610, PAD615)))) #creates new column of 
intense activity by multiplying days and minutes of intense work activity
Paq4 <- Paq3 %>% rowwise() %>% mutate(ModWork = ifelse(any((PAQ625 > 7),(PAD630 > 960)), NA, Reduce('*', c(PAQ625, PAD630)))) #creates new column of 
moderate activity by multiplying days and minutes of moderate work activity
Paq5 <- Paq4 %>% rowwise() %>% mutate(WalkBike = ifelse(any((PAQ640 > 7),(PAD645 > 720)), NA, Reduce('*', c(PAQ640, PAD645)))) #creates new column of
"transportation" activity by multiplying days and minutes spent walking or ciclying to work
Paq6 <- Paq5 %>% rowwise() %>% mutate(VigRec = ifelse(any((PAQ655 > 7),(PAD660 > 720)), NA, Reduce('*', c(PAQ655, PAD660)))) #creates new column of 
intense activity by multiplying days and minutes of intense recreation activity
Paq7 <- Paq6 %>% rowwise() %>% mutate(ModRec = ifelse(any((PAQ670 > 7),(PAD675 > 600)), NA, Reduce('*', c(PAQ670, PAD675)))) #creates new column of 
moderate activity by multiplying days and minutes of moderate recreation activity
Paq8 <- Paq7 %>% rowwise() %>% mutate(VigExercise = sum((VigWork, VigRec, na.rm=TRUE)) #new column with sum of intense activity, ignores NA
Paq9 <- Paq8 %>% rowwise() %>% mutate(ModExercise = sum(ModerateWork, WalkBike, ModerateRec, na.rm=TRUE))  #new column with sum of moderate activity, ignores NA
Paq9$METSVig <- Paq9$VigExercise*8 #new column of total minutes of intense activity times 8 METS 
Paq9$METSMod <- Paq9$ModExercise*4 #new column of total minutes of moderate activity times 4 METS
Paq10 <-  Paq9 %>% rowwise() %>% mutate(TotalMETS = sum(METSVig, METSMod, na.rm=TRUE)) #Sums total METS-minutes of activity
Paq11 <-  Paq10 %>% rowwise() %>% mutate(Exercise = ifelse(TotalMETS > 599, 1, ifelse(TotalMETS <600, 0, NA))) #binary: 1 if enough exercise, 0 if not
write.csv(Paq11, file="~/Path/Paq11.csv") #saves

#Depression data
Dpq <- read.csv("~/Path/Dpq.csv", header=TRUE) #opens file
Dpq2 <- Dpq %>% rowwise() %>% mutate(Result = sum(ifelse(c_across(DPQ010:DPQ090)<4, c_across(DPQ010:DPQ090), NA))) #Sums values from columns DPQ010:DPQ090, keeps NA
Dpq3 <- Dpq2 %>% rowwise() %>% mutate(Depressao = ifelse(SomaDpq > 9, 1, 0)) #binary: 1 if person has depression (sum >9) or 0 if not
write.csv(Dpq3, file="~/Path/Dpq3.csv") #saves

#Cognition data
Cfq <- read.csv("~/Path/Cfq.csv", header=TRUE)  #opens file
Cfq2 <- select(Cfq, SEQN, CFASTAT, CFDCCS, CFDCST1, CFDCST2, CFDCST3, CFDCSR, CFDCIT1, CFDCIT2, CFDCIT3, CFDCIR, CFDAPP, CFDAST, 
CFDDPP, CFDDS) #keeps only columns which will be used
Cfq3 <- Cfq2 %>% rowwise() %>% mutate(SumCerad = sum(c_across(CFDCST1 :CFDCST3))) #sums 3 learning memory scores
write.csv(Cfq3, file="~/Path/Cfq5.csv") #saves
