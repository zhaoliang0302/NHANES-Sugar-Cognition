library(dplyr)

#Demo data
Demo <- read.csv("~/Path/Demo.csv", header=TRUE)  #opens file
Demo2 <- select(Demo, SEQN, SDDSRVYR, RIDSTATR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDMARTL, DMDEDUC2, WTINT2YR, WTMEC2YR, INDFMIN2, 
SDMVSTRA, SDMVPSU, INDFMPIR, INDHHIN2)   #keeps only columns which will be used
Demo3 <- Demo2 %>% rowwise() %>% mutate(Idade = ifelse(RIDAGEYR >= 60 & RIDAGEYR <= 64, “60-64”, 
                                                                ifelse(RIDAGEYR >= 65 & RIDAGEYR <= 69, “65-69”, 
                                                                       ifelse(RIDAGEYR >= 70 & RIDAGEYR <= 74, “70-74”, 
                                                                              ifelse (RIDAGEYR >= 75 & RIDAGEYR <= 79, “75-79”, 
                                                                                      ifelse (RIDAGEYR == 80, “>80”, “59”)))) #age intervals 5 years
Demo4 <- Demo3 %>% rowwise() %>% mutate(Idade10 = ifelse(RIDAGEYR >= 60 & RIDAGEYR <= 69, “60-69”, 
                                                         ifelse(RIDAGEYR >= 70 & RIDAGEYR <= 79, “70-79”, 
                                                                ifelse(RIDAGEYR == 80, “>80”, “59”)))) ##age intervals 10 years
Demo5 <- Demo4 %>% rowwise() %>% mutate(Gender = ifelse(RIAGENDR == 1, “male”, ifelse(RIAGENDR == 2, “female”, NA))) #gender strings
Demo6 <- Demo5 %>% rowwise() %>% mutate(Marital = ifelse(DMDMARTL == 1 | DMDMARTL == 6, “Together”, 
                                                      ifelse(DMDMARTL == 2 | DMDMARTL == 3 | DMDMARTL == 4 DMDMARTL == 5, “Single”, NA))) #marital status 2 strings
Demo7 <-  Demo6 %>% rowwise() %>% mutate(Education = ifelse(DMDEDUC2 == 1 | DMDEDUC2 == 2, “less high school”, 
                                                            ifelse(DMDEDUC2 == 3, “high school”, 
                                                                   ifelse(DMDEDUC2  == 4, “some college”, 
                                                                          ifelse(DMDEDUC2  == 5, “college or more”, NA))) #Education attainment strings                                                       
Demo8 <-  Demo7 %>% rowwise() %>% mutate(Raca = ifelse(RIDRETH3 == 1 | RIDRETH3 == 2, "hispanic", 
                                                                     ifelse(RIDRETH3 == 3, "NH white", 
                                                                            ifelse(RIDRETH3 == 4, "NH black", 
                                                                                   ifelse(RIDRETH3  == 6 | RIDRETH3 == 7, "Other", NA))))) #Race strings 
write.csv(Demo8, file="~/Path/Demo8.csv") #saves

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

#Diabetes data
Diq <- read.csv("~/Path/Diq.csv", header=TRUE)  #opens file
Diq2 <-  Diq %>% rowwise() %>% mutate(DiabetesYN = ifelse(DIQ010 == 1, 1, ifelse(DIQ010 == 2, 0, NA))) #binary: 1 if diabetes diagnosis, 0 if not
write.csv(Diq2, file="~/Path/Diq2.csv") #saves
                                      
#Hypertension data
Bpq <- read.csv("~/Path/Bpq.csv", header=TRUE)  #opens file
Bpq2 <-  Diq %>% rowwise() %>% mutate(HypertensionYN = ifelse(BPQ020 == 1, 1, ifelse(BPQ020 == 2, 0, NA))) #binary: 1 if hypertension diagnosis, 0 if not
write.csv(Diq2, file="~/Path/Diq2.csv") #saves                                      
                                      
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

#Foods data
SoftEnergyDrink2 <- read.csv("~/Path/SoftEnergyDrink2.csv", header=TRUE) #opens SSBs file
SoftEnergyDrink3 <- SoftEnergyDrink2 %>% rowwise() %>% mutate(RefriBinario = ifelse(RefriGrams == 0, 0, 
                                                                                    ifelse(RefriGrams > 0, 1, NA))) #binary: 1 if consumes SSBs, 0 if not
write.csv(SoftEnergyDrink3, file="~/Path/SoftEnergyDrink3.csv") #saves
Solids8 <- read.csv("~/Path/Solids8.csv", header=TRUE) #opens solid desserts file
Solids9 <- Solids8 %>% rowwise() %>% mutate(SolidsBinario = ifelse(SolidsGrams == 0, 0,
                                                                   ifelse(SolidsGrams > 0, 1, NA))) #binary: 1 if consumes solid desserts, 0 if not                                                                           
write.csv(Solids9, file="~/Path/Solids9.csv") #saves                                      
FruitJuices2 <- read.csv("~/Path/FruitJuices2.csv", header=TRUE) #opens juices file
FruitJuices3 <- FruitJuices2 %>% rowwise() %>% mutate(JuiceBinario = ifelse(JuiceGrams == 0, 0,
                                                                            ifelse(JuiceGrams > 0, 1, NA))) #binary: 1 if consumes 100% fruit juices, 0 if not                                                                           
write.csv(FruitJuices3, file="~/Path/FruitJuices3.csv") #saves                                    
                                      
#Cognition data
Cfq <- read.csv("~/Path/Cfq.csv", header=TRUE)  #opens file
Cfq2 <- select(Cfq, SEQN, CFASTAT, CFDCCS, CFDCST1, CFDCST2, CFDCST3, CFDCSR, CFDCIT1, CFDCIT2, CFDCIT3, CFDCIR, CFDAPP, CFDAST, 
CFDDPP, CFDDS) #keeps only columns which will be used
Cfq3 <- Cfq2 %>% rowwise() %>% mutate(SumCerad = sum(c_across(CFDCST1 :CFDCST3))) #sums 3 learning memory scores
write.csv(Cfq3, file="~/Path/Cfq3.csv") #saves
