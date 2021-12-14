library(survey)

survey1 <- subset(nhanes13, !is.na(WTDRD1)) #excludes participants without dietary data (dietary day 1 weights will be used)
survey2 <-  survey1 %>% rowwise() %>% mutate(PesoDieta4YR =WTDRD1 / 2) #creates new column with 2 cycles weights (divides 1 cycle weight by 2)
survey3 <- svydesign(data = survey2, strata = ~SDMVSTRA, id = ~SDMVPSU, nest = TRUE, weights = ~PesoDieta4YR) #weights dataset
survey4 <- subset(survey3,  RIDAGEYR > 59) #excludes those younger than 60 years
survey5 <- subset(survey4,  MCQ160F==2) #excludes those with stroke
survey6 <- subset(survey5,  DR1DRSTZ==1) #excludes those without complete dietary recall
survey7 <- subset(survey6,  DR1TKCAL > 500 & DR1TKCAL < 10000) #excludes those with extreme calorie intake 
survey8 <- subset(survey7,  DMDEDUC2 <= 5) #excludes those with incomplete education data
survey9 <- subset(survey8,  DMDMARTL <= 6) #excludes those with incomplete marital status data
survey10 <- subset(survey9,  BMXBMI <= 83)  #excludes those with incomplete BMI data
survey11 <- subset(survey10,  DIQ010 <= 2)  #excludes those with incomplete diabetes data
survey12 <- subset(survey11,  BPQ020 <= 2)  #excludes those with incomplete hypertension data
survey13 <- subset(survey12,  AlcoholYN == 0 |  AlcoholYN == 1) #excludes those with incomplete alcohol consumption data
survey14 <- subset(survey13,  SmokeYN == "never" | SmokeYN == "current" | SmokeYN == "former") #excludes those with incomplete smoking data
survey15 <- subset(survey14,  Exercise >= 0) #excludes those with incomplete exercise data
survey16 <- subset(survey15, ZscoreCeradTotal < 3) #excludes those with incomplete memory data
survey17 <- subset(survey16, ZscoreAnimal < 5) #excludes those with incomplete verbal fluency data
survey18 <- subset(survey17, ZscoreDigitSymbol < 4) #excludes those with incomplete digit symbol data
survey19 <- subset(survey18, ZscoreCognicao < 4) #excludes those with incomplete global cognition data

print(summary(survey19)) #prints summary of survey design

saveRDS(survey19, file = "~/Path/survey19.rds")  #saves

 
