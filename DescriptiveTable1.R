library(dplyr)

nhanes13 <- read.csv("~/Path/nhanes13.csv) #opens file

#subsets data
DescTable <- subset(nhanes13,  RIDAGEYR > 59) #keeps age > 59 years
DescTable2 <- subset(DescTable, MCQ160F == 2) #deletes who had stroke
DescTable3 <- subset(DescTable2, DR1DRSTZ == 1) #complete food and nutrients data
DescTable4 <- subset(DescTable3, DR1TKCAL > 500 & DR1TKCAL < 10000) #deletes extreme calorie intake
DescTable5 <- subset(DescTable4, DMDEDUC2 <= 5) #complete education data
DescTable6 <- subset(DescTable5, DMDMARTL <= 6) #complete marital status data
DescTable7 <- subset(DescTable6, BMXBMI <= 83) #complete anthropometry measures data
DescTable8 <- subset(DescTable7, DIQ010 <= 2) #complete diabetes data
DescTable9 <- subset(DescTable8, BPQ020 <= 2) #complete hypertension data
DescTable10 <- subset(DescTable9, AlcoholYN == 0 |  AlcoholYN == 1) #complete alcohol  consumption data
DescTable11 <- subset(DescTable10, SmokeYN == "never" | SmokeYN == "current" | SmokeYN == "former") #complete smking data
DescTable12 <- subset(DescTable11, Exercise >= 0) #complete exercise data
DescTable13 <- subset(DescTable12, ZscoreCeradTotal < 3) #complete memory tests
DescTable14 <- subset(DescTable13, ZscoreAnimal < 5) #complete animal fluency tests
DescTable15 <- subset(DescTable14,  ZscoreDigitSymbol < 4) #complete digit symbol tests
DescTable16 <- subset(DescTable15,  ZscoreCognicao < 4) #complete cognition tests

write.csv(DescTable16, file="~/Path/DescTable16.csv") #saves


#Absolute numbers
#age
DescTable16 %>% group_by(RIDAGEYR) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by age
DescTable16 %>% summarise(count = mean(RIDAGEYR)) #mean age of participants (total sample)
DescTable16 %>% summarise(count = sd(RIDAGEYR)) #standard deviation of participants by age (total sample)

#gender
DescTable16 %>% group_by(Gender) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by gender

#race
DescTable16 %>% group_by(Raca) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by race

#education
DescTable16 %>% group_by(Education) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by education attainment

#marital status
DescTable16 %>% group_by(Marital) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by marital status

#BMI
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(BMXBMI)) #counts participants with and without cognitive deficit by mean BMI
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(BMXBMI)) #counts participants with and without cognitive deficit by standard deviation of BMI
DescTable16 %>% summarise(count = mean(BMXBMI)) #mean BMI of participants (total sample)
DescTable16 %>% summarise(count = sd(BMXBMI)) #standard deviation of participants by BMI (total sample)

#total calorie intake
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(DR1TKCAL)) #counts participants with and without cognitive deficit by mean calorie intake
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(DR1TKCAL)) #counts participants with and without cognitive deficit by standard deviation of calorie intake
DescTable16 %>% summarise(count = mean(DR1TKCAL)) #mean calorie intake of participants (total sample)
DescTable16 %>% summarise(count = sd(DR1TKCAL)) #standard deviation of participants by caloria intake (total sample)

#exercise
DescTable16 %>% group_by(Exercise) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by exercise (enough or not)

#hypertension
DescTable16 %>% group_by(HipertensaoYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by hypertension (yes or no)

#diabetes
DescTable16 %>% group_by(DiabetesYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by diabetes (yes or no)

#alcohol consumption
DescTable16 %>% group_by(AlcoholYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by alcohol consumption (yes or no)

#smoking
DescTable16 %>% group_by(SmokeYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by smiking status (current, former, never)

#memory score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(SumCerad)) #counts participants with and without cognitive deficit by mean memory score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(SumCerad)) #counts participants with and without cognitive deficit by standard deviation of memory score
DescTable16 %>% summarise(count = mean(SumCerad)) #mean memory score of participants (total sample)
DescTable16 %>% summarise(count = sd(SumCerad)) #standard deviation of participants by memory score (total sample)

#delayed memory score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(CFDCSR)) #counts participants with and without cognitive deficit by mean delayed memory score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(CFDCSR)) #counts participants with and without cognitive deficit by standard deviation of delayed memory score
DescTable16 %>% summarise(count = mean(CFDCSR)) #mean delayed memory score of participants (total sample)
DescTable16 %>% summarise(count = sd(CFDCSR)) #standard deviation of participants by delayed memory score (total sample)

#verbal fluency score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(CFDAST)) #counts participants with and without cognitive deficit by mean verbal fluency score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(CFDAST)) #counts participants with and without cognitive deficit by standard deviation of verbal fluency score
DescTable16 %>% summarise(count = mean(CFDAST)) #mean verbal fluency score of participants (total sample)
DescTable16 %>% summarise(count = sd(CFDAST)) #standard deviation of participants by verbal fluency score (total sample)

#digit symbol score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(CFDDS)) #counts participants with and without cognitive deficit by mean digit symbol score
DescTable16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(CFDDS)) #counts participants with and without cognitive deficit by standard deviation of digit symbol score
DescTable16 %>% summarise(count = mean(CFDDS)) #mean digit symbol score of participants (total sample)
DescTable16 %>% summarise(count = sd(CFDDS)) #standard deviation of participants by digit symbol score (total sample)

