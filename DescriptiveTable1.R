library(dplyr)

nhanes13 <- read.csv("~/Path/nhanes13.csv) #opens file

#subsets data
DescStat <- subset(nhanes13,  RIDAGEYR > 59) #keeps age > 59 years
DescStat2 <- subset(DescStat, MCQ160F == 2) #deletes who had stroke
DescStat3 <- subset(DescStat2, DR1DRSTZ == 1) #complete food and nutrients data
DescStat4 <- subset(DescStat3, DR1TKCAL > 500 & DR1TKCAL < 10000) #deletes extreme calorie intake
DescStat5 <- subset(DescStat4, DMDEDUC2 <= 5) #complete education data
DescStat6 <- subset(DescStat5, DMDMARTL <= 6) #complete marital status data
DescStat7 <- subset(DescStat6, BMXBMI <= 83) #complete anthropometry measures data
DescStat8 <- subset(DescStat7, DIQ010 <= 2) #complete diabetes data
DescStat9 <- subset(DescStat8, BPQ020 <= 2) #complete hypertension data
DescStat10 <- subset(DescStat9, AlcoholYN == 0 |  AlcoholYN == 1) #complete alcohol  consumption data
DescStat11 <- subset(DescStat10, SmokeYN == "never" | SmokeYN == "current" | SmokeYN == "former") #complete smking data
DescStat12 <- subset(DescStat11, Exercise >= 0) #complete exercise data
DescStat13 <- subset(DescStat12, ZscoreCeradTotal < 3) #complete memory tests
DescStat14 <- subset(DescStat13, ZscoreAnimal < 5) #complete animal fluency tests
DescStat15 <- subset(DescStat14,  ZscoreDigitSymbol < 4) #complete digit symbol tests
DescStat16 <- subset(DescStat15,  ZscoreCognicao < 4) #complete cognition tests

write.csv(DescStat16, file="~/Path/DescStat16.csv") #saves


#Absolute numbers
#age
DescStat16 %>% group_by(RIDAGEYR) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by age
DescStat16 %>% summarise(count = mean(RIDAGEYR)) #mean age of participants (total sample)
DescStat16 %>% summarise(count = sd(RIDAGEYR)) #standard deviation of participants by age (total sample)

#gender
DescStat16 %>% group_by(Gender) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by gender

#race
DescStat16 %>% group_by(Raca) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by race

#education
DescStat16 %>% group_by(Education) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by education attainment

#marital status
DescStat16 %>% group_by(Marital) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by marital status

#BMI
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(BMXBMI)) #counts participants with and without cognitive deficit by mean BMI
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(BMXBMI)) #counts participants with and without cognitive deficit by standard deviation of BMI
DescStat16 %>% summarise(count = mean(BMXBMI)) #mean BMI of participants (total sample)
DescStat16 %>% summarise(count = sd(BMXBMI)) #standard deviation of participants by BMI (total sample)

#total calorie intake
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(DR1TKCAL)) #counts participants with and without cognitive deficit by mean calorie intake
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(DR1TKCAL)) #counts participants with and without cognitive deficit by standard deviation of calorie intake
DescStat16 %>% summarise(count = mean(DR1TKCAL)) #mean calorie intake of participants (total sample)
DescStat16 %>% summarise(count = sd(DR1TKCAL)) #standard deviation of participants by caloria intake (total sample)

#exercise
DescStat16 %>% group_by(Exercise) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by exercise (enough or not)

#hypertension
DescStat16 %>% group_by(HipertensaoYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by hypertension (yes or no)

#diabetes
DescStat16 %>% group_by(DiabetesYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by diabetes (yes or no)

#alcohol consumption
DescStat16 %>% group_by(AlcoholYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by alcohol consumption (yes or no)

#smoking
DescStat16 %>% group_by(SmokeYN) %>% count (CognicaoBinario) #counts participants with and without cognitive deficit by smiking status (current, former, never)

#memory score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(SumCerad)) #counts participants with and without cognitive deficit by mean memory score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(SumCerad)) #counts participants with and without cognitive deficit by standard deviation of memory score
DescStat16 %>% summarise(count = mean(SumCerad)) #mean memory score of participants (total sample)
DescStat16 %>% summarise(count = sd(SumCerad)) #standard deviation of participants by memory score (total sample)

#delayed memory score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(CFDCSR)) #counts participants with and without cognitive deficit by mean delayed memory score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(CFDCSR)) #counts participants with and without cognitive deficit by standard deviation of delayed memory score
DescStat16 %>% summarise(count = mean(CFDCSR)) #mean delayed memory score of participants (total sample)
DescStat16 %>% summarise(count = sd(CFDCSR)) #standard deviation of participants by delayed memory score (total sample)

#verbal fluency score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(CFDAST)) #counts participants with and without cognitive deficit by mean verbal fluency score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(CFDAST)) #counts participants with and without cognitive deficit by standard deviation of verbal fluency score
DescStat16 %>% summarise(count = mean(CFDAST)) #mean verbal fluency score of participants (total sample)
DescStat16 %>% summarise(count = sd(CFDAST)) #standard deviation of participants by verbal fluency score (total sample)

#digit symbol score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = mean(CFDDS)) #counts participants with and without cognitive deficit by mean digit symbol score
DescStat16 %>% group_by(CognicaoBinario) %>% summarise(count = sd(CFDDS)) #counts participants with and without cognitive deficit by standard deviation of digit symbol score
DescStat16 %>% summarise(count = mean(CFDDS)) #mean digit symbol score of participants (total sample)
DescStat16 %>% summarise(count = sd(CFDDS)) #standard deviation of participants by digit symbol score (total sample)

