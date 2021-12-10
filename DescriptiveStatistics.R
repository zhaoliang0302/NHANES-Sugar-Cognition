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
