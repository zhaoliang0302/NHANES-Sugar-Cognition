library(survey)
library(questionr)

#Cerad SSBs Unadjusted
RLCeradBinarioRefriBinarioNovo <- svyglm(CeradBinario ~ RefriBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioRefriBinarioNovo) #results
odds.ratio(RLCeradBinarioRefriBinarioNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioRefriBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioRefriBinarioNovo.rds")  #save model

#Cerad Solid desserts unadjusted
RLCeradBinarioSolidsBinarioNovo <- svyglm(CeradBinario ~ SolidsBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioSolidsBinarioNovo) #results
odds.ratio(RLCeradBinarioSolidsBinarioNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioSolidsBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioSolidsBinarioNovo.rds") #save model

#Cerad 100% Fruit juice Unadjusted
RLCeradBinarioJuiceBinarioNovo <- svyglm(CeradBinario ~ JuiceBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioJuiceBinarioNovo) #results
odds.ratio(RLCeradBinarioJuiceBinarioNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioJuiceBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioJuiceBinarioNovo.rds") #save model

#Cerad SSBs Model 1 (Adjusted for Demographics + Total Calories)
RLCeradBinarioRefriBinarioDNovo <- svyglm(CeradBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCeradBinarioRefriBinarioDNovo) #results
odds.ratio(RLCeradBinarioRefriBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioRefriBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioRefriBinarioDNovo.rds")  #save model

#Cerad Solid Desserts Model 1 (Adjusted for Demographics + Total Calories)
RLCeradBinarioSolidsBinarioDNovo <- svyglm(CeradBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCeradBinarioSolidsBinarioDNovo) #results
odds.ratio(RLCeradBinarioSolidsBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioSolidsBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioSolidsBinarioDNovo.rds") #save model

#Cerad 100% Fruit juice Model 1 (Adjusted for Demographics + Total Calories)
RLCeradBinarioJuiceBinarioDNovo <- svyglm(CeradBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCeradBinarioJuiceBinarioDNovo) #results
odds.ratio(RLCeradBinarioJuiceBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioJuiceBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioJuiceBinarioDNovo.rds") #save model

#Cerad SSBs Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLCeradBinarioRefriBinarioDCNovo <- svyglm(CeradBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise , family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioRefriBinarioDCNovo) #results
odds.ratio(RLCeradBinarioRefriBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioRefriBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioRefriBinarioDCNovo.rds")  #save model

#Cerad Solid Desserts Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLCeradBinarioSolidsBinarioDCNovo <- svyglm(CeradBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioSolidsBinarioDCNovo) #results
odds.ratio(RLCeradBinarioSolidsBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioSolidsBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioSolidsBinarioDCNovo.rds") #save model

#Cerad 100% Fruit juice Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLCeradBinarioJuiceBinarioDCNovo <- svyglm(CeradBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioJuiceBinarioDCNovo) #results
odds.ratio(RLCeradBinarioJuiceBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLCeradBinarioJuiceBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLCeradBinarioJuiceBinarioDCNovo.rds") #save model


#Animal fluency SSBs Unadjusted
RLAnimalBinarioRefriBinarioNovo <- svyglm(AnimalBinario ~ RefriBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLAnimalBinarioRefriBinarioNovo)  #results
odds.ratio(RLAnimalBinarioRefriBinarioNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioRefriBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioRefriBinarioNovo.rds")  #save model

#Animal fluency Solid desserts Unadjusted
RLAnimalBinarioSolidsBinarioNovo <- svyglm(AnimalBinario ~ SolidsBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLAnimalBinarioSolidsBinarioNovo) #results
odds.ratio(RLAnimalBinarioSolidsBinarioNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioSolidsBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioSolidsBinarioNovo.rds") #save model

#Animal fluency 100% Fruit juice Unadjusted
RLAnimalBinarioJuiceBinarioNovo <- svyglm(AnimalBinario ~ JuiceBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLAnimalBinarioJuiceBinarioNovo) #results
odds.ratio(RLAnimalBinarioJuiceBinarioNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioJuiceBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioJuiceBinarioNovo.rds") #save model

#Animal fluency SSBs Model 1 (Adjusted for Demographics + Total Calories)
RLAnimalBinarioRefriBinarioDNovo <- svyglm(AnimalBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression 
summary(RLAnimalBinarioRefriBinarioDNovo) #results
odds.ratio(RLAnimalBinarioRefriBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioRefriBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioRefriBinarioDNovo.rds")  #save model

#Animal fluency Solid desserts Model 1 (Adjusted for Demographics + Total Calories)
RLAnimalBinarioSolidsBinarioDNovo <- svyglm(AnimalBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)#logistics regression
summary(RLAnimalBinarioSolidsBinarioDNovo) #results
odds.ratio(RLAnimalBinarioSolidsBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioSolidsBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioSolidsBinarioDNovo.rds") #save model

#Animal fluency 100% Fruit juice Model 1 (Adjusted for Demographics + Total Calories)
RLAnimalBinarioJuiceBinarioDNovo <- svyglm(AnimalBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)#logistics regression
summary(RLAnimalBinarioJuiceBinarioDNovo) #results
odds.ratio(RLAnimalBinarioJuiceBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioJuiceBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioJuiceBinarioDNovo.rds") #save model

#Animal fluency SSBs Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLAnimalBinarioRefriBinarioDCNovo <- svyglm(AnimalBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression 
summary(RLAnimalBinarioRefriBinarioDCNovo) #results
odds.ratio(RLAnimalBinarioRefriBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioRefriBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioRefriBinarioDCNovo.rds")   #save model

#Animal fluency Solid Desserts Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLAnimalBinarioSolidsBinarioDCNovo <- svyglm(AnimalBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLAnimalBinarioSolidsBinarioDCNovo) #results
odds.ratio(RLAnimalBinarioSolidsBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioSolidsBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioSolidsBinarioDCNovo.rds") #save model

#Animal fluency 100% Fruit Juice Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLAnimalBinarioJuiceBinarioDCNovo <- svyglm(AnimalBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLAnimalBinarioJuiceBinarioDCNovo) #results
odds.ratio(RLAnimalBinarioJuiceBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLAnimalBinarioJuiceBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLAnimalBinarioJuiceBinarioDCNovo.rds") #save model


#Digit Symbol SSBs Unadjusted
RLDigitSymbolBinarioRefriBinarioNovo <- svyglm(DigitSymbolBinario ~ RefriBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLDigitSymbolBinarioRefriBinarioNovo)  #results
odds.ratio(RLDigitSymbolBinarioRefriBinarioNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioRefriBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioRefriBinarioNovo.rds")   #save model

#Digit Symbol Solid Desserts Unadjusted
RLDigitSymbolBinarioSolidsBinarioNovo <- svyglm(DigitSymbolBinario ~ SolidsBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLDigitSymbolBinarioSolidsBinarioNovo) #results
odds.ratio(RLDigitSymbolBinarioSolidsBinarioNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioSolidsBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioSolidsBinarioNovo.rds") #save model

#Digit Symbol 100% Fruit juice unadjusted
RLDigitSymbolBinarioJuiceBinarioNovo <- svyglm(DigitSymbolBinario ~ JuiceBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLDigitSymbolBinarioJuiceBinarioNovo) #results
odds.ratio(RLDigitSymbolBinarioJuiceBinarioNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioJuiceBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioJuiceBinarioNovo.rds") #save model

#Digit Symbol SSBs Model 1 (Adjusted for Demographics + Total Calories)
RLDigitSymbolBinarioRefriBinarioDNovo <- svyglm(DigitSymbolBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLDigitSymbolBinarioRefriBinarioDNovo)  #results
odds.ratio(RLDigitSymbolBinarioRefriBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioRefriBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioRefriBinarioDNovo.rds")  #save model

#Digit Symbol Solid desserts Model 1 (Adjusted for Demographics + Total Calories)
RLDigitSymbolBinarioSolidsBinarioDNovo <- svyglm(DigitSymbolBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLDigitSymbolBinarioSolidsBinarioDNovo) #results
odds.ratio(RLDigitSymbolBinarioSolidsBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioSolidsBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioSolidsBinarioDNovo.rds") #save model

#Digit Symbol 100% Fruit juice Model 1 (Adjusted for Demographics + Total Calories)
RLDigitSymbolBinarioJuiceBinarioDNovo <- svyglm(DigitSymbolBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLDigitSymbolBinarioJuiceBinarioDNovo) #results
odds.ratio(RLDigitSymbolBinarioJuiceBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioJuiceBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioJuiceBinarioDNovo.rds") #save model

#Digit Symbol SSBs Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLDigitSymbolBinarioRefriBinarioDCNovo <- svyglm(DigitSymbolBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression 
summary(RLDigitSymbolBinarioRefriBinarioDCNovo) #results
odds.ratio(RLDigitSymbolBinarioRefriBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioRefriBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioRefriBinarioDCNovo.rds")  #save model

#Digit Symbol Solid desserts Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLDigitSymbolBinarioSolidsBinarioDCNovo <- svyglm(DigitSymbolBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLDigitSymbolBinarioSolidsBinarioDCNovo) #results
odds.ratio(RLDigitSymbolBinarioSolidsBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioSolidsBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioSolidsBinarioDCNovo.rds") #save model

#Digit Symbol 100% Fruit Juice Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLDigitSymbolBinarioJuiceBinarioDCNovo <- svyglm(DigitSymbolBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLDigitSymbolBinarioJuiceBinarioDCNovo) #results
odds.ratio(RLDigitSymbolBinarioJuiceBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLDigitSymbolBinarioJuiceBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLDigitSymbolBinarioJuiceBinarioDCNovo.rds") #save model


#Global cognition SSBs Unadjusted
RLCognicaoBinarioRefriBinarioNovo <- svyglm(CognicaoBinario ~ RefriBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLCognicaoBinarioRefriBinarioNovo) #results
odds.ratio(RLCognicaoBinarioRefriBinarioNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioRefriBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioRefriBinarioNovo.rds") #save model

#Global cognition Solid Desserts Unadjusted
RLCognicaoBinarioSolidsBinarioNovo <- svyglm(CognicaoBinario ~ SolidsBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLCognicaoBinarioSolidsBinarioNovo) #results
odds.ratio(RLCognicaoBinarioSolidsBinarioNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioSolidsBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioSolidsBinarioNovo.rds") #save model

#Global cognition 100% Fruit Juice Unadjusted
RLCognicaoBinarioJuiceBinarioNovo <- svyglm(CognicaoBinario ~ JuiceBinario, family = quasibinomial, design = survey19) #logistics regression
summary(RLCognicaoBinarioJuiceBinarioNovo) #results
odds.ratio(RLCognicaoBinarioJuiceBinarioNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioJuiceBinarioNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioJuiceBinarioNovo.rds") #save model

#Global Cognition SSBs Model 1 (Adjusted for Demographics + Total Calories)
RLCognicaoBinarioRefriBinarioDNovo <- svyglm(CognicaoBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCognicaoBinarioRefriBinarioDNovo) #results
odds.ratio(RLCognicaoBinarioRefriBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioRefriBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioRefriBinarioDNovo.rds")  #save model

#Global Cognition Solid desserts Model 1 (Adjusted for Demographics + Total Calories)
RLCognicaoBinarioSolidsBinarioDNovo <- svyglm(CognicaoBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCognicaoBinarioSolidsBinarioDNovo) #results
odds.ratio(RLCognicaoBinarioSolidsBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioSolidsBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioSolidsBinarioDNovo.rds") #save model

#Global Cognition 100% Fruit Juice Model 1 (Adjusted for Demographics + Total Calories)
RLCognicaoBinarioJuiceBinarioDNovo <- svyglm(CognicaoBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCognicaoBinarioJuiceBinarioDNovo) #results
odds.ratio(RLCognicaoBinarioJuiceBinarioDNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioJuiceBinarioDNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioJuiceBinarioDNovo.rds") #save model

#Global Cognition SSBs Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLCognicaoBinarioRefriBinarioDCNovo <- svyglm(CognicaoBinario ~ RefriBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression 
summary(RLCognicaoBinarioRefriBinarioDCNovo) #results
odds.ratio(RLCognicaoBinarioRefriBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioRefriBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioRefriBinarioDCNovo.rds")   #save model

#Global Cognition Solid desserts Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLCognicaoBinarioSolidsBinarioDCNovo <- svyglm(CognicaoBinario ~ SolidsBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLCognicaoBinarioSolidsBinarioDCNovo) #results
odds.ratio(RLCognicaoBinarioSolidsBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioSolidsBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioSolidsBinarioDCNovo.rds") #save model

#Global Cognition 100% fruit juice Model 2 (Adjusted for Demographics + Total Calories + Clinical Variables)
RLCognicaoBinarioJuiceBinarioDCNovo <- svyglm(CognicaoBinario ~ JuiceBinario+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLCognicaoBinarioJuiceBinarioDCNovo) #results
odds.ratio(RLCognicaoBinarioJuiceBinarioDCNovo, level=0.95) #odds ratio
saveRDS(RLCognicaoBinarioJuiceBinarioDCNovo, file = "~/Documents/PosDoc/Acucar/RLCognicaoBinarioJuiceBinarioDCNovo.rds") #save model

