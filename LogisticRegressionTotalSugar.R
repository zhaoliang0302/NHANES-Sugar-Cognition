library(survey)
library(questionr)

#quasibinomial test acommodates weighted data

#Memory tests
#Unadjusted
RLCeradBinarioSugarTercil <- svyglm(CeradBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLCeradBinarioSugarTercil)
odds.ratio(RLCeradBinarioSugarTercil, level=0.95)
saveRDS(RLCeradBinarioSugarTercil, file = "~/Path/RLCeradBinarioSugarTercil.rds")

#Model 1
RLCeradBinarioSugarTercilD <- svyglm(CeradBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)
summary(RLCeradBinarioSugarTercilD)
odds.ratio(RLCeradBinarioSugarTercilD, level=0.95)
saveRDS(RLCeradBinarioSugarTercilD , file = "~/Path/RLCeradBinarioSugarTercilD.rds")

#Model 2
RLCeradBinarioSugarTercilDC <- svyglm(CeradBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLCeradBinarioSugarTercilDC)
odds.ratio(RLCeradBinarioSugarTercilDC, level=0.95)
saveRDS(RLCeradBinarioSugarTercilDC , file = "~/Path/RLCeradBinarioSugarTercilDC.rds")


#Verbal Fluency
#Unadjusted
RLAnimalBinarioSugarTercil <- svyglm(AnimalBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLAnimalBinarioSugarTercil)
odds.ratio(RLAnimalBinarioSugarTercil, level=0.95)
saveRDS(RLAnimalBinarioSugarTercil, file = "~/Path/RLAnimalBinarioSugarTercil.rds")

#Model 1
RLAnimalBinarioSugarTercilD <- svyglm(AnimalBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)
summary(RLAnimalBinarioSugarTercilD)
odds.ratio(RLAnimalBinarioSugarTercilD, level=0.95)
saveRDS(RLAnimalBinarioSugarTercilD , file = "~/Path/RLAnimalBinarioSugarTercilD.rds")

#Model 2
RLAnimalBinarioSugarTercilDC <- svyglm(AnimalBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLAnimalBinarioSugarTercilDC)
odds.ratio(RLAnimalBinarioSugarTercilDC, level=0.95)
saveRDS(RLAnimalBinarioSugarTercilDC , file = "~/Path/RLAnimalBinarioSugarTercilDC.rds")


#Digit Symbol
#Unadjusted
RLDigitSymbolBinarioSugarTercil <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLDigitSymbolBinarioSugarTercil)
odds.ratio(RLDigitSymbolBinarioSugarTercil, level=0.95)
saveRDS(RLDigitSymbolBinarioSugarTercil, file = "~/Path/RLDigitSymbolBinarioSugarTercil.rds")

#Model 1
RLDigitSymbolBinarioSugarTercilD <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial,
design = survey19)
summary(RLDigitSymbolBinarioSugarTercilD)
odds.ratio(RLDigitSymbolBinarioSugarTercilD, level=0.95)
saveRDS(RLDigitSymbolBinarioSugarTercilD , file = "~/Path/RLDigitSymbolBinarioSugarTercilD.rds")

#Model 2
RLDigitSymbolBinarioSugarTercilDC <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLDigitSymbolBinarioSugarTercilDC)
odds.ratio(RLDigitSymbolBinarioSugarTercilDC, level=0.95)
saveRDS(RLDigitSymbolBinarioSugarTercilDC , file = "~/Path/RLDigitSymbolBinarioSugarTercilDC.rds")


#Global cognition
#Unadjusted
RLCognicaoBinarioSugarTercil <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLCognicaoBinarioSugarTercil)
odds.ratio(RLCognicaoBinarioSugarTercil, level=0.95)
saveRDS(RLCognicaoBinarioSugarTercil, file = "~/Path/RLCognicaoBinarioSugarTercil.rds")

#Model 1
RLCognicaoBinarioSugarTercilD <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)
summary(RLCognicaoBinarioSugarTercilD)
odds.ratio(RLCognicaoBinarioSugarTercilD, level=0.95)
saveRDS(RLCognicaoBinarioSugarTercilD , file = "~/Path/RLCognicaoBinarioSugarTercilD.rds")

#Model 2
RLCognicaoBinarioSugarTercilDC <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLCognicaoBinarioSugarTercilDC)
odds.ratio(RLCognicaoBinarioSugarTercilDC, level=0.95)
saveRDS(RLCognicaoBinarioSugarTercilDC , file = "~/Path/RLCognicaoBinarioSugarTercilDC.rds")
