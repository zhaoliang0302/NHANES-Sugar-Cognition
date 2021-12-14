library(survey)

#quasibinomial test acommodates weighted data

#Memory tests
#Unadjusted
RLCeradBinarioSugarTercil <- svyglm(CeradBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLCeradBinarioSugarTercil)
saveRDS(RLCeradBinarioSugarTercil, file = "~/Path/RLCeradBinarioSugarTercil.rds")

#Model 1
RLCeradBinarioSugarTercilD <- svyglm(CeradBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)
summary(RLCeradBinarioSugarTercilD)
saveRDS(RLCeradBinarioSugarTercilD , file = "~/Path/RLCeradBinarioSugarTercilD.rds")

#Model 2
RLCeradBinarioSugarTercilDC <- svyglm(CeradBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLCeradBinarioSugarTercilDC)
saveRDS(RLCeradBinarioSugarTercilDC , file = "~/Path/RLCeradBinarioSugarTercilDC.rds")


#Verbal Fluency
#Unadjusted
RLAnimalBinarioSugarTercil <- svyglm(AnimalBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLAnimalBinarioSugarTercil)
saveRDS(RLAnimalBinarioSugarTercil, file = "~/Path/RLAnimalBinarioSugarTercil.rds")

#Model 1
RLAnimalBinarioSugarTercilD <- svyglm(AnimalBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)
summary(RLAnimalBinarioSugarTercilD)
saveRDS(RLAnimalBinarioSugarTercilD , file = "~/Path/RLAnimalBinarioSugarTercilD.rds")

#Model 2
RLAnimalBinarioSugarTercilDC <- svyglm(AnimalBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLAnimalBinarioSugarTercilDC)
saveRDS(RLAnimalBinarioSugarTercilDC , file = "~/Path/RLAnimalBinarioSugarTercilDC.rds")


#Digit Symbol
#Unadjusted
RLDigitSymbolBinarioSugarTercil <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLDigitSymbolBinarioSugarTercil)
saveRDS(RLDigitSymbolBinarioSugarTercil, file = "~/Path/RLDigitSymbolBinarioSugarTercil.rds")

#Model 1
RLDigitSymbolBinarioSugarTercilD <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial,
design = survey19)
summary(RLDigitSymbolBinarioSugarTercilD)
saveRDS(RLDigitSymbolBinarioSugarTercilD , file = "~/Path/RLDigitSymbolBinarioSugarTercilD.rds")

#Model 2
RLDigitSymbolBinarioSugarTercilDC <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLDigitSymbolBinarioSugarTercilDC)
saveRDS(RLDigitSymbolBinarioSugarTercilDC , file = "~/Path/RLDigitSymbolBinarioSugarTercilDC.rds")


#Global cognition
#Unadjusted
RLCognicaoBinarioSugarTercil <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19)
summary(RLCognicaoBinarioSugarTercil)
saveRDS(RLCognicaoBinarioSugarTercil, file = "~/Path/RLCognicaoBinarioSugarTercil.rds")

#Model 1
RLCognicaoBinarioSugarTercilD <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19)
summary(RLCognicaoBinarioSugarTercilD)
saveRDS(RLCognicaoBinarioSugarTercilD , file = "~/Path/RLCognicaoBinarioSugarTercilD.rds")

#Model 2
RLCognicaoBinarioSugarTercilDC <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil)+Gender+RIDAGEYR+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)
summary(RLCognicaoBinarioSugarTercilDC)
saveRDS(RLCognicaoBinarioSugarTercilDC , file = "~/Path/RLCognicaoBinarioSugarTercilDC.rds")
