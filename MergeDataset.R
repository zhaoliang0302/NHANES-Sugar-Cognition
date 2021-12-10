
#merges all datasets
nhanes <- merge(x = Demo7, y = Mcq[, c("SEQN", "MCQ160F")], by= "SEQN", all.x = TRUE) #merges Demo and Mcq (stroke column) datasets
nhanes2 <- merge(x = nhanes, y = SoftEnergyDrink3[, c("SEQN", "RefriGrams", "RefriBinario")], by= "SEQN", all.x = TRUE) #merges nhanes and SSBs datasets
nhanes3 <- merge(x = nhanes2, y = Solids9[, c("SEQN", "SolidsGrams", "SolidsBinario")], by= "SEQN", all.x = TRUE) #merges nhanes2 and solid desserts datasets
nhanes4 <- merge(x = nhanes3, y = FruitJuices3[, c("SEQN", "JuiceGrams", "JuiceBinario")], by= "SEQN", all.x = TRUE) #merges nhanes3 and 100% fruit juice datasets
nhanes5 <- merge(x = nhanes4, y = Dr1tot2[, c("SEQN", "WTDRD1", "DR1DRSTZ", "DR1TKCAL", "AcucarTercil")], by= "SEQN", all.x = TRUE) #merges nhanes4 and total nutrients datasets
nhanes6 <- merge(x = nhanes5, y = Bmx[, c("SEQN", "BMXBMI")], by= "SEQN", all.x = TRUE) #merges nhanes5 and anthropometric measures datasets
nhanes7 <- merge(x = nhanes6, y = Alq3[, c("SEQN", "AlcoholYN")], by= "SEQN", all.x = TRUE) #merges nhanes6 and alcohol datasets
nhanes8 <- merge(x = nhanes7, y = Smq3[, c("SEQN", "SmokeYN")], by= "SEQN", all.x = TRUE) #merges nhanes7 and smoking datasets
nhanes9 <- merge(x = nhanes8, y = Paq11[, c("SEQN", "Exercise")], by= "SEQN", all.x = TRUE) #merges nhanes8 and exercise datasets
nhanes10 <- merge(x = nhanes9, y = Diq2[, c("SEQN", "DiabetesYN")], by= "SEQN", all.x = TRUE) #merges nhanes9 and diabetes datasets
nhanes11 <- merge(x = nhanes10, y = Bpq2[, c("SEQN", "HypertensionYN")], by= "SEQN", all.x = TRUE) #merges nhanes10 and hypertension datasets
nhanes12 <- merge(x = nhanes11, y = Cfq3[, c("SEQN", "CFASTAT", "CFDCCS", "CFDCST1", "CFDCST2", "CFDCST3", "CFDCSR", "CFDCIT1", "CFDCIT2", "CFDCIT3",
"CFDCIR", "CFDAPP", "CFDAST", "CFDDPP", "CFDDS", "SumCerad")], by= "SEQN", all.x = TRUE) #merges nhanes11 and cognition datasets
write.csv(nhanes12, file="~/Path/nhanes12.csv") #saves
