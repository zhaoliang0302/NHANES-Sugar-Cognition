#merges all datasets

nhanes <- merge(x = Demo8, y = SoftEnergyDrink3[, c("SEQN", "RefriGrams", "RefriBinario")], by= "SEQN", all.x = TRUE) #merge demo8 and SSBs
nhanes2 <- merge(x = nhanes, y = Solids9[, c("SEQN", "SolidsGrams", "SolidsBinario")], by= "SEQN", all.x = TRUE) #merge nhanes and solid desserts
nhanes3 <- merge(x = nhanes2, y = FruitJuices3[, c("SEQN", "JuiceGrams", "JuiceBinario")], by= "SEQN", all.x = TRUE) #merge nhanes2 and 100% fruit juice
nhanes4 <- merge(x = nhanes3, y = Dr1tot2[, c("SEQN", "WTDRD1", "DR1DRSTZ", "DR1TKCAL", "DR1TSUGR")], by= "SEQN", all.x = TRUE) #merge nhanes3 and total nutrients
nhanes5 <- merge(x = nhanes4, y = Bmx[, c("SEQN", "BMXBMI")], by= "SEQN", all.x = TRUE) #merge nhanes4 and anthropometric measures dataset
nhanes6 <- merge(x = nhanes5, y = Alq3[, c("SEQN", "AlcoholYN")], by= "SEQN", all.x = TRUE) #merge nhanes5 and alcohol dataset
nhanes7 <- merge(x = nhanes6, y = Mcq[, c("SEQN", "MCQ160F")], by= "SEQN", all.x = TRUE) #merge nhanes6 and health questionaire (stroke column) dataset
nhanes8 <- merge(x = nhanes7, y = Smq3[, c("SEQN", "SmokeYN")], by= "SEQN", all.x = TRUE) #merge nhanes7 and smoking dataset
nhanes9 <- merge(x = nhanes8, y = Paq11[, c("SEQN", "Exercise")], by= "SEQN", all.x = TRUE) #merge nhanes8 and exercise dataset
nhanes10 <- merge(x = nhanes9, y = Diq2[, c("SEQN", "DiabetesYN")], by= "SEQN", all.x = TRUE) #merge nhanes9 and diabetes dataset
nhanes11 <- merge(x = nhanes10, y = Bpq2[, c("SEQN", "HypertensionYN")], by= "SEQN", all.x = TRUE) #merge nhanes9 and hypertension dataset
nhanes12 <- merge(x = nhanes11, y = Cfq3, by= "SEQN", all.x = TRUE) #merge nhanes9 and hypertension dataset
write.csv(nhanes12, "~/Path/nhanes12.csv") #save
