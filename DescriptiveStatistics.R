library(survey)

#descriptive statistics 
#normality test age
shapiro.test(svytable( ~ RIDAGEYR, survey19))
svyhist( ~ RIDAGEYR, survey19)

#normality test BMI
shapiro.test(svytable( ~ BMXBMI, survey19))
svyhist( ~ BMXBMI, survey19)

#notmality test total calories
shapiro.test(svytable( ~ DR1TKCAL, survey19))
svyhist( ~ DR1TKCAL, survey19)

#Wilcoxon age, BMI, TotalKcal 
svyranktest(RIDAGEYR~CognicaoBinario, design = survey19, test = "wilcoxon")
svyranktest(BMXBMI~CognicaoBinario, design = survey19, test = "wilcoxon")
svyranktest(DR1TKCAL~CognicaoBinario, design = survey19, test = "wilcoxon")

#chisquare categorical variables
chisq.test(svytable(RIAGENDR~CognicaoBinario, design = survey19))
chisq.test(svytable(DMDMARTL~CognicaoBinario, design = survey19))
chisq.test(svytable(RIDRETH3~CognicaoBinario, design = survey19))
chisq.test(svytable(DMDEDUC2~CognicaoBinario, design = survey19))
chisq.test(svytable(Exercise~CognicaoBinario, design = survey19))
chisq.test(svytable(HipertensaoYN~CognicaoBinario, design = survey19))
chisq.test(svytable(DiabetesYN~CognicaoBinario, design = survey19))
chisq.test(svytable(AlcoholYN~CognicaoBinario, design = survey19))
chisq.test(svytable(SmokeYNN~CognicaoBinario, design = survey19))

#normality test cognition variables
#memory
shapiro.test(svytable( ~ SumCerad, survey19)) 
svyhist( ~ SumCerad, survey19)

#delayed memory
shapiro.test(svytable( ~ CFDCSR, survey19)) 
svyhist( ~ CFDCSR, survey19)

#verbal fluency
shapiro.test(svytable( ~ CFDAST, survey19)) 
svyhist( ~ CFDAST, survey19)

#digit symbol
shapiro.test(svytable( ~ CFDDS, survey19)) 
svyhist( ~ CFDDS, survey19)

#T test and Wilcoxon
svyranktest(SumCerad~CognicaoBinario, design = survey19, test = "wilcoxon") #memory, non parametric
svyttest(CFDCSR~CognicaoBinario, design = survey19) #delayed memory, parametric
svyranktest(CFDAST~CognicaoBinario, design = survey19, test = "wilcoxon") #verbal fluency, non parametric
svyranktest(CFDDS~CognicaoBinario, design = survey19, test = "wilcoxon") #digit symbol,non parametric

