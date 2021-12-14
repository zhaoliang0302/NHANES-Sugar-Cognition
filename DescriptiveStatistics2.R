library(survey)

#statistics (weighted, not adjusted)
#normality test age
shapiro.test(svytable( ~ RIDAGEYR, DescStat16))
svyhist( ~ RIDAGEYR, DescStat16)

#normality test BMI
shapiro.test(svytable( ~ BMXBMI, DescStat16))
svyhist( ~ BMXBMI, DescStat16)

#notmality test total calories
shapiro.test(svytable( ~ DR1TKCAL, DescStat16))
svyhist( ~ DR1TKCAL, DescStat16)

#Wilcoxon age, BMI, TotalKcal 
svyranktest(RIDAGEYR~CognicaoBinario, design = Acucar34, test = "wilcoxon")
svyranktest(BMXBMI~CognicaoBinario, design = Acucar34, test = "wilcoxon")
svyranktest(DR1TKCAL~CognicaoBinario, design = Acucar34, test = "wilcoxon")

#chisquare categorical variables
chisq.test(svytable(RIAGENDR~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(DMDMARTL~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(RIDRETH3~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(DMDEDUC2~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(Exercise~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(HipertensaoYN~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(DiabetesYN~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(AlcoholYN~CognicaoBinario, design = ResultadosDemo36))
chisq.test(svytable(SmokeYNN~CognicaoBinario, design = ResultadosDemo36))

#normality test cognition variables
#memory
shapiro.test(svytable( ~ SumCerad, DescStat16)) 
svyhist( ~ SumCerad, DescStat16)

#delayed memory
shapiro.test(svytable( ~ CFDCSR, DescStat16)) 
svyhist( ~ CFDCSR, DescStat16)

#verbal fluency
shapiro.test(svytable( ~ CFDAST, DescStat16)) 
svyhist( ~ CFDAST, DescStat16)

#digit symbol
shapiro.test(svytable( ~ CFDDS, DescStat16)) 
svyhist( ~ CFDDS, DescStat16)

#T test and Wilcoxon
svyranktest(SumCerad~CognicaoBinario, design = Acucar34, test = "wilcoxon") #memory, non parametric
svyttest(CFDCSR~CognicaoBinario, design = Acucar34) #delayed memory, parametric
svyranktest(CFDAST~CognicaoBinario, design = Acucar34, test = "wilcoxon") #verbal fluency, non parametric
svyranktest(CFDDS~CognicaoBinario, design = Acucar34, test = "wilcoxon") #digit symbol,non parametric

