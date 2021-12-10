library(dplyr)

nhanes12 <- read.csv("~/Path/nhanes12.csv", header=TRUE) #open file
cognition <- nhanes12 #renames

#Calculate zscore
cognition$ZscoreCerad <- scale(cognition$SumCerad) #Z-score of SumCerad
cognition$ZscoreCeradDelayed <- scale (cognition$CFDCSR) #Z-score of delayed recall
cognition$ZscoreAnimal <- scale (cognition$CFDAST) #Z-score of animal fluency 
cognition$ZscoreDigitSymbol <- scale (cognition$CFDDS) #Z-score of digit symbol 
cognition2 <- cognition %>% rowwise() %>% mutate(MeanCerad = mean(c(ZscoreCerad,ZscoreCeradDelayed)))  #mean z-score of all Cerad tests
cognition2$ZscoreCeradTotal <- scale(cognition2$MeanCerad) #z-score of MeanCerad
cognition3 <- cognition2 %>% rowwise() %>% mutate(MeanCognicao = mean(c(ZscoreCerad,ZscoreCeradDelayed, ZscoreAnimal, ZscoreDigitSymbol))) #mean z-score of all cognitive tests
cognition3$ZscoreCognicao <- scale(cognition3$MeanCognicao) #zscore of MeanCognicao (global cognitive score)
write.csv(cognition3, file="~/Path/cognition3.csv") #save


#Divide Cognition into quartiles by age intervals
cognition4 <- cognition3 %>% group_by(Idade10) %>% mutate(QuartilCerad = ntile(ZscoreCeradTotal, 4)) #memory 
cognition5 <- cognition4 %>% group_by(Idade10) %>% mutate(QuartilAnimal = ntile(ZscoreAnimal, 4)) #animal fluency
cognition6 <- cognition5 %>% group_by(Idade10) %>% mutate(QuartilDigitSymbol = ntile(ZscoreDigitSymbol, 4)) #digit symbol
cognition7 <- cognition6 %>% group_by(Idade10) %>% mutate(QuartilCognicao = ntile(ZscoreCognicao, 4)) #global cognition

#Binary: 1 poor cognitive performance (below 25% quartile) and 0 normal cognitive performance (above 25% quartile)
cognition8 <- cognition7 %>% rowwise() %>% mutate(CeradBinario = ifelse(QuartilCerad == 1, 1, ifelse(QuartilCerad > 1, 0, NA))) #memory
cognition9 <- cognition8 %>% rowwise() %>% mutate(AnimalBinario = ifelse(QuartilAnimal == 1, 1, ifelse(QuartilAnimal > 1, 0, NA))) #animal fluency
cognition10 <- cognition9 %>% rowwise() %>% mutate(DigitSymbolBinario = ifelse(QuartilDigitSymbol == 1, 1, ifelse(QuartilDigitSymbol > 1, 0, NA))) #digit symbol
cognition11 <- cognition10 %>% rowwise() %>% mutate(CognicaoBinario = ifelse(QuartilCognicao == 1, 1, ifelse(QuartilCognicao > 1, 0, NA))) #global cognition
write.csv(cognition11, file="~/Path/cognition11.csv") #saves

#merge to full dataset
nhanes13 <- merge(x = nhanes12, y = cognition11[, c("SEQN", "ZscoreCerad","ZscoreCeradDelayed", "ZscoreAnimal", "ZscoreDigitSymbol", "ZscoreCeradTotal", 
                                                    "ZscoreCognicao", "CeradBinario", "AnimalBinario", "DigitSymbolBinario", "CognicaoBinario")], 
                  by= "SEQN", all.x = TRUE) 
write.csv(nhanes13, file="~/Path/nhanes13.csv") #saves
