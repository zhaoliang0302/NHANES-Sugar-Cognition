library(dplyr)

nhanes <- read.csv("~/Path/nhanes.csv", header=TRUE) #open file

#Calculate zscore
ResultadosDemo18$ZscoreCerad <- scale(ResultadosDemo18$SumCerad) #Z-score of SumCerad
ResultadosDemo18$ZscoreCeradDelayed <- scale (ResultadosDemo18$CFDCSR) #Z-score of delayed recall
ResultadosDemo18$ZscoreAnimal <- scale (ResultadosDemo18$CFDAST) #Z-score of animal fluency 
ResultadosDemo18$ZscoreDigitSymbol <- scale (ResultadosDemo18$CFDDS) #Z-score of digit symbol 
ResultadosDemo19 <- ResultadosDemo18 %>% rowwise() %>% mutate(MeanCerad = mean(c(ZscoreCerad,ZscoreCeradDelayed)))  #mean z-score of all Cerad tests
ResultadosDemo19$ZscoreCeradTotal <- scale(ResultadosDemo19$MeanCerad) #z-score of MeanCerad
ResultadosDemo20 <- ResultadosDemo19 %>% rowwise() %>% mutate(MeanCognicao = mean(c(ZscoreCerad,ZscoreCeradDelayed, ZscoreAnimal, ZscoreDigitSymbol))) #mean z-score of all cognitive tests
ResultadosDemo20$ZscoreCognicao <- scale(ResultadosDemo20$MeanCognicao) #zscore of MeanCognicao (global cognitive score)
write.csv(ResultadosDemo20, file="~/Path/ResultadosDemo20.csv") #save


#Divide Cognition into quartiles by age intervals
TabelaBinarios2 <- ResultadosDemo20 %>% group_by(Idade10) %>% mutate(QuartilCerad = ntile(ZscoreCeradTotal, 4)) #creates quartiles of Cerad according to age 
TabelaBinarios3 <- TabelaBinarios2 %>% group_by(Idade10) %>% mutate(QuartilAnimal = ntile(ZscoreAnimal, 4)) #creates quartiles of Animal Fluency according to age
TabelaBinarios4 <- TabelaBinarios3 %>% group_by(Idade10) %>% mutate(QuartilDigitSymbol = ntile(ZscoreDigitSymbol, 4)) #creates quartiles of Digit Symbol according to age
TabelaBinarios5 <- TabelaBinarios4 %>% group_by(Idade10) %>% mutate(QuartilCognicao = ntile(ZscoreCognicao, 4)) #creates quartiles of global cognition according to age

#Binary: 1 poor cognitive performance (below 25% quartile) and 0 normal cognitive performance (above 25% quartile)
TabelaBinarios6 <- TabelaBinarios5 %>% rowwise() %>% mutate(CeradBinario = ifelse(QuartilCerad == 1, 1, ifelse(QuartilCerad > 1, 0, NA))) "memory
TabelaBinarios7 <- TabelaBinarios6 %>% rowwise() %>% mutate(AnimalBinario = ifelse(QuartilAnimal == 1, 1, ifelse(QuartilAnimal > 1, 0, NA))) #animal fluency
TabelaBinarios8 <- TabelaBinarios7 %>% rowwise() %>% mutate(DigitSymbolBinario = ifelse(QuartilDigitSymbol == 1, 1, ifelse(QuartilDigitSymbol > 1, 0, NA))) #digit symbol
TabelaBinarios9 <- TabelaBinarios8 %>% rowwise() %>% mutate(CognicaoBinario = ifelse(QuartilCognicao == 1, 1, ifelse(QuartilCognicao > 1, 0, NA))) #global cognition
write.csv(TabelaBinarios9, file="~/Path/TabelaBinarios9.csv") #saves

#merge full dataset
TabelaNova <- subset(TabelaAcucar2, select = -c(ZscoreCerad,ZscoreCeradDelayed, ZscoreAnimal, ZscoreDigitSymbol, ZscoreCeradTotal, ZscoreCognicao, CeradBinario,AnimalBinario, DigitSymbolBinario, CognicaoBinario))#drop columns with zscore
TabelaNova2 <- merge(x = TabelaNova, y = TabelaBinarios9[, c("SEQN", "ZscoreCerad","ZscoreCeradDelayed", "ZscoreAnimal", "ZscoreDigitSymbol", "ZscoreCeradTotal", "ZscoreCognicao", "CeradBinario", "AnimalBinario", "DigitSymbolBinario", "CognicaoBinario")], by= "SEQN", all.x = TRUE)
write.csv(TabelaBinarios9, file="~/Path/TabelaBinarios9.csv") #saves
