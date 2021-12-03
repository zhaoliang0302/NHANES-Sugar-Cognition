library(dplyr)

#Creates new dataset combining 2011-2012 and 2013-2014 datasets
Demo <- bind_rows(DEMO_G, DEMO_H) #Demo datasets
Dr1iff <- bind_rows(DR1IFF_G, DR1IFF_H) #Diet datasets
Dr1tot <- bind_rows(DR1TOT_G, DR1TOT_H) #Total diet datasets
Bmx <- bind_rows(BMX_G, BMX_H) #Anthropometric measures datasets
Alq <- bind_rows(ALQ_G, ALQ_H) #Alcohol datasets
Cfq <- bind_rows(CFQ_G, CFQ_H) #Cognition datasets
Mcq <- bind_rows(MCQ_G, MCQ_H) #Health status datasets
Smq <- bind_rows(SMQ_G, SMQ_H) #Smoking datasets
Paq <- bind_rows(PAQ_G, PAQ_H) #Exercise datasets
Diq <- bind_rows(DIQ_G, DIQ_H) #Diabetes datasets
Bpq <- bind_rows(BPQ_G, BPQ_H) #Hypertension datasets

#Saves new dataset
write.csv(Demo, file="Path/Demo.csv") 
write.csv(Dr1iff, file="Path/Demo.csv") 
write.csv(Dr1tot, file="Path/Demo.csv")
write.csv(Bmx, file="Path/Demo.csv")
write.csv(Alq, file="Path/Demo.csv")
write.csv(Cfq, file="Path/Demo.csv")
write.csv(Mcq, file="Path/Demo.csv")
write.csv(Smq, file="Path/Demo.csv")
write.csv(Paq, file="Path/Demo.csv")
write.csv(Diq, file="Path/Demo.csv")
write.csv(Bpq, file="Path/Demo.csv")
