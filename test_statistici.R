##TEST CHI QUARO: si usa per vedere se c'è dipendenza tra due variabili, in sto caso guarderemo se c'è dipendeza tra la lingua e il sesso
library(car)
View(SLID)
tab_oss <- table(SLID$language,SLID$sex)
tab_oss
#H0: le due variabili X e Y sono indipendenti
ris<- chisq.test(tab_oss)#p value molot maggiore a 0.05 quindi si accetta l'ipotesi nulla ossia che vi sia indipendenza tra la variabile sesso e lingua
ris #come parametro gli passiamo la tabella a doppia entrata contentente le frequenze osservate

ris$expected #frequenze teoriche, devono essere tutte > 5 affinchè il test sia affidabile
ris$statistics# ti da solo il risultato dela statistica test

