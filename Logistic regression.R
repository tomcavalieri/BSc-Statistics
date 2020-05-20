dati <- read.table("datiCoppie.csv", header = TRUE, sep = ";")
head(dati)

dati$eta.factor <- as.factor(dati$ETA)

levels(dati$eta.factor) <- c(rep("65-69", 5), rep("70-74", 5), rep("75-79", 5), rep("80-84", 5), rep("85+", 12))


dati$sesso.factor <- as.factor(dati$SESSO)

levels(dati$sesso.factor) <- c("M", "F")


dati$istruzione.factor <- as.factor(dati$ISTRUZIONE)

levels(dati$istruzione.factor) <- c("ALTO", "ALTO", "ALTO", "ALTO", "MEDIO", "MEDIO", "MEDIO", "BASSO", "BASSO", "BASSO")


dati$regione.factor <- as.factor(dati$REGIONE)


levels(dati$regione.factor) <- c("NORD", "NORD", "NORD", "NORD", "NORD", "NORD", "NORD", "CENTRO", "CENTRO", "CENTRO", "CENTRO", "CENTRO", "CENTRO", "SUD", "SUD", "SUD", "SUD", "SUD", "SUD")




dati$reddito.factor <- as.factor(dati$REDDITO)

levels(dati$reddito.factor) <- c("DA LAVORO", "DA LAVORO", "PENSIONE", "ALTRO", "ALTRO", "MANTENIMENTO FAMILIARI")


dati$salute.factor <- as.factor(dati$SALUTE)

levels(dati$salute.factor) <- c("BUONA SALUTE", "BUONA SALUTE", "Nè BENE, Nè MALE", "CATTIVA SALUTE", "CATTIVA SALUTE")


dati$AIUTIDATI[is.na(dati$AIUTIDATI)] = "NO"

dati$aiutodato.factor <- as.factor(dati$AIUTIDATI)

levels(dati$aiutodato.factor) <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0) #1 si, 2 no


dati$AIUTIRICEVUTI[is.na(dati$AIUTIRICEVUTI)] = "NO"


dati$aiutoricevuto.factor <- as.factor(dati$AIUTIRICEVUTI)

levels(dati$aiutoricevuto.factor) <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)

# ora 0 e 1 sono considerati dati numerici,aiutoricevutonew è il vettore "buono"

as.integer(dati$aiutoricevuto.factor)
dati$aiutoricevutonew <- abs(as.integer(dati$aiutoricevuto.factor) - 2)


dati$vicini.factor <- as.factor(dati$VICINI)

levels(dati$vicini.factor) <- c("NO", "Sì", "Sì")


dati$rete.factor <- as.factor(dati$RETE)

levels(dati$rete.factor) <- c("NESSUNO", "FAMIGLIA E NON", "FAMIGLIA", "FAMIGLIA", "FAMIGLIA E NON", "FAMIGLIA E NON", "FAMIGLIA", "NO FAMIGLIA")


table(dati$eta.factor)
table(dati$istruzione.factor)
table(dati$regione.factor)
table(dati$reddito.factor)
table(dati$salute.factor)
table(dati$aiutodato.factor)
table(dati$aiutoricevuto.factor)

# no plot reddito
table(dati$sesso.factor, dati$aiutoricevuto.factor)
plot(dati$eta.factor, dati$aiutoricevuto.factor)
plot(dati$istruzione.factor, dati$aiutoricevuto.factor)
plot(dati$regione.factor, dati$aiutoricevuto.factor)

#modifichiamo regione
dati$regione1.factor <- as.factor(dati$REGIONE)

levels(dati$regione1.factor) <- c("NORD", "NORD", "NORD", "NORD", "NORD", "NORD", "NORD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD", "CENTRO E SUD")

plot(dati$regione1.factor, dati$salute.factor)

plot(dati$salute.factor, dati$aiutoricevuto.factor)
plot(dati$rete.factor, dati$aiutoricevuto.factor)

#modifica variabile rete
dati$rete1.factor <- as.factor(dati$RETE)

levels(dati$rete1.factor) <- c("POCHI", "TANTI", "POCHI", "POCHI", "TANTI", "TANTI", "POCHI", "POCHI")

plot(dati$rete1.factor, dati$aiutoricevuto.factor)

plot(dati[dati$sesso.factor == "F", ]$aiutodato.factor, dati[dati$sesso.factor == "F", ]$aiutoricevuto.factor)


fit.f <- glm(aiutoricevutonew ~ eta.factor + istruzione.factor + regione.factor + reddito.factor + salute.factor + aiutodato.factor+ rete.factor, data = dati[dati$sesso.factor == "F", ], family = binomial(link = logit))
summary(fit.f)

fit.f2 <- glm(aiutoricevutonew ~ eta.factor  + regione.factor  + salute.factor + aiutodato.factor + rete.factor, data = dati[dati$sesso.factor == "F", ], family = binomial(link = logit))
summary(fit.f2) # senza istruzione

fit.fist <- glm(aiutoricevutonew ~ istruzione.factor, data = dati[dati$sesso.factor == "F", ],family = binomial(link = logit))
summary(fit.fist) # istruzione no significativa

fit.fred <- glm(aiutoricevutonew ~ reddito.factor, data = dati[dati$sesso.factor == "F", ],family = binomial(link = logit))
summary(fit.fred) # reddito no significativo

fit.fret1 <- glm(aiutoricevutonew ~ rete1.factor, data = dati[dati$sesso.factor == "F", ],family = binomial(link = logit))
summary(fit.fret1) # la differenza tra "pochi" (nessuno,no fam,fam) è significativa rispetto a "TANTI" (famiglia e non)

fit.2 <- glm(aiutoricevutonew ~ eta.factor + salute.factor + rete1.factor + regione.factor + aiutodato.factor, data = dati[dati$sesso.factor=="F",], family = binomial(link = logit))
summary(fit.2) #AIC 1288.4

fit.4 <- glm(aiutoricevutonew ~ eta.factor + salute.factor + rete1.factor + regione1.factor + aiutodato.factor, data = dati[dati$sesso.factor=="F",], family = binomial(link = logit))
summary(fit.4) #AIC 1286.6

# regione1 (a due livelli) è poco significativo nel modello perchè parte dell'effetto che ha su y è
# spiegato in realtà da diversi stati medi di salute nelle due macro aree (peggiore centro-sud)
# tuttavia, pur essendo significativa al 99% la teniamo perchè il modello risulta migliore in base al criterio AIC



###########################################################
###################MASCHI###################

fit.m <- glm(aiutoricevutonew ~ eta.factor + istruzione.factor + regione.factor + reddito.factor + salute.factor + aiutodato.factor + rete.factor, data = dati[dati$sesso.factor == "M", ], family = binomial(link = logit))
summary(fit.m)

fit.m2 <- glm(aiutoricevutonew ~ eta.factor  + regione.factor  + salute.factor + aiutodato.factor + rete.factor, data = dati[dati$sesso.factor == "M", ], family = binomial(link = logit))
summary(fit.m2) #senza istruzione

fit.mist <- glm(aiutoricevutonew ~ istruzione.factor, data = dati[dati$sesso.factor == "M", ], family = binomial(link = logit))
summary(fit.mist) #istruzione no significativa

fit.mred <- glm(aiutoricevutonew ~ reddito.factor, data = dati[dati$sesso.factor == "M", ], family = binomial(link = logit))
summary(fit.mred) #reddito no significativo

fit.mret1 <- glm(aiutoricevutonew ~ rete1.factor, data = dati[dati$sesso.factor == "M", ], family = binomial(link = logit))
summary(fit.mret1) #La differenza tra "pochi" (nessuno,no fam,fam) è significativa rispetto a "TANTI" (famiglia e non)

fit.3 < -glm(aiutoricevutonew ~ eta.factor + salute.factor + rete1.factor + regione.factor + aiutodato.factor, data = dati[dati$sesso.factor=="M",], family = binomial(link = logit))
summary(fit.3) #AIC 1317.6

fit.5 <- glm(aiutoricevutonew ~ eta.factor + salute.factor + rete1.factor + regione1.factor + aiutodato.factor, data = dati[dati$sesso.factor == "M",], family = binomial(link = logit))
summary(fit.5) #AIC 1316

plot(dati[dati$sesso.factor == "M", ]$aiutodato.factor, dati[dati$sesso.factor == "M", ]$aiutoricevuto.factor)

fit.6 <- glm(aiutoricevutonew ~ eta.factor + salute.factor + rete1.factor + regione1.factor, data = dati[dati$sesso.factor =="M",],family = binomial(link = logit))
summary(fit.6) #AIC 1316.1


  
#############################################################################################
####Analisi per coppie###########


n = length(dati$REGIONE)
regioneC = c(rep (0, n / 2))


j = 1

i = 1

for (j in 1 : (n / 2)) {

  regioneC[j] = dati$regione.factor[i]

  i = i + 2

}

table(regioneC)

# creato la variabile regioneC 1 NORD 2 Centro 3 sud
  
# creo la variabile età (entrambi over 85, uno solo over 85, entrambi sotto gli 85)
  
i = 1

etaC = c(rep ("0", n / 2))
  
for (j in 1 : 1722) {               #n/2 non gli piaceva allora ho messo direttamente 1722
    
  if(dati$ETA[i]>=85 && dati$ETA[i+1]>=85)
     
    {etaC[j]="2over85"}
    
  else if((dati$ETA[i]>=85 && dati$ETA[i+1]<85)|| 
             (dati$ETA[i]<85 && dati$ETA[i+1]>=85))
    {etaC[j]="1over85"}
    
  else 
      {etaC[j]="2under85"}
    
    i = i + 2
  }
  
table(etaC)
etaC
  
dati$salute1.factor <- as.factor(dati$SALUTE)
    
levels(dati$salute1.factor) <- c(2, 2, 1, 0, 0)
  
# buona salute 2 ne bene ne male 1 cattiva salute 0
# nel dubbio ho trasformato il vettore dandogli valori numerici invece di stringhe
  
  
i = 1
saluteC=c(rep ("0", n / 2))
  
for (j in 1 : 1722) {
  
    if(dati$salute1.factor[i]==2 && dati$salute1.factor[i+1]==2)
      
      {saluteC[j]="2 BUONA SALUTE"}
    
    else if((dati$salute1.factor[i]==0 && dati$salute1.factor[i+1]!= 0)|| 
            (dati$salute1.factor[i]!=0 && dati$salute1.factor[i+1]==0))
      
      {saluteC[j]="UNO CATTIVA SALUTE"}
    
    else if ((dati$salute1.factor[i]==0 && dati$salute1.factor[i+1]== 0))
      
      {saluteC[j]="2 CATTIVA SALUTE"}
    
    else 
      
      {saluteC[j]="NE MALE NE BENE"}
   
    i = i + 2
  }
  
saluteC
table(saluteC)
  
  
i = 1

AiutoDatoC = c(rep ("0", n / 2))
  
for (j in 1 : 1722) {
    
  if(dati$aiutodato.factor[i]==1 && dati$aiutodato.factor[i+1]==1)
      
    {AiutoDatoC[j]="2 aiutidati"}
    
  else if ((dati$aiutodato.factor[i]==0 && dati$aiutodato.factor[i+1]== 0))
      
    {AiutoDatoC[j]="0 aiuti dati"}
    
  else 
    
    {AiutoDatoC[j]="1 aiutodato"}
   
    
    i = i + 2
    
  }
  
AiutoDatoC
table(AiutoDatoC)
  
  
i = 1
ReteC = c(rep ("0", n / 2))
  
for (j in 1 : 1722) {
    
  if(dati$rete1.factor[i]=="TANTI" && dati$rete1.factor[i+1]=="TANTI")
      
    {ReteC[j]="2 TANTI"}
    
  else if ((dati$rete1.factor[i]=="POCHI" && dati$rete1.factor[i+1]== "POCHI"))
    
    {ReteC[j]="0 TANTI"}
    
  else  
    {ReteC[j]="1 TANTO"}
    
    i = i + 2
    
  }
  
ReteC
table(ReteC)

AiutoRicevutoC <- rep(0, n / 2)

i = 1

for (j in 1 : (n / 2)) {
  
  AiutoRicevutoC[j] = dati$aiutoricevutonew[i]
  
  i = i + 2
}
  
DatiC <- data.frame(etaC, saluteC, ReteC, AiutoDatoC, regioneC, AiutoRicevutoC)

fit.c <- glm(AiutoRicevutoC ~ etaC + regioneC + saluteC + AiutoDatoC+ ReteC, data = DatiC, family = binomial(link = logit))
summary(fit.c)

fit.c1 <- glm(AiutoRicevutoC ~ etaC + saluteC + AiutoDatoC+ ReteC, data = DatiC, family = binomial(link = logit))
summary(fit.c1) #AIC 1295.7

fit.creg <- glm(AiutoRicevutoC ~ regioneC, data = DatiC, family = binomial(link = logit))
summary(fit.creg)
 
# i risultati della regressione semplice con la variabile regione sono in linea con quanto visto già in precedenza con le regressioni
# di maschi e femmine: essa è infatti significativa nella regressione semplice, ma cessa di esserlo nella regressione multipla
# questo fenomeno di collinearità è dovuto alla lineare dipendenza tra la variabile regione e la variabile salute
