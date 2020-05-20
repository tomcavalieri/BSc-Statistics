install.packages("lifecontingencies")
library(lifecontingencies)

r.tasso <- function(i0, length){   # length è in anni 
  
  k = 0.7
  theta = 0.005
  sigma = 0.15
  delta = 1/12
 
  x <- rnorm(n = length * 12 - 1, mean = 0, sd = 1)
  
  itdelta <- c(i0, pmax(i0 + k * (theta - i0) * delta + sigma * sqrt(delta) * (x), rep(0, length * 12 - 1)))
  
  return(itdelta)
  
}

# freq è espressa in 12esimi, es. rate mensili -> 1/12
ammortamento.r <- function(capitale, durata, freq, interesse){ 
  
  nrate <- durata / freq
  
  ik = ((1 + interesse) ^ freq) - 1
  
  # si converte il tasso annuo in tasso equivalente in basa alla frequenza di pagamento scelta
  
  rate <- c(0, rep((capitale / annuity (interesse, durata, 0, 1 / freq)) * freq, nrate))
  
  #la funzione annuity calcola il valore attuale di una rendita unitaria pagata 1/freq volte all'anno 
  # per un numero di anni pari a durata. Per costruzione annuity restituisce il pagamento totale 
  # nell'anno, ragione per cui moltiplichiamo questo totale per freq, che corrisponde a dividere per 
  # il numero di rate pagate nell'anno.
  
  # inizializzo i vettori
  deb.res <- c(capitale, rep(0, nrate - 1))
  
  qint <- rep(0, nrate)
  
  qcap <- rep(0, nrate)
  
  
  for (i in 1 : nrate + 1){
    
    qint[i] <- deb.res[i - 1] * ik
    
    qcap[i] <- rate[i] - qint[i]
    
    capitale <- capitale - qcap[i]
    
    deb.res[i] <- capitale 
  }
  
  deb.res[(nrate) + 1] <- 0
  
  qcap[nrate + 1] <- deb.res[nrate]
  
  rate[nrate + 1] <- qcap[nrate + 1] + qint[nrate + 1]
  
  # qint[1] <- "-"
  # qcap[1] <- "-"
  # rate[1] <- "-"
  
  pianoamm <- data.frame(j = 0 : nrate, Rj = rate, Ij = qint, Cj = qcap, Qj = deb.res)
  return(pianoamm)
  
}

ammortamento.k <- function(capitale, durata, freq, interesse){
  
  nrate <- durata / freq
  
  ik = ((1 + interesse) ^ freq) - 1
  
  qcap <- c(0, rep((capitale / nrate), round(nrate)))

 
  # inizializzo i vettori
  deb.res <- c(capitale, rep(0, nrate - 1))
  
  qint <- rep(0, nrate)
  
  rate <- rep(0, nrate)
  
  for (i in 1 : nrate + 1){
    
    deb.res[i] <- deb.res[i - 1] - qcap[i]
    
    qint[i] <- deb.res[i - 1] * ik
    
    rate[i] <- qint[i] + qcap[i]
    
  }
  
  pianoamm <- data.frame(j = 0 : (nrate), Rj = rate, Ij = qint, Cj = qcap, Qj = deb.res)
  
  return(pianoamm)
}

ammortamento.c <- function(capitale, durata, freq, interesse, qcap.date){
  
  nrate <- durata / freq
  
  ik = ((1 + interesse) ^ freq) - 1
  
  qcap <- qcap.date
  
  
  # inizializzo i vettori
  deb.res <- c(capitale, rep(0, nrate - 1))
  
  qint <- rep(0, nrate)
  
  rate <- rep(0, nrate)
  
  for (i in 1 : nrate + 1){
    
    deb.res[i] <- deb.res[i - 1] - qcap[i]
    
    qint[i] <- deb.res[i - 1] * ik
    
    rate[i] <- qint[i] + qcap[i]
    
  }
  
  pianoamm <- data.frame(j = 0 : nrate, Rj = rate, Ij = qint, Cj = qcap, Qj = deb.res)
  return(pianoamm)
}






amm.var <- function(capitale, durata, freq, i0, freq.reset, tipo, print = TRUE){ #freq.reset funziona come freq

  # essendo la funzione amm.var costruita per gli ammortamenti a tasso variabile, la durata dev'essere 
  # strettamente maggiore di freq.reset perché altrimenti risulterebbe essere un ammortamento a tasso 
  # fisso; la freq.reset sarà multiplo di freq, eventualmente possono anche coincidere
  
  d <- durata
  
  #ci serve salvare la durata iniziale, prima che essa inizi ad essere decrementata, per poi utilizzarla
  # nel calcolo dei tir
  
  
  ncambi <- freq.reset / freq
  
  
  #indica il numero di rate nel piano ammortamento che vengono #calcolate con uno stesso tasso di interesse
  
  vett.int <- r.tasso(i0, durata)
  
  x <- ammortamento.r(capitale = capitale, durata = durata, freq = freq, interesse = i0)$Cj
  
  #prelevo dal dataframe la colonna contenente le quote capitali, da #utilizzare se l'ammortamento è di tipo 3.
  
  if (tipo == 1){
    risultato <- ammortamento.r(capitale = capitale, durata = durata, freq = freq, interesse = i0)
  } 
  else if (tipo == 2) {
    risultato <- ammortamento.k(capitale = capitale, durata = durata, freq = freq, interesse = i0)
  } 
  else if (tipo == 3){
    risultato <- ammortamento.c(capitale = capitale, durata = durata, freq = freq, interesse = i0, qcap.date = x)
  } 
  
  #queste istruzioni fuori ciclo permettono di realizzare un primo #dataframe con piano di ammortamento 
  # di un certo tipo, costruito come se l'interesse restasse fisso per tutta la durata
  
  amm.finale <- risultato
  
  # viene salvato questo primo piano di ammortamento ad interesse #fisso il amm.finale, che sarà il 
  # dataframe che verrà di volta in volta sovrascritto per ottenere quello finale ad interessi variabili
  
  amm.finale$int.annuo[1] <- i0 
  

  
  
  for (j in 1 : (ceiling((durata / freq.reset) - 1))){
    
    # ciclo tante volte quante cambia il tasso; è necessario usare ceiling perché se è decimale dobbiamo
    # utilizzare l'intero successivo per ciclare
    # ad esempio durata = 3, freq = 4/12 e freq.reset = 8/12 senza ceiling ciclerebbe 3/(8/12) -1 = 3.5
    # Si avrebbero dunque 3 cambi interesse mentre invece in totale se ne devono effettuare 4 
    # (senza contare il primo piano ammortamento che è esterno al ciclo)
    # (l'ultimo cambio interesse si applica solo per il quadrimestre residuo)
    
    ij <- vett.int[1 + 12 * freq.reset * j]   # seleziono l'interesse annuo vigente al momento del cambio tasso
    
    z <- ncambi * j
    
    if (tipo == 1){
      risultatoj <- ammortamento.r(capitale = risultato$Qj[ncambi + 1], durata = (durata - freq.reset), freq = freq, interesse = ij)
    } 
    else if (tipo == 2){
      
      risultatoj <- ammortamento.k(capitale = risultato$Qj[ncambi + 1], durata = (durata - freq.reset), freq = freq, interesse = ij)
    
      } 
    else if (tipo == 3){
      risultatoj <- ammortamento.c(capitale = risultato$Qj[ncambi + 1], durata = (durata - freq.reset), freq = freq, interesse = ij, qcap.date = x[-c(1 : z)])
    }
    

    risultato <- risultatoj    # passo nuovo piano ammortamento da cui si preleveranno i nuovi debiti residui
    durata <- (durata - freq.reset)    # aggiorno la durata del piano amm "residuo"

    # la durata decimale non dà problemi, infatti essa interviene solo nel calcolo numero rate nelle varie funzioni di ammortamento (durata / freq.reset)
  
    amm.finale$Ij[seq(from = ncambi * j + 2, to = ncambi * j + 1 + min(ncambi, round(durata / freq)) , by = 1)] =
      risultatoj$Ij[seq(from = 2, to = min(ncambi, round(durata / freq)) + 1, by = 1)]

    amm.finale$Rj[seq(from = ncambi * j + 2, to = ncambi * j + 1 + min(ncambi, round(durata / freq)), by = 1)] =
      risultatoj$Rj[seq(from = 2, to = min(ncambi, round(durata / freq)) + 1, by = 1)]

    amm.finale$Qj[seq(from = ncambi * j + 2, to = ncambi * j + 1 + min(ncambi, round(durata / freq)), by = 1)] =
      risultatoj$Qj[seq(from = 2, to = min(ncambi, round(durata / freq)) + 1, by = 1)]

    amm.finale$Cj[seq(from = ncambi * j + 2, to = ncambi * j + 1 + min(ncambi, round(durata / freq)), by = 1)] =
      risultatoj$Cj[seq(from = 2, to = min(ncambi, round(durata / freq)) + 1, by = 1)]

    amm.finale$int.annuo[seq(from = ncambi * j + 2, to = ncambi * j + 1 + min(ncambi, round(durata / freq)), by = 1)] = ij

    amm.finale$int.annuo[1] <- "-"

    # sovrascrizione delle varie componenti del piano ammortamento calcolato ad ogni cambio tasso: 
    # d ogni ciclo si sostituiscono un numero pari a ncambi componenti ad ogni colonna
    # l'utilizzo della funzione min serve a gestire il caso in cui le rate residue sono meno del numero ncambi; 
    # ad esempio, se l'ammortamento è annuo con rate mensili e reset ogni 5 mesi, si ha che ncambi sarà 5, mentre 
    # la seconda volta che verrà resettato il tasso resteranno da pagare due sole rate, quindi nell'ammortamento 
    # finale saranno da sovrascrivere le righe in posizione 12 e 13 e non da 12 a 16 (le righe 14-15-16 non 
    # sono definite perchè amm.finale in questo esempio è un dataframe con 13 righe)
    
    
  }  
  
  # l'ampio utilizzo della funzione round è dovuto alle approssimazioni di R 
  # Le operazioni sulle quali abbiamo chiamato la funzione round dovrebbero teoricamente restituire
  # un numero intero, ma ciò non avviene.
  
  
  flussi <- c(capitale, -amm.finale$Rj[-1])
  
  date <- seq(0, d, by = freq)
  
  
  VAN <- function(i){
    
    fs <- (1+i) ^ - date
    as.numeric(flussi %*% fs)
    
  }
  
  TIR <- uniroot(f = VAN, lower = 0, upper = 1)$root
  
  #la funzione uniroot trova lo zero della funzione VAN
 
  
  if (print == TRUE){
    
  print("Tassi annui generati mensilmente", quote = FALSE)
  
  print(vett.int)
  
  print (" ", quote = FALSE)
  
  print("Piano di ammortamento", quote = FALSE)
  
  print(amm.finale)
  
  print (" ", quote = FALSE)
  
  print("TIR", quote = FALSE)
  
  }
  
  return(TIR)
  
}

amm.var(100000, 3, 2/12, 0.035, 8/12, 2)




#########################################################################
####SIMULAZIONE MONTECARLO####
#Generare tramite il metodo monte carlo la distribuzione del TIR 
#di un dato piano di ammortamento

set.seed(1)

MonteCarlo <- function(nsim, capitale, durata, freq, i0, freq.reset, tipo){
  
  tir <- rep(0, nsim)
  
  for (i in 1:nsim){
    
    tir[i] <- amm.var(capitale = capitale, durata = durata, freq = freq, i0 = i0, freq.reset = freq.reset, tipo = tipo, print = FALSE)
    
  }
  
  hist(tir, breaks = 10, main = "Distibuzione TIR", xlab = "TIR", ylab = "Frequenza", ylim = c(0, nsim / 3))
  
  MEDIA <- mean(tir)
  
  SD <- sd(tir)
  
  MEDIANA <- median(tir)
  
  M2 <- mean(tir^2)
 
  print("MEDIA", quote = FALSE)
  print(MEDIA)
  
  print(" ",quote = FALSE)
  
  print("DEVIAZIONE STANDARD", quote = FALSE)
  print(SD)
  
  print(" ", quote = FALSE)
  
  print("MEDIANA", quote = FALSE)
  print(MEDIANA)
  
  print(" ", quote = FALSE)
  
  print("Primo quartile", quote = FALSE)
  quartile1 <- quantile(tir, probs = 0.25)
  print(quartile1)
  
  print(" ", quote = FALSE)
  
  print("Terzo quartile", quote = FALSE)
  quartile3 <- quantile(tir, probs = 0.75)
  print(quartile3)
  
  print(" ", quote = FALSE)
  
  print("E[X^2]", quote = FALSE)
  print(M2)
  
}

MonteCarlo(10000, 90000, 5, 4/12, 0.03, 12/12, 2)

