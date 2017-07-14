


#part b)

options(digits=2)


dataPat <- dataCompl %>% 
  group_by(PATTERNS) %>% 
  mutate(sumTXspP = sum(TXs))

patSummary <- dataPat[!duplicated(dataPat$PATTERNS),]    #only picks one row per pattern 

patSummary <- data.frame(patSummary$CATEGORY,patSummary$PATTERNS, patSummary$sumTXspP)
patSummary <- patSummary %>% 
  rename(category = patSummary.CATEGORY, pattern = patSummary.PATTERNS, txs = patSummary.sumTXspP)




#define patterns

patterns <- c("DPT","DPA","Oracle","Vote","Kill","Math")



#define components
finComponents <- data$category[c(2,5,7,14,20,21,18, 27, 23)]
NotComponents <- data$category[c(9,3)]
WalComponents <- data$category[22]
GameComponents <- data$category[c(19,4,19,10,13,16,17,20)] 
LibComponents <- data$category[c(6,26)]


for (j in 1:length(patterns))  {
  
  
  #loop to get total amount of contracts per category
  Fin <- c()
  TokFin <- c()
  
  for (i in 1:length(finComponents)) {
    Fin[i] <- sum(dataPat$CATEGORY == as.character(finComponents[i]))
    TokFin[i] <- sum(str_count(dataPat$PATTERNS, as.character(patterns[j])) &  str_count(dataPat$CATEGORY, as.character(finComponents[i])))
    
  }
  Fin <- sum(Fin)
  TokFin <- sum(TokFin)
  #percentage of contracts in that category using this pattern
  PercFinWTok <- TokFin/Fin
  
  
  
  Not <- c()
  TokNot <- c()
  
  for (i in 1:length(NotComponents)) {
    Not[i] <- sum(dataPat$CATEGORY == as.character(NotComponents[i]))
    TokNot[i] <- sum(str_count(dataPat$PATTERNS, as.character(patterns[j])) &  str_count(dataPat$CATEGORY, as.character(NotComponents[i])))
    
  }
  Not <- sum(Not)
  TokNot <- sum(TokNot)
  
  PercNotWTok <- TokNot/Not
  
  
  
  
  Gam <- c()
  TokGam <- c()
  for (i in 1:length(GameComponents)) {
    Gam[i] <- sum(dataPat$CATEGORY == as.character(GameComponents[i]))
    TokGam[i] <- sum(str_count(dataPat$PATTERNS, as.character(patterns[j])) &  str_count(dataPat$CATEGORY, as.character(GameComponents[i])))
    
  }
  Gam <- sum(Gam)
  TokGam <- sum(TokGam)
  PercGamWTok <- TokGam/Gam
  
  
  
  Wal <- c()
  TokWal <- c()
  for (i in 1:length(WalComponents)) {
    Wal[i] <- sum(dataPat$CATEGORY == as.character(WalComponents[i]))
    TokWal[i] <- sum(str_count(dataPat$PATTERNS, as.character(patterns[j])) &  str_count(dataPat$CATEGORY, as.character(WalComponents[i])))
    
  }
  Wal <- sum(Wal)
  TokWal <- sum(TokWal)
  PercWalWTok <- TokWal/Wal
  
  
  
  Lib <- c()
  TokLib <- c()
  for (i in 1:length(LibComponents)) {
    Lib[i] <- sum(dataPat$CATEGORY == as.character(LibComponents[i]))
    TokLib[i] <- sum(str_count(dataPat$PATTERNS, as.character(patterns[j])) &  str_count(dataPat$CATEGORY, as.character(LibComponents[i])))
    
  }
  Lib <- sum(Lib)
  TokLib <- sum(TokLib)
  PercLibWTok <- TokLib/Lib
  
  
  #total amount of contracts
  Total <- sum(Fin, Not, Gam, Wal, Lib)
  
  
  
  if (j == 1) {
    token <- c(PercFinWTok,PercNotWTok,PercGamWTok, PercWalWTok, PercLibWTok )
  } else if (j == 2) {
    authorization <- c(PercFinWTok,PercNotWTok,PercGamWTok, PercWalWTok, PercLibWTok )
  } else if (j == 3) {
    oracle <- c(PercFinWTok,PercNotWTok,PercGamWTok, PercWalWTok, PercLibWTok )
  } else if (j == 4) {
    poll <- c(PercFinWTok,PercNotWTok,PercGamWTok, PercWalWTok, PercLibWTok )
  } else if (j == 5) {
    termination <- c(PercFinWTok,PercNotWTok,PercGamWTok, PercWalWTok, PercLibWTok )
  } else {
    math <- c(PercFinWTok,PercNotWTok,PercGamWTok, PercWalWTok, PercLibWTok )
  }
  
  
}

final <- data.frame(category = c("Financial", "Notary", "Game", "Wallet","Library"), 
                    token, authorization, oracle, poll, termination, math)

final

