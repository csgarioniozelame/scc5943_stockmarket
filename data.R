#discretized input data for BN estimation

#preparacao do banco de dados para classificacao
PrepData <- function(Pdata = Pdata, Bdata = Bdata, name = "AAPL"){
  
  #selecionando apenas variaveis referentes ao stock de interesse
  Sub <- subset(Pdata, Pdata[,"stock"] == name, select = c(1:6))
  
  #merging os bancos kaggle e extra-kaggle
  SubData <- merge(Sub, Bdata, by = intersect("date", "date"))
  
  #Sub <- subset(SubData, SubData[,"stock"] == stock)
  #claculando o RSI
  SubData$rsi <- RSI(SubData[,"close"], n = 14)
  #dividindo em overbought e oversold
  SubData$RSI <- "intermediate"
  SubData$RSI[SubData$rsi >= 70] <- "oversold"
  SubData$RSI[SubData$rtn <= 30] <- "overbought"
  
  #criando as bbands
  SubData <- cbind(SubData, BBands( SubData[,c("high","low","close")] ))
  #calculando TDI
  SubData$tdi <- TDI(SubData[,"close"], n = 14)
  #claculando o retorno
  suppressWarnings(SubData$rtn <- as.numeric(c('NA',diff(log(SubData[,"close"])))))
  #seccionando retorno
  SubData$retor <- "hold"
  med <- median(na.omit(SubData$rtn)); desvio <- sd(na.omit(SubData$rtn))
  SubData$retor[SubData$rtn >= (med + (1/2)*desvio)] <- "sell"
  SubData$retor[SubData$rtn <= (med - (1/2)*desvio)] <- "buy"
  
  #names(SubData)
  
  #selecionando apenas as variáveis uteis
  data <- SubData[,-c(1:4,7,14,19,20,26,27)]
  
  #funcao para categorizacao
  cat <- function(data){
    mat <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
    for (j in 1:ncol(data)) {
      for (i in 2:nrow(data)) { 
        ifelse(data[i, j] >= data[i-1, j], mat[i, j] <-  "Increasing", 
               mat[i,j] <-  "Decreasing") 
      }
    }
    return(mat)
  }
  
  #discretizacao do banco
  disc <- cat(data = data)
  
  #juntando as variaveis na dataframe final
  cn <- colnames(data)
  data <- as.data.frame(apply(cbind(SubData[,c("retor","date","RSI")],disc),2, as.factor))[-1,]
  colnames(data) <- c("Y","date","RSI", cn)
  
  return(data)
}