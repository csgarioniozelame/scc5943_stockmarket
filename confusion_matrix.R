###################################################################################################################
#confusion matrix for the method, data and period
#
#criando a matriz de confusao para testar o m�todo de predicao
Confusion <- function(data = data, periodo = 112, metodo = "hc", medida = "k2"){
  
  
  #criando a matriz que vai ser preenchida depois da classifica��o
  CM <- matrix(0,3,3)
  
  #vetor de datas para deslizar a janela
  dias <- data$date
  
  #loop ara classifica��o de todos os dias considerando a janela deslizante
  #warmup evita os NA do banco
  warmup = (periodo+20)
  
  for (i in warmup:nrow(data)){
    
    #contador
    start = dias[i]
    
    #verificando a classe da vari�vel "data"
    data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
    #selecionando as observa��es menores do que o dia da posi��o "i" do vetor
    DataSub <- data[data$date <= as.Date(start),]
    DataSub <- DataSub[nrow(DataSub):1,]
    DataSub <- DataSub[1:(periodo+1),]
    
    #names(DataSub)
    #criando os subsets de treinamento (do tamanho do per�odo) e de teste (di�rio)
    #retirando as vari�veis data e outras que n�o agregavam ao aprendizado
    data.test <- DataSub[as.Date(DataSub$date) == as.Date(as.character(start)),-c(2,4)]
    data.train <- DataSub[-(as.Date(DataSub$date) == as.Date(as.character(start))),-c(2,4)]
    
    names(data.train)
    names(DataSub)
    #se o metodo escolhido for o HC, metrica selecionada, e fa�a
    if (metodo == "hc"){
      fi1 <- hc(na.omit(data.train), score = medida)
      fit1 <- bn.fit(fi1, na.omit(data.train), method="bayes")
      pred <- predict(fit1, node = "Y", method = "bayes-lw", 
                      data = data.test, prob = TRUE)}
    
    #se o metodo escolhido for o TABU, metrica selecionada, e fa�a
    if (metodo == "tabu"){
      fi1 <- tabu(data.train, score = medida)
      fit1 <- bn.fit(fi1, data.train, method="bayes")
      pred <- predict(fit1, node = "Y", method = "bayes-lw", 
                      data = data.test, prob = TRUE)}
    
    #se o metodo escolhido for o NBC, fa�a
    if (metodo == "nb"){
      nb <- bnc('nb', 'Y', data.train[,-c(5:7,9,16)], smooth = 1)
      pred <- predict(nb, data.test)}
    
    #se o metodo escolhido for o AODE, fa�a
    if (metodo == "aode"){
      aode <- bnc('aode', 'Y', data.train[,-c(5:7,9,16)], smooth = 1)
      pred <- predict(aode, data.test)}
    
    #se o metodo escolhido for o TAN, fa�a
    if (metodo == "tan"){
      tan <- bnc('tan_cl', 'Y', data.train[,-c(5:7,9,16)], smooth = 1)
      pred <- predict(tan, data.test)}
    
    
    #criando a matriz de confus�o para o dia 
    CM1 <- table(data.test$Y,pred)
    
    #se for o caso a soma das matrizes de confus�o para cada um dos dias preditos
    CM <- CM + CM1
  }  
  return(CM)
}



###################################################################################################################