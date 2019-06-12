##############################################################################################################
#script to download the stocks prices in a given list
require(quantmod)
require(Quandl)

#file with the tickers from the companies currently in the S&P500 
n <- read.csv(file = "all_tickers.csv", header = T)
#obj is the vector w/ all companies downloaded
#getSymbols set the prices and volume for a specific period
obj <- getSymbols(as.matrix(n), src = "yahoo", from = "2013-01-01", to = "2019-05-31")

#changing the directory to save the new files in a given folder
setwd("C:\\Users\\Camila\\Documents\\aulas_mestrado\\2019\\scc\\project_scc\\stocks")

#gets the object name and save a csv file w/ the data
for (i in 1:length(obj)){
  nnn <- index(get(obj[,i]))
  write.csv(x = get(obj[,i]), file = paste0("hist_",obj[,i],".csv"), row.names = nnn)
  rm(nnn)
}

#getting the data in a single file
joint <- function(obj = obj){
  nnn <- data.frame(NULL)
  i=1
  for (i in 1:length(obj)){
    n <- read.csv(file = paste0("hist_",obj[,i],".csv"))
    nn <- cbind(n, obj[,i])
    names(nn) <- names(nnn)
    nnn <- rbind(nnn,nn)
  i=i+1
    }
  colnames(nnn) <- c("date","open","high","low","close","volume","adj","stock")
  return(nnn)
}

#data frame with daily stocks prices
jj <- joint(obj = obj)

setwd("..")

write.csv(x = jj[,-7], file = paste0("hist_all.csv"))

###############################################################################################################
