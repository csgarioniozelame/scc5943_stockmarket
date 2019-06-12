#########################
#MCC multiclass #
#########################
#Example of a Confusion Matrix
#CM <- table(data.test$Y,pred)

#help source https://searchcode.com/codesearch/view/50446038/
#and https://arxiv.org/pdf/1008.2908v1.pdf

mcc <- function(CM){

  num=numeric(1)
  den1=numeric(1)
  den2=numeric(1)
  
for (k in 1:nrow(CM)){
  
  den1.1=numeric(1)
  den1.2=numeric(1)
  den2.1=numeric(1)
  den2.2=numeric(1)
  
  for (l in 1:nrow(CM)){
        if (l != k){
          for (g in 1:nrow(CM)){
            den1.1 = CM[g,l] + den1.1
            den2.1 = CM[l,g] + den2.1
          }
        }
    for (m in 1:nrow(CM)){
      num = num + CM[k,k]*CM[m,l]-CM[l,k]*CM[k,m]
    }
    den1.2 = den1.2 + CM[l,k]
    den2.2 = den2.2 + CM[k,l]
  }
  den1 = den1 + den1.1*den1.2
  den2 = den2 + den2.1*den2.2
}
  mcc = num / (sqrt(den1)*sqrt(den2))
  return(MCC = mcc)
}


