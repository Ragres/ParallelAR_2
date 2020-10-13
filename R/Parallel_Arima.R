#' @import snow
#' @import parallel
Mejor_arima=function(final_i,final_j,datos){
  n=detectCores()
  cl<-makeCluster(n,type="SOCK")
  ar=function(i){
    mejor.arma<-c(0,0,0)
    AIC.arma=Inf
    for(j in 0:final_j){
      aic.temporal=try(AIC(arima(datos,order=c(i,0,j),method = "ML")))
      if(class(aic.temporal)=="try-error"){
        aic.temporal=try(AIC(stats::arima(datos,order=c(i,0,j),method = "ML",optim.method = "Nelder-Mead")))
      }
      
      if(aic.temporal<AIC.arma){
        AIC.arma=aic.temporal
        mejor.arma=c(i,0,j)
      }
    } 
    return(list(AIC.arma,mejor.arma))
  }
  clusterExport(cl=cl,"datos",
                envir=environment())
  mejores=clusterApplyLB(cl, 0:final_i, ar)
  MejorA=Inf
  z=0
  for(i in 1:length(mejores)){
    if(mejores[[i]][1]<MejorA){
      MejorA=as.numeric(mejores[[i]][1])
      z=i
    }  
  }
  
  mejor_modelo=mejores[[z]][2]
  mejor_AIC=mejores[[z]][1]
  stopCluster(cl)
  return(list(mejor_modelo,mejor_AIC))
}




