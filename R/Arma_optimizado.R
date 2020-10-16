ARMA_Optimizado=function(i,j,datos){
  a=expand.grid(0:i,0:j)

  ar=function(i){
    aic.temporal=try(AIC(arima(datos,order=c(a[i,1],0,a[i,2]),method = "ML")))
    if(class(aic.temporal)=="try-error"){
      aic.temporal=try(AIC(stats::arima(datos,order=c(a[i,1],0,a[i,2]),method = "ML",optim.method = "Nelder-Mead")))
    }
    return(aic.temporal)
  }
  cl<-makeCluster(detectCores(),type="SOCK")
  clusterExport(cl=cl,"datos",
                envir=environment())
  clusterExport(cl=cl,"a",envir=environment())
  todos=clusterApplyLB(cl, 1:nrow(a), ar)
  mejorA=Inf
  orden=0
  for(i in 1:nrow(a)){
    if(todos[[i]]<mejorA){
      mejorA=todos[[i]]
      orden=i
    }
  }
  return(list(c(a[orden,1],0,a[orden,2]),mejorA))
  stopCluster(cl)
}

