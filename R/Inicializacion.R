Inicializacion=function(){
  if(require(snow)==FALSE){
  install.packages("snow")
  }
  library(snow)
  library(parallel)
}
