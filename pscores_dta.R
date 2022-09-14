#load libraries required
l<-c("stringr","mvtnorm","readxl")
lapply(l, require, character.only = TRUE)

#load functions required
#change path to where the functions are saved
source("Pscores.R")
source("prep.R")

#Pscores function "pscores.dta"
pscores.dta=function(results,sm,k,m){
  data<-prep(results,sm=sm,k=k)
  sens<-data$Abs_dif_sens
  spec<-data$Abs_dif_spec
  SE_sens<-data$SE_abs_dif_sens
  SE_spec<-data$SE_abs_dif_spec
  
  sensitivity=array((sens),dim=c(dim((sens))[1],dim((sens))[2],1))
  var.sensitivity=array(SE_sens,dim=c(dim(SE_sens)[1],dim(SE_sens)[2],1))
  
  specificity=array((spec),dim=c(dim((spec))[1],dim((spec))[2],1))
  var.specificity=array(SE_spec,dim=c(dim(SE_spec)[1],dim(SE_spec)[2],1))
  
  test<-NULL
  for (i in 1:k)
  {test[i]<-paste("Test",i)}
  labels=as.vector(test)
  
  DOR<-data$DOR
  sens<-data$sens
  spec<-data$spec
  
  if (m=="sens")
  {pscr<- pscores(sensitivity,var.sensitivity,0,0,"B",label=labels)}
  
  if (m=="spec")
  {pscr<-pscores(specificity,var.specificity,0,0,"B",label=labels)}
  
  if (m=="sens+spec")
  {c=2 #number of outcomes
  p=k #number of tests
  outcomes<-array(rep(0,p^2*c),c(p,p,c))
  outcomes[,,1]<-sensitivity
  outcomes[,,2]<-specificity
  var.outcomes<-array(rep(0,p^2*c),c(p,p,c))
  var.outcomes[,,1]<-var.sensitivity
  var.outcomes[,,2]<-var.specificity
  correlation=matrix(c(1,as.numeric(data$rho),as.numeric(data$rho),1),2,2)
  pscr<-pscores(outcomes,var.outcomes,correlation,c(0,0),type=c("B","B"),labels)
  }
  
  res<-data.frame(round(pscr,2),round(DOR,2),round(sens,2),round(spec,2))
  names(res)<-c("P-scores","DOR","sensitivity","specificity")
  res}

