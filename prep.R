#results is the summary results output of model Nyaga
#sm is the the summary measure used. Made for absolute differences of spec and sens-can be extended to relative ratio
#k is the number of tests included in dataset
#prep returns the "league table" and SE league table, sens and spec and DOR for each test
#prep function prepares data as required for pscores
library(stringr)

prep<-function(results,sm,k){ 
  
  results<-results 
  sm<-as.character(sm)
  a<-grep(sm, rownames((results)))
  lrd_results<-as.data.frame(results[a,])
  lrd_results$names<-rownames(lrd_results)
  k<-k
  
  x<-str_split_fixed(lrd_results$names, c(","), 3)
  x<-data.frame(x)
  lrd_results$test1<-x$X2 #test 1 in difference
  y<-str_split_fixed(x$X3, c("]"), 2)                
  y<-as.data.frame(y)
  x$x1<-gsub("LRD", "", x$X1)
  sens<-grep("1", x$X1)
  spec<-grep("2", x$X1)
  lrd_results$test2<-y$V1 # test 2 in difference
  lrd_results$type<-c(rep("sens",length(sens)),rep("spec",length(spec)))
  lrd_results_sens<-lrd_results[lrd_results$type=="sens",]
  lrd_results_spec<-lrd_results[lrd_results$type=="spec",]
  
  #league table of difference of logit sensitivity and specificity
  LRD_SE<-matrix(lrd_results_sens$mean,nrow=k,ncol=k,byrow=T)
  LRD_SE[which(lower.tri(LRD_SE==T))]<--LRD_SE[which(lower.tri(LRD_SE==T))]
  LRD_SPE<-matrix(lrd_results_spec$mean,nrow=k,ncol=k,byrow=T)
  LRD_SPE[which(lower.tri(LRD_SPE==T))]<--LRD_SPE[which(lower.tri(LRD_SE==T))]
  
  #standard error
  LRD_SE_SE<-matrix(lrd_results_sens$se_mean,nrow=k,ncol=k,byrow=T)
  LRD_SE_SPE<-matrix(lrd_results_spec$se_mean,nrow=k,ncol=k,byrow=T)
  
  #correlation f sensitivity and specificity
  b<-min(grep("rho", rownames((results))))
  cor_sens_spec<-results[b,][1]
  
  #raw estimates of sensitivity and specificity
  d<-grep("MU", rownames((results)))
  sens_spec_results<-results[d,]
  sens<-sens_spec_results[1:k,][,1]
  spec<-sens_spec_results[(k+1):(2*k),][,1]
  
  #  #DOR
  DOR<-(sens*spec)/((1-spec)*(1-sens))

  #return arguments
  li<-list(LRD_SE,LRD_SPE,LRD_SE_SE,LRD_SE_SPE,cor_sens_spec,DOR,sens,spec)
  names(li)<-c("Abs_dif_sens","Abs_dif_spec","SE_abs_dif_sens","SE_abs_dif_spec","rho","DOR","sens","spec")
  return(li)}
