#Example

#Dataset used
#Randall Met al.
#Diagnostic tests for autism spectrum disorder (ASD) in preschool children. 
#Cochrane Database Syst Rev. 2018 Jul 24;7(7):CD009044. 
#doi: 10.1002/14651858.CD009044.pub2.
#PMID: 30075057

#Nyaga ANOVA model
#100k Iteration, 10k burn-in, thin=10

autism_data<-read.csv("autism_data.csv") #load data
sm="LRD" #difference of logit-sensitivity and logit-specificity
k=3 #number of tests

m1<-pscores.dta(autism_data,sm="LRD",k=3,m="sens")
m2<-pscores.dta(autism_data,sm="LRD",k=3,m="spec")
m3<-pscores.dta(autism_data,sm="LRD",k=3,m="sens+spec")