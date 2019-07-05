rm(list=ls()) #Clear workspace
cat("\014")   #Clear console

#### REPLICATION CODE FOR 
#### LIERL & HOLMLUND "PERFORMANCE INFORMATION AND VOTING BEHAVIOR IN BURKINA FASO'S MUNICIPAL ELECTIONS: 
#### SEPARATING THE EFFECTS OF INFORMATION CONTENT AND INFORMATION DELIVERY"

#### BY MALTE LIERL, THIS VERSION: 2018-10-15

#### SELECT WORKING DIRECTORY ####

filepath <- file.choose()  # browse and select "Replication Code for Burkina Faso Chapter 2018-01-15.R" in the window
filename <- "Replication Code for Burkina Faso Chapter 2018-01-15.R"
dir <- substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)

#### LOAD PACKAGES ####
require(ggplot2)
require(ggthemes)

#### FUNCTION TO SCALE WEIGHTED DATA
require(Hmisc)
weighted.scale<-function(x,w) {
  (x-wtd.mean(x,w))/sqrt(wtd.var(x,w))
}

#### FUNCTION TO CALCULATE WEIGHTED MEAN ESTIMATE WITH CONFIDENCE INTERVAL
wt.meanci<-function(y,w){
  parm<-lm(y~1,weights=w)
  ci<-confint(parm)
  c(parm$coefficients[1],ci)
}

#### FUNCTION TO CACULATE WEIGHTED MEAN ESTIMATE WITH STANDARD ERROR
wt.meanse<-function(y,w){
  parm<-lm(y~1,weights=w)
  stderr<-se(parm)
  c(parm$coefficients[1],stderr)
}

#### FUNCTION TO EXTRACT NUMBER OF OBSERVATIONS FROM LM AND ROBCOV FIT
require(rms)

nobs.2<-function(fit) {length(fit$coefficients)+fit$df.residual}

robcov.nobs<-function(fit) {
  fit$stats["n"]
}

#### FUNCTION TO MAKE A T-TABLE FROM ROBCOV FIT
require(rms)

robcov.tTable<-function(fit) {
  cbind(fit$coefficients,sqrt(diag(fit$var)),fit$coefficients/sqrt(diag(fit$var)),2 * (1 - pt(abs(fit$coefficients/sqrt(diag(fit$var))), fit$df.residual)))
}


### FUNCTIONS TO GENERATE TEX CODE
execute<-function(text) {
  eval(parse(text=text))
}

tex<-function(code,file) {
  split.code.1<-unlist(strsplit(code,">>"))
  split.code<-strsplit(split.code.1,"<<")
  if (length(split.code[[1]])<2) {split.code[[1]]<-c('""',split.code[[1]])}
  if (length(split.code[[length(split.code)]])<2) {split.code[[length(split.code)]]<-c(split.code[[length(split.code)]],'""')}
  split.code<-matrix(unlist(split.code),byrow=TRUE,ncol=2)
  split.code[,2]<-sapply(split.code[,2],execute)
  write(t(split.code),file=file,sep="",ncolumns=nrow(split.code)*ncol(split.code))
}

#### FUNCTION TO LABEL COEFFICIENTS WITH SIGNIFICANCE STARS
stars<-function(x,v,digits) {
  x<-signif(x,digits)
  xcoef<-x[v,1]
  pval<-x[v,4]
  out<-""
  if (pval<=0.1&pval>0.05) {out<-"$^{*}$"}
  if (pval<=0.05&pval>0.01) {out<-"$^{**}$"}
  if (pval<=0.01&pval>0.001) {out<-"$^{***}$"}
  #if (pval<=0.001) {out<-"$^{***}$"}
  return(paste(xcoef,out,sep=""))
}


#### READ DATA
survey<-read.csv2("Data/Burkina Faso Metaketa Individual Data 2017-10-09.csv")
survey$weight<-survey$weight_2
  
## HOW MANY TREATMENT, HOW MANY CONTROL INDIVIDUALS?
table(survey$treatment.info)
## NUMBER OF COMMUNES IN THE STUDY
length(unique(paste(survey$region,survey$commune)))
## NUMBER OF VILLAGES IN THE STUDY
length(unique(paste(survey$region,survey$commune,survey$village)))
## TOTAL NON-RESPONSE
(sum(table(survey$treatment.info))-sum(table(survey$treatment.info[survey$valid==1])))/sum(table(survey$treatment.info))
# ...of which attrition: 
sum(table(survey$treatment.info[is.na(survey$valid)]))/sum(table(survey$treatment.info))
# ...of which invalid votes:  
sum(table(survey$treatment.info[survey$valid==0]))/sum(table(survey$treatment.info))

# TOTAL SAMPLE SIZE
sum(table(survey$treatment.info[survey$valid==1]))

### COMPREHENSION OF THE INFORMATION TREATMENT

source("Code/Section_2_4.r")

### TREATMENT EFFECT ESTIMATES

## ESTIMATE ATE IN GOOD NEWS SUBGROUP
#N+: standardized measure of good news. N+=scale(bias.quantile)
survey$N.plus<-survey$bias.quantile
survey$N.plus[survey$goodnews.quantile==0]<-NA
survey$N.plus<-weighted.scale(survey$N.plus,survey$weight)
summary(lm(vote.incumbent~treatment.info*N.plus+factor(v.unique),weights=weight,data=survey))
#Size of "good news" subgroup
length(survey$N.plus[is.na(survey$vote.incumbent)==FALSE&is.na(survey$N.plus)==FALSE])


## ESTIMATE ATE IN BAD NEWS SUBGROUP
survey$N.minus<--survey$bias.quantile
survey$N.minus[survey$goodnews.quantile==1]<-NA
survey$N.minus<-weighted.scale(survey$N.minus,survey$weight)
hist(survey$N.minus)
summary(lm(vote.incumbent~treatment.info*N.minus+factor(v.unique),weights=weight,data=survey))
#Size of "bad news" subgroup
length(survey$N.minus[is.na(survey$vote.incumbent)==FALSE&is.na(survey$N.minus)==FALSE])

### FIGURES

source("Code/Figure4.r")

source("Code/Figure6.r")

### TABLES

source("Code/Table1.r")

source("Code/Table2.r")

source("Code/Table3.r")

source("Code/Table4and5.r")

source("Code/Table6.r")

