######################################################################################
######################################################################################
#Replication code for:

#Mark T. Buntaine, Sarah S. Bush, Ryan Jablonski, Daniel Nielson, Paula Pickering
#Budgets, SMS Texts, and Votes in Uganda

#Prepared by: Mark Buntaine
#Mark Buntaine contact (as of January 2019): buntaine@bren.ucsb.edu

#Compiled using R Version 3.5.1 (version "Feather Spray") on Mac running OS X 10.14.3
######################################################################################
######################################################################################

######################################################################################
###Packages
######################################################################################
library(lfe) #Version 2.8-2
#Note: Contains felm() with demeaning and SE clustering
#Note2: on warnings, see: https://cran.r-project.org/web/packages/lfe/vignettes/identification.pdf
library(stargazer) #Version 5.2.2

######################################################################################
###Functions
######################################################################################
felm.ri <- function(formula, dta, treat.var, rand.ob, rand.ob.info.cols, join.var, sims, ...){
  require(lfe)
  print("1. data and rand.ob must have rows organized in identical order")
  print("2. treat.var should be first right-side entry in formula")
  print("3. join.var must have identical name between data and rand.ob")
  
  ate <- coef(felm(formula, data=dta))[1]
  N <- felm(formula, data=dta)$N
  ate.samp.dist <- rep(NA,sims)
  
  for (i in 1:sims){
    dta[,treat.var] <- rand.ob[rand.ob[,join.var] %in% dta[,join.var], rand.ob.info.cols+i]
    
    ate.samp.dist[i] <- coef(felm(formula, data=dta))[1]
  }
  
  p.two.way <- sum(abs(ate)<abs(ate.samp.dist))/sims
  p.one.way.greater <- sum(ate<ate.samp.dist)/sims
  p.one.way.lesser <- sum(ate>ate.samp.dist)/sims
  se <- sd(ate.samp.dist)
  
  coun <- list("ate" = ate, "ate.samp.dist" = ate.samp.dist, "se"=se, "p.two.way" = p.two.way, "p.one.way.greater" = p.one.way.greater, "p.one.way.lesser" = p.one.way.lesser, "N" = N)
  return(coun)
}

felm.ri2 <- function(formula, dta, treat.var, rand.ob, rand.ob.info.cols, join.var, sims, ...){
  require(lfe)
  print("1. data and rand.ob must have rows organized in identical order")
  print("2. treat.var should be first right-side entry in formula")
  print("3. join.var must have identical name between data and rand.ob")
  
  #Note: have to make this for multiple factor of one randomized treatment
  
  ran.coef.num <- length(unique(dta[,treat.var][!is.na(dta[,treat.var])]))-1 #Gives number of coefficients to keep for crossed treatment indicator
  N <- felm(formula, data=dta)$N
  coef <- coef(felm(formula, data=dta))[1:ran.coef.num]
  coef.samp.dist <- matrix(data=NA, nrow=ran.coef.num, ncol=sims)
  #row.names(coef.samp.dist) <- names(coef)
  
  for (i in 1:sims){
    dta[,treat.var] <- rand.ob[rand.ob[,join.var] %in% dta[,join.var], rand.ob.info.cols+i]
    coef.samp.dist[,i] <- coef(felm(formula, data=dta))[1:ran.coef.num]
  }
  
  se <- apply(coef.samp.dist, 1, sd) #Cannot get SEs off non-randomized parameters
  
  p.two.way <- rep(NA,length(coef))
  p.one.way.greater <- rep(NA,length(coef))
  p.one.way.lesser <- rep(NA,length(coef))
  
  for (i in 1:length(coef)){
    p.two.way[i] <- sum(abs(coef[i])<abs(coef.samp.dist[i,]))/sims
    p.one.way.greater[i] <- sum(coef[i]<coef.samp.dist[i,])/sims
    p.one.way.lesser[i] <- sum(coef[i]>coef.samp.dist[i,])/sims
  }
  
  coun <- list("coef" = coef, "coef.samp.dist" = coef.samp.dist, "se"=se, "p.two.way" = p.two.way, "p.one.way.greater" = p.one.way.greater, "p.one.way.lesser" = p.one.way.lesser, "N" = N)
  return(coun)
}

######################################################################################
###Data input, change file paths as appropriate
######################################################################################

#Update "root" to directory of current file
root <- "."
setwd(root)

data <- read.csv("../data/MASTER_Analysis_190312.csv", stringsAsFactors=FALSE)

# Save data in RDS due to memory limit (100MB) in Github repository
# Budget_RI <- read.csv("../data/Budget_RI.csv", stringsAsFactors=FALSE)
# saveRDS(Budget_RI, "../data/Budget_RI.RDS")
Budget_RI <- readRDS("../data/Budget_RI.RDS")

Budget_RI <- Budget_RI[match(data$id.cleaned, Budget_RI$id.cleaned),] #reordering to match "data"

# Save data in RDS due to memory limit (100MB) in Github repository
# Density_RI <- read.csv("../data/Density_RI.csv", stringsAsFactors=FALSE)
# saveRDS(Density_RI, "../data/Density_RI.RDS")
Density_RI <- readRDS("../data/Density_RI.RDS")
Density_RI <- Density_RI[match(data$id.cleaned, Density_RI$id.cleaned),] #reordering to match "data"

#Audit data for all districts
budget_audit=read.csv("../data/lc5_budget_and_audit.csv", stringsAsFactors = FALSE)

#Electoral results for all lc5 councillors
candidates=read.csv("../data/lc5_candidates.csv", stringsAsFactors = FALSE)

#Comparison of survey and official voting
votes.councillor=read.csv("../data/councilor_official_vs_survey_votes.csv", stringsAsFactors = FALSE)
votes.chair=read.csv("../data/chair_official_vs_survey_votes.csv", stringsAsFactors = FALSE)


######################################################################################
###Data setup
######################################################################################
schooling.levels <- c("no_schooling","some_primary_s","completed_prim","some_secondary","completed_seco","some_universit","completed_univ","some_post_grad","completed_mast","refuse_to_answ")
data$r.education <- factor(data$r.How_much_schooling_have_you_co, levels=schooling.levels)
data$budget.certainty <- factor(data$budget.certainty, levels=c("very_certain","certain","not_certain","very_uncertain","missing"))
data$budget.importance <- factor(data$budget.importance, levels=c("not_important","not_very_impor","somewhat_impor","very_important","missing"))
data$b.trust.AG <- factor(data$b.trust.AG, levels=c("do_not_trust_a","don_t_know","trust_a_little","trust_a_lot","missing"))
data$b.trust.Twaweza <- factor(data$b.trust.Twaweza, levels=c("do_not_trust_a","don_t_know","trust_a_little","trust_a_lot","missing"))
data$b.vote.buying <- factor(data$b.vote.buying, levels=c("very_likel","somewhat_likel","somewhat_unlik","very_unlikely","don_t_know","missing"))
data$party.attachment <- factor(data$party.attachment, levels=c(7,6,5,4,3,2,1,"not_applicable","missing"))

#Remaking "bd.multi.treat2" because of unwanted class conversion in "data" from read.csv()
data$bd.multi.treat2 <- ifelse(data$budget.treat==1 & data$density.treat2==1,"1.1",NA)
data$bd.multi.treat2 <- ifelse(data$budget.treat==1 & data$density.treat2==0,"1.0",data$bd.multi.treat2)
data$bd.multi.treat2 <- ifelse(data$budget.treat==0 & data$density.treat2==1,"0.1",data$bd.multi.treat2)
data$bd.multi.treat2 <- ifelse(data$budget.treat==0 & data$density.treat2==0,"0.0",data$bd.multi.treat2)

#Adding missing category to some variables
data$aligned.lc5.chair.inc.orig <- data$aligned.lc5.chair.inc
data$aligned.lc5.councillor.inc.orig <- data$aligned.lc5.councillor.inc

data$aligned.lc5.chair.inc <- ifelse(is.na(data$aligned.lc5.chair.inc), "missing", data$aligned.lc5.chair.inc)
data$aligned.lc5.councillor.inc <- ifelse(is.na(data$aligned.lc5.councillor.inc), "missing", data$aligned.lc5.councillor.inc)

data$lc5.councillor.same.tribe <- ifelse(data$b.Q2_tribe_same_as_LC5=="yes",1,0)
data$lc5.councillor.same.tribe <- ifelse(data$b.Q2_tribe_same_as_LC5=="refused_to_ans","missing", data$lc5.councillor.same.tribe)


######################################################################################
###Budget subsets
######################################################################################

#Prior-defined subgroups
budget.good <- subset(data, budget.actual > budget.prior | (budget.actual==budget.prior & budget.actual>=4))
budget.bad <- subset(data, budget.actual < budget.prior | (budget.actual==budget.prior & budget.actual<=2))

###Factor levels
budget.good$budget.prior.diff <- factor(budget.good$budget.prior.diff, label=c("same","better1","better2","better3","better4"))
budget.bad$budget.prior.diff <- factor(budget.bad$budget.prior.diff, levels=c(0,-1,-2,-3,-4), label=c("same","worse1","worse2","worse3","worse4"))

#2. Subset where no individual incumbent switched parties and ran again in 2016 (includes elections w/o incumbent individual)
budget.good.lc5.chair.comp <- subset(budget.good, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.good.lc5.councillor.comp <- subset(budget.good, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))
budget.bad.lc5.chair.comp <- subset(budget.bad, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.bad.lc5.councillor.comp <- subset(budget.bad, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))

#Verified Recipient subgroups
budget.good.lc5.chair.comp.c <- subset(budget.good, lc5.chair.competitive==1 & lc5.chair.party.switch==0 & d.X_11_Over_the_last_several_days=="yes")
budget.good.lc5.councillor.comp.c <- subset(budget.good, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016) & d.X_11_Over_the_last_several_days=="yes")
budget.bad.lc5.chair.comp.c <- subset(budget.bad, lc5.chair.competitive==1 & lc5.chair.party.switch==0 & d.X_11_Over_the_last_several_days=="yes")
budget.bad.lc5.councillor.comp.c <- subset(budget.bad, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016) & d.X_11_Over_the_last_several_days=="yes")


######################################################################################
###Public Services subsets
######################################################################################

ps.good <- subset(data, ps.good.noNA==1)
ps.bad <- subset(data, ps.bad.noNA==1)

ps.good.lc5.chair.comp <- subset(ps.good, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
ps.good.lc5.councillor.comp <- subset(ps.good, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))
ps.bad.lc5.chair.comp <- subset(ps.bad, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
ps.bad.lc5.councillor.comp <- subset(ps.bad, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))

ps.good.lc3.chair.comp <- subset(ps.good, lc3.chair.competitive==1 & lc3.chair.party.switch==0 & is.na(lc3.chair.redistricted))
ps.good.lc3.councillor.comp <- subset(ps.good, lc3.councillor.competitive==1 & lc3.councillor.party.switch==0 & is.na(lc3.councillor.redistricted2016))
ps.bad.lc3.chair.comp <- subset(ps.bad, lc3.chair.competitive==1 & lc3.chair.party.switch==0 & is.na(lc3.chair.redistricted))
ps.bad.lc3.councillor.comp <- subset(ps.bad, lc3.councillor.competitive==1 & lc3.councillor.party.switch==0 & is.na(lc3.councillor.redistricted2016))


######################################################################################
###Figure 7.1: CONSORT diagram
#To Do: update numbers to match replication file
######################################################################################

consort.data=data
consort=data.frame(NA)
consort.data$c=1
num.in.village=aggregate(consort.data$c, by=list(consort.data$location.id), FUN=sum)
consort.data$num.in.village=num.in.village[match(consort.data$location.id,num.in.village$Group.1), "x"]

#Row 1 Column 1
consort$assessed_for_eligibility=30296 #from baseline recruitment files available on request

#Row 1 Column 2  
consort$excluded=14213 #from baseline recruitment files available on request
consort$excluded.refused=218 #from baseline recruitment files available on request
consort$excluded.unreachable=13995 #from baseline recruitment files available on request

#Row 2 Column 1
consort$randomized=nrow(consort.data)

#Row 3 Column 1
consort$treatment=sum(consort.data$budget.treat==1)
consort$treatment80=sum(consort.data$density.treat==1 & consort.data$budget.treat==1 & consort.data$num.in.village>=15)
consort$treatment50=sum(consort.data$density.treat==0 & consort.data$budget.treat==1 & consort.data$num.in.village<15)
consort$treatment20=sum(consort.data$density.treat==0 & consort.data$budget.treat==1 & consort.data$num.in.village>=15)
consort$treatment==consort$treatment80+consort$treatment50+consort$treatment20

#Row 3 Column 2
consort$control=sum(consort.data$budget.treat==0)
consort$control80=sum(consort.data$density.treat==1 & consort.data$budget.treat==0 & consort.data$num.in.village>=15)
consort$control50=sum(consort.data$density.treat==0 & consort.data$budget.treat==0 & consort.data$num.in.village<15)
consort$control20=sum(consort.data$density.treat==0 & consort.data$budget.treat==0 & consort.data$num.in.village>=15)
consort$control==consort$control80+consort$control50+consort$control20

#Row 4 Column 1
consort$treat_surveyed_endline = sum(consort.data$d.attr==0 & consort.data$budget.treat==1)
consort$treat_unreachable_endline = consort$treatment-consort$treat_surveyed_endline

#Row 4 Column 2
consort$control_surveyed_endline = sum(consort.data$d.attr==0 & consort.data$budget.treat==0)
consort$control_unreachable_endline = consort$control-consort$control_surveyed_endline

#Row 5 Column 1

#analyzed
consort$treat_analyzed_chair=sum(budget.bad.lc5.chair.comp$budget.treat==1 & !is.na(budget.bad.lc5.chair.comp$lc5.chair.inc.vote) & !is.na(budget.bad.lc5.chair.comp$budget.treat) & !is.na(budget.bad.lc5.chair.comp$lc5.chair.intent))+
  sum(budget.good.lc5.chair.comp$budget.treat==1 & !is.na(budget.good.lc5.chair.comp$lc5.chair.inc.vote) & !is.na(budget.good.lc5.chair.comp$budget.treat) & !is.na(budget.good.lc5.chair.comp$lc5.chair.intent))
consort$treat_analyzed_councillor=sum(budget.bad.lc5.councillor.comp$budget.treat==1 & !is.na(budget.bad.lc5.councillor.comp$lc5.councillor.inc.vote) & !is.na(budget.bad.lc5.councillor.comp$budget.treat) & !is.na(budget.bad.lc5.councillor.comp$lc5.councillor.intent))+
  sum(budget.good.lc5.councillor.comp$budget.treat==1 & !is.na(budget.good.lc5.councillor.comp$lc5.councillor.inc.vote) & !is.na(budget.good.lc5.councillor.comp$budget.treat) & !is.na(budget.good.lc5.councillor.comp$lc5.councillor.intent) )

#reasons for exclusion
consort$treat_refused_voting=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & (!(consort.data$d.vote_in_LC5_election.x %in% c("no","yes")) & !is.na(consort.data$d.vote_in_LC5_election.x)))
consort$treat_refused_vote_choice_chair=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & ((consort.data$d.party_of_LC5_chairperson_voted.x %in% c("refused_to_answer")) ))
consort$treat_refused_vote_choice_councillor=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & ((consort.data$d.party_of_LC5_councillor_voted.x %in% c("refused_to_answer")) ))
consort$treat_refused_vote_intent_chair=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & ((consort.data$b.Q6_vote_for_current_LC5chair %in% c("refused_to_ans")) ))
consort$treat_refused_vote_intent_councillor=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & ((consort.data$b.Q7_vote_for_current_LC3council %in% c("refused_to_ans")) ))
consort$treat_prior_nonresponsive=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & ((consort.data$b.Q22_LC5_record_of_budget_mgmt %in% c("", NA)) ))
consort$treat_chair_uncompetitive=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & consort.data$lc5.chair.competitive==0)
consort$treat_councillor_uncompetitive=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & (consort.data$lc5.councillor.competitive==FALSE | is.na(consort.data$lc5.councillor.competitive)))
consort$treat_changed_parties_chair=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & consort.data$lc5.chair.party.switch==1)
consort$treat_changed_parties_councillor=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & consort.data$lc5.councillor.party.switch==1)
consort$treat_did_not_vote=sum(consort.data$d.attr==0 & consort.data$budget.treat==1 & (consort.data$d.vote_in_LC5_election.x=="no" & !is.na(consort.data$d.vote_in_LC5_election.x)))

#Row 5 Column 2

#analyzed
consort$control_analyzed_chair=sum(budget.bad.lc5.chair.comp$budget.treat==0 & !is.na(budget.bad.lc5.chair.comp$lc5.chair.inc.vote) & !is.na(budget.bad.lc5.chair.comp$budget.treat) & !is.na(budget.bad.lc5.chair.comp$lc5.chair.intent))+
  sum(budget.good.lc5.chair.comp$budget.treat==0 & !is.na(budget.good.lc5.chair.comp$lc5.chair.inc.vote) & !is.na(budget.good.lc5.chair.comp$budget.treat) & !is.na(budget.good.lc5.chair.comp$lc5.chair.intent))

consort$control_analyzed_councillor=sum(budget.bad.lc5.councillor.comp$budget.treat==0 & !is.na(budget.bad.lc5.councillor.comp$lc5.councillor.inc.vote) & !is.na(budget.bad.lc5.councillor.comp$budget.treat) & !is.na(budget.bad.lc5.councillor.comp$lc5.councillor.intent))+
  sum(budget.good.lc5.councillor.comp$budget.treat==0 & !is.na(budget.good.lc5.councillor.comp$lc5.councillor.inc.vote) & !is.na(budget.good.lc5.councillor.comp$budget.treat) & !is.na(budget.good.lc5.councillor.comp$lc5.councillor.intent) )

#reasons for exclusion
consort$control_refused_voting=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & (!(consort.data$d.vote_in_LC5_election.x %in% c("no","yes")) & !is.na(consort.data$d.vote_in_LC5_election.x)))
consort$control_refused_vote_choice_chair=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & ((consort.data$d.party_of_LC5_chairperson_voted.x %in% c("refused_to_answer")) ))
consort$control_refused_vote_choice_councillor=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & ((consort.data$d.party_of_LC5_councillor_voted.x %in% c("refused_to_answer")) ))
consort$control_refused_vote_intent_chair=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & ((consort.data$b.Q6_vote_for_current_LC5chair %in% c("refused_to_ans")) ))
consort$control_refused_vote_intent_councillor=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & ((consort.data$b.Q7_vote_for_current_LC3council %in% c("refused_to_ans")) ))
consort$control_prior_nonresponsive=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & ((consort.data$b.Q22_LC5_record_of_budget_mgmt %in% c("", NA)) ))
consort$control_chair_uncompetitive=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & consort.data$lc5.chair.competitive==0)
consort$control_councillor_uncompetitive=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & (consort.data$lc5.councillor.competitive==FALSE | is.na(consort.data$lc5.councillor.competitive)))
consort$control_changed_parties_chair=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & consort.data$lc5.chair.party.switch==1)
consort$control_changed_parties_councillor=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & consort.data$lc5.councillor.party.switch==1)
consort$control_did_not_vote=sum(consort.data$d.attr==0 & consort.data$budget.treat==0 & (consort.data$d.vote_in_LC5_election.x=="no" & !is.na(consort.data$d.vote_in_LC5_election.x)))

for(i in c(2:length(consort))){
  cat(paste(colnames(consort)[i],": ", consort[1,i], "\n", sep=""))
}

consort.data=NULL


######################################################################################
###Figure 7.2: Map
#Excluded for reasons of anonymity. Study sites and map replication can be requested from the authors.
######################################################################################


######################################################################################
###Figure 7.3: Treatment effects of budget disclosures
######################################################################################

##### Main Effects #####

###Budget / LC5 chair results
chair.good <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name,
                      dta=budget.good.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                      rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.bad <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, 
                     dta=budget.bad.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                     rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.good.cov <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name,
                          dta=budget.good.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                          rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.bad.cov <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, 
                         dta=budget.bad.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                         rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)


###Budget / LC5 councillor results
coun.good <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name,
                     dta=budget.good.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                     rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.bad <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, 
                    dta=budget.bad.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                    rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.good.cov <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name,
                         dta=budget.good.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                         rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.bad.cov <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, 
                        dta=budget.bad.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                        rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

##### Verified Recipient Effects #####

###Budget / LC5 chair results, verified compliers
chair.good.c <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name,
                        dta=budget.good.lc5.chair.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                        rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.bad.c <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, 
                       dta=budget.bad.lc5.chair.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                       rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.good.cov.c <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name,
                            dta=budget.good.lc5.chair.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                            rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.bad.cov.c <- felm.ri(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, 
                           dta=budget.bad.lc5.chair.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                           rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)


###Budget / LC5 councillor results, verified compliers
coun.good.c <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name,
                       dta=budget.good.lc5.councillor.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                       rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.bad.c <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, 
                      dta=budget.bad.lc5.councillor.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                      rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.good.cov.c <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name,
                           dta=budget.good.lc5.councillor.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                           rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.bad.cov.c <- felm.ri(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, 
                          dta=budget.bad.lc5.councillor.comp.c, treat.var = "budget.treat", rand.ob=Budget_RI, 
                          rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)


###Main Effects, Piped Values
te.good <- c(chair.good$ate, chair.good.cov$ate, coun.good$ate, coun.good.cov$ate)
se.good <- c(chair.good$se, chair.good.cov$se, coun.good$se, coun.good.cov$se)
base.c.good <- c(mean(budget.good.lc5.chair.comp$lc5.chair.inc.vote[budget.good.lc5.chair.comp$budget.treat==0], na.rm=TRUE),
                 mean(budget.good.lc5.chair.comp$lc5.chair.inc.vote[budget.good.lc5.chair.comp$budget.treat==0], na.rm=TRUE),
                 mean(budget.good.lc5.councillor.comp$lc5.councillor.inc.vote[budget.good.lc5.councillor.comp$budget.treat==0], na.rm=TRUE),
                 mean(budget.good.lc5.councillor.comp$lc5.councillor.inc.vote[budget.good.lc5.councillor.comp$budget.treat==0], na.rm=TRUE))
n.good <- c(chair.good$N,coun.good$N)

te.bad <- c(chair.bad$ate, chair.bad.cov$ate, coun.bad$ate, coun.bad.cov$ate)
se.bad <- c(chair.bad$se, chair.bad.cov$se, coun.bad$se, coun.bad.cov$se)
base.c.bad <- c(mean(budget.bad.lc5.chair.comp$lc5.chair.inc.vote[budget.bad.lc5.chair.comp$budget.treat==0], na.rm=TRUE),
                mean(budget.bad.lc5.chair.comp$lc5.chair.inc.vote[budget.bad.lc5.chair.comp$budget.treat==0], na.rm=TRUE),
                mean(budget.bad.lc5.councillor.comp$lc5.councillor.inc.vote[budget.bad.lc5.councillor.comp$budget.treat==0], na.rm=TRUE),
                mean(budget.bad.lc5.councillor.comp$lc5.councillor.inc.vote[budget.bad.lc5.councillor.comp$budget.treat==0], na.rm=TRUE))
n.bad <- c(chair.bad$N,coun.bad$N)

###Compliers, Piped Valued
te.good.c <- c(chair.good.c$ate, chair.good.cov.c$ate, coun.good.c$ate, coun.good.cov.c$ate)
se.good.c <- c(chair.good.c$se, chair.good.cov.c$se, coun.good.c$se, coun.good.cov.c$se)
base.c.good.c <- c(mean(budget.good.lc5.chair.comp.c$lc5.chair.inc.vote[budget.good.lc5.chair.comp.c$budget.treat==0], na.rm=TRUE),
                   mean(budget.good.lc5.chair.comp.c$lc5.chair.inc.vote[budget.good.lc5.chair.comp.c$budget.treat==0], na.rm=TRUE),
                   mean(budget.good.lc5.councillor.comp.c$lc5.councillor.inc.vote[budget.good.lc5.councillor.comp.c$budget.treat==0], na.rm=TRUE),
                   mean(budget.good.lc5.councillor.comp.c$lc5.councillor.inc.vote[budget.good.lc5.councillor.comp.c$budget.treat==0], na.rm=TRUE))
n.good.c <- c(chair.good.c$N,coun.good.c$N)

te.bad.c <- c(chair.bad.c$ate, chair.bad.cov.c$ate, coun.bad.c$ate, coun.bad.cov.c$ate)
se.bad.c <- c(chair.bad.c$se, chair.bad.cov.c$se, coun.bad.c$se, coun.bad.cov.c$se)
base.c.bad.c <- c(mean(budget.bad.lc5.chair.comp.c$lc5.chair.inc.vote[budget.bad.lc5.chair.comp.c$budget.treat==0], na.rm=TRUE),
                  mean(budget.bad.lc5.chair.comp.c$lc5.chair.inc.vote[budget.bad.lc5.chair.comp.c$budget.treat==0], na.rm=TRUE),
                  mean(budget.bad.lc5.councillor.comp.c$lc5.councillor.inc.vote[budget.bad.lc5.councillor.comp.c$budget.treat==0], na.rm=TRUE),
                  mean(budget.bad.lc5.councillor.comp.c$lc5.councillor.inc.vote[budget.bad.lc5.councillor.comp.c$budget.treat==0], na.rm=TRUE))
n.bad.c <- c(chair.bad.c$N,coun.bad.c$N)

x.axis.lab <- "Proportion Voting for Incumbent or Incumbent Party"

###Plot
#pdf("VoteChoice_Main_170816.pdf",height=7, width=6)
heights <- c(8,6,3,1)
layout(matrix(1:5, ncol = 1), widths = 1, heights = c(4,4,4,4,1), respect = FALSE)

###Good News Plot (Main)
par(mar=c(3.1,9.1,3.1,6.1), mgp=c(2,0.8,0), cex.main=1.6, cex.axis=1.4, bty="o")
plot(1, type="n", xlim=c(min(c(base.c.good+te.good-1.645*se.good,base.c.bad+te.bad-1.645*se.bad)), max(c(base.c.good+te.good+1.645*se.good,base.c.bad+te.bad+1.645*se.bad))+0.01), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
title("Good News (All Subjects)", line=0.5)
points(x=te.good+base.c.good,y=heights, pch=c(1,16,1,16), cex=3)

for (i in 1:length(te.good)){
  lines(x=c(te.good[i]+base.c.good[i]-1.645*se.good[i],te.good[i]+base.c.good[i]+1.645*se.good[i]), y=c(heights[i],heights[i]), lwd=2)
}

for (i in c(2,4)){
  lines(x=c(base.c.good[i],base.c.good[i]), y=c(heights[i]+3,heights[i]-1), lty="dotdash")
}

axis(2, at=c(7,2), labels=c("LC5 Chair", "LC5 Councillor"), las=2, tck=0, cex.axis=1.4)
axis(4, at=c(7,2), labels=c(paste("n=",n.good[1],sep=""), paste("n=",n.good[2],sep="")), las=2, tck=0, cex.axis=1.4)

###Good News Plot (Compliers)
par(mar=c(3.1,9.1,3.1,6.1), mgp=c(2,0.8,0), cex.main=1.6, cex.axis=1.4, bty="o")
#plot(1, type="n", xlim=c(min(c(base.c.good.c+te.good.c-1.645*se.good.c,base.c.bad.c+te.bad.c-1.645*se.bad.c)), max(c(base.c.good.c+te.good.c+1.645*se.good.c,base.c.bad.c+te.bad.c+1.645*se.bad.c))), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
plot(1, type="n", xlim=c(min(c(base.c.good+te.good-1.645*se.good,base.c.bad+te.bad-1.645*se.bad)), max(c(base.c.good+te.good+1.645*se.good,base.c.bad+te.bad+1.645*se.bad))+0.01), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
title("Good News (Verified Recipients)", line=0.5)
points(x=te.good.c+base.c.good.c,y=heights, pch=c(1,16,1,16), cex=3)

for (i in 1:length(te.good.c)){
  lines(x=c(te.good.c[i]+base.c.good.c[i]-1.645*se.good.c[i],te.good.c[i]+base.c.good.c[i]+1.645*se.good.c[i]), y=c(heights[i],heights[i]), lwd=2)
}

for (i in c(2,4)){
  lines(x=c(base.c.good.c[i],base.c.good.c[i]), y=c(heights[i]+3,heights[i]-1), lty="dotdash")
}

axis(2, at=c(7,2), labels=c("LC5 Chair", "LC5 Councillor"), las=2, tck=0, cex.axis=1.4)
axis(4, at=c(7,2), labels=c(paste("n=",n.good.c[1],sep=""), paste("n=",n.good.c[2],sep="")), las=2, tck=0, cex.axis=1.4)

###Bad News Plot (Main)
par(mar=c(3.1,9.1,3.1,6.1), mgp=c(2,0.8,0), cex.main=1.6, cex.axis=1.4, bty="o")
plot(1, type="n", xlim=c(min(c(base.c.good+te.good-1.645*se.good,base.c.bad+te.bad-1.645*se.bad)), max(c(base.c.good+te.good+1.645*se.good,base.c.bad+te.bad+1.645*se.bad))+0.01), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
title("Bad News (All Subjects)", line=0.5)
points(x=te.bad+base.c.bad,y=heights, pch=c(1,16,1,16), cex=3)

for (i in 1:length(te.bad)){
  lines(x=c(te.bad[i]+base.c.bad[i]-1.645*se.bad[i],te.bad[i]+base.c.bad[i]+1.645*se.bad[i]), y=c(heights[i],heights[i]), lwd=2)
}

for (i in c(2,4)){
  lines(x=c(base.c.bad[i],base.c.bad[i]), y=c(heights[i]+3,heights[i]-1), lty="dotdash")
}

axis(2, at=c(7,2), labels=c("LC5 Chair", "LC5 Councillor"), las=2, tck=0, cex.axis=1.4)
axis(4, at=c(7,2), labels=c(paste("n=",n.bad[1],sep=""), paste("n=",n.bad[2],sep="")), las=2, tck=0, cex.axis=1.4)

###Bad News Plot (Compliers)
#plot(1, type="n", xlim=c(min(c(base.c.good.c+te.good.c-1.645*se.good.c,base.c.bad.c+te.bad.c-1.645*se.bad.c)), max(c(base.c.good.c+te.good.c+1.645*se.good.c,base.c.bad.c+te.bad.c+1.645*se.bad.c))), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
plot(1, type="n", xlim=c(min(c(base.c.good+te.good-1.645*se.good,base.c.bad+te.bad-1.645*se.bad)), max(c(base.c.good+te.good+1.645*se.good,base.c.bad+te.bad+1.645*se.bad))+0.01), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
title("Bad News (Verified Recipients)", line=0.5)
points(x=te.bad.c+base.c.bad.c,y=heights, pch=c(1,16,1,16), cex=3)

for (i in 1:length(te.bad.c)){
  lines(x=c(te.bad.c[i]+base.c.bad.c[i]-1.645*se.bad.c[i],te.bad.c[i]+base.c.bad.c[i]+1.645*se.bad.c[i]), y=c(heights[i],heights[i]), lwd=2)
}

for (i in c(2,4)){
  lines(x=c(base.c.bad.c[i],base.c.bad.c[i]), y=c(heights[i]+3,heights[i]-1), lty="dotdash")
}

axis(2, at=c(7,2), labels=c("LC5 Chair", "LC5 Councillor"), las=2, tck=0, cex.axis=1.4)
axis(4, at=c(7,2), labels=c(paste("n=",n.bad.c[1],sep=""), paste("n=",n.bad.c[2],sep="")), las=2, tck=0, cex.axis=1.4)

###Legend
par(mar=c(0.1,0.1,0.1,0.1), bty="n")
plot(1, type="n", xlim=c(0,10), ylim=c(0,2), xaxt="n", yaxt="n", ylab="", xlab="")
points(x=c(4.5,7.5),y=c(0.8,0.8), pch=c(1,16), cex=3)
lines(x=c(1.2,1.2),y=c(1.7,0.3), lty="dotdash")
text(x=1.3, y=0.8, pos=4, "Mean Value (Control)", cex=1.4)
text(x=4.6, y=0.8, pos=4, "Without Covariates", cex=1.4)
text(x=7.6, y=0.8, pos=4, "With Covariates", cex=1.4)

#dev.off()


######################################################################################
###Figure 7.4: treatment effect of good and bad news on turnout
######################################################################################

h7a.b.1 <- felm(lc5.turnout ~ budget.treat + lc5.turnout.intent | location.id | 0 | district_lower, data=budget.good)
h7b.b.1 <- felm(lc5.turnout ~ budget.treat + lc5.turnout.intent | location.id | 0 | district_lower, data=budget.bad)
h7a.b.1c <- felm(lc5.turnout ~ budget.treat + lc5.turnout.intent + b.Q1_living_conditions + turnout.2011.LC5 + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.good)
h7b.b.1c <- felm(lc5.turnout ~ budget.treat + lc5.turnout.intent + b.Q1_living_conditions + turnout.2011.LC5 + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.bad)

lc5.turnout.good <- felm.ri(lc5.turnout ~ budget.treat + lc5.turnout.intent | location.id | 0 | district_lower,
                            dta=budget.good, treat.var = "budget.treat", rand.ob=Budget_RI, 
                            rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

lc5.turnout.good.cov <- felm.ri(lc5.turnout ~ budget.treat + lc5.turnout.intent + b.Q1_living_conditions + turnout.2011.LC5 + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower,
                                dta=budget.good, treat.var = "budget.treat", rand.ob=Budget_RI, 
                                rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

lc5.turnout.bad <- felm.ri(lc5.turnout ~ budget.treat + lc5.turnout.intent | location.id | 0 | district_lower, 
                           dta=budget.bad, treat.var = "budget.treat", rand.ob=Budget_RI, 
                           rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

lc5.turnout.bad.cov <- felm.ri(lc5.turnout ~ budget.treat + lc5.turnout.intent + b.Q1_living_conditions + turnout.2011.LC5 + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, 
                               dta=budget.bad, treat.var = "budget.treat", rand.ob=Budget_RI, 
                               rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

###Piped values
te.both <- c(lc5.turnout.good$ate, lc5.turnout.good.cov$ate, lc5.turnout.bad$ate, lc5.turnout.bad.cov$ate) #-2.422159e-03, -4.341485e-03, -6.695883e-05, -1.759361e-04
se.both <- c(lc5.turnout.good$se, lc5.turnout.good.cov$se, lc5.turnout.bad$se, lc5.turnout.bad.cov$se) #0.01200256, 0.01188337, 0.01229442, 0.01223369
base.both <- c(mean(budget.good$lc5.turnout[budget.good$budget.treat==0], na.rm=TRUE),
               mean(budget.good$lc5.turnout[budget.good$budget.treat==0], na.rm=TRUE),
               mean(budget.bad$lc5.turnout[budget.good$budget.treat==0], na.rm=TRUE),
               mean(budget.bad$lc5.turnout[budget.good$budget.treat==0], na.rm=TRUE))
n.both <- c(6468,5836)
x.axis.lab <- "Proportion Voting in District Election"

#pdf("VoteChoice_Turnout_170220.pdf",height=3, width=6.5)
heights <- c(8,6,3,1)
layout(matrix(1:2, ncol = 1), widths = 1, heights = c(4,1), respect = FALSE)

### Plot
par(mar=c(3.1,6.1,3.1,5.1), mgp=c(2,0.8,0), cex=1, cex.main=1.6, cex.axis=1, bty="o")
plot(1, type="n", xlim=c(min(base.both+te.both-1.645*se.both)-0.055, max(base.both+te.both+1.645*se.both)+0.05), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1)
title("", line=0.5)
points(x=te.both+base.both,y=heights, pch=c(1,16,1,16), cex=3)

for (i in 1:length(te.both)){
  lines(x=c(te.both[i]+base.both[i]-1.645*se.both[i],te.both[i]+base.both[i]+1.645*se.both[i]), y=c(heights[i],heights[i]), lwd=2)
}

for (i in c(2,4)){
  lines(x=c(base.both[i],base.both[i]), y=c(heights[i]+3,heights[i]-1), lty="dotdash", col="red", lwd=2)
}

axis(2, at=c(7,2), labels=c("Good News", "Bad News"), las=2, tck=0, cex.axis=1)
axis(4, at=c(7,2), labels=c(paste("n=",n.both[1],sep=""), paste("n=",n.both[2],sep="")), las=2, tck=0, cex.axis=1)

###Legend
par(mar=c(0.1,0.1,0.1,0.1), bty="n")
plot(1, type="n", xlim=c(0,10), ylim=c(0,2), xaxt="n", yaxt="n", ylab="", xlab="")
points(x=c(4.5,7.5),y=c(0.8,0.8), pch=c(1,16), cex=3)
lines(x=c(1.2,1.2),y=c(1.7,0.3), lty="dotdash", col="red", lwd=2)
text(x=1.3, y=0.8, pos=4, "Mean Value (Control)", cex=1)
text(x=4.6, y=0.8, pos=4, "Without Covariates", cex=1)
text(x=7.6, y=0.8, pos=4, "With Covariates", cex=1)
#dev.off()


######################################################################################
###Table 7.1: Conditional effect of budget treatment on turnout based on alignment with incumbents
######################################################################################

h8a <- felm(lc5.turnout ~ budget.treat*aligned.lc5.chair.inc.orig + budget.treat*aligned.lc5.councillor.inc.orig + lc5.turnout.intent + b.Q1_living_conditions + turnout.2011.LC5 + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.good)

h8b <- felm(lc5.turnout ~ budget.treat*aligned.lc5.chair.inc.orig + budget.treat*aligned.lc5.councillor.inc.orig + lc5.turnout.intent + b.Q1_living_conditions + turnout.2011.LC5 + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.bad)

stargazer(h8a, h8b, type = "latex", 
          column.labels = c("Good News","Bad News"),
          dep.var.labels.include = FALSE,
          keep = c("budget.treat","aligned.lc5.chair.inc.orig","aligned.lc5.councillor.inc.orig","budget.treat:aligned.lc5.chair.inc.orig","budget.treat:aligned.lc5.councillor.inc.orig"),
          add.lines = list(c("Polling station fixed effects", "Yes", "Yes"),
                           c("Covariates", "Yes", "Yes")),
          dep.var.caption  = "DV: Turnout for LC5 Election",
          covariate.labels = c("Budget Treatment","LC5 Chair Alignment","LC5 Councillor Alignment","Budget Treatment X LC5 Chair Alignment", "Budget Treatment X LC5 Councillor Alignment"),
          notes.label = "Note: SEs clustered by politician; one-tailed tests; contested elections only.",
          df = FALSE, omit.stat = c("rsq","ser")
)


######################################################################################
###Table 7.2: Effects of budget treatment on evaluations of candidate integrity and effort
######################################################################################

#H3 (Candidate Integrity, suprised at corruption by LC5 chair)
h3a <- felm(lc5.integrity ~ budget.treat | location.id | 0 | district_lower, data=budget.good)
summary(h3a)

h3a.cov <- felm(lc5.integrity ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.good)
summary(h3a.cov)

h3b <- felm(lc5.integrity ~ budget.treat | location.id | 0 | district_lower, data=budget.bad)
summary(h3b)

h3b.cov <- felm(lc5.integrity ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.bad)
summary(h3b.cov)

#H4 (Candidate Effort)
h4a <- felm(lc5.effort ~ budget.treat | location.id | 0 | district_lower, data=budget.good)
summary(h4a)

h4a.cov <- felm(lc5.effort ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.good)
summary(h4a.cov)

h4b <- felm(lc5.effort ~ budget.treat | location.id | 0 | district_lower, data=budget.bad)
summary(h4b)

h4b.cov <- felm(lc5.effort ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | district_lower, data=budget.bad)
summary(h4b.cov)

stargazer(h3a, h3a.cov, h3b, h3b.cov, h4a, h4a.cov, h4b, h4b.cov, type="latex",
          dep.var.labels = c("LC5 Chair Integrity", "LC5 Councillor Effort"),
          keep = c("budget.treat"),
          column.labels = c("Good News","Bad News","Good News","Bad News"), column.separate = c(2,2,2,2),
          covariate.labels = c("Budget Treatment"),
          add.lines = list(c("Covariates", "No", "Yes","No","Yes","No", "Yes","No","Yes"),
                           c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes")),
          df = FALSE, omit.stat = c("rsq","ser"),
          notes.label = "Note: SEs clustered by politician; one-tailed tests; contested elections only."
) 


######################################################################################
###Table 7.3: Moderation of Information Effects on Vote Choice
######################################################################################

h1a.omni.ch <- felm(lc5.chair.inc.vote ~ budget.treat*budget.prior.diff + budget.certainty*budget.treat + budget.importance*budget.treat + aligned.lc5.chair.inc*budget.treat + b.trust.AG*budget.treat + b.trust.Twaweza*budget.treat + b.vote.buying*budget.treat + party.attachment*budget.treat + lc5.chair.intent | location.id | 0 | district_lower, data=budget.good.lc5.chair.comp)
summary(h1a.omni.ch)

h1a.omni.co <- felm(lc5.councillor.inc.vote ~ budget.treat*budget.prior.diff + budget.certainty*budget.treat + budget.importance*budget.treat + lc5.councillor.same.tribe*budget.treat + aligned.lc5.councillor.inc*budget.treat + b.trust.AG*budget.treat + b.trust.Twaweza*budget.treat + b.vote.buying*budget.treat + party.attachment*budget.treat + lc5.councillor.intent | location.id | 0 | subcounty.x, data=budget.good.lc5.councillor.comp)
summary(h1a.omni.co)

h1b.omni.ch <- felm(lc5.chair.inc.vote ~ budget.treat*budget.prior.diff + budget.certainty*budget.treat + budget.importance*budget.treat + aligned.lc5.chair.inc*budget.treat + b.trust.AG*budget.treat + b.trust.Twaweza*budget.treat + b.vote.buying*budget.treat + party.attachment*budget.treat + lc5.chair.intent | location.id | 0 | district_lower, data=budget.bad.lc5.chair.comp)
summary(h1b.omni.ch)

h1b.omni.co <- felm(lc5.councillor.inc.vote ~ budget.treat*budget.prior.diff + budget.certainty*budget.treat + budget.importance*budget.treat + lc5.councillor.same.tribe*budget.treat + aligned.lc5.councillor.inc*budget.treat + b.trust.AG*budget.treat + b.trust.Twaweza*budget.treat + b.vote.buying*budget.treat + party.attachment*budget.treat + lc5.councillor.intent | location.id | 0 | subcounty.x, data=budget.bad.lc5.councillor.comp)
summary(h1b.omni.co)

stargazer(h1a.omni.ch,h1a.omni.co,h1b.omni.ch,h1b.omni.co, single.row = TRUE)
#Note: post-formatting in Latex is necessary
#Note: the chapter table reports interaction coefficients, direct effects at top of output are not reported in table


######################################################################################
###Figure 7.5: Treatment effect of higher density among treated
######################################################################################

h11a.b.1 <- felm.ri(lc5.chair.inc.vote ~ density.treat2 + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name, 
                    dta=budget.good.lc5.chair.comp[budget.good.lc5.chair.comp$budget.treat==1,],
                    treat.var = "density.treat2", rand.ob=Density_RI,
                    rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

h11a.b.2 <- felm.ri(lc5.councillor.inc.vote ~ density.treat2 + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, 
                    dta=budget.good.lc5.councillor.comp[budget.good.lc5.councillor.comp$budget.treat==1,],
                    treat.var = "density.treat2", rand.ob=Density_RI,
                    rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

h11b.b.1 <- felm.ri(lc5.chair.inc.vote ~ density.treat2 + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name, 
                    dta=budget.bad.lc5.chair.comp[budget.bad.lc5.chair.comp$budget.treat==1,],
                    treat.var = "density.treat2", rand.ob=Density_RI,
                    rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

h11b.b.2 <- felm.ri(lc5.councillor.inc.vote ~ density.treat2 + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, 
                    dta=budget.bad.lc5.councillor.comp[budget.bad.lc5.councillor.comp$budget.treat==1,],
                    treat.var = "density.treat2", rand.ob=Density_RI,
                    rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

###Piped Values for Density Effect Among Treated, Figure 5
te.good <- c(h11a.b.1$ate, NA, h11a.b.2$ate, NA) #-0.03465979,NA,-0.04034953,NA
se.good <- c(h11a.b.1$se, NA, h11a.b.2$se, NA) #0.02791619,NA,0.05255581,NA
base.c.good <- c(mean(budget.good.lc5.chair.comp$lc5.chair.inc.vote[budget.good.lc5.chair.comp$budget.treat==1 & budget.good.lc5.chair.comp$density.treat2==0], na.rm=TRUE),
                 mean(budget.good.lc5.chair.comp$lc5.chair.inc.vote[budget.good.lc5.chair.comp$budget.treat==1 & budget.good.lc5.chair.comp$density.treat2==0], na.rm=TRUE),
                 mean(budget.good.lc5.councillor.comp$lc5.councillor.inc.vote[budget.good.lc5.councillor.comp$budget.treat==1 & budget.good.lc5.councillor.comp$density.treat==0], na.rm=TRUE),
                 mean(budget.good.lc5.councillor.comp$lc5.councillor.inc.vote[budget.good.lc5.councillor.comp$budget.treat==1 & budget.good.lc5.councillor.comp$density.treat==0], na.rm=TRUE))
n.good <- c(1703,1335)

te.bad <- c(h11b.b.1$ate, NA, h11b.b.2$ate, NA) #0.001373277,NA,-0.035120041,NA
se.bad <- c(h11b.b.1$se, NA, h11b.b.2$se, NA) #0.02863102,NA,0.05652717,NA
base.c.bad <- c(mean(budget.bad.lc5.chair.comp$lc5.chair.inc.vote[budget.bad.lc5.chair.comp$budget.treat==1 & budget.bad.lc5.chair.comp$density.treat2==0], na.rm=TRUE),
                mean(budget.bad.lc5.chair.comp$lc5.chair.inc.vote[budget.bad.lc5.chair.comp$budget.treat==1 & budget.bad.lc5.chair.comp$density.treat2==0], na.rm=TRUE),
                mean(budget.bad.lc5.councillor.comp$lc5.councillor.inc.vote[budget.bad.lc5.councillor.comp$budget.treat==1 & budget.bad.lc5.councillor.comp$density.treat2==0], na.rm=TRUE),
                mean(budget.bad.lc5.councillor.comp$lc5.councillor.inc.vote[budget.bad.lc5.councillor.comp$budget.treat==1 & budget.bad.lc5.councillor.comp$density.treat2==0], na.rm=TRUE))
n.bad <- c(1325,1157)
x.axis.lab <- "Proportion Voting for Incumbent or Incumbent Party"

#pdf("VoteChoice_Fig5_Density_170323.pdf",height=3.7, width=6)
heights <- c(8,6,3,1)
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(4,4,1), respect = FALSE)

###Good News Plot
par(mar=c(3.1,9.1,3.1,6.1), mgp=c(2,0.8,0), cex.main=1.8, cex.axis=1.4, bty="o")
plot(1, type="n", xlim=c(min(c(base.c.good+te.good-1.645*se.good,base.c.bad+te.bad-1.645*se.bad), na.rm=TRUE), max(c(base.c.good+te.good+1.645*se.good,base.c.bad+te.bad+1.645*se.bad), na.rm=TRUE)), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
title("Good News", line=0.5)
points(x=te.good+base.c.good,y=heights-1, pch=c(16,1,16,1), cex=3)

for (i in 1:length(te.good)){
  lines(x=c(te.good[i]+base.c.good[i]-1.645*se.good[i],te.good[i]+base.c.good[i]+1.645*se.good[i]), y=c(heights[i]-1,heights[i]-1), lwd=2)
}

for (i in c(2,4)){
  lines(x=c(base.c.good[i],base.c.good[i]), y=c(heights[i]+3,heights[i]-1), lty="dotdash")
}

axis(2, at=c(7,2), labels=c("LC5 Chair", "LC5 Councillor"), las=2, tck=0, cex.axis=1.4)
axis(4, at=c(7,2), labels=c(paste("n=",n.good[1],sep=""), paste("n=",n.good[2],sep="")), las=2, tck=0, cex.axis=1.4)

###Bad News Plot
plot(1, type="n", xlim=c(min(c(base.c.good+te.good-1.645*se.good,base.c.bad+te.bad-1.645*se.bad), na.rm=TRUE), max(c(base.c.good+te.good+1.645*se.good,base.c.bad+te.bad+1.645*se.bad), na.rm=TRUE)), ylim=c(0,9), yaxt="n", ylab="", xlab=x.axis.lab, cex.lab=1.4)
title("Bad News", line=0.5)
points(x=te.bad+base.c.bad,y=heights-1, pch=c(16,1,16,1), cex=3)

for (i in 1:length(te.bad)){
  lines(x=c(te.bad[i]+base.c.bad[i]-1.645*se.bad[i],te.bad[i]+base.c.bad[i]+1.645*se.bad[i]), y=c(heights[i]-1,heights[i]-1), lwd=2)
}

for (i in c(2,4)){
  lines(x=c(base.c.bad[i],base.c.bad[i]), y=c(heights[i]+3,heights[i]-1), lty="dotdash")
}

axis(2, at=c(7,2), labels=c("LC5 Chair", "LC5 Councillor"), las=2, tck=0, cex.axis=1.4)
axis(4, at=c(7,2), labels=c(paste("n=",n.bad[1],sep=""), paste("n=",n.bad[2],sep="")), las=2, tck=0, cex.axis=1.4)

###Legend
par(mar=c(0.1,0.1,0.1,0.1), bty="n")
plot(1, type="n", xlim=c(0,10), ylim=c(0,2), xaxt="n", yaxt="n", ylab="", xlab="")
points(x=c(6),y=c(0.88), pch=16, cex=3)
lines(x=c(2.7,2.7),y=c(1.7,0.3), lty="dotdash")
text(x=2.8, y=0.8, pos=4, "Mean Value (Control)", cex=1.4)
text(x=6.1, y=0.8, pos=4, "With Covariates", cex=1.4)
#dev.off()


######################################################################################
###Table 7.4: Effect of budget treatment density on vote choice for district (LC V) incumbents
######################################################################################

b.ri <- data.matrix(Budget_RI)
d.ri <- data.matrix(Density_RI)
#Note: NAs introduced by coercion warning generated because "district" and "village" variables cannot be made numeric
#Note: these NAs are excluded when only the treatment variables are pasted into the final "BD_RI" dataframe in the block below

BD_RI <- matrix(paste(b.ri,d.ri,sep="."),nrow=16083,ncol=10003)
BD_RI[BD_RI=="0.NA" | BD_RI=="1.NA"] <- NA
BD_RI[BD_RI=="0. 0"] <- "0.0"
BD_RI[BD_RI=="0. 1"] <- "0.1"
BD_RI[BD_RI=="1. 0"] <- "1.0"
BD_RI[BD_RI=="1. 1"] <- "1.1"
BD_RI <- as.data.frame(BD_RI)
BD_RI <- cbind(Budget_RI[,1:3],BD_RI[,4:ncol(BD_RI)])
remove(b.ri,d.ri)

h11a.b.1 <- felm(lc5.chair.inc.vote ~ bd.multi.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
p1a <- ifelse(h11a.b.1$coef[1]>0, summary(h11a.b.1)$coef[1,4]/2, 1-summary(h11a.b.1)$coef[1,4]/2)
p1b <- ifelse(h11a.b.1$coef[2]>0, summary(h11a.b.1)$coef[2,4]/2, 1-summary(h11a.b.1)$coef[2,4]/2)
p1c <- ifelse(h11a.b.1$coef[3]>0, summary(h11a.b.1)$coef[3,4]/2, 1-summary(h11a.b.1)$coef[3,4]/2)

h11a.b.2 <- felm(lc5.councillor.inc.vote ~ bd.multi.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
p2a <- ifelse(h11a.b.2$coef[1]>0, summary(h11a.b.2)$coef[1,4]/2, 1-summary(h11a.b.2)$coef[1,4]/2)
p2b <- ifelse(h11a.b.2$coef[2]>0, summary(h11a.b.2)$coef[2,4]/2, 1-summary(h11a.b.2)$coef[2,4]/2)
p2c <- ifelse(h11a.b.2$coef[3]>0, summary(h11a.b.2)$coef[3,4]/2, 1-summary(h11a.b.2)$coef[3,4]/2)

h11b.b.1 <- felm(lc5.chair.inc.vote ~ bd.multi.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
p3a <- ifelse(h11b.b.1$coef[1]<0, summary(h11b.b.1)$coef[1,4]/2, 1-summary(h11b.b.1)$coef[1,4]/2)
p3b <- ifelse(h11b.b.1$coef[2]<0, summary(h11b.b.1)$coef[2,4]/2, 1-summary(h11b.b.1)$coef[2,4]/2)
p3c <- ifelse(h11b.b.1$coef[3]<0, summary(h11b.b.1)$coef[3,4]/2, 1-summary(h11b.b.1)$coef[3,4]/2)

h11b.b.2 <- felm(lc5.councillor.inc.vote ~ bd.multi.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
p4a <- ifelse(h11b.b.2$coef[1]<0, summary(h11b.b.2)$coef[1,4]/2, 1-summary(h11b.b.2)$coef[1,4]/2)
p4b <- ifelse(h11b.b.2$coef[2]<0, summary(h11b.b.2)$coef[2,4]/2, 1-summary(h11b.b.2)$coef[2,4]/2)
p4c <- ifelse(h11b.b.2$coef[3]<0, summary(h11b.b.2)$coef[3,4]/2, 1-summary(h11b.b.2)$coef[3,4]/2)

#Note: treatment coefficients displayed in a different order than output table

chair.good.crossed <- felm.ri2(lc5.chair.inc.vote ~ bd.multi.treat2 + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name,
                               dta=budget.good.lc5.chair.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                               rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)


coun.good.crossed <- felm.ri2(lc5.councillor.inc.vote ~ bd.multi.treat2 + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, 
                              dta=budget.good.lc5.councillor.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                              rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.bad.crossed <- felm.ri2(lc5.chair.inc.vote ~ bd.multi.treat2 + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name,
                              dta=budget.bad.lc5.chair.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                              rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)


coun.bad.crossed <- felm.ri2(lc5.councillor.inc.vote ~ bd.multi.treat2 + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, 
                             dta=budget.bad.lc5.councillor.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                             rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

##Top part of table
stargazer(h11a.b.1, h11a.b.2, h11b.b.1, h11b.b.2, type = "latex", 
          keep = c("bd.multi.treatbudget0.density1","bd.multi.treatbudget1.density0","bd.multi.treatbudget1.density1"),
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(2,2),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Control, High Density (RI)", "Treated, Low Density (RI)", "Treated, High Density (RI)"),
          notes = "Note: SEs derived from RI; one-tailed tests as pre-registered; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          se = list(chair.good.crossed$se,coun.good.crossed$se,chair.bad.crossed$se,coun.bad.crossed$se),
          p = list(chair.good.crossed$p.one.way.greater, coun.good.crossed$p.one.way.greater, chair.bad.crossed$p.one.way.lesser, coun.bad.crossed$p.one.way.lesser),
          order=c(1,2,3),
          notes.append = FALSE,
          notes.label = "",
          add.lines = list(c("Paired village fixed effects", "Yes", "Yes","Yes","Yes"),
                           c("Covariates", "Yes", "Yes","Yes","Yes"))
)

##Bottom part of table
stargazer(h11a.b.1, h11a.b.2, h11b.b.1, h11b.b.2, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(2,2),
          keep = c("bd.multi.treatbudget0.density1","bd.multi.treatbudget1.density0","bd.multi.treatbudget1.density1","lc5.chair.intent","lc5.councillor.intent"),
          add.lines = list(c("Paired village fixed effects", "Yes", "Yes","Yes","Yes"),
                           c("Covariates", "Yes", "Yes","Yes","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Control, High Density","Budget Treatment, Low Density", "Budget Treatment, High Density", "LC V Chair Intent", "LC V Councillor Intent"),
          notes.label = "Note: SEs clustered by politician; one-tailed tests; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          p=list(c(p1a,p1b,p1c),c(p2a,p2b,p2c),c(p3a,p3b,p3c),c(p4a,p4b,p4c))
)
#Note: post-formatting in LaTex required


######################################################################################
###Table 7.5: Main effects of salient public services treatment.
######################################################################################

#H1 (Vote Choice / Main)
h1a.ps.1 <- felm(lc5.chair.inc.vote ~ ps.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=ps.good.lc5.chair.comp)
summary(h1a.ps.1)

h1a.ps.2 <- felm(lc5.councillor.inc.vote ~ ps.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=ps.good.lc5.councillor.comp)
summary(h1a.ps.2)

h1a.ps.3 <- felm(lc3.chair.inc.vote ~ ps.treat + lc3.chair.intent | location.id | 0 | lc3.chair.name, data=ps.good.lc3.chair.comp)
summary(h1a.ps.3)

h1a.ps.4 <- felm(lc3.councillor.inc.vote ~ ps.treat + lc3.councillor.intent | location.id | 0 | lc3.councillor.name, data=ps.good.lc3.councillor.comp)
summary(h1a.ps.4)

h1b.ps.1 <- felm(lc5.chair.inc.vote ~ ps.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=ps.bad.lc5.chair.comp)
summary(h1b.ps.1)

h1b.ps.2 <- felm(lc5.councillor.inc.vote ~ ps.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=ps.bad.lc5.councillor.comp)
summary(h1b.ps.2)

h1b.ps.3 <- felm(lc3.chair.inc.vote ~ ps.treat + lc3.chair.intent | location.id | 0 | lc3.chair.name, data=ps.bad.lc3.chair.comp)
summary(h1b.ps.3)

h1b.ps.4 <- felm(lc3.councillor.inc.vote ~ ps.treat + lc3.councillor.intent | location.id | 0 | lc3.councillor.name, data=ps.bad.lc3.councillor.comp)
summary(h1b.ps.4)

stargazer(h1a.ps.1, h1a.ps.2, h1a.ps.3, h1a.ps.4, h1b.ps.1, h1b.ps.2, h1b.ps.3, h1b.ps.4, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC3 Chair", "LC3 Councillor", "LC5 Chair", "LC5 Councillor", "LC3 Chair", "LC3 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes")),
          dep.var.caption  = "<em>DV: Vote Choice for the Incumbent; Intention as Control<em>",
          notes.label = "<em>Note:</em> SEs clustered by politician",
          df = FALSE, omit.stat = c("rsq","ser"), star.cutoffs=c(0.2,0.1,0.02), report = c('vcsp'))

#H7 (Turn Out / Main)
h7a.ps.1 <- felm(lc5.turnout ~ ps.treat + lc5.turnout.intent | location.id | 0 | district_lower, data=ps.good)
summary(h7a.ps.1)

h7a.ps.2 <- felm(lc3.turnout ~ ps.treat + lc3.turnout.intent | location.id | 0 | subcounty.x, data=ps.good)
summary(h7a.ps.2)

h7b.ps.1 <- felm(lc5.turnout ~ ps.treat + lc5.turnout.intent | location.id | 0 | district_lower, data=ps.bad)
summary(h7b.ps.1)

h7b.ps.2 <- felm(lc3.turnout ~ ps.treat + lc3.turnout.intent | location.id | 0 | subcounty.x, data=ps.bad)
summary(h7b.ps.2)

stargazer(h7a.ps.1, h7a.ps.2, h7b.ps.1, h7b.ps.2, type = "latex", 
          dep.var.labels = c("LC5 Elections", "LC3 Elections", "LC5 Elections", "LC3 Elections"),
          column.labels = c("Good News","Bad News"), column.separate = c(2,2),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes")),
          dep.var.caption  = "<em>DV: Turnout, Intention as Covariate<em>",
          notes.label = "Note: SEs clustered by politician",
          df = FALSE, omit.stat = c("rsq","ser"), star.cutoffs=c(0.2,0.1,0.02), report = c('vcsp')
)

#H3 (Candidate Integrity)
h3a.ps.1 <- felm(lc5.integrity ~ ps.treat | location.id | 0 | district_lower, data=ps.good)
summary(h3a.ps.1)

h3a.ps.2 <- felm(lc3.integrity ~ ps.treat | location.id | 0 | subcounty.x, data=ps.good)
summary(h3a.ps.2)

h3b.ps.1 <- felm(lc5.integrity ~ ps.treat | location.id | 0 | district_lower, data=ps.bad)
summary(h3b.ps.1)

h3b.ps.2 <- felm(lc3.integrity ~ ps.treat | location.id | 0 | subcounty.x, data=ps.bad)
summary(h3b.ps.2)

stargazer(h3a.ps.1, h3a.ps.2, h3b.ps.1, h3b.ps.2, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC3 Chair", "LC5 Chair", "LC3 Chair"),
          column.labels = c("Good News","Bad News"), column.separate = c(2,2),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes")),
          dep.var.caption  = "<em>DV: Surprised at corruption by Chairperson<em>",
          notes.label = "<em>Note:</em> SEs clustered by politician",
          df = FALSE, omit.stat = c("rsq","ser"), star.cutoffs=c(0.2,0.1,0.02), report = c('vcsp')
)

#H4 (Candidate Effort)
h4a.ps.1 <- felm(lc5.effort ~ ps.treat | location.id | 0 | lc5.councillor.name, data=ps.good)
summary(h4a.ps.1)

h4a.ps.2 <- felm(lc3.effort ~ ps.treat | location.id | 0 | lc3.councillor.name, data=ps.good)
summary(h4a.ps.2)

h4b.ps.1 <- felm(lc5.effort ~ ps.treat | location.id | 0 | lc5.councillor.name, data=ps.bad)
summary(h4b.ps.1)

h4b.ps.2 <- felm(lc3.effort ~ ps.treat | location.id | 0 | lc3.councillor.name, data=ps.bad)
summary(h4b.ps.2)

stargazer(h4a.ps.1, h4a.ps.2, h4b.ps.1, h4b.ps.2, type = "latex", 
          dep.var.labels = c("LC5 Councillor", "LC3 Councillor", "LC5 Councillor", "LC3 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(2,2),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes")),
          dep.var.caption  = "<em>DV: Effort by Councillor<em>",
          notes.label = "<em>Note:</em> SEs clustered by politician",
          df = FALSE, omit.stat = c("rsq","ser"), star.cutoffs=c(0.2,0.1,0.02), report = c('vcsp')
)

#notes: each of these tables produces the treatment effects manually formatted in the reported table, along with associated SEs and p-values
#notes: p-values are manually transformed into one-sided values in LaTex
#notes: stars are manually transformed to match one-sided p-values
