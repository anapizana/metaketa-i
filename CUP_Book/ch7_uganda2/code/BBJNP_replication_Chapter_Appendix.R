######################################################################################
######################################################################################
#Replication code for:

#Mark T. Buntaine, Sarah S. Bush, Ryan Jablonski, Daniel Nielson, Paula Pickering
#Budgets, SMS Texts, and Votes in Uganda

#Prepared by: Mark Buntaine
#Mark Buntaine contact (as of April 2019): buntaine@bren.ucsb.edu

#Compiled using R Version 3.5.1 (version "Feather Spray") on Mac running OS X 10.14.3
######################################################################################
######################################################################################

#Options turned on for replication report only
#+ fig.width=12, fig.height=12,warning=FALSE,message=FALSE
options(warn=-1)

######################################################################################
###Packages
######################################################################################
library(lfe) #Version 2.8-2
#Note: on warnings, see: https://cran.r-project.org/web/packages/lfe/vignettes/identification.pdf
library(stargazer) #Version 5.2.2
library(dplyr) #Version 0.7.8
library(cobalt) #Version 3.6.1
library(xtable) #Version 1.8-3
library(ggplot2) #Version 3.1.0
library(grid) #Version 3.5.1


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

felm.ri3 <- function(formula, dta, treat.var, rand.ob, rand.ob.info.cols, join.var, sims, weights, ...){
  require(lfe)
  print("1. data and rand.ob must have rows organized in identical order")
  print("2. treat.var should be first right-side entry in formula")
  print("3. join.var must have identical name between data and rand.ob")
  
  ate <- coef(felm(formula, data=dta, weights = weights))[1]
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

#function to plot multiple ggplot objects together
#from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Function for returning the mode of a distribution as a single integer
SingleMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


######################################################################################
###Data input, change file paths as appropriate
######################################################################################

#Update "root" as needed to directory with replication materials
root <- "~/Google Drive/Uganda Vote Choice project/Analysis/Replication/Replication_Chapter/"
setwd(root)

# data <- read.csv("MASTER_Analysis_181115.csv", stringsAsFactors=FALSE)
# data.pnas <- read.csv("~/Google Drive/Uganda Vote Choice project/Analysis/Replication/Replication_PNAS/data/MASTER_Analysis_Anon_180424.csv", stringsAsFactors=FALSE)
# #sum(data$id.cleaned==data.pnas$id.cleaned) #datasets match order
# data$end.y <- data.pnas$end.y
# data$start.y <- data.pnas$start.y
# data$X_submission_time.y <- data.pnas$X_submission_time.y
# write.csv(data, "MASTER_ANALYSIS_190312.csv", row.names = FALSE)

data <- read.csv("MASTER_Analysis_190312.csv", stringsAsFactors=FALSE)

Budget_RI <- read.csv("Budget_RI.csv", stringsAsFactors=FALSE)
Budget_RI <- Budget_RI[match(data$id.cleaned, Budget_RI$id.cleaned),] #reordering to match "data"

Density_RI <- read.csv("Density_RI.csv", stringsAsFactors=FALSE)
Density_RI <- Density_RI[match(data$id.cleaned, Density_RI$id.cleaned),] #reordering to match "data"

#Audit data for all districts
budget_audit=read.csv("lc5_budget_and_audit.csv", stringsAsFactors = FALSE)

#Electoral results for all lc5 councillors
candidates=read.csv("lc5_candidates.csv", stringsAsFactors = FALSE)

#Comparison of survey and official voting
votes.councillor=read.csv("councilor_official_vs_survey_votes.csv", stringsAsFactors = FALSE)
votes.chair=read.csv("chair_official_vs_survey_votes.csv", stringsAsFactors = FALSE)


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

data$partyID.slim <- factor(data$partyID.slim, levels=c("nrm","opposition","independent"))
data$lc5.councillor.party.slim <- ifelse(data$lc5.councillor.party %in% c("dp","fdc","upc"), "opposition", data$lc5.councillor.party)
data$lc5.councillor.party.vote.slim <- ifelse(data$lc5.councillor.party.vote %in% c("cp","dp","fdc","jeema","other","upc"), "opposition", data$lc5.councillor.party.vote)

#Adding missing category to some variables
data$aligned.lc5.chair.inc.orig <- data$aligned.lc5.chair.inc
data$aligned.lc5.councillor.inc.orig <- data$aligned.lc5.councillor.inc

data$aligned.lc5.chair.inc <- ifelse(is.na(data$aligned.lc5.chair.inc), "missing", data$aligned.lc5.chair.inc)
data$aligned.lc5.councillor.inc <- ifelse(is.na(data$aligned.lc5.councillor.inc), "missing", data$aligned.lc5.councillor.inc)

data$lc5.councillor.same.tribe <- ifelse(data$b.Q2_tribe_same_as_LC5=="yes",1,0)
data$lc5.councillor.same.tribe <- ifelse(data$b.Q2_tribe_same_as_LC5=="refused_to_ans","missing", data$lc5.councillor.same.tribe)

data$lc5.chair.inc.vote.orig <- ifelse(data$lc5.chair.party.vote==data$lc5.chair.party.orig, 1, 0)
data$lc5.councillor.inc.vote.orig <- ifelse(data$lc5.councillor.party.vote==data$lc5.councillor.party.orig, 1, 0)
#Note: "orig" variable is the party elected to the seat in 2011, regardless of subsequent party switching

#Fixed effect weights
data$fe.w <- ifelse(is.na(data$density.treat2), 1/(.5*.5), 1/(.8*.2))


######################################################################################
###Budget subsets
######################################################################################

#Prior-defined subgroups
budget.good <- subset(data, budget.actual > budget.prior | (budget.actual==budget.prior & budget.actual>=4))
budget.bad <- subset(data, budget.actual < budget.prior | (budget.actual==budget.prior & budget.actual<=2))

#These are the subgroups not defined by priors, for an extended analysis
budget.positive <- subset(data, budget.actual>=4)
budget.negative <- subset(data, budget.actual<=2)

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
###Figure A2: Comparison of incumbent vote share, pre- and post-election results beingannounced by politician
#checked: 3/14/19
######################################################################################

data$end.y <- as.POSIXct(data$end.y)
data$start.y <- as.POSIXct(data$start.y)
data$X_submission_time.y <- as.POSIXct(data$X_submission_time.y)

data$before.results <- ifelse((data$end.y < "2016-02-25 17:00 PST" & data$end.y > "2016-02-25 00:00 PST") | (data$end.y < "2016-02-25 00:00 PST" & data$X_submission_time.y < "2016-02-25 17:00 PST"), 1, 0)

###Creating chairs dataframe and plot
name <- as.character(unique(data$lc5.chair.name))
table(data$lc5.chair.name,data$lc5.chair.competitive) #To find uncompetitive chairs to remove
chair.not.competitive <- c("bazanye, milton mutabazi","birungi, norman k. b.","kanaku, michael","orot, ismael")
chairs <- data.frame(name[!(name %in% chair.not.competitive)])
names(chairs)[1] <- "name"

for (i in 1:nrow(chairs)){
  sub <- subset(data, lc5.chair.name==chairs$name[i])
  chairs$vote.before[i] <- nrow(sub[sub$lc5.chair.inc.vote==1 & !is.na(sub$lc5.chair.inc.vote) & sub$before.results==1,])
  chairs$n.before[i] <- nrow(sub[!is.na(sub$lc5.chair.inc.vote) & sub$before.results==1,])
  chairs$vote.after[i] <- nrow(sub[sub$lc5.chair.inc.vote==1 & !is.na(sub$lc5.chair.inc.vote) & sub$before.results==0,])
  chairs$n.after[i] <- nrow(sub[!is.na(sub$lc5.chair.inc.vote) & sub$before.results==0,])
}

chairs <- subset(chairs, n.before>0)
chairs$percent.before <- chairs$vote.before*100/chairs$n.before
chairs$percent.after <- chairs$vote.after*100/chairs$n.after

chair.ba <- ggplot(chairs, aes(x=percent.after, y=percent.before)) +
  geom_point(size=2, shape=20) + theme_bw() + xlab("% Vote for Incumbent (Post-Results)") +
  ylab("% Vote for Incumbent (Pre-Results)") + coord_cartesian(xlim = c(0,100),ylim = c(0,100)) +
  ggtitle("LC V Chairs") + geom_smooth(method=lm, se=FALSE, color="black")

cor(chairs$percent.before,chairs$percent.after)

##Creating councillor dataframe and plot
tab<-table(data$lc5.councillor.name,data$lc5.councillor.competitive) #To find uncompetitive councillors to remove
tab0<-data.matrix(tab[,1])
tab1<-data.matrix(tab[,2])
tab.dta <- data.frame(tab0,tab1)
tab.dta <- subset(tab.dta, tab0==0 & tab1>0)

councillors <- data.frame(row.names(tab.dta))
names(councillors)[1] <- "name"

for (i in 1:nrow(councillors)){
  sub <- subset(data, lc5.councillor.name==councillors$name[i])
  councillors$vote.before[i] <- nrow(sub[sub$lc5.councillor.inc.vote==1 & !is.na(sub$lc5.councillor.inc.vote) & sub$before.results==1,])
  councillors$n.before[i] <- nrow(sub[!is.na(sub$lc5.councillor.inc.vote) & sub$before.results==1,])
  councillors$vote.after[i] <- nrow(sub[sub$lc5.councillor.inc.vote==1 & !is.na(sub$lc5.councillor.inc.vote) & sub$before.results==0,])
  councillors$n.after[i] <- nrow(sub[!is.na(sub$lc5.councillor.inc.vote) & sub$before.results==0,])
}

councillors <- subset(councillors, n.before>0 & n.after>0)
councillors$percent.before <- councillors$vote.before*100/councillors$n.before
councillors$percent.after <- councillors$vote.after*100/councillors$n.after

councillor.ba <- ggplot(councillors, aes(x=percent.after, y=percent.before)) +
  geom_point(size=2, shape=20) + theme_bw() + xlab("% Vote for Incumbent (Post-Results)") +
  ylab("% Vote for Incumbent (Pre-Results)") + coord_cartesian(xlim = c(0,100),ylim = c(0,100)) +
  geom_smooth(method=lm, se=FALSE, color="black") + ggtitle("LC V Councillors")

cor(councillors$percent.before,councillors$percent.after)

##Plotting together
#pdf("before-after_180424.pdf", width=6.5, height=4)
multiplot(chair.ba,councillor.ba, cols=2)
#dev.off()


######################################################################################
###Figure A3: Comparison of incumbent vote share, pre- and post-election results being announced by politician
#checked: 3/12/19
######################################################################################

#exclude any cases with only one vote
votes.councillor=votes.councillor[votes.councillor$number_votes>1,]
votes.chair=votes.chair[votes.chair$number_votes>1,]

votes.councillor$incumbent_share_official=votes.councillor$incumbent_share_official*100
votes.councillor$incumbent_share_survey=votes.councillor$incumbent_share_survey*100
votes.chair$incumbent_share_official=votes.chair$incumbent_share_official*100
votes.chair$incumbent_share_survey=votes.chair$incumbent_share_survey*100

plot.councillors=ggplot(votes.councillor, aes(incumbent_share_survey, incumbent_share_official))+
  geom_point(alpha = 20/20, position=position_jitter(width=0,height=0))+
  scale_colour_manual(name="",  values =c("black"))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black")+
  scale_fill_brewer(guide = guide_legend(title = ""))+
  labs(x = "% Votes for Incumbent (Survey)",
       y=  "% Votes for Incumbent (Official)"
  )+
  ylim(0,100)+
  theme_bw()+
  ggtitle("LC V Councillors")

plot.chairs=ggplot(votes.chair, aes(incumbent_share_survey, incumbent_share_official))+
  geom_point(alpha = 20/20, position=position_jitter(width=0,height=0))+
  scale_colour_manual(name="",  values =c("black"))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black")+
  scale_fill_brewer(guide = guide_legend(title = ""))+
  labs(x = "% Votes for Incumbent (Survey)",
       y=  "% Votes for Incumbent (Official)"
  )+
  ylim(0,100)+
  theme_bw()+
  ggtitle("LC V Chairs")

#jpeg("./figures/FigureS5.jpg",width=700,height=400, quality=100)
multiplot(plotlist=list(plot.chairs, plot.councillors), cols=2)
#dev.off()


######################################################################################
###Figure A4: Share of respondents in each district able to name the water basin colorat  their  polling  place
#checked: 3/12/19
######################################################################################

#Exclude cases that were non responsive or missing
data.temp=data[data$d.color_of_water_basin.x !="don_t_remember",]
data.temp=data.temp[data.temp$d.color_of_water_basin.x !="refused_to_ans",]
data.temp=data.temp[data.temp$d.color_of_water_basin.x !="another_object",]
data.temp=data.temp[data.temp$d.color_of_water_basin.x !="",]
data.temp$basin_color=factor(data.temp$d.color_of_water_basin.x)
data.temp$basin_color_int = as.integer(data.temp$basin_color)
data.temp$goodnews = ifelse(data.temp$budget.actual>data.temp$budget.prior | (data.temp$budget.actual==data.temp$budget.prior & data.temp$budget.actual>=4), 1, 0)
data.temp$badnews = ifelse(data.temp$budget.actual<data.temp$budget.prior | (data.temp$budget.actual==data.temp$budget.prior & data.temp$budget.actual<=2), 1, 0)

data.simple=data.frame(badnews=data.temp$badnews, goodnews=data.temp$goodnews, treat=data.temp$budget.treat, village=data.temp$location.id, district=data.temp$district_lower, basin_color_int=data.temp$basin_color_int, count=rep_len(1,nrow(data.temp)))

#check whether respondent color choices equal the modal choice in a village
data.aggregate=aggregate(data.simple, by=list(village_name=data.simple$village), FUN=SingleMode)
data.merge=merge(data.simple, data.aggregate, by="village")
data.merge$count=1
data.merge$correct = NA
data.merge$correct = ifelse(data.merge$basin_color_int.x==data.merge$basin_color_int.y, TRUE, data.merge$correct)
data.merge$correct = ifelse(data.merge$basin_color_int.x!=data.merge$basin_color_int.y, FALSE, data.merge$correct)

data.treat=data.merge[data.merge$treat.x==1,]
data.treat=data.treat[!is.na(data.treat$district.x),]
m.treat=round(mean(data.treat$correct),3)
sd.treat=round(sd(data.treat$correct),3)

m.treat.bad=round(mean(data.treat[data.treat$badnews.x==1,]$correct, na.rm = TRUE),3)
sd.treat.bad=round(sd(data.treat[data.treat$badnews.x==1,]$correct, na.rm = TRUE),3)
m.treat.good=round(mean(data.treat[data.treat$goodnews.x==1,]$correct, na.rm = TRUE),3)
sd.treat.good=round(sd(data.treat[data.treat$goodnews.x==1,]$correct, na.rm = TRUE),3)

data.control=data.merge[data.merge$treat.x==0,]
data.control=data.control[!is.na(data.control$district.x),]
m.control=round(mean(data.control$correct),3)
sd.control=round(sd(data.control$correct),3)

m.control.bad=round(mean(data.control[data.control$badnews.x==1,]$correct, na.rm = TRUE),3)
sd.control.bad=round(sd(data.control[data.control$badnews.x==1,]$correct, na.rm = TRUE),3)
m.control.good=round(mean(data.control[data.control$goodnews.x==1,]$correct, na.rm = TRUE),3)
sd.control.good=round(sd(data.control[data.control$goodnews.x==1,]$correct, na.rm = TRUE),3)

treatplot=ggplot(data.treat, aes(reorder(factor(data.treat$district.x), data.treat$count, sum), fill=factor(correct))) +
  geom_bar()+
  theme_bw()+
  scale_fill_manual(guide = guide_legend(title = "Same as Modal \nResponse",
                                         keywidth=3,
                                         keyheight=3),
                    values=c("#d0d3d4","#000000"),
                    labels = c("False", "True", "")
  )+
  theme(axis.text = element_text(size = 24, angle = 90,hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=25),
        legend.text=element_text(size = 22),
        legend.title=element_text(size = 22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
  )+
  ylab("Number of Respondents")+
  scale_y_continuous(expand=c(0.02,0), limits = c(0, 500))+
  annotate(geom="text", x=12, y=450, label="Treatment Group", size=15,
           color="black")+
  annotate(geom="text", x=12, y=410, label=paste("mu==",m.treat), size=12,
           color="black", parse=TRUE)+
  annotate(geom="text", x=12, y=380, label=paste("mu['+']==",m.treat.good), size=12,
           color="black", parse=TRUE)+
  annotate(geom="text", x=12, y=350, label=paste("mu['-']==",m.treat.bad), size=12,
           color="black", parse=TRUE)

controlplot=ggplot(data.control, aes(reorder(factor(data.control$district.x), data.control$count, sum), fill=factor(correct))) +
  geom_bar()+
  theme_bw()+
  scale_fill_manual(guide = guide_legend(title = "Same as Modal \nResponse",
                                         keywidth=3,
                                         keyheight=3),
                    values=c("#d0d3d4","#000000"),
                    labels = c("False", "True", "")
  )+
  theme(axis.text = element_text(size = 24, angle = 90,hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=25),
        legend.text=element_text(size = 22),
        legend.title=element_text(size = 22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
  )+
  ylab("Number of Respondents")+
  scale_y_continuous(expand=c(0.02,0), limits = c(0, 500))+
  annotate(geom="text", x=12, y=450, label="Control Group", size=15,
           color="black")+
  annotate(geom="text", x=12, y=410, label=paste("mu==",m.control), size=12,
           color="black", parse=TRUE)+
  annotate(geom="text", x=12, y=380, label=paste("mu['+']==",m.control.good), size=12,
           color="black", parse=TRUE)+
  annotate(geom="text", x=12, y=350, label=paste("mu['-']==",m.control.bad), size=12,
           color="black", parse=TRUE)

#jpeg("./Figures/FigureS6.jpg",width=2000,height=1000, quality=100)
multiplot(treatplot, controlplot, cols=2)
#dev.off()

data.simple=NULL
data.temp=NULL
data.merge=NULL
data.treat=NULL
data.control=NULL


######################################################################################
###Table A2: Treatment effects among respondents who named modal basin color
#checked: 2/19/19
######################################################################################

data.basin <- subset(data, d.color_of_water_basin.x==modal.basin.color)
data.orig <- data #storing pre-manipulated data
data <- data.basin

###Setting up again
#Prior-defined subgroups
budget.good <- subset(data, budget.actual > budget.prior | (budget.actual==budget.prior & budget.actual>=4))
budget.bad <- subset(data, budget.actual < budget.prior | (budget.actual==budget.prior & budget.actual<=2))

#These are the subgroups not defined by priors, for an extended analysis
budget.positive <- subset(data, budget.actual>=4)
budget.negative <- subset(data, budget.actual<=2)

#Main subsets: no uncontested elections, no individual incumbent switched parties and ran again in 2016, and no redistricting (includes elections w/o incumbent individual)
budget.good.lc5.chair.comp <- subset(budget.good, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.good.lc5.councillor.comp <- subset(budget.good, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))
budget.bad.lc5.chair.comp <- subset(budget.bad, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.bad.lc5.councillor.comp <- subset(budget.bad, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))
budget.positive.lc5.chair.comp <- subset(budget.positive, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.positive.lc5.councillor.comp <- subset(budget.positive, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))
budget.negative.lc5.chair.comp <- subset(budget.negative, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.negative.lc5.councillor.comp <- subset(budget.negative, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))

#Verified Recipient subgroups
budget.good.lc5.chair.comp.c <- subset(budget.good, lc5.chair.competitive==1 & lc5.chair.party.switch==0 & d.X_11_Over_the_last_several_days=="yes")
budget.good.lc5.councillor.comp.c <- subset(budget.good, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016) & d.X_11_Over_the_last_several_days=="yes")
budget.bad.lc5.chair.comp.c <- subset(budget.bad, lc5.chair.competitive==1 & lc5.chair.party.switch==0 & d.X_11_Over_the_last_several_days=="yes")
budget.bad.lc5.councillor.comp.c <- subset(budget.bad, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016) & d.X_11_Over_the_last_several_days=="yes")


h1a.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
summary(h1a.b.1)
p1 <- ifelse(h1a.b.1$coef[1]>0, summary(h1a.b.1)$coef[1,4]/2, 1-summary(h1a.b.1)$coef[1,4]/2)

h1a.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
summary(h1a.b.2)
p2 <- ifelse(h1a.b.2$coef[1]>0, summary(h1a.b.2)$coef[1,4]/2, 1-summary(h1a.b.2)$coef[1,4]/2)

h1b.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
summary(h1b.b.1)
p3 <- ifelse(h1b.b.1$coef[1]<0, summary(h1b.b.1)$coef[1,4]/2, 1-summary(h1b.b.1)$coef[1,4]/2)

h1b.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
summary(h1b.b.2)
p4 <- ifelse(h1b.b.2$coef[1]<0, summary(h1b.b.2)$coef[1,4]/2, 1-summary(h1b.b.2)$coef[1,4]/2)

h1a.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
summary(h1a.b.1c)
p5 <- ifelse(h1a.b.1c$coef[1]>0, summary(h1a.b.1c)$coef[1,4]/2, 1-summary(h1a.b.1c)$coef[1,4]/2)

h1a.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
summary(h1a.b.2c)
p6 <- ifelse(h1a.b.2c$coef[1]>0, summary(h1a.b.2c)$coef[1,4]/2, 1-summary(h1a.b.2c)$coef[1,4]/2)

h1b.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
summary(h1b.b.1c)
p7 <- ifelse(h1b.b.1c$coef[1]<0, summary(h1b.b.1c)$coef[1,4]/2, 1-summary(h1b.b.1c)$coef[1,4]/2)

h1b.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
summary(h1b.b.2c)
p8 <- ifelse(h1b.b.2c$coef[1]<0, summary(h1b.b.2c)$coef[1,4]/2, 1-summary(h1b.b.2c)$coef[1,4]/2)

#Table for eight models with & without covariates
stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          keep = c("budget.treat","lc5.chair.intent","lc5.councillor.intent"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Covariates", "No","Yes","No","Yes","No","Yes","No","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Budget Treatment","LC5 Chair Intent","LC5 Councillor Intent"),
          notes = "Notes: SEs clustered by politician; one-tailed tests as pre-registered; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          p = list(p1,p5,p2,p6,p3,p7,p4,p8),
          se = list(h1a.b.1$cse[1:2],
                    h1a.b.1c$cse[1:2],
                    h1a.b.2$cse[1:2],
                    h1a.b.2c$cse[1:2],
                    h1b.b.1$cse[1:2],
                    h1b.b.1c$cse[1:2],
                    h1b.b.2$cse[1:2],
                    h1b.b.2c$cse[1:2]),
          report = c('vc*s'),
          notes.append = FALSE,
          notes.label = ""
) 

data <- data.orig #restoring full dataset & other subsets
budget.good <- subset(data, budget.actual > budget.prior | (budget.actual==budget.prior & budget.actual>=4))
budget.bad <- subset(data, budget.actual < budget.prior | (budget.actual==budget.prior & budget.actual<=2))
budget.positive <- subset(data, budget.actual>=4)
budget.negative <- subset(data, budget.actual<=2)
budget.good$budget.prior.diff <- factor(budget.good$budget.prior.diff, label=c("same","better1","better2","better3","better4"))
budget.bad$budget.prior.diff <- factor(budget.bad$budget.prior.diff, levels=c(0,-1,-2,-3,-4), label=c("same","worse1","worse2","worse3","worse4"))
budget.good.lc5.chair.comp <- subset(budget.good, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.good.lc5.councillor.comp <- subset(budget.good, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))
budget.bad.lc5.chair.comp <- subset(budget.bad, lc5.chair.competitive==1 & lc5.chair.party.switch==0)
budget.bad.lc5.councillor.comp <- subset(budget.bad, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016))
budget.good.lc5.chair.comp.c <- subset(budget.good, lc5.chair.competitive==1 & lc5.chair.party.switch==0 & d.X_11_Over_the_last_several_days=="yes")
budget.good.lc5.councillor.comp.c <- subset(budget.good, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016) & d.X_11_Over_the_last_several_days=="yes")
budget.bad.lc5.chair.comp.c <- subset(budget.bad, lc5.chair.competitive==1 & lc5.chair.party.switch==0 & d.X_11_Over_the_last_several_days=="yes")
budget.bad.lc5.councillor.comp.c <- subset(budget.bad, lc5.councillor.competitive==1 & lc5.councillor.party.switch==0 & is.na(lc5.councillor.redistricted2016) & d.X_11_Over_the_last_several_days=="yes")


######################################################################################
###Table A3: Balance on all pre-treatment covariates by budget treatment 
#updated & checked: 2/27/19
######################################################################################

###Cleaning the data and levels
covs0 <- data[,c("budget.treat",
                 "b.Q1_living_conditions",
                 "lc5.councillor.same.tribe",
                 "lc5.turnout.intent",
                 "lc5.councillor.intent",
                 "lc5.chair.intent",
                 "partyID",
                 "party.attachment",
                 "b.Q11_2011_Vote_for_LC5councilor",
                 "b.Q12_2011_vote_for_LC5chair",
                 "b.vote.buying",
                 "budget.importance",
                 "ps.importance",
                 "b.Q18_Most_important_public_serv",
                 "b.Q19_Second_most_important_publ",
                 "b.Q21a_Trust_in_politicians_for_",
                 "b.trust.Twaweza",
                 "b.Q21c_Trust_in_NGOs_for_info",
                 "b.trust.AG",
                 "b.Q22_LC5_record_of_budget_mgmt",
                 "budget.certainty",
                 "b.Q24_Powerful_ppl_learning_how_",
                 "b.Q25_Will_counting_votes_be_fai",
                 "r.What_is_your_gender",
                 "r.education",
                 "r.In_which_language_would_you_pr",
                 "r.What_is_your_age",
                 "d.attr")
              ]

##Changing variable names for plot labeling
#names(covs0)
names(covs0) <- c("budget.treat","Living Conditions","Same Tribe Councillor","Turnout Intent","Councillor Vote Intent",
                  "Chair Vote Intent","Party","Party Attachment","Voted for Councillor 2011","Voted for Chair 2011","Vote Buying",
                  "Budget Importance","Services Importance","Most Important Service","Second Important Service","Trust Local Politicians",
                  "Trust Twaweza","Trust NGOs","Trust Auditor General","Budget Prior","Budget Prior Certainty","Voting Secret","Vote Count Fair",
                  "Gender","Education","Language","Age")

##Changing labels within variables
cols <- 2:26
covs0[,cols] = apply(covs0[,cols], 2, function(x) (as.character(x)))

table(covs0[,2], useNA="always")
covs0[covs0=="don_t_know"] <- "don't know"
covs0[covs0=="much_better"] <- "much better"
covs0[covs0=="much_worse"] <- "much worse"
covs0[covs0=="refused_to_ans"] <- "refused"
covs0[covs0=="the_same"] <- "same"
table(covs0[,3], useNA="always")
covs0[,3] <- ifelse(is.na(covs0[,3]),"missing",covs0[,3])
covs0[,3] <- ifelse(covs0[,3]==" 1","yes",covs0[,3])
covs0[,3] <- ifelse(covs0[,3]==" 0","no",covs0[,3])
table(covs0[,4], useNA="always")
covs0[,4] <- ifelse(covs0[,4]==" 1","yes",covs0[,4])
covs0[,4] <- ifelse(covs0[,4]==" 0","no",covs0[,4])
covs0[,4] <- ifelse(is.na(covs0[,4]),"missing",covs0[,4])
table(covs0[,5], useNA="always")
covs0[,5] <- ifelse(covs0[,5]==" 1","yes",covs0[,5])
covs0[,5] <- ifelse(covs0[,5]==" 0","no",covs0[,5])
covs0[,5] <- ifelse(is.na(covs0[,5]),"missing",covs0[,5])
table(covs0[,6], useNA="always")
covs0[,6] <- ifelse(covs0[,6]==" 1","yes",covs0[,6])
covs0[,6] <- ifelse(covs0[,6]==" 0","no",covs0[,6])
covs0[,6] <- ifelse(is.na(covs0[,6]),"missing",covs0[,6])
table(covs0[,7], useNA="always")
covs0[,7] <- ifelse(covs0[,7]=="other-noincumbentpartymatch","other",covs0[,7])
covs0[,7] <- ifelse(is.na(covs0[,7]),"missing",covs0[,7])
table(covs0[,8], useNA="always")
covs0[,8] <- ifelse(covs0[,8]=="1","1 (very little)",covs0[,8])
covs0[,8] <- ifelse(covs0[,8]=="7","7 (very much)",covs0[,8])
covs0[,8] <- ifelse(covs0[,8]=="not_applicable","no party",covs0[,8])
table(covs0[,9], useNA="always")
covs0[covs0=="i_did_not_vote"] <- "didn't vote"
covs0[covs0==""] <- "missing"
table(covs0[,10], useNA="always")
table(covs0[,11], useNA="always")
covs0[covs0=="somewhat_likel"] <- "somewhat likely"
covs0[covs0=="somewhat_unlik"] <- "somewhat unlikely"
covs0[covs0=="very_likel"] <- "very likely"
covs0[covs0=="very_unlikely"] <- "very unlikely"
table(covs0[,12], useNA="always")
covs0[covs0=="not_important"] <- "not important"
covs0[covs0=="not_very_impor"] <- "not very important"
covs0[covs0=="somewhat_impor"] <- "somewhat important"
covs0[covs0=="very_important"] <- "very important"
table(covs0[,13], useNA="always")
covs0[is.na(covs0)] <- "missing"
table(covs0[,14], useNA="always")
covs0[covs0=="local_health_services"] <- "health"
covs0[covs0=="local_roads"] <- "roads"
covs0[covs0=="primary_schools"] <- "schools"
covs0[covs0=="water_access"] <- "water"
covs0[covs0=="refused_to_answer"] <- "refused"
table(covs0[,15], useNA="always")
covs0[covs0=="local_health_s"] <- "health"
covs0[covs0=="primary_school"] <- "schools"
table(covs0[,16], useNA="always")
covs0[covs0=="do_not_trust_a"] <- "don't trust at all"
covs0[covs0=="option_5"] <- "refused"
covs0[covs0=="trust_a_little"] <- "trust a little"
covs0[covs0=="trust_a_lot"] <- "trust a lot"
table(covs0[,17], useNA="always")
table(covs0[,18], useNA="always")
table(covs0[,19], useNA="always")
table(covs0[,20], useNA="always")
covs0[covs0=="a_little_worse"] <- "a little worse"
table(covs0[,21], useNA="always")
covs0[covs0=="not_certain"] <- "not certain"
covs0[covs0=="very_certain"] <- "very certain"
covs0[covs0=="very_uncertain"] <- "very uncertain"
table(covs0[,22], useNA="always")
covs0[covs0=="not_at_all_lik"] <- "not at all likely"
covs0[covs0=="not_very_likel"] <- "not very likely"
covs0[covs0=="very_likely"] <- "very likely"
table(covs0[,23], useNA="always")
table(covs0[,24], useNA="always")
table(covs0[,25], useNA="always")
covs0[covs0=="completed_mast"] <- "complete graduate"
covs0[covs0=="completed_prim"] <- "complete primary"
covs0[covs0=="completed_seco"] <- "complete secondary"
covs0[covs0=="completed_univ"] <- "complete university"
covs0[covs0=="no_schooling"] <- "no schooling"
covs0[covs0=="refuse_to_answ"] <- "refused"
covs0[covs0=="some_post_grad"] <- "some graduate"
covs0[covs0=="some_primary_s"] <- "some primary"
covs0[covs0=="some_secondary"] <- "some secondary"
covs0[covs0=="some_universit"] <- "some university"
table(covs0[,26], useNA="always") #Rename these in letters
covs0[,26] <- ifelse(covs0[,26]!="luganda" & covs0[,26]!="english" & covs0[,26]!="runyankole" & covs0[,26]!="langi" & covs0[,26]!="ateso", "other", covs0[,26])
covs0[covs0=="AUTOMATIC"] <- "other"
table(covs0[,27], useNA="always")

##Creating factors with appropriate levels
table(covs0[,2], useNA="always")
covs0[,2] <- factor(covs0[,2], levels=rev(c("much better","better","same","worse","much worse","don't know","missing","refused")))
sort(table(covs0[,7]))
covs0[,7] <- factor(covs0[,7], levels=c("missing","ufa","other","upc","dp","fdc","independent","nrm"))
covs0[,8] <- factor(covs0[,8], levels=rev(c("7 (very much)","6","5","4","3","2","1 (very little)","no party","missing")))
covs0[,9] <- factor(covs0[,9], levels=c("missing","refused","don't know","didn't vote","no","yes"))
covs0[,10] <- factor(covs0[,10], levels=c("missing","refused","don't know","didn't vote","no","yes"))
covs0[,11] <- factor(covs0[,11], levels=c("missing","don't know","very likely","somewhat likely","somewhat unlikely","very unlikely"))
covs0[,14] <- factor(covs0[,14], levels=c("missing","refused","don't know","schools","roads","water","health"))
covs0[,15] <- factor(covs0[,15], levels=c("missing","refused","don't know","schools","roads","water","health"))
covs0[,16] <- factor(covs0[,16], levels=c("missing","refused","don't know","don't trust at all","trust a little","trust a lot"))
covs0[,17] <- factor(covs0[,17], levels=c("missing","refused","don't know","don't trust at all","trust a little","trust a lot"))
covs0[,18] <- factor(covs0[,18], levels=c("missing","refused","don't know","don't trust at all","trust a little","trust a lot"))
covs0[,19] <- factor(covs0[,19], levels=c("missing","refused","don't know","don't trust at all","trust a little","trust a lot"))
covs0[,20] <- factor(covs0[,20], levels=c("missing","refused","much worse","a little worse","don't know","better","much better"))
covs0[,21] <- factor(covs0[,21], levels=c("missing","very uncertain","not certain","certain","very certain"))
covs0[,22] <- factor(covs0[,22], levels=c("missing","refused","don't know","not at all likely","not very likely","somewhat likely","very likely"))
covs0[,23] <- factor(covs0[,23], levels=c("missing","refused","don't know","not at all likely","not very likely","somewhat likely","very likely"))
covs0[,25] <- factor(covs0[,25], levels=c("refused","no schooling","some primary","complete primary","some secondary","complete secondary","some university","complete university","some graduate","complete graduate"))
covs0[,26] <- factor(covs0[,26], levels=c("other","ateso","langi","runyankole","english","luganda"))

cols.to.factor <- c("Same Tribe Councillor", "Turnout Intent", "Councillor Vote Intent", "Chair Vote Intent", "Budget Importance", "Services Importance", "Gender")
covs0[cols.to.factor] <- lapply(covs0[cols.to.factor], factor)

btab <- bal.tab(covs0[2:27], treat=covs0$budget.treat, disp.means=TRUE, quick=TRUE, s.d.denom="control")

options(scipen=999)

balance=btab$Balance
balance$id=(1:nrow(balance))
balance$treat = balance$M.1.Un
balance$control = balance$M.0.Un
balance$difference = balance$Diff.Un
balance=balance[,c("treat", "control", "difference")]
balance <- balance[rev(order(balance$difference)),]

N=nrow(covs0)
N.t <- sum(covs0$budget.treat) #8064
N.c <- N - N.t #8019
balance$v1=((balance$treat)*(1-balance$treat)/N.t)
balance$v2=((balance$control)*(1-balance$control)/N.c)
balance$v3=balance$v1+balance$v2

balance$treat=paste(round(balance$treat, 4), "  (", round(sqrt(balance$v1),4), ")", sep="")
balance$control=paste(round(balance$control, 4), "  (", round(sqrt(balance$v2),4), ")", sep="")
balance$difference=paste(round(balance$difference, 4), "  (", round(sqrt(balance$v3),4), ")", sep="")

balance$id=NULL
balance$v1=NULL
balance$v2=NULL
balance$v3=NULL

#Computing standard errors for the one continuous variable, entered manually in table
age.treat.se <- sd(covs0$Age[covs0$budget.treat==1])/sqrt(length(covs0$Age[covs0$budget.treat==1]))
age.control.se <- sd(covs0$Age[covs0$budget.treat==0])/sqrt(length(covs0$Age[covs0$budget.treat==0]))
age.diff.se <- sqrt(age.treat.se+age.control.se)

xtable(balance)

options(scipen=0)


######################################################################################
###Table A4:  Distribution of voter party identification and alignment
#PNAS Table S7
#checked 2/27/19
######################################################################################

budget.good.lc5.chair.comp.complete <- subset(budget.good.lc5.chair.comp, !is.na(lc5.chair.inc.vote) & !is.na(budget.treat) & !is.na(lc5.chair.intent) & !is.na(lc5.chair.name))
budget.good.lc5.councillor.comp.complete <- subset(budget.good.lc5.councillor.comp, !is.na(lc5.councillor.inc.vote) & !is.na(budget.treat) & !is.na(lc5.councillor.intent) & !is.na(lc5.councillor.name))
budget.bad.lc5.chair.comp.complete <- subset(budget.bad.lc5.chair.comp, !is.na(lc5.chair.inc.vote) & !is.na(budget.treat) & !is.na(lc5.chair.intent) & !is.na(lc5.chair.name))
budget.bad.lc5.councillor.comp.complete <- subset(budget.bad.lc5.councillor.comp, !is.na(lc5.councillor.inc.vote) & !is.na(budget.treat) & !is.na(lc5.councillor.intent) & !is.na(lc5.councillor.name))

budget.good.lc5.chair.comp.complete$partyID=ifelse(budget.good.lc5.chair.comp.complete$partyID=="upc", "other", budget.good.lc5.chair.comp.complete$partyID)
budget.good.lc5.chair.comp.complete$partyID=ifelse(budget.good.lc5.chair.comp.complete$partyID=="ufa", "other", budget.good.lc5.chair.comp.complete$partyID)
budget.good.lc5.chair.comp.complete$partyID=ifelse(budget.good.lc5.chair.comp.complete$partyID=="other-noincumbentpartymatch", "other", budget.good.lc5.chair.comp.complete$partyID)
xtabs(~budget.good.lc5.chair.comp.complete$partyID)

budget.good.lc5.councillor.comp.complete$partyID=ifelse(budget.good.lc5.councillor.comp.complete$partyID=="upc", "other", budget.good.lc5.councillor.comp.complete$partyID)
budget.good.lc5.councillor.comp.complete$partyID=ifelse(budget.good.lc5.councillor.comp.complete$partyID=="ufa", "other", budget.good.lc5.councillor.comp.complete$partyID)
budget.good.lc5.councillor.comp.complete$partyID=ifelse(budget.good.lc5.councillor.comp.complete$partyID=="other-noincumbentpartymatch", "other", budget.good.lc5.councillor.comp.complete$partyID)
xtabs(~budget.good.lc5.councillor.comp.complete$partyID)

budget.bad.lc5.chair.comp.complete$partyID=ifelse(budget.bad.lc5.chair.comp.complete$partyID=="upc", "other", budget.bad.lc5.chair.comp.complete$partyID)
budget.bad.lc5.chair.comp.complete$partyID=ifelse(budget.bad.lc5.chair.comp.complete$partyID=="ufa", "other", budget.bad.lc5.chair.comp.complete$partyID)
budget.bad.lc5.chair.comp.complete$partyID=ifelse(budget.bad.lc5.chair.comp.complete$partyID=="other-noincumbentpartymatch", "other", budget.bad.lc5.chair.comp.complete$partyID)
xtabs(~budget.bad.lc5.chair.comp.complete$partyID)

budget.bad.lc5.councillor.comp.complete$partyID=ifelse(budget.bad.lc5.councillor.comp.complete$partyID=="upc", "other", budget.bad.lc5.councillor.comp.complete$partyID)
budget.bad.lc5.councillor.comp.complete$partyID=ifelse(budget.bad.lc5.councillor.comp.complete$partyID=="ufa", "other", budget.bad.lc5.councillor.comp.complete$partyID)
budget.bad.lc5.councillor.comp.complete$partyID=ifelse(budget.bad.lc5.councillor.comp.complete$partyID=="other-noincumbentpartymatch", "other", budget.bad.lc5.councillor.comp.complete$partyID)
xtabs(~budget.bad.lc5.councillor.comp.complete$partyID)

t1.P=prop.table(sort(table(budget.good.lc5.chair.comp.complete$partyID), decreasing=TRUE))
t1.N=sort(table(budget.good.lc5.chair.comp.complete$partyID), decreasing=TRUE)

t2.P=prop.table(sort(table(budget.bad.lc5.chair.comp.complete$partyID), decreasing=TRUE))
t2.N=sort(table(budget.bad.lc5.chair.comp.complete$partyID), decreasing=TRUE)

t3.P=prop.table(sort(table(budget.good.lc5.councillor.comp.complete$partyID), decreasing=TRUE))
t3.N=sort(table(budget.good.lc5.councillor.comp.complete$partyID), decreasing=TRUE)

t4.P=prop.table(sort(table(budget.bad.lc5.councillor.comp.complete$partyID), decreasing=TRUE))
t4.N=sort(table(budget.bad.lc5.councillor.comp.complete$partyID), decreasing=TRUE)

##Incumbent Party ID
t5.P=prop.table(sort(table(budget.good.lc5.chair.comp.complete$lc5.chair.party), decreasing=TRUE))
t5.N=(sort(table(budget.good.lc5.chair.comp.complete$lc5.chair.party), decreasing=TRUE))

t6.P=prop.table(sort(table(budget.bad.lc5.chair.comp.complete$lc5.chair.party), decreasing=TRUE))
t6.N=(sort(table(budget.bad.lc5.chair.comp.complete$lc5.chair.party), decreasing=TRUE))

t7.P=prop.table(sort(table(budget.good.lc5.councillor.comp.complete$lc5.councillor.party), decreasing=TRUE))
t7.N=(sort(table(budget.good.lc5.councillor.comp.complete$lc5.councillor.party), decreasing=TRUE))

t8.P=prop.table(sort(table(budget.bad.lc5.councillor.comp.complete$lc5.councillor.party), decreasing=TRUE))
t8.N=(sort(table(budget.bad.lc5.councillor.comp.complete$lc5.councillor.party), decreasing=TRUE))

##Voter-Incumbent Alignment
t9.P=prop.table(sort(table(budget.good.lc5.chair.comp.complete$aligned.lc5.chair.inc), decreasing=TRUE))
t9.N=(sort(table(budget.good.lc5.chair.comp.complete$aligned.lc5.chair.inc), decreasing=TRUE))

t10.P=prop.table(sort(table(budget.bad.lc5.chair.comp.complete$aligned.lc5.chair.inc), decreasing=TRUE))
t10.N=(sort(table(budget.bad.lc5.chair.comp.complete$aligned.lc5.chair.inc), decreasing=TRUE))

t11.P=prop.table(sort(table(budget.good.lc5.chair.comp.complete$aligned.lc5.councillor.inc), decreasing=TRUE))
t11.N=(sort(table(budget.good.lc5.chair.comp.complete$aligned.lc5.councillor.inc), decreasing=TRUE))

t12.P=prop.table(sort(table(budget.bad.lc5.chair.comp.complete$aligned.lc5.councillor.inc), decreasing=TRUE))
t12.N=(sort(table(budget.bad.lc5.chair.comp.complete$aligned.lc5.councillor.inc), decreasing=TRUE))

#totals
t13.N=nrow(budget.good.lc5.chair.comp.complete)
t14.N=nrow(budget.bad.lc5.chair.comp.complete)

t15.N=nrow(budget.good.lc5.councillor.comp.complete)
t16.N=nrow(budget.bad.lc5.councillor.comp.complete)

for(i in 1:8){
  N=eval(parse(text=paste("data.frame(t",i,".N)[,2]", sep="")))
  P=eval(parse(text=paste("data.frame(t",i,".P)[,2]", sep="")))
  S=paste(N, " (", round(P, 2)*100, "%)", sep="")
  assign(paste("t", i, sep=""), data.frame(S))
}

for(i in 9:12){
  N=eval(parse(text=paste("data.frame(t",i,".N)[1,2]", sep="")))
  P=eval(parse(text=paste("data.frame(t",i,".P)[1,2]", sep="")))
  S=paste(N, " (", round(P, 2)*100, "%)", sep="")
  assign(paste("t", i, sep=""), data.frame(S))
}

partylabels.councillor=c("Party ID voter: NRM", "Party ID voter: Independent", "Party ID voter: FDC", "Party ID voter: DP", "Party ID voter: Other", "Party ID Incumbent: NRM", "Party ID Incumbent: Independent", "Party ID Incumbent: FDC", "Party ID Incumbent: DP", "Party ID Incumbent: Other", "Voter-Incumbent Alignment", "Total Responses")
partylabels.chair=c("Party ID voter: NRM", "Party ID voter: Independent", "Party ID voter: FDC", "Party ID voter: DP", "Party ID voter: Other", "Party ID Incumbent: NRM", "Party ID Incumbent: Independent", "Party ID Incumbent: FDC", "Voter-Incumbent Alignment", "Total Responses")

headers=c("Good News", "Bad News")

tab.chair=NULL
tab.chair=rbind(data.frame(t1,t2), data.frame(t5,t6))
tab.align=data.frame( t9[1],t10[1])
names(tab.align)=names(tab.chair)
tab.tot=data.frame(toString(t13.N),toString(t14.N))
names(tab.tot)=names(tab.chair)
tab.chair=rbind(tab.chair, tab.align, tab.tot)
names(tab.chair)=headers
row.names(tab.chair)=partylabels.chair

tab.councillor=NULL
tab.councillor=rbind(data.frame(t3,t4), data.frame(t7,t8))
tab.align=data.frame(t11[1],t12[1])
names(tab.align)=names(tab.councillor)
tab.tot=data.frame(toString(t15.N),toString(t16.N))
names(tab.tot)=names(tab.councillor)
tab.councillor=rbind(tab.councillor, tab.align, tab.tot)
names(tab.councillor)=headers
row.names(tab.councillor)=partylabels.councillor

print(xtable(tab.chair))
print(xtable(tab.councillor))


######################################################################################
###Table A5: Randomization check
#checked 2/27/19
######################################################################################

fml <- paste("budget.treat ~ ",paste(names(data)[c(6:25,32:39,122:125)], collapse = "+"), sep="")
bal.mod <- lm(fml, data=data)
stargazer(bal.mod)


######################################################################################
###Table A6: Attrition with respect to the budget treatment
#PNAS Table S8
#checked 2/27/19
######################################################################################

d.attr.tab <- table(data$budget.treat,data$d.attr)
d.attr.tab

chisq.test(d.attr.tab)

d.attr.prop <- prop.table(d.attr.tab, margin=1)
d.attr.prop

#note: values formatted manually in LaTex


######################################################################################
###Table A7: Attrition with respect to the budget treatment and density
#PNAS Table S9
#checked 2/27/19
######################################################################################

d.attr.tab <- table(data$bd.multi.treat,data$d.attr)
d.attr.tab

chisq.test(d.attr.tab)

d.attr.prop <- prop.table(d.attr.tab, margin=1)
d.attr.prop

#note: values formatted manually in LaTex


##################################################################################
###Tables A8: Party vote for LC5 councillor by incumbent party ... 
### and voter alignment for subjects eligible to receive good news
#checked 3/12/19
##################################################################################

budget.good.lc5.chair.comp.complete <- subset(budget.good.lc5.chair.comp, !is.na(lc5.chair.inc.vote) & !is.na(budget.treat) & !is.na(lc5.chair.intent) & !is.na(lc5.chair.name))
budget.good.lc5.councillor.comp.complete <- subset(budget.good.lc5.councillor.comp, !is.na(lc5.councillor.inc.vote) & !is.na(budget.treat) & !is.na(lc5.councillor.intent) & !is.na(lc5.councillor.name))
budget.bad.lc5.chair.comp.complete <- subset(budget.bad.lc5.chair.comp, !is.na(lc5.chair.inc.vote) & !is.na(budget.treat) & !is.na(lc5.chair.intent) & !is.na(lc5.chair.name))
budget.bad.lc5.councillor.comp.complete <- subset(budget.bad.lc5.councillor.comp, !is.na(lc5.councillor.inc.vote) & !is.na(budget.treat) & !is.na(lc5.councillor.intent) & !is.na(lc5.councillor.name))

a8.trt <- budget.good.lc5.councillor.comp.complete[budget.good.lc5.councillor.comp.complete$budget.treat==1,]

tab_a8.trt <- table(a8.trt$partyID.slim, a8.trt$lc5.councillor.party.slim, a8.trt$lc5.councillor.party.vote.slim)
ftable(tab_a8.trt)
#Manual formatting into table needed

a8.ctl <- budget.good.lc5.councillor.comp.complete[budget.good.lc5.councillor.comp.complete$budget.treat==0,]

tab_a8.ctl <- table(a8.ctl$partyID.slim, a8.ctl$lc5.councillor.party.slim, a8.ctl$lc5.councillor.party.vote.slim)
ftable(tab_a8.ctl)
#Manual formatting into table needed


##################################################################################
###Tables A9: Party vote for LC5 councillor by incumbent party ... 
### and voter alignment for subjects eligible to receive bad news
#checked 3/12/19
##################################################################################

a9.trt <- budget.bad.lc5.councillor.comp.complete[budget.bad.lc5.councillor.comp.complete$budget.treat==1,]

tab_a9.trt <- table(a9.trt$partyID.slim, a9.trt$lc5.councillor.party.slim, a9.trt$lc5.councillor.party.vote.slim)
ftable(tab_a9.trt)
#Manual formatting into table needed

a9.ctl <- budget.bad.lc5.councillor.comp.complete[budget.bad.lc5.councillor.comp.complete$budget.treat==0,]

tab_a9.ctl <- table(a9.ctl$partyID.slim, a9.ctl$lc5.councillor.party.slim, a9.ctl$lc5.councillor.party.vote.slim)
ftable(tab_a9.ctl)
#Manual formatting into table needed


##################################################################################
###Table A10: Results on vote choice for primary specification
#checked 3/12/19
##################################################################################

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


h1a.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
h1a.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
h1b.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
h1b.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
h1a.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
h1a.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
h1b.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
h1b.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)

stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          covariate.labels = c("Budget Treatment (RI)","LC5 Chair Intent","Living Conditions: Don't Know","Living Conditions: Missing","Living Conditions: Much Better","Living Conditions: Much Worse","Living Conditions: Refused","Living Conditions: Same", "Living Conditions: Worse", "Trust in Twaweza: Don't Know", "Trust in Twaweza: Missing", "Trust in Twaweza: A Little", "Trust in Twaweza: A Lot", "Voting Not Secret: Missing", "Voting Not Secret: Not At All Likely", "Voting Not Secret: Not Very Likely", "Voting Not Secret: Refused", "Voting Not Secret: Somewhat Likely", "Voting Not Secret: Very Likely", "Fair Vote Counting: Missing", "Fair Vote Counting: Not At All Likely", "Fair Vote Counting: Not Very Likely", "Fair Vote Counting: Refused", "Fair Vote Counting: Somewhat Likely", "Fair Vote Counting: Very Likely", "Male", "Education: Some Primary", "Education: Completed Primary", "Education: Some Secondary", "Education: Completed Secondary", "Education: Some University", "Education: Completed University", "Education: Some Post-Graduate", "Education: Completed Masters", "Education: Refused", "Age", "LC5 Councillor Intent"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          notes = "Notes: SEs derived from randomization inference for treatment and clustered by politician for all other covariates; one-tailed tests on treatment as pre-registered; contested elections where incumbent did not switch parties only.",
          df = FALSE, omit.stat = c("rsq","ser"),
          se = list(c(chair.good$se,as.numeric(h1a.b.1$cse[2:length(h1a.b.1$cse)])),
                    c(chair.good.cov$se,as.numeric(h1a.b.1c$cse[2:length(h1a.b.1c$cse)])),
                    c(coun.good$se,as.numeric(h1a.b.2$cse[2:length(h1a.b.2$cse)])),
                    c(coun.good.cov$se,as.numeric(h1a.b.2c$cse[2:length(h1a.b.2c$cse)])),
                    c(chair.bad$se,as.numeric(h1b.b.1$cse[2:length(h1b.b.1$cse)])),
                    c(chair.bad.cov$se,as.numeric(h1b.b.1c$cse[2:length(h1b.b.1c$cse)])),
                    c(coun.bad$se,as.numeric(h1b.b.2$cse[2:length(h1b.b.2$cse)])),
                    c(coun.bad.cov$se,as.numeric(h1b.b.2c$cse[2:length(h1b.b.2c$cse)]))
          ),
          p = c(chair.good$p.one.way.greater,
                chair.good.cov$p.one.way.greater,
                coun.good$p.one.way.greater,
                coun.good.cov$p.one.way.greater,
                chair.bad$p.one.way.lesser,
                chair.bad.cov$p.one.way.lesser,
                coun.bad$p.one.way.lesser,
                coun.bad.cov$p.one.way.lesser),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Sun, Mar 18, 2018 - 08:58:59


##################################################################################
###Table A11: Results on vote choice for primary specification
#checked 3/12/19
##################################################################################

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

h1a.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp.c)
h1a.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp.c)
h1b.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp.c)
h1b.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp.c)
h1a.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp.c)
h1a.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp.c)
h1b.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp.c)
h1b.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp.c)

stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          covariate.labels = c("Budget Treatment (RI)","LC5 Chair Intent","Living Conditions: Don't Know","Living Conditions: Missing","Living Conditions: Much Better","Living Conditions: Much Worse","Living Conditions: Refused","Living Conditions: Same", "Living Conditions: Worse", "Trust in Twaweza: Don't Know", "Trust in Twaweza: Missing", "Trust in Twaweza: A Little", "Trust in Twaweza: A Lot", "Voting Not Secret: Missing", "Voting Not Secret: Not At All Likely", "Voting Not Secret: Not Very Likely", "Voting Not Secret: Refused", "Voting Not Secret: Somewhat Likely", "Voting Not Secret: Very Likely", "Fair Vote Counting: Missing", "Fair Vote Counting: Not At All Likely", "Fair Vote Counting: Not Very Likely", "Fair Vote Counting: Refused", "Fair Vote Counting: Somewhat Likely", "Fair Vote Counting: Very Likely", "Male", "Education: Some Primary", "Education: Completed Primary", "Education: Some Secondary", "Education: Completed Secondary", "Education: Some University", "Education: Completed University", "Education: Some Post-Graduate", "Education: Completed Masters", "Education: Refused", "Age", "LC5 Councillor Intent"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          notes = "Notes: SEs derived from randomization inference for treatment and clustered by politician for all other covariates; one-tailed tests on treatment as pre-registered; contested elections where incumbent did not switch parties only.",
          df = FALSE, omit.stat = c("rsq","ser"),
          se = list(c(chair.good.c$se,as.numeric(h1a.b.1$cse[2:length(h1a.b.1$cse)])),
                    c(chair.good.cov.c$se,as.numeric(h1a.b.1c$cse[2:length(h1a.b.1c$cse)])),
                    c(coun.good.c$se,as.numeric(h1a.b.2$cse[2:length(h1a.b.2$cse)])),
                    c(coun.good.cov.c$se,as.numeric(h1a.b.2c$cse[2:length(h1a.b.2c$cse)])),
                    c(chair.bad.c$se,as.numeric(h1b.b.1$cse[2:length(h1b.b.1$cse)])),
                    c(chair.bad.cov.c$se,as.numeric(h1b.b.1c$cse[2:length(h1b.b.1c$cse)])),
                    c(coun.bad.c$se,as.numeric(h1b.b.2$cse[2:length(h1b.b.2$cse)])),
                    c(coun.bad.cov.c$se,as.numeric(h1b.b.2c$cse[2:length(h1b.b.2c$cse)]))
          ),
          p = c(chair.good.c$p.one.way.greater,
                chair.good.cov.c$p.one.way.greater,
                coun.good.c$p.one.way.greater,
                coun.good.cov.c$p.one.way.greater,
                chair.bad.c$p.one.way.lesser,
                chair.bad.cov.c$p.one.way.lesser,
                coun.bad.c$p.one.way.lesser,
                coun.bad.cov.c$p.one.way.lesser),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Date and time: Sun, Mar 18, 2018 - 11:13:02


##################################################################################
###Table A12: Results on vote choice for primary specification
#checked 3/12/19
##################################################################################

h1a.b.1 <- felm(lc5.chair.transformed ~ budget.treat | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
h1a.b.2 <- felm(lc5.councillor.transformed ~ budget.treat | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
h1b.b.1 <- felm(lc5.chair.transformed ~ budget.treat | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
h1b.b.2 <- felm(lc5.councillor.transformed ~ budget.treat | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
h1a.b.1c <- felm(lc5.chair.transformed ~ budget.treat + b.Q1_living_conditions + b.Q6_vote_for_current_LC5chair + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
h1a.b.2c <- felm(lc5.councillor.transformed ~ budget.treat + b.Q1_living_conditions + b.Q5_vote_for_current_LC5council + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
h1b.b.1c <- felm(lc5.chair.transformed ~ budget.treat + b.Q1_living_conditions + b.Q6_vote_for_current_LC5chair + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
h1b.b.2c <- felm(lc5.councillor.transformed ~ budget.treat + b.Q1_living_conditions + b.Q5_vote_for_current_LC5council + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)

t.chair.good <- felm.ri(lc5.chair.transformed ~ budget.treat | location.id | 0 | lc5.chair.name,
                        dta=budget.good.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                        rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

t.chair.bad <- felm.ri(lc5.chair.transformed ~ budget.treat | location.id | 0 | lc5.chair.name, 
                       dta=budget.bad.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                       rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

t.chair.good.cov <- felm.ri(lc5.chair.transformed ~ budget.treat + b.Q1_living_conditions + b.Q6_vote_for_current_LC5chair + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name,
                            dta=budget.good.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                            rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

t.chair.bad.cov <- felm.ri(lc5.chair.transformed ~ budget.treat + b.Q1_living_conditions + b.Q6_vote_for_current_LC5chair + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, 
                           dta=budget.bad.lc5.chair.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                           rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

t.coun.good <- felm.ri(lc5.councillor.transformed ~ budget.treat | location.id | 0 | lc5.councillor.name,
                       dta=budget.good.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                       rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

t.coun.bad <- felm.ri(lc5.councillor.transformed ~ budget.treat | location.id | 0 | lc5.councillor.name, 
                      dta=budget.bad.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                      rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

t.coun.good.cov <- felm.ri(lc5.councillor.transformed ~ budget.treat + b.Q1_living_conditions + b.Q5_vote_for_current_LC5council + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name,
                           dta=budget.good.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                           rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

t.coun.bad.cov <- felm.ri(lc5.councillor.transformed ~ budget.treat + b.Q1_living_conditions + b.Q5_vote_for_current_LC5council + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, 
                          dta=budget.bad.lc5.councillor.comp, treat.var = "budget.treat", rand.ob=Budget_RI, 
                          rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          keep = c("budget.treat"),
          covariate.labels = c("Budget Treatment (RI)"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Covariates","No","Yes","No","Yes","No","Yes","No","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          notes = "Notes: SEs derived from randomization inference for treatment and clustered by politician for all other covariates; one-tailed tests on treatment as pre-registered; contested elections where incumbent did not switch parties only.",
          df = FALSE, omit.stat = c("rsq","ser"),
          se = list(c(t.chair.good$se),
                    c(t.chair.good.cov$se),
                    c(t.coun.good$se),
                    c(t.coun.good.cov$se),
                    c(t.chair.bad$se),
                    c(t.chair.bad.cov$se),
                    c(t.coun.bad$se),
                    c(t.coun.bad.cov$se)
          ),
          p = c(t.chair.good$p.one.way.greater,
                t.chair.good.cov$p.one.way.greater,
                t.coun.good$p.one.way.greater,
                t.coun.good.cov$p.one.way.greater,
                t.chair.bad$p.one.way.lesser,
                t.chair.bad.cov$p.one.way.lesser,
                t.coun.bad$p.one.way.lesser,
                t.coun.bad.cov$p.one.way.lesser),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Date and time: Fri, Dec 08, 2017 - 07:46:55


##################################################################################
###Table A13: Effect of budget treatment and treatment density on vote choice for 
## ... district (LCV) incumbents,estimated as pre-specified
#checked 3/12/19
##################################################################################

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

h11a.b.1 <- felm(lc5.chair.transformed ~ bd.multi.treat + b.Q6_vote_for_current_LC5chair  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
h11a.b.2 <- felm(lc5.councillor.transformed ~ bd.multi.treat + b.Q5_vote_for_current_LC5council  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
h11b.b.1 <- felm(lc5.chair.transformed ~ bd.multi.treat + b.Q6_vote_for_current_LC5chair  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
h11b.b.2 <- felm(lc5.councillor.transformed ~ bd.multi.treat + b.Q5_vote_for_current_LC5council  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)

chair.good.crossed.pap <- felm.ri2(lc5.chair.transformed ~ bd.multi.treat2 + b.Q6_vote_for_current_LC5chair  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name,
                                   dta=budget.good.lc5.chair.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                                   rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.good.crossed.pap <- felm.ri2(lc5.councillor.transformed ~ bd.multi.treat2 + b.Q5_vote_for_current_LC5council  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, 
                                  dta=budget.good.lc5.councillor.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                                  rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

chair.bad.crossed.pap <- felm.ri2(lc5.chair.transformed ~ bd.multi.treat2 + b.Q6_vote_for_current_LC5chair  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.chair.name,
                                  dta=budget.bad.lc5.chair.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                                  rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

coun.bad.crossed.pap <- felm.ri2(lc5.councillor.transformed ~ bd.multi.treat2 + b.Q5_vote_for_current_LC5council  + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | d.block | 0 | lc5.councillor.name, 
                                 dta=budget.bad.lc5.councillor.comp, treat.var = "bd.multi.treat2", rand.ob=BD_RI,
                                 rand.ob.info.cols = 4, join.var = "id.cleaned", sims=9999)

stargazer(h11a.b.1, h11a.b.2, h11b.b.1, h11b.b.2, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(2,2),
          keep = c("bd.multi.treatbudget0.density1","bd.multi.treatbudget1.density0","bd.multi.treatbudget1.density1"),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Control, High Density (RI)","Treated, Low Density (RI)", "Treated, High Density (RI)"),
          notes = "Notes: SEs derived from RI; one-tailed tests as pre-registered; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          se = list(chair.good.crossed.pap$se,coun.good.crossed.pap$se,chair.bad.crossed.pap$se,coun.bad.crossed.pap$se),
          p = list(chair.good.crossed.pap$p.one.way.greater, coun.good.crossed.pap$p.one.way.greater, chair.bad.crossed.pap$p.one.way.lesser, coun.bad.crossed.pap$p.one.way.lesser),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Date and time: Sat, Dec 09, 2017 - 12:32:30


##################################################################################
###Table A14: Effects of budget treatment on votes for individual incumbents who ran for re-election as members of the same party
#checked 3/12/19
##################################################################################

h1a.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=filter(budget.good.lc5.chair.comp, lc5.chair.ran2016==TRUE))
p1 <- ifelse(h1a.b.1$coef[1]>0, summary(h1a.b.1)$coef[1,4]/2, 1-summary(h1a.b.1)$coef[1,4]/2)

h1a.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=filter(budget.good.lc5.councillor.comp, lc5.councillor.ran2016==TRUE))
p2 <- ifelse(h1a.b.2$coef[1]>0, summary(h1a.b.2)$coef[1,4]/2, 1-summary(h1a.b.2)$coef[1,4]/2)

h1b.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=filter(budget.bad.lc5.chair.comp, lc5.chair.ran2016==TRUE))
p3 <- ifelse(h1b.b.1$coef[1]<0, summary(h1b.b.1)$coef[1,4]/2, 1-summary(h1b.b.1)$coef[1,4]/2)

h1b.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=filter(budget.bad.lc5.councillor.comp, lc5.councillor.ran2016==TRUE))
p4 <- ifelse(h1b.b.2$coef[1]<0, summary(h1b.b.2)$coef[1,4]/2, 1-summary(h1b.b.2)$coef[1,4]/2)

h1a.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=filter(budget.good.lc5.chair.comp, lc5.chair.ran2016==TRUE))
p5 <- ifelse(h1a.b.1c$coef[1]>0, summary(h1a.b.1c)$coef[1,4]/2, 1-summary(h1a.b.1c)$coef[1,4]/2)

h1a.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=filter(budget.good.lc5.councillor.comp, lc5.councillor.ran2016==TRUE))
p6 <- ifelse(h1a.b.2c$coef[1]>0, summary(h1a.b.2c)$coef[1,4]/2, 1-summary(h1a.b.2c)$coef[1,4]/2)

h1b.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=filter(budget.bad.lc5.chair.comp, lc5.chair.ran2016==TRUE))
p7 <- ifelse(h1b.b.1c$coef[1]<0, summary(h1b.b.1c)$coef[1,4]/2, 1-summary(h1b.b.1c)$coef[1,4]/2)

h1b.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=filter(budget.bad.lc5.councillor.comp, lc5.councillor.ran2016==TRUE))
p8 <- ifelse(h1b.b.2c$coef[1]<0, summary(h1b.b.2c)$coef[1,4]/2, 1-summary(h1b.b.2c)$coef[1,4]/2)

stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          keep = c("budget.treat","lc5.chair.intent","lc5.councillor.intent"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Covariates", "No","Yes","No","Yes","No","Yes","No","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Budget Treatment","LC5 Chair Intent","LC5 Councillor Intent"),
          notes = "Notes: SEs clustered by politician; one-tailed tests as pre-registered; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          p = list(p1,p5,p2,p6,p3,p7,p4,p8),
          se = list(h1a.b.1$cse[1:2],
                    h1a.b.1c$cse[1:2],
                    h1a.b.2$cse[1:2],
                    h1a.b.2c$cse[1:2],
                    h1b.b.1$cse[1:2],
                    h1b.b.1c$cse[1:2],
                    h1b.b.2$cse[1:2],
                    h1b.b.2c$cse[1:2]),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Date and time: Sat, Dec 09, 2017 - 09:38:56


##################################################################################
###Table A15: Effects of budget treatment on votes for district incumbents, with incumbency defined as the party previously elected to the seat
#checked 3/12/19
##################################################################################

h1a.b.1 <- felm(lc5.chair.inc.vote.orig ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=filter(budget.good, lc5.chair.competitive==1))
p1 <- ifelse(h1a.b.1$coef[1]>0, summary(h1a.b.1)$coef[1,4]/2, 1-summary(h1a.b.1)$coef[1,4]/2)

h1a.b.2 <- felm(lc5.councillor.inc.vote.orig ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=filter(budget.good, lc5.councillor.competitive==1  & is.na(lc5.councillor.redistricted2016)))
p2 <- ifelse(h1a.b.2$coef[1]>0, summary(h1a.b.2)$coef[1,4]/2, 1-summary(h1a.b.2)$coef[1,4]/2)

h1b.b.1 <- felm(lc5.chair.inc.vote.orig ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=filter(budget.bad, lc5.chair.competitive==1))
p3 <- ifelse(h1b.b.1$coef[1]<0, summary(h1b.b.1)$coef[1,4]/2, 1-summary(h1b.b.1)$coef[1,4]/2)

h1b.b.2 <- felm(lc5.councillor.inc.vote.orig ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=filter(budget.bad, lc5.councillor.competitive==1  & is.na(lc5.councillor.redistricted2016)))
p4 <- ifelse(h1b.b.2$coef[1]<0, summary(h1b.b.2)$coef[1,4]/2, 1-summary(h1b.b.2)$coef[1,4]/2)

h1a.b.1c <- felm(lc5.chair.inc.vote.orig ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=filter(budget.good, lc5.chair.competitive==1))
p5 <- ifelse(h1a.b.1c$coef[1]>0, summary(h1a.b.1c)$coef[1,4]/2, 1-summary(h1a.b.1c)$coef[1,4]/2)

h1a.b.2c <- felm(lc5.councillor.inc.vote.orig ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=filter(budget.good, lc5.councillor.competitive==1 & is.na(lc5.councillor.redistricted2016)))
p6 <- ifelse(h1a.b.2c$coef[1]>0, summary(h1a.b.2c)$coef[1,4]/2, 1-summary(h1a.b.2c)$coef[1,4]/2)

h1b.b.1c <- felm(lc5.chair.inc.vote.orig ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=filter(budget.bad, lc5.chair.competitive==1))
p7 <- ifelse(h1b.b.1c$coef[1]<0, summary(h1b.b.1c)$coef[1,4]/2, 1-summary(h1b.b.1c)$coef[1,4]/2)

h1b.b.2c <- felm(lc5.councillor.inc.vote.orig ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=filter(budget.bad, lc5.councillor.competitive==1 & is.na(lc5.councillor.redistricted2016)))
p8 <- ifelse(h1b.b.2c$coef[1]<0, summary(h1b.b.2c)$coef[1,4]/2, 1-summary(h1b.b.2c)$coef[1,4]/2)

stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          keep = c("budget.treat","lc5.chair.intent","lc5.councillor.intent"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Covariates", "No","Yes","No","Yes","No","Yes","No","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Budget Treatment","LC5 Chair Intent","LC5 Councillor Intent"),
          notes = "Notes: SEs clustered by politician; one-tailed tests as pre-registered; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          p = list(p1,p5,p2,p6,p3,p7,p4,p8),
          se = list(h1a.b.1$cse[1:2],
                    h1a.b.1c$cse[1:2],
                    h1a.b.2$cse[1:2],
                    h1a.b.2c$cse[1:2],
                    h1b.b.1$cse[1:2],
                    h1b.b.1c$cse[1:2],
                    h1b.b.2$cse[1:2],
                    h1b.b.2c$cse[1:2]),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Date and time: Sat, Dec 09, 2017 - 09:51:49


##################################################################################
###Table A16: Effects of budget treatment on votes for district incumbents, with incumbency defined by individual if the individual is running for re-election
#checked 3/12/19
##################################################################################

h1a.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=filter(budget.good, lc5.chair.competitive==1))
p1 <- ifelse(h1a.b.1$coef[1]>0, summary(h1a.b.1)$coef[1,4]/2, 1-summary(h1a.b.1)$coef[1,4]/2)

h1a.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=filter(budget.good, lc5.councillor.competitive==1))
p2 <- ifelse(h1a.b.2$coef[1]>0, summary(h1a.b.2)$coef[1,4]/2, 1-summary(h1a.b.2)$coef[1,4]/2)

h1b.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=filter(budget.bad, lc5.chair.competitive==1))
p3 <- ifelse(h1b.b.1$coef[1]<0, summary(h1b.b.1)$coef[1,4]/2, 1-summary(h1b.b.1)$coef[1,4]/2)

h1b.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=filter(budget.bad, lc5.councillor.competitive==1))
p4 <- ifelse(h1b.b.2$coef[1]<0, summary(h1b.b.2)$coef[1,4]/2, 1-summary(h1b.b.2)$coef[1,4]/2)

h1a.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=filter(budget.good, lc5.chair.competitive==1))
p5 <- ifelse(h1a.b.1c$coef[1]>0, summary(h1a.b.1c)$coef[1,4]/2, 1-summary(h1a.b.1c)$coef[1,4]/2)

h1a.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=filter(budget.good, lc5.councillor.competitive==1))
p6 <- ifelse(h1a.b.2c$coef[1]>0, summary(h1a.b.2c)$coef[1,4]/2, 1-summary(h1a.b.2c)$coef[1,4]/2)

h1b.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=filter(budget.bad, lc5.chair.competitive==1))
p7 <- ifelse(h1b.b.1c$coef[1]<0, summary(h1b.b.1c)$coef[1,4]/2, 1-summary(h1b.b.1c)$coef[1,4]/2)

h1b.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=filter(budget.bad, lc5.councillor.competitive==1))
p8 <- ifelse(h1b.b.2c$coef[1]<0, summary(h1b.b.2c)$coef[1,4]/2, 1-summary(h1b.b.2c)$coef[1,4]/2)

stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          keep = c("budget.treat","lc5.chair.intent","lc5.councillor.intent"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Covariates", "No","Yes","No","Yes","No","Yes","No","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Budget Treatment","LC5 Chair Intent","LC5 Councillor Intent"),
          notes = "Notes: SEs clustered by politician; one-tailed tests as pre-registered; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          p = list(p1,p5,p2,p6,p3,p7,p4,p8),
          se = list(h1a.b.1$cse[1:2],
                    h1a.b.1c$cse[1:2],
                    h1a.b.2$cse[1:2],
                    h1a.b.2c$cse[1:2],
                    h1b.b.1$cse[1:2],
                    h1b.b.1c$cse[1:2],
                    h1b.b.2$cse[1:2],
                    h1b.b.2c$cse[1:2]),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) # Date and time: Sat, Dec 09, 2017 - 10:16:04


##################################################################################
###Table A17: Conditional effects of budget treatment by unsure budget prior
#checked 3/12/19
##################################################################################

h1a.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
p1 <- ifelse(h1a.b.1$coef[1]>0, summary(h1a.b.1)$coef[1,4]/2, 1-summary(h1a.b.1)$coef[1,4]/2)

h1a.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
p2 <- ifelse(h1a.b.2$coef[1]>0, summary(h1a.b.2)$coef[1,4]/2, 1-summary(h1a.b.2)$coef[1,4]/2)

h1b.b.1 <- felm(lc5.chair.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.chair.intent | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
p3 <- ifelse(h1b.b.1$coef[1]<0, summary(h1b.b.1)$coef[1,4]/2, 1-summary(h1b.b.1)$coef[1,4]/2)

h1b.b.2 <- felm(lc5.councillor.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.councillor.intent | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
p4 <- ifelse(h1b.b.2$coef[1]<0, summary(h1b.b.2)$coef[1,4]/2, 1-summary(h1b.b.2)$coef[1,4]/2)

h1a.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
p5 <- ifelse(h1a.b.1c$coef[1]>0, summary(h1a.b.1c)$coef[1,4]/2, 1-summary(h1a.b.1c)$coef[1,4]/2)

h1a.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
p6 <- ifelse(h1a.b.2c$coef[1]>0, summary(h1a.b.2c)$coef[1,4]/2, 1-summary(h1a.b.2c)$coef[1,4]/2)

h1b.b.1c <- felm(lc5.chair.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.chair.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
p7 <- ifelse(h1b.b.1c$coef[1]<0, summary(h1b.b.1c)$coef[1,4]/2, 1-summary(h1b.b.1c)$coef[1,4]/2)

h1b.b.2c <- felm(lc5.councillor.inc.vote ~ budget.treat*budget.prior.dontknow + lc5.councillor.intent + b.Q1_living_conditions + b.Q21b_Trust_in_Twaweza_for_info + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
p8 <- ifelse(h1b.b.2c$coef[1]<0, summary(h1b.b.2c)$coef[1,4]/2, 1-summary(h1b.b.2c)$coef[1,4]/2)

#Table for eight models with & without covariates
stargazer(h1a.b.1, h1a.b.1c, h1a.b.2, h1a.b.2c, h1b.b.1, h1b.b.1c, h1b.b.2, h1b.b.2c, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Good News","Bad News"), column.separate = c(4,4),
          keep = c("budget.treat","budget.prior.dontknow","lc5.chair.intent","lc5.councillor.intent"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Covariates", "No","Yes","No","Yes","No","Yes","No","Yes")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          covariate.labels = c("Budget Treatment","Unsure Budget Prior","LC5 Chair Intent","LC5 Councillor Intent","Budget Treatment * Unsure Budget Prior"),
          notes = "Notes: SEs clustered by politician; one-tailed tests as pre-registered; contested elections only",
          df = FALSE, omit.stat = c("rsq","ser"),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Date and time: Sat, Dec 09, 2017 - 14:55:02


##################################################################################
###Table A18: Conditional effects of budget treatment by trust in Twaweza and Auditor General
#checked 3/12/19
##################################################################################

#Models
h1a.b.1.ag <- felm(lc5.chair.inc.vote ~ budget.treat*b.trust.AG + lc5.chair.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
h1a.b.2.ag <- felm(lc5.councillor.inc.vote ~ budget.treat*b.trust.AG + lc5.councillor.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
h1b.b.1.ag <- felm(lc5.chair.inc.vote ~ budget.treat*b.trust.AG + lc5.chair.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
h1b.b.2.ag <- felm(lc5.councillor.inc.vote ~ budget.treat*b.trust.AG + lc5.councillor.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)
h1a.b.1.tw <- felm(lc5.chair.inc.vote ~ budget.treat*b.trust.Twaweza + lc5.chair.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.good.lc5.chair.comp)
h1a.b.2.tw <- felm(lc5.councillor.inc.vote ~ budget.treat*b.trust.Twaweza + lc5.councillor.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.good.lc5.councillor.comp)
h1b.b.1.tw <- felm(lc5.chair.inc.vote ~ budget.treat*b.trust.Twaweza + lc5.chair.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.chair.name, data=budget.bad.lc5.chair.comp)
h1b.b.2.tw <- felm(lc5.councillor.inc.vote ~ budget.treat*b.trust.Twaweza + lc5.councillor.intent + b.Q1_living_conditions + b.Q24_Powerful_ppl_learning_how_ + b.Q25_Will_counting_votes_be_fai + r.What_is_your_gender + r.education + r.What_is_your_age | location.id | 0 | lc5.councillor.name, data=budget.bad.lc5.councillor.comp)

stargazer(h1a.b.1.ag, h1a.b.2.ag, h1b.b.1.ag, h1b.b.2.ag, h1a.b.1.tw, h1a.b.2.tw, h1b.b.1.tw, h1b.b.2.tw, type = "latex", 
          dep.var.labels = c("LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor","LC5 Chair", "LC5 Councillor", "LC5 Chair", "LC5 Councillor"),
          column.labels = c("Trust in Auditor General","Trust in Twaweza"), column.separate = c(4,4),
          keep = c("budget.treat","b.trust.AG","b.trust.Twaweza"),
          covariate.labels = c("Budget Treatment","AG Trust: Don't Know","AG Trust: Missing","AG Trust: A Little","AG Trust: A Lot","Twaweza Trust: Don't Know","Twaweza Trust: Missing","Twaweza Trust: A Little","Twaweza Trust: A Lot", "Treatment * AG Trust: Don't Know","Treatment * AG Trust: Missing","Treatment * AG Trust: A Little","Treatment * AG Trust: A Lot", "Treatment * Twaweza Trust: Don't Know","Treatment * Twaweza Trust: Missing","Treatment * Twaweza Trust: A Little","Treatment * Twaweza Trust: A Lot"),
          add.lines = list(c("Village fixed effects", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Covariates", "Yes", "Yes","Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("News Type","Good","Good","Bad","Bad","Good","Good","Bad","Bad")),
          dep.var.caption  = "DV: Vote Choice for the Incumbent",
          notes = "Notes: SEs clustered by politician; two-tailed tests; contested elections only without party switching by incumbent.",
          df = FALSE, omit.stat = c("rsq","ser"),
          report = c('vcsp'),
          notes.append = FALSE,
          notes.label = ""
) #Date and time: Wed, Dec 06, 2017 - 11:28:41

