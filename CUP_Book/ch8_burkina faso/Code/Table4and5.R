
### TABLES 4 AND 5: HETEROGENEOUS TREATMENT EFFECTS

# (1) Plain estimate of treatment effect
# (2) Incumbent supporter in 2012 elections (proxy for partisan identity)
# (3) Campaign gift expected
# (4) Perceived competitiveness of 2016 elections (at the village level, measured in the control group)
# (5) Beliefs about electoral integrity (fair vote counting)
# (6) Beliefs about ballot secrecy

## (1) Plain estimate of treatment effect

# Pro-Incumbent Voting Intent
all.vc.pn <- summary(lm(vote.incumbent ~ treatment.info + factor(v.unique), weights = survey$weight, data = survey)) # pooled
all.vc.gn <- summary(lm(vote.incumbent ~ treatment.info + factor(v.unique), weights = weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
all.vc.bn <- summary(lm(vote.incumbent ~ treatment.info + factor(v.unique), weights = weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

all.vc.p <- signif(coef(all.vc.pn), 2) 
all.vc.g <- signif(coef(all.vc.gn), 2)
all.vc.b <- signif(coef(all.vc.bn), 2)

# Extrapolated Incumbent Vote Share
all.ev.pn <- summary(lm(vote.incumbent ~ treatment.info + factor(v.unique), weights = turnout.intent2*weight, data = survey)) # pooled
all.ev.gn <- summary(lm(vote.incumbent ~ treatment.info + factor(v.unique), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
all.ev.bn <- summary(lm(vote.incumbent ~ treatment.info + factor(v.unique), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

all.ev.p <- signif(coef(all.ev.pn), 2) 
all.ev.g <- signif(coef(all.ev.gn), 2)
all.ev.b <- signif(coef(all.ev.bn), 2)

## (2) Incumbent supporter in 2012 elections (proxy for partisan identity)

#Indicator of whether respondent voted for the incumbent party in 2012
survey$vote.incumbent.2012<-as.numeric(survey$vote_incumbent_party_mun_2012=="Yes")
#Set vote.incumbent.2012=0 if respondent did not vote in 2012
survey$vote.incumbent.2012[survey$vote_municipal_elections_2012!="Yes"]<-0

#Indicator of voting eligibility in 2012
survey$votingage.2012<-as.numeric(survey$age>=21)

# Pro-Incumbent Voting Intent
incvi.pfe <- summary(lm(vote.incumbent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = weight, data = survey)) # pooled
incvi.gfe <- summary(lm(vote.incumbent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
incvi.bfe <- summary(lm(vote.incumbent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Turnout Intent
intent.pfe <- summary(lm(turnout.intent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = weight, data = survey)) # pooled
intent.gfe <- summary(lm(turnout.intent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
intent.bfe <- summary(lm(turnout.intent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Estimated incumbent vote share
eivs.pfe <- summary(lm(vote.incumbent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = turnout.intent*weight, data = survey)) # pooled
eivs.gfe <- summary(lm(vote.incumbent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = turnout.intent*weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
eivs.bfe <- summary(lm(vote.incumbent ~ treatment.info * vote.incumbent.2012 + factor(v.unique) + factor(commune), weights = turnout.intent*weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# make table
incvi.psfe <- signif(coef(incvi.pfe), 2) 
incvi.gsfe <- signif(coef(incvi.gfe), 2)
incvi.bsfe <- signif(coef(incvi.bfe), 2)
partisan.vc.p<-incvi.psfe
partisan.vc.g<-incvi.gsfe
partisan.vc.b<-incvi.bsfe
partisan.vc.pn<-incvi.pfe
partisan.vc.gn<-incvi.gfe
partisan.vc.bn<-incvi.bfe

intent.psfe <- signif(coef(intent.pfe), 2) 
intent.gsfe <- signif(coef(intent.gfe), 2)
intent.bsfe <- signif(coef(intent.bfe), 2)
partisan.to.p<-intent.psfe
partisan.to.g<-intent.gsfe
partisan.to.b<-intent.bsfe
partisan.to.pn<-intent.pfe
partisan.to.gn<-intent.gfe
partisan.to.bn<-intent.bfe

eivs.psfe <- signif(coef(eivs.pfe), 2) 
eivs.gsfe <- signif(coef(eivs.gfe), 2)
eivs.bsfe <- signif(coef(eivs.bfe), 2)
partisan.ev.p<-eivs.psfe
partisan.ev.g<-eivs.gsfe
partisan.ev.b<-eivs.bsfe
partisan.ev.pn<-eivs.pfe
partisan.ev.gn<-eivs.gfe
partisan.ev.bn<-eivs.bfe

## (3) Campaign gift expected

# Does the study participant expect to receive a campaign gift? 
survey$campaign_gift_expected<-as.numeric(survey$mun_campaign_gifts=="Yes")

# Pro-Incumbent Voting Intent
incvi.pfe <- summary(lm(vote.incumbent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights=weight, data = survey)) # pooled
incvi.gfe <- summary(lm(vote.incumbent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
incvi.bfe <- summary(lm(vote.incumbent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Turnout Intent
intent.pfe <- summary(lm(turnout.intent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights=weight, data = survey)) # pooled
intent.gfe <- summary(lm(turnout.intent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
intent.bfe <- summary(lm(turnout.intent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Estimated incumbent vote share
eivs.pfe <- summary(lm(vote.incumbent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = survey)) # pooled
eivs.gfe <- summary(lm(vote.incumbent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
eivs.bfe <- summary(lm(vote.incumbent ~ treatment.info*campaign_gift_expected + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

# make table
incvi.psfe <- signif(coef(incvi.pfe), 2) 
incvi.gsfe <- signif(coef(incvi.gfe), 2)
incvi.bsfe <- signif(coef(incvi.bfe), 2)
votebuying.vc.p<-incvi.psfe
votebuying.vc.g<-incvi.gsfe
votebuying.vc.b<-incvi.bsfe
votebuying.vc.pn<-incvi.pfe
votebuying.vc.gn<-incvi.gfe
votebuying.vc.bn<-incvi.bfe

intent.psfe <- signif(coef(intent.pfe), 2) 
intent.gsfe <- signif(coef(intent.gfe), 2)
intent.bsfe <- signif(coef(intent.bfe), 2)
votebuying.to.p<-intent.psfe
votebuying.to.g<-intent.gsfe
votebuying.to.b<-intent.bsfe
votebuying.to.pn<-intent.pfe
votebuying.to.gn<-intent.gfe
votebuying.to.bn<-intent.bfe

eivs.psfe <- signif(coef(eivs.pfe), 2) 
eivs.gsfe <- signif(coef(eivs.gfe), 2)
eivs.bsfe <- signif(coef(eivs.bfe), 2)
votebuying.ev.p<-eivs.psfe
votebuying.ev.g<-eivs.gsfe
votebuying.ev.b<-eivs.bsfe
votebuying.ev.pn<-eivs.pfe
votebuying.ev.gn<-eivs.gfe
votebuying.ev.bn<-eivs.bfe

## (4) Perceived competitiveness of 2016 elections (at the village level, measured in the control group)

# Perceived competitiveness of elections in the respondent's village
for (r in unique(survey$region)) {
  for (c in unique(survey$commune[survey$region==r])) {
    for (v in unique(survey$village[survey$region==r&survey$commune==c])) {
      survey$perceived.competitiveness.village[survey$region==r&survey$commune==c&survey$village==v]<-mean(survey$expected.votemargin.incumbent[survey$region==r&survey$commune==c&survey$village==v&survey$treatment.info=="Control"],na.rm=TRUE)
    }
  }
}

# Pro-Incumbent Voting Intent
incvi.pfe <- robcov(ols(vote.incumbent ~ treatment.info*abs(perceived.competitiveness.village), weights=weight, data=survey, x=TRUE, y=TRUE),cluster=survey$v.unique) # pooled
incvi.gfe <- robcov(ols(vote.incumbent ~ treatment.info*abs(perceived.competitiveness.village), weights=weight, data=subset(survey,goodnews.quantile == 1), x=TRUE, y=TRUE),cluster=subset(survey,goodnews.quantile == 1)$v.unique) # good news subgroup
incvi.bfe <- robcov(ols(vote.incumbent ~ treatment.info*abs(perceived.competitiveness.village), weights=weight, data=subset(survey,goodnews.quantile == 0), x=TRUE, y=TRUE),cluster=subset(survey,goodnews.quantile == 0)$v.unique) # bad news subgroup

# Turnout Intent
intent.pfe <- robcov(ols(turnout.intent ~ treatment.info*abs(perceived.competitiveness.village), weights=weight, data=survey, x=TRUE, y=TRUE),cluster=survey$v.unique) # pooled
intent.gfe <- robcov(ols(turnout.intent ~ treatment.info*abs(perceived.competitiveness.village), weights=weight, data=subset(survey,goodnews.quantile == 1), x=TRUE, y=TRUE),cluster=subset(survey,goodnews.quantile == 1)$v.unique) # good news subgroup
intent.bfe <- robcov(ols(turnout.intent ~ treatment.info*abs(perceived.competitiveness.village), weights=weight, data=subset(survey,goodnews.quantile == 0), x=TRUE, y=TRUE),cluster=subset(survey,goodnews.quantile == 0)$v.unique) # bad news subgroup

# Estimated incumbent vote share
eivs.pfe <- robcov(ols(vote.incumbent ~ treatment.info*abs(perceived.competitiveness.village), weights=turnout.intent2*weight, data=survey, x=TRUE, y=TRUE),cluster=survey$v.unique) # pooled
eivs.gfe <- robcov(ols(vote.incumbent ~ treatment.info*abs(perceived.competitiveness.village), weights=turnout.intent2*weight, data=subset(survey,goodnews.quantile == 1), x=TRUE, y=TRUE),cluster=subset(survey,goodnews.quantile == 1)$v.unique) # good news subgroup
eivs.bfe <- robcov(ols(vote.incumbent ~ treatment.info*abs(perceived.competitiveness.village), weights=turnout.intent2*weight, data=subset(survey,goodnews.quantile == 0), x=TRUE, y=TRUE),cluster=subset(survey,goodnews.quantile == 0)$v.unique) # bad news subgroup

# make table
incvi.psfe <- signif(robcov.tTable(incvi.pfe), 2)
incvi.gsfe <- signif(robcov.tTable(incvi.gfe), 2)
incvi.bsfe <- signif(robcov.tTable(incvi.bfe), 2)
winningmargin.vc.p<-incvi.psfe
winningmargin.vc.g<-incvi.gsfe
winningmargin.vc.b<-incvi.bsfe
winningmargin.vc.pn<-incvi.pfe
winningmargin.vc.gn<-incvi.gfe
winningmargin.vc.bn<-incvi.bfe

intent.psfe <- signif(robcov.tTable(intent.pfe), 2)
intent.gsfe <- signif(robcov.tTable(intent.gfe), 2)
intent.bsfe <- signif(robcov.tTable(intent.bfe), 2)
winningmargin.to.p<-intent.psfe
winningmargin.to.g<-intent.gsfe
winningmargin.to.b<-intent.bsfe
winningmargin.to.pn<-intent.pfe
winningmargin.to.gn<-intent.gfe
winningmargin.to.bn<-intent.bfe

eivs.psfe <- signif(robcov.tTable(eivs.pfe), 2)
eivs.gsfe <- signif(robcov.tTable(eivs.gfe), 2)
eivs.bsfe <- signif(robcov.tTable(eivs.bfe), 2)
winningmargin.ev.p<-eivs.psfe
winningmargin.ev.g<-eivs.gsfe
winningmargin.ev.b<-eivs.bsfe
winningmargin.ev.pn<-eivs.pfe
winningmargin.ev.gn<-eivs.gfe
winningmargin.ev.bn<-eivs.bfe


# (5) Beliefs about electoral integrity (fair vote counting)

# Likelihood that counting is fair? 
survey$fair.counting<-as.numeric(survey$likelihood_fair_vote_counting)
survey$fair.counting[survey$fair.counting==5]<-NA
survey$fair.counting<-5-survey$fair.counting

# Pro-Incumbent Voting Intent
incvi.pfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights = weight, data = survey)) # pooled
incvi.gfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
incvi.bfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Turnout Intent
intent.pfe <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights = weight, data = survey)) # pooled
intent.gfe <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
intent.bfe <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Estimated incumbent vote share
eivs.pfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = survey)) # pooled
eivs.gfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
eivs.bfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(fair.counting,weight) + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

# make table
incvi.psfe <- signif(coef(incvi.pfe), 2) 
incvi.gsfe <- signif(coef(incvi.gfe), 2)
incvi.bsfe <- signif(coef(incvi.bfe), 2)
faircounting.vc.p<-incvi.psfe
faircounting.vc.g<-incvi.gsfe
faircounting.vc.b<-incvi.bsfe
faircounting.vc.pn<-incvi.pfe
faircounting.vc.gn<-incvi.gfe
faircounting.vc.bn<-incvi.bfe

intent.psfe <- signif(coef(intent.pfe), 2) 
intent.gsfe <- signif(coef(intent.gfe), 2)
intent.bsfe <- signif(coef(intent.bfe), 2)
faircounting.to.p<-intent.psfe
faircounting.to.g<-intent.gsfe
faircounting.to.b<-intent.bsfe
faircounting.to.pn<-intent.pfe
faircounting.to.gn<-intent.gfe
faircounting.to.bn<-intent.bfe

eivs.psfe <- signif(coef(eivs.pfe), 2) 
eivs.gsfe <- signif(coef(eivs.gfe), 2)
eivs.bsfe <- signif(coef(eivs.bfe), 2)
faircounting.ev.p<-eivs.psfe
faircounting.ev.g<-eivs.gsfe
faircounting.ev.b<-eivs.bsfe
faircounting.ev.pn<-eivs.pfe
faircounting.ev.gn<-eivs.gfe
faircounting.ev.bn<-eivs.bfe

## (6) Beliefs about ballot secrecy

# How likely do you think it is that powerful people can find out how you vote in the municipal elections, even if you do not want them to find out? 
survey$no.secrecy<-as.numeric(survey$likelihood_vote_knowledge)
survey$no.secrecy[survey$no.secrecy==5]<-NA
survey$no.secrecy<-5-survey$no.secrecy

# Pro-Incumbent Voting Intent
incvi.pfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights = weight, data = survey)) # pooled
incvi.gfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
incvi.bfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Turnout Intent
intent.pfe <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights = weight, data = survey)) # pooled
intent.gfe <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 1))) # good news subgroup
intent.bfe <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights=weight, subset(survey,goodnews.quantile == 0))) # bad news subgroup

# Estimated incumbent vote share
eivs.pfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = survey)) # pooled
eivs.gfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
eivs.bfe <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(no.secrecy,weight) + factor(v.unique) + factor(commune), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

# make table
incvi.psfe <- signif(coef(incvi.pfe), 2) 
incvi.gsfe <- signif(coef(incvi.gfe), 2)
incvi.bsfe <- signif(coef(incvi.bfe), 2)
ballotsecrecy.vc.p<-incvi.psfe
ballotsecrecy.vc.g<-incvi.gsfe
ballotsecrecy.vc.b<-incvi.bsfe
ballotsecrecy.vc.pn<-incvi.pfe
ballotsecrecy.vc.gn<-incvi.gfe
ballotsecrecy.vc.bn<-incvi.bfe

intent.psfe <- signif(coef(intent.pfe), 2) 
intent.gsfe <- signif(coef(intent.gfe), 2)
intent.bsfe <- signif(coef(intent.bfe), 2)
ballotsecrecy.to.p<-intent.psfe
ballotsecrecy.to.g<-intent.gsfe
ballotsecrecy.to.b<-intent.bsfe
ballotsecrecy.to.pn<-intent.pfe
ballotsecrecy.to.gn<-intent.gfe
ballotsecrecy.to.bn<-intent.bfe

eivs.psfe <- signif(coef(eivs.pfe), 2) 
eivs.gsfe <- signif(coef(eivs.gfe), 2)
eivs.bsfe <- signif(coef(eivs.bfe), 2)
ballotsecrecy.ev.p<-eivs.psfe
ballotsecrecy.ev.g<-eivs.gsfe
ballotsecrecy.ev.b<-eivs.bsfe
ballotsecrecy.ev.pn<-eivs.pfe
ballotsecrecy.ev.gn<-eivs.gfe
ballotsecrecy.ev.bn<-eivs.bfe


## OUTPUT TABLE 4 (DEP. VAR.: PRO-INCUMBENT VOTING)

tex("
    \\begin{center}
    \\begin{tabular}{l c c c c c c }
    \\hline
    &\\multicolumn{6}{c}{\\emph{Pro-Incumbent Voting Intent}}\\\\
    &(1)&(2)&(3)&(4)&(5)&(6)\\\\
    \\hline
    \\textbf{Panel A: Pooled}\\\\
    \\hline
    Information treatment &<<stars(all.vc.p,2,2)>>&<<stars(partisan.vc.p,2,2)>>&<<stars(votebuying.vc.p,2,2)>>&<<stars(winningmargin.vc.p,2,2)>>&<<stars(faircounting.vc.p,2,2)>>&<<stars(ballotsecrecy.vc.p,2,2)>>\\\\
    &(<<all.vc.p[2, 2]>>)&(<<partisan.vc.p[2, 2]>>)&(<<votebuying.vc.p[2, 2]>>)&(<<winningmargin.vc.p[2, 2]>>)&(<<faircounting.vc.p[2, 2]>>)&(<<ballotsecrecy.vc.p[2, 2]>>)\\\\
    $\\times$ Incumbent supporter & &<<stars(partisan.vc.p,nrow(partisan.vc.p), 2)>>& & & & \\\\
    & &(<<partisan.vc.p[nrow(partisan.vc.p), 2]>>)& & & & \\\\
    $\\times$ Expects campaign gift & & &<<stars(votebuying.vc.p,nrow(votebuying.vc.p), 2)>> & & & \\\\
    & & &(<<votebuying.vc.p[nrow(votebuying.vc.p), 2]>>) & & & \\\\
    $\\times$ abs(Expected vote margin) & & & &<<stars(winningmargin.vc.p,nrow(winningmargin.vc.p), 2)>>& & \\\\
    & & & &(<<winningmargin.vc.p[nrow(winningmargin.vc.p), 2]>>$^{(c)}$)& & \\\\
    $\\times$ Fair vote count$^{(s)}$ & & & & &<<stars(faircounting.vc.p,nrow(faircounting.vc.p), 2)>>& \\\\
    & & & & &(<<faircounting.vc.p[nrow(faircounting.vc.p), 2]>>)& \\\\
    $\\times$ Ballot secrecy$^{(s)}$ & & & & & &<<stars(ballotsecrecy.vc.p,nrow(ballotsecrecy.vc.p), 2)>>\\\\
    & & & & & &(<<ballotsecrecy.vc.p[nrow(ballotsecrecy.vc.p), 2]>>)\\\\
    Observations &<<all.vc.pn$df[1]+all.vc.pn$df[2]>>&<<partisan.vc.pn$df[1]+partisan.vc.pn$df[2]>>&<<votebuying.vc.pn$df[1]+votebuying.vc.pn$df[2]>>&<<nobs(winningmargin.vc.pn)>>&<<faircounting.vc.pn$df[1]+faircounting.vc.pn$df[2]>>&<<ballotsecrecy.vc.pn$df[1]+ballotsecrecy.vc.pn$df[2]>>\\\\
    \\hline
    \\textbf{Panel B: Good News Subgroup}\\\\
    \\hline
    Information treatment &<<stars(all.vc.g,2,2)>>&<<stars(partisan.vc.g,2,2)>>&<<stars(votebuying.vc.g,2,2)>>&<<stars(winningmargin.vc.g,2,2)>>&<<stars(faircounting.vc.g,2,2)>>&<<stars(ballotsecrecy.vc.g,2,2)>>\\\\
    &(<<all.vc.g[2, 2]>>)&(<<partisan.vc.g[2, 2]>>)&(<<votebuying.vc.g[2, 2]>>)&(<<winningmargin.vc.g[2, 2]>>)&(<<faircounting.vc.g[2, 2]>>)&(<<ballotsecrecy.vc.g[2, 2]>>)\\\\
    $\\times$ Incumbent supporter & &<<stars(partisan.vc.g,nrow(partisan.vc.g),2)>>& & & & \\\\
    & &(<<partisan.vc.g[nrow(partisan.vc.g),2]>>)& & & & \\\\
    $\\times$ Expects campaign gift & & &<<stars(votebuying.vc.g,nrow(votebuying.vc.g),2)>> & & & \\\\
    & & &(<<votebuying.vc.g[nrow(votebuying.vc.g), 2]>>) & & & \\\\
    $\\times$ abs(Expected vote margin) & & & &<<stars(winningmargin.vc.g,nrow(winningmargin.vc.g),2)>>& & \\\\
    & & & &(<<winningmargin.vc.g[nrow(winningmargin.vc.g), 2]>>$^{(c)}$)& & \\\\
    $\\times$ Fair vote count$^{(s)}$ & & & & &<<stars(faircounting.vc.g,nrow(faircounting.vc.g),2)>>& \\\\
    & & & & &(<<faircounting.vc.g[nrow(faircounting.vc.g),2]>>)& \\\\
    $\\times$ Ballot secrecy$^{(s)}$ & & & & & &<<stars(ballotsecrecy.vc.g,nrow(ballotsecrecy.vc.g), 2)>>\\\\
    & & & & & &(<<ballotsecrecy.vc.g[nrow(ballotsecrecy.vc.g), 2]>>)\\\\
    Observations &<<all.vc.gn$df[1]+all.vc.gn$df[2]>>&<<partisan.vc.gn$df[1]+partisan.vc.gn$df[2]>>&<<votebuying.vc.gn$df[1]+votebuying.vc.gn$df[2]>>&<<nobs(winningmargin.vc.gn)>>&<<faircounting.vc.gn$df[1]+faircounting.vc.gn$df[2]>>&<<ballotsecrecy.vc.gn$df[1]+ballotsecrecy.vc.gn$df[2]>>\\\\
    \\hline
    \\textbf{Panel C: Bad News Subgroup}\\\\
    \\hline
    Information treatment &<<stars(all.vc.b,2,2)>>&<<stars(partisan.vc.b,2,2)>>&<<stars(votebuying.vc.b,2,2)>>&<<stars(winningmargin.vc.b,2,2)>>&<<stars(faircounting.vc.b,2,2)>>&<<stars(ballotsecrecy.vc.b,2,2)>>\\\\
    &(<<all.vc.b[2, 2]>>)&(<<partisan.vc.b[2, 2]>>)&(<<votebuying.vc.b[2, 2]>>)&(<<winningmargin.vc.b[2, 2]>>)&(<<faircounting.vc.b[2, 2]>>)&(<<ballotsecrecy.vc.b[2, 2]>>)\\\\
    $\\times$ Incumbent supporter & &<<stars(partisan.vc.b,nrow(partisan.vc.b),2)>>& & & & \\\\
    & &(<<partisan.vc.b[nrow(partisan.vc.b), 2]>>)& & & & \\\\
    $\\times$ Expects campaign gift & & &<<stars(votebuying.vc.b,nrow(votebuying.vc.b),2)>> & & & \\\\
    & & &(<<votebuying.vc.b[nrow(votebuying.vc.b), 2]>>) & & & \\\\
    $\\times$ abs(Expected vote margin)  & & & &<<stars(winningmargin.vc.b,nrow(winningmargin.vc.b),2)>>& & \\\\
    & & & &(<<winningmargin.vc.b[nrow(winningmargin.vc.b), 2]>>$^{(c)}$)& & \\\\
    $\\times$ Fair vote count$^{(s)}$ & & & & &<<stars(faircounting.vc.b,nrow(faircounting.vc.b),2)>>& \\\\
    & & & & &(<<faircounting.vc.b[nrow(faircounting.vc.b), 2]>>)& \\\\
    $\\times$ Ballot secrecy$^{(s)}$ & & & & & &<<stars(ballotsecrecy.vc.b,nrow(ballotsecrecy.vc.b),2)>>\\\\
    & & & & & &(<<ballotsecrecy.vc.b[nrow(ballotsecrecy.vc.b), 2]>>)\\\\
    Observations &<<all.vc.bn$df[1]+all.vc.bn$df[2]>>&<<partisan.vc.bn$df[1]+partisan.vc.bn$df[2]>>&<<votebuying.vc.bn$df[1]+votebuying.vc.bn$df[2]>>&<<nobs(winningmargin.vc.bn)>>&<<faircounting.vc.bn$df[1]+faircounting.vc.bn$df[2]>>&<<ballotsecrecy.vc.bn$df[1]+ballotsecrecy.vc.bn$df[2]>>\\\\
    \\hline\\hline
    \\end{tabular}
    \\end{center}", 
    file = "Tables/BF_TableHeterogeneity.tex")


## OUTPUT TABLE 5 (DEP. VAR.: TURNOUT-ADJUSTED, EXTRAPOLATED INCUMBENT VOTE SHARE)

tex("
    \\begin{center}
    \\begin{tabular}{l c c c c c c }
    \\hline
    &\\multicolumn{6}{c}{\\emph{Extrapolated Incumbent Vote Share}}\\\\
    &(1)&(2)&(3)&(4)&(5)&(6)\\\\
    \\hline
    \\textbf{Panel A: Pooled}\\\\
    \\hline
    Information treatment &<<stars(all.ev.p,2,2)>>&<<stars(partisan.ev.p,2,2)>>&<<stars(votebuying.ev.p,2,2)>>&<<stars(winningmargin.ev.p,2,2)>>&<<stars(faircounting.ev.p,2,2)>>&<<stars(ballotsecrecy.ev.p,2,2)>>\\\\
    &(<<all.ev.p[2, 2]>>)&(<<partisan.ev.p[2, 2]>>)&(<<votebuying.ev.p[2, 2]>>)&(<<winningmargin.ev.p[2, 2]>>)&(<<faircounting.ev.p[2, 2]>>)&(<<ballotsecrecy.ev.p[2, 2]>>)\\\\
    $\\times$ Incumbent supporter & &<<stars(partisan.ev.p,nrow(partisan.ev.p), 2)>>& & & & \\\\
    & &(<<partisan.ev.p[nrow(partisan.ev.p), 2]>>)& & & & \\\\
    $\\times$ Expects campaign gift & & &<<stars(votebuying.ev.p,nrow(votebuying.ev.p), 2)>> & & & \\\\
    & & &(<<votebuying.ev.p[nrow(votebuying.ev.p), 2]>>) & & & \\\\
    $\\times$ abs(Expected vote margin) & & & &<<stars(winningmargin.ev.p,nrow(winningmargin.ev.p), 2)>>& & \\\\
    & & & &(<<winningmargin.ev.p[nrow(winningmargin.ev.p), 2]>>$^{(c)}$)& & \\\\
    $\\times$ Fair vote count$^{(s)}$ & & & & &<<stars(faircounting.ev.p,nrow(faircounting.ev.p), 2)>>& \\\\
    & & & & &(<<faircounting.ev.p[nrow(faircounting.ev.p), 2]>>)& \\\\
    $\\times$ Ballot secrecy$^{(s)}$ & & & & & &<<stars(ballotsecrecy.ev.p,nrow(ballotsecrecy.ev.p), 2)>>\\\\
    & & & & & &(<<ballotsecrecy.ev.p[nrow(ballotsecrecy.ev.p), 2]>>)\\\\
    Observations &<<all.ev.pn$df[1]+all.ev.pn$df[2]>>&<<partisan.ev.pn$df[1]+partisan.ev.pn$df[2]>>&<<votebuying.ev.pn$df[1]+votebuying.ev.pn$df[2]>>&<<nobs(winningmargin.ev.pn)>>&<<faircounting.ev.pn$df[1]+faircounting.ev.pn$df[2]>>&<<ballotsecrecy.ev.pn$df[1]+ballotsecrecy.ev.pn$df[2]>>\\\\
    \\hline
    \\textbf{Panel B: Good News Subgroup}\\\\
    \\hline
    Information treatment &<<stars(all.ev.g,2,2)>>&<<stars(partisan.ev.g,2,2)>>&<<stars(votebuying.ev.g,2,2)>>&<<stars(winningmargin.ev.g,2,2)>>&<<stars(faircounting.ev.g,2,2)>>&<<stars(ballotsecrecy.ev.g,2,2)>>\\\\
    &(<<all.ev.g[2, 2]>>)&(<<partisan.ev.g[2, 2]>>)&(<<votebuying.ev.g[2, 2]>>)&(<<winningmargin.ev.g[2, 2]>>)&(<<faircounting.ev.g[2, 2]>>)&(<<ballotsecrecy.ev.g[2, 2]>>)\\\\
    $\\times$ Incumbent supporter & &<<stars(partisan.ev.g,nrow(partisan.ev.g),2)>>& & & & \\\\
    & &(<<partisan.ev.g[nrow(partisan.ev.g),2]>>)& & & & \\\\
    $\\times$ Expects campaign gift & & &<<stars(votebuying.ev.g,nrow(votebuying.ev.g),2)>> & & & \\\\
    & & &(<<votebuying.ev.g[nrow(votebuying.ev.g), 2]>>) & & & \\\\
    $\\times$ abs(Expected vote margin) & & & &<<stars(winningmargin.ev.g,nrow(winningmargin.ev.g),2)>>& & \\\\
    & & & &(<<winningmargin.ev.g[nrow(winningmargin.ev.g), 2]>>$^{(c)}$)& & \\\\
    $\\times$ Fair vote count$^{(s)}$ & & & & &<<stars(faircounting.ev.g,nrow(faircounting.ev.g),2)>>& \\\\
    & & & & &(<<faircounting.ev.g[nrow(faircounting.ev.g),2]>>)& \\\\
    $\\times$ Ballot secrecy$^{(s)}$ & & & & & &<<stars(ballotsecrecy.ev.g,nrow(ballotsecrecy.ev.g), 2)>>\\\\
    & & & & & &(<<ballotsecrecy.ev.g[nrow(ballotsecrecy.ev.g), 2]>>)\\\\
    Observations &<<all.ev.gn$df[1]+all.ev.gn$df[2]>>&<<partisan.ev.gn$df[1]+partisan.ev.gn$df[2]>>&<<votebuying.ev.gn$df[1]+votebuying.ev.gn$df[2]>>&<<nobs(winningmargin.ev.gn)>>&<<faircounting.ev.gn$df[1]+faircounting.ev.gn$df[2]>>&<<ballotsecrecy.ev.gn$df[1]+ballotsecrecy.ev.gn$df[2]>>\\\\
    \\hline
    \\textbf{Panel C: Bad News Subgroup}\\\\
    \\hline
    Information treatment &<<stars(all.ev.b,2,2)>>&<<stars(partisan.ev.b,2,2)>>&<<stars(votebuying.ev.b,2,2)>>&<<stars(winningmargin.ev.b,2,2)>>&<<stars(faircounting.ev.b,2,2)>>&<<stars(ballotsecrecy.ev.b,2,2)>>\\\\
    &(<<all.ev.b[2, 2]>>)&(<<partisan.ev.b[2, 2]>>)&(<<votebuying.ev.b[2, 2]>>)&(<<winningmargin.ev.b[2, 2]>>)&(<<faircounting.ev.b[2, 2]>>)&(<<ballotsecrecy.ev.b[2, 2]>>)\\\\
    $\\times$ Incumbent supporter & &<<stars(partisan.ev.b,nrow(partisan.ev.b),2)>>& & & & \\\\
    & &(<<partisan.ev.b[nrow(partisan.ev.b), 2]>>)& & & & \\\\
    $\\times$ Expects campaign gift & & &<<stars(votebuying.ev.b,nrow(votebuying.ev.b),2)>> & & & \\\\
    & & &(<<votebuying.ev.b[nrow(votebuying.ev.b), 2]>>) & & & \\\\
    $\\times$ abs(Expected vote margin)  & & & &<<stars(winningmargin.ev.b,nrow(winningmargin.ev.b),2)>>& & \\\\
    & & & &(<<winningmargin.ev.b[nrow(winningmargin.ev.b), 2]>>$^{(c)}$)& & \\\\
    $\\times$ Fair vote count$^{(s)}$ & & & & &<<stars(faircounting.ev.b,nrow(faircounting.ev.b),2)>>& \\\\
    & & & & &(<<faircounting.ev.b[nrow(faircounting.ev.b), 2]>>)& \\\\
    $\\times$ Ballot secrecy$^{(s)}$ & & & & & &<<stars(ballotsecrecy.ev.b,nrow(ballotsecrecy.ev.b),2)>>\\\\
    & & & & & &(<<ballotsecrecy.ev.b[nrow(ballotsecrecy.ev.b), 2]>>)\\\\
    Observations &<<all.ev.bn$df[1]+all.ev.bn$df[2]>>&<<partisan.ev.bn$df[1]+partisan.ev.bn$df[2]>>&<<votebuying.ev.bn$df[1]+votebuying.ev.bn$df[2]>>&<<nobs(winningmargin.ev.bn)>>&<<faircounting.ev.bn$df[1]+faircounting.ev.bn$df[2]>>&<<ballotsecrecy.ev.bn$df[1]+ballotsecrecy.ev.bn$df[2]>>\\\\
    \\hline\\hline
    \\end{tabular}
    \\end{center}", 
    file = "Tables/BF_TableHeterogeneityExtrapolated.tex")

