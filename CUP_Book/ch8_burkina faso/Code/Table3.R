
### TABLE 3: RESULTS BY GOOD NEWS AND BAD NEWS SUBGROUPS


# Pro-Incumbent Voting Intent
prior.vc.pn <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(bias.quantile,weight) + factor(v.unique), weights = weight, data = survey)) # pooled
prior.vc.gn <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(bias.quantile,weight) + factor(v.unique), weights = weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
prior.vc.bn <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(bias.quantile,weight) + factor(v.unique), weights = weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

prior.vc.p <- signif(coef(prior.vc.pn), 2) 
prior.vc.g <- signif(coef(prior.vc.gn), 2)
prior.vc.b <- signif(coef(prior.vc.bn), 2)

# Turnout Propensity
prior.to.pn <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(bias.quantile,weight) + factor(v.unique), weights = survey$weight, data = survey)) # pooled
prior.to.gn <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(bias.quantile,weight) + factor(v.unique), weights = weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
prior.to.bn <- summary(lm(turnout.intent ~ treatment.info*weighted.scale(bias.quantile,weight) + factor(v.unique), weights = weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

prior.to.p <- signif(coef(prior.to.pn), 2) 
prior.to.g <- signif(coef(prior.to.gn), 2)
prior.to.b <- signif(coef(prior.to.bn), 2)

# Extrapolated Incumbent Vote Share (Pro-incumbent voting intent weighted by turnout intent)
# --> eights are multiplied by turnout.intent2
# --> turnout.intent2 is turnout.intent scaled so that its elements sum up to n
survey$turnout.intent2 <- ((survey$turnout.intent * sum((!is.na(survey$turnout.intent)))) / sum(survey$turnout.intent, na.rm = T))

prior.ev.pn <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(bias.quantile,turnout.intent2*weight) + factor(v.unique), weights = turnout.intent2*weight, data = survey)) # pooled
prior.ev.gn <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(bias.quantile,turnout.intent2*weight) + factor(v.unique), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 1))) # good news subgroup
prior.ev.bn <- summary(lm(vote.incumbent ~ treatment.info*weighted.scale(bias.quantile,turnout.intent2*weight) + factor(v.unique), weights = turnout.intent2*weight, data = subset(survey,goodnews.quantile == 0))) # bad news subgroup

prior.ev.p <- signif(coef(prior.ev.pn), 2) 
prior.ev.g <- signif(coef(prior.ev.gn), 2)
prior.ev.b <- signif(coef(prior.ev.bn), 2)


tex("
    \\begin{center}
    \\begin{tabular}{l c c c }
    \\hline
    &(1)&(2)&(3)\\\\
    &Pro-incumbent&Turnout &Extrapolated incumbent\\\\
    &voting intent& Propensity&vote share\\\\
    \\hline
    \\textbf{Panel A: Pooled}\\\\
    \\hline
    Information treatment&<<stars(prior.vc.p,2,2)>>&<<stars(prior.to.p,2,2)>>&<<stars(prior.ev.p,2,2)>>\\\\
    &(<<prior.vc.p[2,2]>>)&(<<prior.to.p[2,2]>>)&(<<prior.ev.p[2,2]>>)\\\\
    $\\times$ Gap between priors and information$^{(s)}$ &<<stars(prior.vc.p,nrow(prior.vc.p),2)>>&<<stars(prior.to.p,nrow(prior.to.p),2)>>&<<stars(prior.ev.p,nrow(prior.ev.p),2)>>\\\\
    &(<<prior.vc.p[nrow(prior.vc.p),2]>>)&(<<prior.to.p[nrow(prior.to.p),2]>>)&(<<prior.ev.p[nrow(prior.ev.p),2]>>)\\\\
    Observations&<<prior.vc.pn$df[1]+prior.vc.pn$df[2]>>&<<prior.to.pn$df[1]+prior.to.pn$df[2]>>&<<prior.ev.pn$df[1]+prior.ev.pn$df[2]>>\\\\
    \\hline
    \\textbf{Panel B: Good News Subgroup}\\\\
    \\hline
    Information treatment&<<stars(prior.vc.g,2,2)>>&<<stars(prior.to.g,2,2)>>&<<stars(prior.ev.g,2,2)>>\\\\
    &(<<prior.vc.g[2,2]>>)&(<<prior.to.g[2,2]>>)&(<<prior.ev.g[2,2]>>)\\\\
    $\\times$ Gap between priors and information$^{(s)}$ &<<stars(prior.vc.g,nrow(prior.vc.g),2)>>&<<stars(prior.to.g,nrow(prior.to.g),2)>>&<<stars(prior.ev.g,nrow(prior.ev.g),2)>>\\\\
    &(<<prior.vc.g[nrow(prior.vc.g),2]>>)&(<<prior.to.g[nrow(prior.to.g),2]>>)&(<<prior.ev.g[nrow(prior.ev.g),2]>>)\\\\
    Observations&<<prior.vc.gn$df[1]+prior.vc.gn$df[2]>>&<<prior.to.gn$df[1]+prior.to.gn$df[2]>>&<<prior.ev.gn$df[1]+prior.ev.gn$df[2]>>\\\\
    \\hline
    \\textbf{Panel C: Bad News Subgroup}\\\\
    \\hline
    Information treatment&<<stars(prior.vc.b,2,2)>>&<<stars(prior.to.b,2,2)>>&<<stars(prior.ev.b,2,2)>>\\\\
    &(<<prior.vc.b[2,2]>>)&(<<prior.to.b[2,2]>>)&(<<prior.ev.b[2,2]>>)\\\\
    $\\times$ Gap between priors and information$^{(s)}$ &<<stars(prior.vc.b,nrow(prior.vc.b),2)>>&<<stars(prior.to.b,nrow(prior.to.b),2)>>&<<stars(prior.ev.b,nrow(prior.ev.b),2)>>\\\\
    &(<<prior.vc.b[nrow(prior.vc.b),2]>>)&(<<prior.to.b[nrow(prior.to.b),2]>>)&(<<prior.ev.b[nrow(prior.ev.b),2]>>)\\\\
    Observations&<<prior.vc.bn$df[1]+prior.vc.bn$df[2]>>&<<prior.to.bn$df[1]+prior.to.bn$df[2]>>&<<prior.ev.bn$df[1]+prior.ev.bn$df[2]>>\\\\
    \\hline\\hline
    \\end{tabular}
    \\end{center}", 
    file = "Tables/BF_TableResults.tex")

