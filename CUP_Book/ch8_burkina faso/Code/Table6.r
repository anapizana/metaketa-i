
### TABLE 6: HETEROGENEOUS EFFECTS BY UNCERTAINTY ABOUT PRIOR BELIEFS

fit.ambiguity.1<-lm(vote.incumbent~uncertainty.quantile*treatment.info+factor(v.unique)+factor(commune),weights=weight,data=survey)
amb1<-signif(summary(fit.ambiguity.1)$coefficients,2)
amb1.n<-length(fit.ambiguity.1$fitted.values)

fit.ambiguity.2<-lm(vote.incumbent~uncertainty.quantile*treatment.info+vote.incumbent.2012+weighted.scale(prior.quantile,weight)+weighted.scale(performance.quantile,weight)+factor(v.unique) + factor(commune),weights=weight,data=survey)
amb2<-signif(summary(fit.ambiguity.2)$coefficients,2)
amb2.n<-length(fit.ambiguity.2$fitted.values)

fit.ambiguity.3<-lm(vote.incumbent~uncertainty.quantile*treatment.info+vote.incumbent.2012+gender+edyears+age+weighted.scale(prior.quantile,weight)+weighted.scale(performance.quantile,weight)+factor(v.unique) + factor(commune),weights=weight,data=survey)
amb3<-signif(summary(fit.ambiguity.3)$coefficients,2)
amb3.n<-length(fit.ambiguity.3$fitted.values)


tex("
    \\begin{center}
    \\begin{tabular}{l c c c}
    \\hline
    &\\multicolumn{3}{c}{\\emph{Pro-incumbent vote choice}}\\\\
    &(1)&(2)&(3)\\\\
    \\hline  
    Information treatment&<<stars(amb1,'treatment.infoInformation',2)>>&<<stars(amb2,'treatment.infoInformation',2)>>&<<stars(amb3,'treatment.infoInformation',2)>>\\\\
    &(<<amb1['treatment.infoInformation',2]>>)&(<<amb2['treatment.infoInformation',2]>>)&(<<amb3['treatment.infoInformation',2]>>)\\\\
    Uncertainty about priors&<<stars(amb1,'uncertainty.quantile',2)>>&<<stars(amb2,'uncertainty.quantile',2)>>&<<stars(amb3,'uncertainty.quantile',2)>>\\\\
    &(<<amb1['uncertainty.quantile',2]>>)&(<<amb2['uncertainty.quantile',2]>>)&(<<amb3['uncertainty.quantile',2]>>)\\\\
    Information treatment$\\times$Uncertainty&<<stars(amb1,'uncertainty.quantile:treatment.infoInformation',2)>>&<<stars(amb2,'uncertainty.quantile:treatment.infoInformation',2)>>&<<stars(amb3,'uncertainty.quantile:treatment.infoInformation',2)>>\\\\
    &(<<amb1['uncertainty.quantile:treatment.infoInformation',2]>>)&(<<amb2['uncertainty.quantile:treatment.infoInformation',2]>>)&(<<amb3['uncertainty.quantile:treatment.infoInformation',2]>>)\\\\
    Incumbent performance$^{(s)}$&&<<stars(amb2,'weighted.scale(performance.quantile, weight)',2)>>&<<stars(amb3,'weighted.scale(performance.quantile, weight)',2)>>\\\\
    &&(<<amb2['weighted.scale(performance.quantile, weight)',2]>>)&(<<amb3['weighted.scale(performance.quantile, weight)',2]>>)\\\\
    Prior beliefs$^{(s)}$&&<<stars(amb2,'weighted.scale(prior.quantile, weight)',2)>>&<<stars(amb3,'weighted.scale(prior.quantile, weight)',2)>>\\\\
    &&(<<amb2['weighted.scale(prior.quantile, weight)',2]>>)&(<<amb3['weighted.scale(prior.quantile, weight)',2]>>)\\\\
    Voted for Incumbent in 2012&&<<stars(amb2,'vote.incumbent.2012',2)>>&<<stars(amb3,'vote.incumbent.2012',2)>>\\\\
    &&(<<amb2['vote.incumbent.2012',2]>>)&(<<amb3['vote.incumbent.2012',2]>>)\\\\
    Individual controls&&&Yes\\\\
    Village \\& municipality fixed effects&Yes&Yes&Yes\\\\
    \\hline
    Observations&<<amb1.n>>&<<amb2.n>>&<<amb3.n>>\\\\
    \\hline\\hline
    \\end{tabular}
    \\end{center}
    \\caption{Heterogeneous effects of performance information by uncertainty about prior beliefs. Uncertainty is measured on a 5-point scale (0...completely certain to 1...no idea). The table reports OLS coefficients. Standard errors in parentheses. Individual controls in column (3) are gender, age, years of education. $^{(s)}$Standardized. $^{*} p<0.1$ $^{**} p<0.05$ $^{***} p<0.01$. }
    ", file = "Tables/BF_TableAmbiguity.tex")
