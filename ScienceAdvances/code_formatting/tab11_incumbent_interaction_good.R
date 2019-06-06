gentab11 <- function(){
  stargazer(t1ncovc2, t1ncovc3, t1ncovc4, t1ncovc5, t1ncovc6,
          t1ncovc7, t1ncovc9,
          type = "latex",
          label = "Interaction_good",
          df = FALSE, column.sep.width = "1pt",
          omit = c("Constant"),
          omit.stat = c("adj.rsq", "ser"),
          no.space = T,
          header = F, table.placement = "!htbp",
          dep.var.labels.include = FALSE,
          omit.table.layout = "n",
          dep.var.caption = "Incumbent vote choice, good news",
          column.labels = c("ALL","BEN","BRZ","BF","MEX","UG 1","UG 2"),
          add.lines=list(c("Covariates", rep(c("Yes"),7))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Interaction analysis: Effect of good news on incumbent vote choice",
          order = c(1,2,10,3,11,4,12,5,13,6,14,7,15,8,16,9,17,10,18,11,19),
          covariate.labels = c("Treatment", "$N_{ij}$","Treatment * $N_{ij}$",
                               "Age", "Treatment * Age",
                               "Education", "Treatment * Education",
                               "Wealth", "Treatment * Wealth",
                               "Voted previously", "Treatment * Voted previously",
                               "Supported incumbent", "Treatment * Supported incumbent",
                               "Clientelism", "Treatment * Clientelism",
                               "Credible source", "Treatment * Credible source",
                               "Secret ballot", "Treatment * Secret ballot",
                               "Free, fair election", "Treatment * free, fair election"))}

t11.11 <- capture.output(gentab11())

#output to replication and ms folder
fileConn <- file("tables/tab_11.11_interaction_incumbent_good.tex")

writeLines(unlist(list("\\begin{sidewaystable}[!htbp]\\centering",
                       t11.11[3:4],
                       "\\scalebox{0.7}{",
                       t11.11[5:60],
                       "\\end{tabular}}",
                       "\\begin{flushleft} \\hspace{1cm} \\scriptsize{{\\textit{Note:}  The table presents results from fitting equation \\ref{metaeq.main1a}. Standard errors are clustered at the level of treatment assignment. Results in columns (1) and (7) include vote choice for LCV councilors as well as chairs in the Uganda 2 study (see Buntaine et al., Chapter 8). This means each respondent in the Uganda 2 study enters twice, and we cluster the standard errors at the individual level. $^{*}$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ }} \\end{flushleft} \\end{sidewaystable}")), fileConn)
close(fileConn)