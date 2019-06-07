# Generate Table 2

createt11.2 <- function(type = "latex"){
  stargazer(m30mc1_o, m30mc2_o, m30mc3_o, m30mc4_o, m30mc5_o, m30mc6_o,
            type = type,
            df = FALSE, column.sep.width = "1pt",
            label = "mcheck",
            omit = c("Constant", "n", "m"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Correct Recollection",
            column.labels = c("Overall", "Benin", "Brazil",
                              "Mexico", "Uganda 1", "Uganda 2"),
            covariate.labels = c("Treatment"),
            add.lines=list(c("Covariates", rep(c("No"),6))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Manipulation check: Effect of treatment on correct recollection, pooling good and bad news [unregistered analysis]",
            omit.table.layout = "n")
}

t11.2 <- capture.output(createt11.2())


#output to replication and ms folder
fileConn <- file("tables/tab_11.2_mcheck.tex")

n <- length(t11.2)
notes <- "\\begin{flushleft}\\textit{Notes:} The table reports results on manipulation checks across studies, using recollection or accuracy tests at endline that were specific to the content of each study's interventions (MPAP measure M30). The dependent variable, correct recollection, is dichotomized in each study using the following measures: Benin: whether correctly recalled the relative performance of incumbent in plenary and committee work; Brazil: whether correctly recalled whether municipal account was accepted or rejected; Mexico: identification of content of the flyer; Uganda 1: index consisting of knowledge of MP responsibilities, MP priorities for constituency, and identities of contesting candidates. Individuals with an index equal to or greater than 1.5 on a 0-3 scale were coded as correct recalls; Uganda 2: whether correctly recalled relative financial accountability relative to other districts. We include randomization block fixed effects. Standard errors are clustered at the level of treatment assignment. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.\\end{flushleft}"
t11.2 <- list(t11.2[1:(n-1)], notes, t11.2[n])
writeLines(unlist(t11.2), fileConn)
close(fileConn)

