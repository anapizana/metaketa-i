# Generate Table 2b - overall news

print_t11.2_good <- function(type = "latex"){
  stargazer(m30mc1_g, m30mc2_g, m30mc3_g, m30mc6_g,
            type = type,
            df = FALSE, column.sep.width = "1pt",
            label = "mcheck2",
            omit = c("Constant", "n", "m"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Absolute difference between posterior and prior beliefs",
            column.labels = c("Overall", "Benin", "Brazil",
                              "Uganda 2"),
            covariate.labels = c("Treatment"),
            add.lines=list(c("Covariates", rep(c("No"),4))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Manipulation check: Absolute difference between posterior and prior beliefs for pooled good and bad news [unregistered analysis]",
            omit.table.layout = "n")
}

t11.2_good <- capture.output(print_t11.2_good())


#output to replication and ms folder
fileConn <- file("tables/tab_11.2_mcheck2.tex")

n <- length(t11.2_good)
notes <- "\\begin{flushleft}\\textit{Notes:} The table reports differences between beliefs about politician performance after (MPAP measure M30) and prior to treatment (MPAP measure M9). Posterior beliefs are measured using recollection tests at endline specific to the content of each study's intervention. Burkina Faso is excluded because their recollection measure was collected among treated subjects only. Mexico is excluded from results because the study does not contain pre-treatment measures of subjects beliefs. Uganda 1 is not included because the M30 measure is an aggregate measure of subjects' political knowledge and cannot be directly compared with the scale used for measuring priors. We include randomization block fixed effects. Standard errors are clustered at the level of treatment assignment. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.\\end{flushleft}"
t11.2_good <- list(t11.2_good[1:(n-1)], notes, t11.2_good[n])
writeLines(unlist(t11.2_good), fileConn)
close(fileConn)
