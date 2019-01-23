# generate table 6

# Round RI p-values
ps <- sapply(list(m8bc1, m8bc2, m8bc3), function(l) round(l$p, 3))

print_t11.6 <- function(type = "latex"){
  stargazer(m8bc1$estimates, m8bc2$estimates, m8bc3$estimates,
            df = FALSE, column.sep.width = "1pt",
            label = "backlash",
            omit = c("Constant", "n", "m"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Politician response / backlash",
            column.labels = c("Overall", "Benin", "Mexico"),
            covariate.labels = c("Treatment effect"),
            add.lines=list(c("Control mean", m8bc1$mean.control, m8bc2$mean.control, m8bc3$mean.control),
                           c("RI $p$-value", ps),
                           c("Covariates", rep(c("No"),3))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Effect of bad news on politician backlash",
            omit.table.layout = "n",
            type = type)
}

t11.6 <- capture.output(print_t11.6)

#output to replication and ms folder
fileConn <- file("tables/tab_11.6_backlash.tex")

n <- length(t11.6)
notes <- "\\begin{flushleft}\\textit{Note:} The table reports on whether the treatment led to the incumbent party or candidate campaigning on dimensions of the dissemminated information (MPAP measure M8). Backlash was measured for studies with clustered assignment. Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"
t11.6 <- list(t11.6[1:(n-1)], notes, t11.6[n])
writeLines(unlist(t11.6), fileConn)
close(fileConn)
