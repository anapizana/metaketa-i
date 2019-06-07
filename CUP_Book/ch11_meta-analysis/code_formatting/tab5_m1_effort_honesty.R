# Generate Table 11.5

print_t11.5 <- function(type = "latex"){
  stargazer(m1m5c1g, m1m6c1g, m1m5c1b, m1m6c1b,
            df = FALSE, column.sep.width = "1pt",
            label = "m1_effort_honesty",
            omit = c("Constant"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Incumbent vote choice",
            column.labels = c("Good news", "Bad news"),
            column.separate = c(2, 2),
            covariate.labels = c("Effort", "Dishonesty"),
            add.lines=list(c("Covariates", rep(c("No"),4))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Relationship between evaluation of  politician effort and honesty with vote choice [unregistered analysis]",
            omit.table.layout = "n",
            type = type)
}

t11.5 <- capture.output(print_t11.5())

#output to replication and ms folder
fileConn <- file("tables/tab_11.5_m1_effort_honesty.tex")
n <- length(t11.5)
notes <- "\\begin{flushleft}\\textit{Note:} The table reports the effects of information and the credibility of the information source on voter's perception of how hard-working (MPAP measure M5) and dishonest (MPAP measure M6) the incumbent politician is. We pool Benin, Burkina Faso, Uganda 1, and Uganda 2 in columns (1) and (3), and Benin, Burkina Faso, Mexico, and Uganda 2 in columns (2) and (4). Results exclude non-contested seats and include vote choice for LCV councilors as well as chairs in the Uganda 2 study. Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"
t11.5 <- list(t11.5[1:10], "\\cline{2-5}", t11.5[11:(n-1)], notes, t11.5[n])
writeLines(unlist(t11.5), fileConn)
close(fileConn)
