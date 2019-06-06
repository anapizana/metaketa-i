# Generate Table 11.14

print_t11.15 <- function(type = "latex"){
  stargazer(m1c1_b$estimates, m1c2_b$estimates, m1c3_b$estimates, m1c4_b$estimates,
            type = type,
            label = "pvt_pub_bad",
            df = FALSE,
            omit = c("Constant", "n", "m"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Incumbent vote choice, bad news",
            column.labels = c("Overall", "Benin", "Mexico", "Uganda 1"),
            covariate.labels = c("Private information", "Public information"),
            add.lines=list(c("Control mean", m1c1_b$mean.control, m1c2_b$mean.control, m1c3_b$mean.control, m1c4_b$mean.control),
                           c("F-test $p$-value", f_b),
                           c("Covariates", rep(c("No"),4))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            omit.table.layout = "n",
            title = "Private vs Public Information: Effect of bad news on incumbent vote choice")
}

t11.14 <- capture.output(print_t11.15())

notes <- "\\begin{flushleft}\\textit{Note:} The table reports results of the effect of bad news about the incumbent on vote choice, depending on whether voters received this information in private or public settings. We pool Benin, Mexico, and Uganda 1. Regressions include randomization block fixed effects and standard errors are clustered at the level of treatment assignment. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"

fileConn <- file("tables/tab_11.14_pvt_pub_bad.tex")
writeLines(unlist(list(t11.14[1:27], notes, t11.14[28])), fileConn)
close(fileConn)
