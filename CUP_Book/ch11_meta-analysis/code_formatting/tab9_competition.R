# Generate table 11.9

# Get RI p-values of joint good/bad news distribution
p_gb_m1pool_m25l_unadj <- joint_ps(m1_m25lc1_g, m1_m25lc1_b) %>% round(., 3)
p_gb_m1pool_m25h_unadj <- joint_ps(m1_m25hc1_g, m1_m25hc1_b) %>% round(., 3)

# Round RI p-values
ps <- sapply(list(m1_m25lc1_g, m1_m25lc1_b, m1_m25hc1_g, m1_m25hc1_b), function(l) round(l$p, 3))

print_t11.9 <- function(type = "latex"){
  stargazer(m1_m25lc1_g$estimates, m1_m25lc1_b$estimates, m1_m25hc1_g$estimates, m1_m25hc1_b$estimates,
            df = FALSE, column.sep.width = "1pt",
            label = "competition",
            omit = c("Constant"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Incumbent vote choice",
            column.labels = rep(c("Good news", "Bad news"), 2),
            covariate.labels = c("Treatment"),
            add.lines=list(c("Control mean", m1_m25lc1_g$mean.control, m1_m25lc1_b$mean.control,
                             m1_m25hc1_g$mean.control, m1_m25hc1_b$mean.control),
                           c("RI $p$-values", ps),
                           c("Covariates", rep(c("No"),4))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Effect of information and electoral competition on vote choice",
            omit.table.layout = "n",
            type = type)
}

t11.9 <- capture.output(print_t11.9())

ri_line <- grep("RI", t11.9)
joint_p <- paste0("Joint RI $p$-value & \\multicolumn{2}{c}{", p_gb_m1pool_m25l_unadj , "} & \\multicolumn{2}{c}{", p_gb_m1pool_m25h_unadj , "} \\\\")
notes <- "\\begin{flushleft}\\textit{Note:} The table reports results of whether the treatment had different effects in constituencies with low or high levels of electoral competition (MPAP measure M25). We pool Benin, Brazil, Mexico, and Uganda 1. Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"

#output to replication and ms folder
fileConn <- file("tables/tab_11.9_competition.tex")
t11.9 <- list(t11.9[1:9], "& \\multicolumn{2}{c}{Low competition} &\\multicolumn{2}{c}{High competition} \\\\", "\\cline{2-5}", t11.9[10:24], notes, t11.9[25])
writeLines(unlist(t11.9), fileConn)
close(fileConn)
