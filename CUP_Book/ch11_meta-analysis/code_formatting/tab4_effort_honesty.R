# Generate Table 11.4

p_gb_m5_m24pool_unadj <- joint_ps(m5_m24c1_g, m5_m24c1_b) %>% round(., 3)
p_gb_m6_m24pool_unadj <- joint_ps(m6_m24c1_g, m6_m24c1_b) %>% round(., 3)

# Round RI p-values
ps <- sapply(list(m5_m24c1_g, m5_m24c1_b, m6_m24c1_g, m6_m24c1_b), function(l) round(l$p, 3))

print_t11.4 <- function(type = "latex"){
  stargazer(m5_m24c1_g$estimates, m5_m24c1_b$estimates, m6_m24c1_g$estimates, m6_m24c1_b$estimates,
            df = FALSE, column.sep.width = "1pt",
            label = "effort_honesty_credibility",
            omit = c("Constant"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            # dep.var.caption = "Dependent variable",
            column.labels = rep(c("Good News", "Bad News"), 2),
            covariate.labels = c("Treatment","Credible Source",
                                 "Treatment * Credible Source"),
            add.lines=list(c("Control mean", m5_m24c1_g$mean.control, m5_m24c1_b$mean.control, m6_m24c1_g$mean.control, m6_m24c1_b$mean.control),
                           c("RI $p$-values", ps),
                           c("Covariates", rep(c("No"),4))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Effect of information and source credibility on evaluation of politician effort and honesty [unregistered analysis]",
            omit.table.layout = "n",
            type = type)
}

t11.4 <- capture.output(print_t11.4())

#output to replication and ms folder
fileConn <- file("tables/tab_11.4_effort_honesty_credibility.tex")


n <- length(t11.4)
ri_line <- grep("RI", t11.4)
joint_p <- paste0("Joint RI $p$-value & \\multicolumn{2}{c}{", p_gb_m5_m24pool_unadj , "} & \\multicolumn{2}{c}{", p_gb_m6_m24pool_unadj , "} \\\\")
notes <- "\\begin{flushleft}\\textit{Note:} The table reports the effects of information and the credibility of the information source on voter's perception of how hard-working (MPAP measure M5) and dishonest (MPAP measure M6) the incumbent politician is. We pool Benin, Burkina Faso, Uganda 1, and Uganda 2 in columns (1) and (2), and Benin, Burkina Faso, Mexico, and Uganda 2 in columns (3) and (4). Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"
header <- list("& \\multicolumn{2}{c}{Effort}&\\multicolumn{2}{c}{Dishonesty}\\\\",
               "\\cline{2-5}")
t11.4 <- list(t11.4[1:9], header, t11.4[10:ri_line], joint_p, t11.4[(ri_line+1):(n-1)], notes, t11.4[n])
writeLines(unlist(t11.4), fileConn)
close(fileConn)
