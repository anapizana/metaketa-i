# Generate Table 11.3

# Get RI p-values of joint good/bad news distribution
p_gb_m5pool_unadj <- joint_ps(m5gc1, m5bc1) %>% round(., 3)
p_gb_m6pool_unadj <- joint_ps(m6gc1, m6bc1) %>% round(., 3)

# Round RI p-values
ps <- sapply(list(m5gc1, m5bc1, m6gc1, m6bc1), function(l) round(l$p, 3))

print_t11.3 <- function(type="latex"){
  stargazer(m5gc1$estimates, m5bc1$estimates,
            m6gc1$estimates, m6bc1$estimates,
            label = "effort_honesty",
            df = FALSE, column.sep.width = "1pt",
            omit = c("Constant", "n", "m"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = NULL,
            column.labels = rep(c("Good News", "Bad News"),2),
            covariate.labels = c("Treatment effect"),
            add.lines=list(c("Control mean", m5gc1$mean.control, m5bc1$mean.control, m6gc1$mean.control, m6bc1$mean.control),
                           c("RI $p$-value", ps),
                           c("Covariates", rep(c("No"),4))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Effect of information on perception of importance of politician effort and honesty",
            omit.table.layout = "n",
            type = type)
}

t11.3 <- capture.output(print_t11.3())

#output to replication and ms folder
fileConn <- file("tables/tab_11.3_effort_honesty.tex")

ri_line <- grep("RI", t11.3)

n <- length(t11.3)
notes <- c("\\begin{flushleft}\\textit{Note:} The table reports the effect of the treatment on voters' perception of how hard-working (MPAP measure M5) and dishonest (MPAP measure M6) the incumbent politician is. We pool Benin, Burkina Faso, Uganda 1, and Uganda 2 in columns (1) and (3), and Benin, Burkina Faso, Mexico, and Uganda 2 in columns (2) and (4). MPAP measures M5 (effort) and M6 (dishonesty). Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment.\\end{flushleft}", "\\begin{flushleft} $^{*}$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$\\end{flushleft}")
joint_p <- paste0("Joint RI $p$-value & \\multicolumn{2}{c}{", p_gb_m5pool_unadj , "} & \\multicolumn{2}{c}{", p_gb_m6pool_unadj , "} \\\\")
t11.3 <- list(t11.3[1:ri_line], joint_p, t11.3[(ri_line+1):(n-1)], notes, t11.3[n])

writeLines(
  unlist(lapply(t11.3, function(x){
    sub("\\multicolumn{4}{c}{\\textit{Dependent variable:}}",
        "\\multicolumn{2}{c}{Effort}&\\multicolumn{2}{c}{Dishonesty}", fixed = T, x)})), fileConn)
close(fileConn)
