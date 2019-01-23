# Generate Table 11.10

# Get RI p-values of joint good/bad news distribution
p_gb_m1pool_N_unadj <- joint_ps(t1nc1_g, t1nc1_b) %>% round(., 3)
p_gb_m1pool_m24_unadj <- joint_ps(m1_m24c1_g, m1_m24c1_b) %>% round(., 3)
p_gb_m1pool_m23_unadj <- joint_ps(m1_m23c1_g, m1_m23c1_b) %>% round(., 3)

# Round RI p-values
ps <- sapply(list(t1nc1_g, t1nc1_b, m1_m23c1_g, 
                  m1_m23c1_b, m1_m24c1_g, m1_m24c1_b), function(l) round(l$p, 3))

print_t11.10 <- function(type = "latex"){
  stargazer(t1nc1_g$estimates, t1nc1_b$estimates, m1_m23c1_g$estimates,
            m1_m23c1_b$estimates, m1_m24c1_g$estimates, m1_m24c1_b$estimates,
            df = FALSE, column.sep.width = "1pt",
            label = "intervention_hetero",
            omit = c("Constant"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Incumbent vote choice",
            column.labels = rep(c("Good news", "Bad news"), 3),
            covariate.labels = c("Treatment","N$_{ij}$","Treatment * N$_{ij}$",
                                 "Information salient",
                                 "Treatment * Information salient",
                                 "Credible source",
                                 "Treatment * Credible source"),
            add.lines=list(c("Control mean", t1nc1_g$mean.control, t1nc1_b$mean.control, m1_m23c1_g$mean.control,
                             m1_m23c1_b$mean.control, m1_m24c1_g$mean.control, m1_m24c1_b$mean.control),
                           c("RI $p$-values", ps),
                           c("Covariates", rep(c("No"),6))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Effect of information and intervention-specific heterogenity on vote choice",
            omit.table.layout = "n",
            type = type)
}

t11.10 <- capture.output(print_t11.10())

ri_line <- grep("RI", t11.10)
joint_p <- paste0("Joint RI $p$-value & \\multicolumn{2}{c}{", p_gb_m1pool_N_unadj, "} & \\multicolumn{2}{c}{",
                  p_gb_m1pool_m23_unadj, "} & \\multicolumn{2}{c}{", p_gb_m1pool_m24_unadj, "} \\\\")

notes <- "\\begin{flushleft}\\textit{Note:} The table reports results of the effect of information and (a) the gap between priors and information (MPAP measure $N_{ij}$), (b) salience of information (MPAP measure M23) and (c) credibility of information source on voters' decision to vote for the incumbent. Columns 1, 3, 4 and 6 pool observations from all studies while Columns 2 and 5 pool Benin, Brazil, Uganda 1 and Uganda 2. Results exclude non-contested seats and include vote choice for LCV councilors as well as chairs in the Uganda 2 study. Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"
t11.10[2] <- paste(t11.10[2], "\\footnotesize") 

#output to replication and ms folder
fileConn <- file("tables/tab_11.10_intervention_hetero.tex")
writeLines(unlist(list(t11.10[1:10],"\\cline{2-7}",t11.10[11:ri_line], joint_p, t11.10[(ri_line+1):(length(t11.10)-1)], notes, t11.10[length(t11.10)])), fileConn)
close(fileConn)
