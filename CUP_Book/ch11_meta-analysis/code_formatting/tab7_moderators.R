# Generate Table 11.7

# Get RI p-values of joint good/bad news distribution
p_gb_m1poolm15 <- joint_ps(m1_m15mc_g1, m1_m15mc_b1) %>% round(., 3)
p_gb_m1poolm19 <- joint_ps(m1_m19bin_g1, m1_m19bin_b1) %>% round(., 3)
p_gb_m1pool_client_unadj <- joint_ps(m1_m22mc_g1, m1_m22mc_b1) %>% round(., 3)

# Round RI p-values
ps <- sapply(list(m1_m15mc_g1, m1_m15mc_b1, m1_m19bin_g1,
                  m1_m19bin_b1, m1_m22mc_g1, m1_m22mc_b1), function(l) round(l$p, 3))

print_t11.7 <- function(type = "latex"){
  stargazer(m1_m15mc_g1$estimates, m1_m15mc_b1$estimates,
            m1_m19bin_g1$estimates, m1_m19bin_b1$estimates,
            m1_m22mc_g1$estimates, m1_m22mc_b1$estimates,
            label = "moderators",
            df = FALSE, column.sep.width = "1pt",
            omit = c("Constant"),
            omit.stat = c("adj.rsq", "ser"),
            no.space = T,
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Incumbent vote choice",
            column.labels = rep(c("Good news", "Bad news"),3),
            add.lines=list(c("Control mean",
                             m1_m15mc_g1$mean.control,
                             m1_m15mc_b1$mean.control,
                             m1_m19bin_g1$mean.control,
                             m1_m19bin_b1$mean.control,
                             m1_m22mc_g1$mean.control,
                             m1_m22mc_b1$mean.control),
                           c("RI $p$-values", ps),
                           c("Covariates", rep(c("No"),6))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Effect of moderators on incumbent vote choice",
            covariate.labels = c("Treatment", "Coethnicity",
                                 "Treatment * Coethnicity",
                                 "Copartisanship","Treatment * Copartisanship",
                                 "Clientelism","Treatment * Clientelism"),
            omit.table.layout = "n",
            type = type)
}

t11.7 <- capture.output(print_t11.7())

#output to replication and ms folder
fileConn <- file("tables/tab_11.7_moderators.tex")
n <- length(t11.7)
ri_line <- grep("RI", t11.7)
joint_p <- paste0("Joint RI $p$-value & \\multicolumn{2}{c}{", p_gb_m1poolm15 ,
                  "} & \\multicolumn{2}{c}{", p_gb_m1poolm19, "} & \\multicolumn{2}{c}{",
                  p_gb_m1pool_client_unadj, "} \\\\")

notes <- "\\begin{flushleft}\\textit{Note:} The table reports results of the treatment on three pre-specified moderators---coethnicity (MPAP measure M15), copartisanship (MPAP measure M19) and indulging in clienetelistic practices (MPAP measure M22)---on incumbent vote choice. The following cases are included in each regression: Co-ethnicity---Benin, Brazil, Uganda 1, Uganda 2; Co-partisanship---Benin, Brazil, Mexico, Uganda 1, Uganda 2; Clientelism---Benin, Burkina Faso, Brazil, Mexico, Uganda 1, Uganda 2. Pooled results exclude non-contested seats and include vote choice for LCV councilors as well as chairs in the Uganda 2 study. Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment. $^{*}$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"
t11.7 <- list(t11.7[1:10], "\\cline{2-7}", t11.7[11:ri_line], joint_p, t11.7[(ri_line+1):(n-1)], notes, t11.7[n])
writeLines(unlist(t11.7), fileConn)
close(fileConn)
