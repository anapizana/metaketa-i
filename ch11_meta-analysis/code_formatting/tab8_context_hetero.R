# Generate Table 11.8

# Get RI p-values of joint good/bad news distribution

p_gb_m1pool_m11_unadj <- joint_ps(m1_m11c1_g, m1_m11c1_b) %>% round(., 3)
p_gb_m1pool_m26_unadj <- joint_ps(m1_m26c1_g, m1_m26c1_b) %>% round(., 3)
p_gb_m1pool_m27_unadj <- joint_ps(m1_m27c1_g, m1_m27c1_b) %>% round(., 3)

# Round RI p-values
ps <- sapply(list(m1_m11c1_g, m1_m11c1_b, m1_m26c1_g, m1_m26c1_b,
                  m1_m27c1_g, m1_m27c1_b), function(l) round(l$p, 3))

print_t11.8 <- function(type = "latex"){
  stargazer(m1_m11c1_g$estimates, m1_m11c1_b$estimates,
            m1_m26c1_g$estimates, m1_m26c1_b$estimates, 
            m1_m27c1_g$estimates, m1_m27c1_b$estimates,
            label = "context_hetero",
            df = FALSE, column.sep.width = "1pt",
            omit = c("Constant"),
            omit.stat = c("adj.rsq", "ser"),
            no.space = T,
            header = F, table.placement = "!htbp",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Incumbent vote choice",
            column.labels = rep(c("Good news", "Bad news"), 3),
            add.lines=list(c("Control means", m1_m11c1_g$mean.control, m1_m11c1_b$mean.control,
                             m1_m26c1_b$mean.control, m1_m26c1_g$mean.control,
                             m1_m27c1_g$mean.control, m1_m27c1_b$mean.control),
                           c("RI $p$-values", ps),
                           c("Covariates", rep(c("No"),6))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            title = "Effect of information and context heterogenity on incumbent vote choice",
            omit.table.layout = "n",
            covariate.labels = c("Treatment", "Certainty",
                                 "Treatment * Certainty",
                                 "Secret ballot","Treatment * Secret ballot",
                                 "Free, fair election",
                                 "Treatment * Free, fair election"),
            type = type)
}

t11.8 <- capture.output(print_t11.8())

ri_line <- grep("RI", t11.8)
joint_p <- paste0("Joint RI $p$-value & \\multicolumn{2}{c}{", p_gb_m1pool_m11_unadj , "} & \\multicolumn{2}{c}{",
                  p_gb_m1pool_m26_unadj, "} & \\multicolumn{2}{c}{", p_gb_m1pool_m27_unadj, "}\\\\")

notes <- "\\begin{flushleft}\\textit{Note:} The table reports results of whether the treatment had different effects depending on voters' certainty about their priors (MPAP measure M11), and their perceptions about the secrecy of their ballot (MPAP measure M26) and how free and fair the election was (MPAP measure M27). Pooled results exclude non-contested seats and include vote choice for LCV councilors as well as chairs in the Uganda 2 study. Regressions include randomization block fixed effects; standard errors are clustered at the level of treatment assignment. $^{*}$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"

#output to replication and ms folder
fileConn <- file("tables/tab_11.8_context_hetero.tex")
writeLines(unlist(list(t11.8[1:10], "\\cline{2-7}", t11.8[11:ri_line], joint_p, t11.8[(ri_line+1):(length(t11.8)-1)], notes, t11.8[length(t11.8)])), fileConn)
close(fileConn)
