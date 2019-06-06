# Gen Table 11.1

# Get RI p-values of joint good/bad news distribution
p_gb_m1pool <- joint_ps(t1ncovc2, t2ncovc2) %>% round(., 3)
p_gb_m3pool <- joint_ps(t3ncovc2, t4ncovc2) %>% round(., 3)

# Round RI p-values
ps <- sapply(list(t1ncovc2, t2ncovc2, t3ncovc2, t4ncovc2, t_m1_o, t_m3_o), function(l) round(l$p, 3))

createt11.1 <- function(type = "latex"){
  stargazer(t1ncovc2$estimates, t2ncovc2$estimates,
            t3ncovc2$estimates, t4ncovc2$estimates,
            t_m1_o$estimates, t_m3_o$estimates,
            title = "Effect of Information, Conditional on Distance between Information and Priors, on Vote Choice and Turnout",
            label = "main_results",
            df = FALSE, column.sep.width = "1pt",
            omit = c("Constant", "m"),
            omit.stat = c("adj.rsq", "ser"),
            header = F, table.placement = "!htbp",
            dep.var.labels="",
            column.labels = c(rep(c("Good News", "Bad News"), 2), "Overall", "Overall"),
            covariate.labels = c("Treatment", "N$_{ij}$","Treatment * N$_{ij}$"),
            add.lines=list(c("Control mean", t1ncovc2$mean.control, t2ncovc2$mean.control,
                             t3ncovc2$mean.control, t4ncovc2$mean.control,
                             t_m1_o$mean.control, t_m3_o$mean.control),
                           c("RI $p$-value", ps),
                           c("Covariates", rep(c("Yes"),6))),
            star.cutoffs = c(0.05, 0.01, 0.001),
            omit.table.layout = "n",
            type = type)
}

t11.1 <- capture.output(createt11.1())

#output to replication and ms folder
fileConn <- file("tables/tab_11.1_main_effects.tex")

ri_line <- grep("RI", t11.1)
n <- length(t11.1)
joint_p <- paste0("Joint RI $p$-value & \\multicolumn{2}{c}{", p_gb_m1pool , "} & \\multicolumn{2}{c}{", p_gb_m3pool , "} \\\\")
notes <- "\\begin{flushleft}\\textit{Note:}  Columns 1-4 estimate equations (\\ref{metaeq.main1a}) and (\\ref{metaeq.main1b}), while columns 5-6 estimate equation (\\ref{metaeq.main3}).  ``Vote choice'' indicates support for the incumbent candidate or party. Standard errors are clustered at the level of treatment assignment. Pooled results exclude non-contested seats and include vote choice for LCV councilors as well as chairs in the Uganda 2 study (see Buntaine et al., Chapter 8). This means each respondent in the Uganda 2 study enters twice, and we cluster the standard errors at the individual level. We include randomization block fixed effects and a full set of covariate-treatment interactions. Control mean is the weighted and unadjusted average in the control group. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"
t11.1 <- list(t11.1[1:9], t11.1[11:ri_line], joint_p, t11.1[(ri_line+1):(n-1)], notes, t11.1[n])

writeLines(
  unlist(lapply(t11.1, function(x){
    l <- sub("\\multicolumn{6}{c}{\\textit{Dependent variable:}}",
        "\\multicolumn{2}{c}{Vote Choice}&\\multicolumn{2}{c}{Turnout}& Vote Choice & Turnout", fixed = T, x)
    sub("Overall & Overall",
        "\\multicolumn{2}{c}{Overall}", fixed = T, l)})), fileConn)
close(fileConn)
