# Generate Figure 11.1 

# 1a Main effecs for incumbent vote choice ----
inc_r1 <- summary(t1c1)$coefficients[1,] # good news, unadjusted
inc_r2 <- summary(t1ncovc1)$coefficients[1,] # good news, N + cov
inc_r3 <- summary(t1uncovc1)$coefficients[1,] # good news, N + cov unweighted

inc_r4 <- summary(t2c1)$coefficients[1,] # bad news, unadjusted
inc_r5 <- summary(t2ncovc1)$coefficients[1,] # bad news, N + cov
inc_r6 <- summary(t2uncovc1)$coefficients[1,] # bad news, N + cov unweighted

inctab <- rbind.data.frame(inc_r1, inc_r2, inc_r3, 
                           inc_r4, inc_r5, inc_r6)
colnames(inctab) <- c("estimate", "std.error", "statistic", "p.value")
# inctab$model <- c(rep(1:(nrow(inctab)/2), 2))
inctab$model <- c(rep(c("Unadjusted weighted", "Covariate adjusted (weighted)",
                        "Covariate adjusted (unweighted)"), 2))
inctab$term <- c(rep("Good news",nrow(inctab)/2),
                 rep("Bad news",nrow(inctab)/2))

dwplot(inctab, 
       dot_args = list(aes(colour = model, shape = model), size = 2)) +
  theme_bw() +
  xlab("Estimated effect of information on support for the incumbent") +
  ylab("") +
  geom_vline(xintercept = 0, colour = "grey5", linetype = 5) +
  ggtitle("Treatment effect of information on incumbent vote choice") +
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        axis.text.y = element_text(colour="black",size=14),
        legend.justification=c(0, 0),
        legend.position=c(0, 0),
        # legend.position="none",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5, legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = c("red","blue","darkgreen")) + 
  coord_cartesian(xlim = c(-0.18, 0.18)) + 
  scale_x_continuous(breaks=seq(-0.2, .2, 0.05))