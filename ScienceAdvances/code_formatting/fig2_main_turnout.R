# Generate Figure 11.2

turn_r1 <- summary(t3c1)$coefficients[1,] # good news, unadjusted
turn_r2 <- summary(t3ncovc1)$coefficients[1,] # good news, N + cov
turn_r3 <- summary(t3uncovc1)$coefficients[1,] # good news, N + cov unweighted

turn_r4 <- summary(t4c1)$coefficients[1,] # bad news, unadjusted
turn_r5 <- summary(t4ncovc1)$coefficients[1,] # bad news, N + cov
turn_r6 <- summary(t4uncovc1)$coefficients[1,] # bad news, N + cov unweighted

turntab <- rbind.data.frame(turn_r1, turn_r2, turn_r3, 
                            turn_r4, turn_r5, turn_r6) 

colnames(turntab) <- c("estimate", "std.error", "statistic", "p.value")
turntab$model <- c(rep(c("Unadjusted weighted", "Covariate adjusted (weighted)",
                         "Covariate adjusted (unweighted)"), 2))
turntab$term <- c(rep("Good news",nrow(turntab)/2),
                  rep("Bad news",nrow(turntab)/2))

dwplot(turntab, 
       dot_args = list(aes(colour = model, shape = model), size = 2)) +
  theme_bw() +
  xlab("Estimated effect of information on voter turnout") +
  ylab("") +
  geom_vline(xintercept = 0, colour = "grey5", linetype = 5) +
  ggtitle("Treatment effect of information on voter turnout") +
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