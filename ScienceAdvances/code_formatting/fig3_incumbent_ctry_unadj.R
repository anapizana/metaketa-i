# generate figure 3


# 1b - Main effects on incumbent vote choice by country
inc_ctry_r1  <- summary(t2c1$estimates)$coefficients[1,] # bad news, unadjusted
inc_ctry_r2  <- summary(t2c2$estimates)$coefficients[1,] # bad news, ben
inc_ctry_r3 <- summary(t2c3$estimates)$coefficients[1,] # bad news, brz
inc_ctry_r4 <- summary(t2c4$estimates)$coefficients[1,] # bad news, bf
inc_ctry_r5 <- summary(t2c5$estimates)$coefficients[1,] # bad news, mex
inc_ctry_r6 <- summary(t2c6$estimates)$coefficients[1,] # bad news, ug1
inc_ctry_r7 <- summary(t2c7$estimates)$coefficients[1,] # bad news, ug2

inc_ctry_r8 <- summary(t1c1$estimates)$coefficients[1,] # good news, unadjusted
inc_ctry_r9 <- summary(t1c2$estimates)$coefficients[1,] # good news, ben
inc_ctry_r10 <- summary(t1c3$estimates)$coefficients[1,] # good news, brz
inc_ctry_r11 <- summary(t1c4$estimates)$coefficients[1,] # good news, bf
inc_ctry_r12 <- summary(t1c5$estimates)$coefficients[1,] # good news, mex
inc_ctry_r13 <- summary(t1c6$estimates)$coefficients[1,] # good news, ug1
inc_ctry_r14 <- summary(t1c7$estimates)$coefficients[1,] # good news, ug2

inc_ctry_tab <- rbind.data.frame(inc_ctry_r1, inc_ctry_r2, inc_ctry_r3, inc_ctry_r4,
                                 inc_ctry_r5, inc_ctry_r6, inc_ctry_r7, inc_ctry_r8,
                                 inc_ctry_r9, inc_ctry_r10, inc_ctry_r11, inc_ctry_r12,
                                 inc_ctry_r13, inc_ctry_r14)
colnames(inc_ctry_tab) <- c("estimate", "std.error", "statistic", "p.value")
inc_ctry_tab$term <- rep(c("Overall", "Benin", "Brazil", "Burkina Faso",
                           "Mexico", "Uganda 1", "Uganda 2"), 2)
inc_ctry_tab$model <- c(rep("Bad news",7), rep("Good news",7))


inc_ctry_tab_perc <- apply(inc_ctry_tab[,1:4], 2, function(x){
  x <- x*100}
)
inc_ctry_tab_perc <- cbind.data.frame(inc_ctry_tab_perc, inc_ctry_tab[,5:6])

inc_ctry_tab_g <- inc_ctry_tab[inc_ctry_tab$model == "Good news",]
inc_ctry_tab_g <- inc_ctry_tab_g[rev(1:nrow(inc_ctry_tab_g)),]
inc_ctry_tab_b <- inc_ctry_tab[inc_ctry_tab$model == "Bad news",]
inc_ctry_tab_b <- inc_ctry_tab_b[rev(1:nrow(inc_ctry_tab_b)),]

#p-values
tab_g_p <- unlist(lapply(list(t1c1, t1c2, t1c3, t1c4, t1c5, t1c6, t1c7), function(i) i$p)) %>% rev %>% round(., 2) %>% parse(text = paste0("italic(P) == ", .))
tab_b_p <- unlist(lapply(list(t2c1, t2c2, t2c3, t2c4, t2c5, t2c6, t2c7), function(i) i$p)) %>% rev %>% round(., 2) %>% parse(text = paste0("italic(P) == ", .))

# labs <- sapply(tab_g_p, function(l) expression(paste(italic(p))))

fig11.3 <- function(){
mar.default <- c(5,4,4,2) + 0.1
par(mfrow = c(1,2),mar = mar.default + c(0, 2, 0, 0))
plot(x = inc_ctry_tab_g$estimate, y = 1:nrow(inc_ctry_tab_g), xlim = c(-.18, .18),
     xlab = "Effect Sizes (95% confidence)", pch = 19, axes = FALSE, ylim = c(.5,7),
     ylab = "", main = "Treatment effect of good news on vote choice")
text(x = .00, y = 1:nrow(inc_ctry_tab_g)-.2, labels = unlist(tab_g_p), pos = 4, cex = .8)
axis(1)
axis(2, at = 1:nrow(inc_ctry_tab_g), labels = rev(c("Overall", "Benin", "Brazil", "Burkina Faso", "Mexico", "Uganda 1", "Uganda 2")), las = 1)
segments(inc_ctry_tab_g$estimate-1.96*inc_ctry_tab_g$std.error, 1:nrow(inc_ctry_tab_g),
         inc_ctry_tab_g$estimate+1.96*inc_ctry_tab_g$std.error, 1:nrow(inc_ctry_tab_g))
abline(v=0, col = "black")
box()
plot(x = inc_ctry_tab_b$estimate, y = 1:nrow(inc_ctry_tab_b), xlim = c(-.18, .18),
     xlab = "Effect Sizes (95% confidence)", pch = 19, axes = FALSE, ylim = c(.5,7),
     ylab = "", main = "Treatment effect of bad news on vote choice")
text(x =.00, y = 1:nrow(inc_ctry_tab_b)-.2, labels = unlist(tab_b_p), pos = 4, cex = .8)
axis(1)
axis(2, at = 1:nrow(inc_ctry_tab_b), labels = rev(c("Overall", "Benin", "Brazil", "Burkina Faso", "Mexico", "Uganda 1", "Uganda 2")), las = 1)
segments(inc_ctry_tab_b$estimate-1.96*inc_ctry_tab_b$std.error, 1:nrow(inc_ctry_tab_b),
         inc_ctry_tab_b$estimate+1.96*inc_ctry_tab_b$std.error, 1:nrow(inc_ctry_tab_b))
abline(v=0, col = "black")
box()
}
