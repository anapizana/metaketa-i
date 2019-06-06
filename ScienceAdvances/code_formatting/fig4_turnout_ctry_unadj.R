# Generate Figure 11.4

# 2b - Main effects on turnout by country
turn_ctry_r1 <- summary(t4c1$estimates)$coefficients[1,] # bad news, unadjusted
turn_ctry_r2 <- summary(t4c2$estimates)$coefficients[1,] # bad news, ben
turn_ctry_r3 <- summary(t4c3$estimates)$coefficients[1,] # bad news, brz
turn_ctry_r4 <- summary(t4c4$estimates)$coefficients[1,] # bad news, bf
turn_ctry_r5 <- summary(t4c5$estimates)$coefficients[1,] # bad news, mex
turn_ctry_r6 <- summary(t4c6$estimates)$coefficients[1,] # bad news, ug1
turn_ctry_r7 <- summary(t4c7$estimates)$coefficients[1,] # bad news, ug2

turn_ctry_r8  <- summary(t3c1$estimates)$coefficients[1,] # good news, unadjusted
turn_ctry_r9  <- summary(t3c2$estimates)$coefficients[1,] # good news, ben
turn_ctry_r10 <- summary(t3c3$estimates)$coefficients[1,] # good news, brz
turn_ctry_r11 <- summary(t3c4$estimates)$coefficients[1,] # good news, bf
turn_ctry_r12 <- summary(t3c5$estimates)$coefficients[1,] # good news, mex
turn_ctry_r13 <- summary(t3c6$estimates)$coefficients[1,] # good news, ug1
turn_ctry_r14 <- summary(t3c7$estimates)$coefficients[1,] # good news, ug2

turn_ctry_tab <- rbind.data.frame(turn_ctry_r1, turn_ctry_r2, turn_ctry_r3, turn_ctry_r4, turn_ctry_r5, turn_ctry_r6, turn_ctry_r7, turn_ctry_r8, turn_ctry_r9, turn_ctry_r10, turn_ctry_r11, turn_ctry_r12, turn_ctry_r13, turn_ctry_r14)
colnames(turn_ctry_tab) <- c("estimate", "std.error", "statistic", "p.value")
turn_ctry_tab$term <- rep(c("Meta", "Benin", "Brazil", "Burkina Faso",
                            "Mexico", "Uganda 1", "Uganda 2"), 2)
turn_ctry_tab$model <- c(rep("Bad news",7), rep("Good news",7))

turn_ctry_tab_g <- turn_ctry_tab[turn_ctry_tab$model == "Good news",]
turn_ctry_tab_g <- turn_ctry_tab_g[rev(1:nrow(turn_ctry_tab_g)),]
turn_ctry_tab_b <- turn_ctry_tab[turn_ctry_tab$model == "Bad news",]
turn_ctry_tab_b <- turn_ctry_tab_b[rev(1:nrow(turn_ctry_tab_b)),]

#RI p-values
tab_g_p <- unlist(lapply(list(t3c1, t3c2, t3c3, t3c4, t3c5, t3c6, t3c7), function(i) i$p)) %>% rev %>% round(., 2) %>% parse(text = paste0("italic(P) == ", .))
tab_b_p <- unlist(lapply(list(t4c1, t4c2, t4c3, t4c4, t4c5, t4c6, t4c7), function(i) i$p)) %>% rev %>% round(., 2) %>% parse(text = paste0("italic(P) == ", .))

fig11.4 <- function(){
mar.default <- c(5,4,4,2) + 0.1
par(mfrow = c(1,2),mar = mar.default + c(0, 2, 0, 0))
plot(x = turn_ctry_tab_g$estimate, y = 1:nrow(turn_ctry_tab_g), xlim = c(-.18, .18),
     xlab = "Effect Sizes (95% confidence)", pch = 19, axes = FALSE, ylim = c(.5,7),
     ylab = "", main = "Treatment effect of good news on voter turnout")
text(x = .00, y = 1:nrow(turn_ctry_tab_g) -.2, labels = tab_g_p, pos = 4, cex = .8)

axis(1)
axis(2, at = 1:nrow(turn_ctry_tab_g), labels = rev(c("Overall", "Benin", "Brazil", "Burkina Faso", "Mexico", "Uganda 1", "Uganda 2")), las = 1)
segments(turn_ctry_tab_g$estimate-1.96*turn_ctry_tab_g$std.error, 1:nrow(turn_ctry_tab_g),
         turn_ctry_tab_g$estimate+1.96*turn_ctry_tab_g$std.error, 1:nrow(turn_ctry_tab_g))
abline(v=0, col = "black")
box()
plot(x = turn_ctry_tab_b$estimate, y = 1:nrow(turn_ctry_tab_b), xlim = c(-.18, .18),
     xlab = "Effect Sizes (95% confidence)", pch = 19, axes = FALSE, ylim = c(.5,7),
     ylab = "", main = "Treatment effect of bad news on voter turnout")
text(x = .00, y = 1:nrow(turn_ctry_tab_b) -.2, labels = tab_b_p, pos = 4, cex = .8)
axis(1)
axis(2, at = 1:nrow(turn_ctry_tab_b), labels = rev(c("Overall", "Benin", "Brazil", "Burkina Faso", "Mexico", "Uganda 1", "Uganda 2")), las = 1)
segments(turn_ctry_tab_b$estimate-1.96*turn_ctry_tab_b$std.error, 1:nrow(turn_ctry_tab_b),
         turn_ctry_tab_b$estimate+1.96*turn_ctry_tab_b$std.error, 1:nrow(turn_ctry_tab_b))
abline(v=0, col = "black")
box()
}
