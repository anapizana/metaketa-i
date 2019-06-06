# # Create P - Q graphing function
#
# # Load and append country data
# files <- list.files("data/temp", pattern = "covinter", full.names = TRUE)
# for (f in files) {
#   load(f)
# }
#
# # Metanalysis data
# madatpq <- rbind.fill(ben, brz, bf, mexsur, ug1, ug2)
#
# #standarize P and Q so that both P has mean 0 , sd 1 and Q normalized with same scaling
# madatpq <- madatpq %>%
#   group_by(ctry) %>%
#   mutate(Pi = (P - mean(P, na.rm = TRUE))/sd(P, na.rm = TRUE),
#          Qi = (Q - mean(Q, na.rm = TRUE))/sd(Q, na.rm = TRUE),
#          Nchecki = Q - m9,
#          Nchecki = (Nchecki - mean(Nchecki, na.rm = TRUE))/sd(Nchecki, na.rm = TRUE)) %>%
#   ungroup()

jitter    <- function(x, k = .02) rnorm(length(x), 0, k * sd(x, na.rm = TRUE))
get_range <- function(a, b) c(min(c(a, b), na.rm = TRUE), max(c(a, b), na.rm = TRUE))
get_range_centered <- function(a, b) c(min(-abs(get_range(a, b))),-min(-abs(get_range(a, b))))

madatpq <- subset(madatpq,!is.na(Pi) & !is.na(Qi))

pq_all_rugged <- function(){
  ggplot(madatpq, aes(Pi + jitter(Pi), Qi + jitter(Pi))) + 
    geom_point(aes(color = as.factor(N_good)), size = .4) +
    coord_cartesian(xlim = get_range_centered(madatpq$Pi, madatpq$Qi),
                    ylim = get_range_centered(madatpq$Pi, madatpq$Qi)) +
    geom_smooth(method = "lm", color = "red", se = FALSE, size = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    geom_segment(aes(y    = madatpq$Qi,
                     yend = madatpq$Qi, 
                     x    = -3, 
                     xend = -3 + .15), size = .3, alpha = .2) +
    geom_segment(aes(y    = -3, 
                     yend = -3 + .15, 
                     x    = madatpq$Pi,
                     xend = madatpq$Pi), size = .3, alpha = .2) +
    scale_color_grey() +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = "none") +
    labs(x =  "P: Prior", y = "Q: Information")
}

pq_study <- function(study = "ben", main = "Benin"){
  dat <- madatpq[madatpq$ctry == study,]
  ggplot(dat, aes(P + jitter(P), Q + jitter(P))) +
    geom_point(aes(color = as.factor(N_good)), size = .4) +
    geom_smooth(method = "lm", color = "red", se = FALSE, size = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    scale_color_grey() +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = "none") +
    labs(x =  "P", y = "Q", title = main) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
}

pq_mex <- function(){
  ggplot(madatpq[madatpq$ctry == "mex",], aes(Q)) + geom_histogram(aes(fill = as.factor(N_good))) +
    scale_fill_grey() +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    labs(x =  "Q", title = "Mexico", y = "Frequency")
  
}

pq_graph_new <- function(){
  gridExtra::grid.arrange(
    pq_study(),
    pq_study(study = "bf", main = "Burkina Faso"),
    pq_study(study = "brz", main = "Brazil"), #+ labs(y = "Relative Frequency") + theme(axis.title.y =element_text(size=14)),
    pq_study(study = "ug1", main = "Uganda 1"), #+ scale_y_continuous(name = "Density", sec.axis = sec_axis(~./100/3, name = "Relative Frequency")) + theme(axis.title.y = element_text(size=14)),
    pq_study(study = "ug2", main = "Uganda 2"), 
    pq_mex(), #+ labs(x = "Control Posteriors") , 
    pq_all_rugged(),
    nrow = 3, ncol = 5,
    layout_matrix = rbind(c(1,2,7,7,7),
                          c(3,4,7,7,7),
                          c(5,6,7,7,7))
  )
}

