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

jitter    <- function(x, k = .03) rnorm(length(x), 0, k * sd(x, na.rm = TRUE))
get_range <- function(a, b) c(min(c(a, b), na.rm = TRUE), max(c(a, b), na.rm = TRUE))
get_range_centered <- function(a, b) c(min(-abs(get_range(a, b))),-min(-abs(get_range(a, b))))

madatpq <- subset(madatpq, ctry != "mex")
madatpq <- subset(madatpq,!is.na(Pi) & !is.na(Qi))

PQ_plot <- function(data = ben, main = "Benin") {
  with(data,
       {
         plot(
           P + jitter(P),
           Q + jitter(P),
           main = main,
           cex = .3,
           pch = 16,
           col = c("black", "grey")[factor(N_good)],
           xlim = get_range(P, Q),
           ylim = get_range(P, Q),
           xlab = "P",
           ylab = "Q"
         )
         abline(a = 0, b = 1)
         abline(lm(Q ~ P), col = "red")
       })
}

PQ_graph <- function() {
  par(pty = "s")
  par(mfrow = c(2, 5))
  par(mar = c(4, 4, 1.5, 1.5))
  layout(matrix(c(2, 3, 4, 1, 1, 5, 6, 7, 1, 1), 2, 5, byrow = TRUE))

  plot(
    madatpq$Pi + jitter(madatpq$Pi),
    madatpq$Qi + jitter(madatpq$Pi),
    main = "All",
    col = c("black", "grey")[factor(madatpq$N_good)],
    cex = .4,
    pch = 16,
    xlim = get_range_centered(madatpq$Pi, madatpq$Qi),
    ylim = get_range_centered(madatpq$Pi, madatpq$Qi),
    xlab = "P: Prior",
    ylab = "Q: Information"
  )
  text(-2, 0.3, "Good\nNews")
  text(0.3, -2, "Bad\nNews")
  abline(a = 0, b = 1)
  abline(lm(madatpq$Qi ~ madatpq$Pi), col = "red")

  PQ_plot(data = ben, main = "Benin")
  PQ_plot(data = brz, main = "Brazil")
  PQ_plot(data = bf, main = "Burkina")

  hist(
    mexsur$Q,
    main = "Mexico",
    xlab = "Q",
    col = "black",
    xlim = c(-.5, .2),
    breaks = 14
  )
  hist(
    mexsur$Q[mexsur$N_good == 1],
    col = "grey",
    add = T,
    xlim = c(-.5, .2),
    breaks = 7
  )
  box()

  PQ_plot(data = ug1, main = "Uganda 1")
  PQ_plot(data = ug2, main = "Uganda 2")
}
