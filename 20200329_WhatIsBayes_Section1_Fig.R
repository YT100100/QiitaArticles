setwd('20200329_WhatIsBayes/')

# function to draw plot
DrawPlot_plant <- function(
  x, y, main = '', ...
) {
  par(mar = c(7, 7, 7, 2) + 0.1)
  plot(x, y, ann = FALSE, cex = 3, cex.axis = 2, ...)
  title(main = main, cex.main = 3)
  title(xlab = expression(paste('水の量 ', italic(x), ' (mL)')), 
        line = 5, cex.lab = 3, family = 'serif')
  title(ylab = expression(paste('植物体の大きさ  ', italic(t), ' (g)')),
        line = 4, cex.lab = 3, family = 'serif')
  par(mar = c(5, 4, 4, 2) + 0.1)
}

# data creation (1): water and plant
CreatePlantData <- function() {
  set.seed(1984)
  x <- seq(50, 95, by = 5)
  n <- length(x)
  e <- rnorm(n, mean = 0, sd = 5)
  y <- 50 + x * 0.5 + e
  data.frame(x = x, y = y)
}
dat_plant <- CreatePlantData()

# plot (1): data visualization
png('Plant_fig1.png', width = 960, height = 960)
with(dat_plant, DrawPlot_plant(x, y, xlim = c(45, 100), ylim = c(50, 120), 
                               main = 'Fig 1'))
for (i in 1:nrow(dat_plant)) {

  if (i == 3) {
    is_text_under_point <- FALSE
  } else if (i == 8) {
    is_text_under_point <- TRUE
  } else {
    is_text_under_point <- (i %% 2 == 1)
  }
  y_slide <- ifelse(is_text_under_point, -3, 3)

  text_i <- bquote((italic(x[.(i)] * ', ' * t[.(i)])))
  with(dat_plant, text(x[i], y[i] + y_slide, text_i, 
                       cex = 1.7, family = 'serif', col = 'gray20'))
}
dev.off()

# plot (2): samples of regression lines
coefcand <- data.frame(w0 = c(15, 100, 90, 150, 50),
                       w1 = c(1, -0.3, 0.2, -1, 0.1),
                       col = c('red', 'blue', 'green4', 'gold3', 'purple'),
                       stringsAsFactors = FALSE)
legend_text <- NULL
for (i in 1:nrow(coefcand)) {
  legend_text_i <- as.expression(bquote(italic(w[0]) * '=' * .(coefcand$w0[i]) * ', ' * italic(w[1]) * '=' * .(coefcand$w1[i])))
  legend_text <- c(legend_text, legend_text_i)
}
png('Plant_fig2.png', width = 960, height = 960)
with(dat_plant, DrawPlot_plant(x, y, xlim = c(45, 100), ylim = c(50, 120), 
                               main = 'Fig 2'))
apply(coefcand, 1, function(val) {
  abline(val[1], val[2], col = val[3])
})
legend('topleft', legend = legend_text,
       col = coefcand$col, lty = 1, cex = 2)
dev.off()

# plot (3): samples of regression lines
lmres <- lm(y ~ x, dat_plant)
coefcand <- rbind(coefcand, c(coef(lmres)[1], coef(lmres)[2], 'black'))
coefcand[, 1:2] <- apply(coefcand[, 1:2], 2, as.numeric)
coefcand$sse <- apply(coefcand[, 1:2], 1, function(val) {
  e <- dat_plant$y - (val[1] + val[2] * dat_plant$x)
  round(sum(e ^ 2))
})
png('Plant_fig3.png', width = 960, height = 960)
with(dat_plant, DrawPlot_plant(x, y, xlim = c(45, 100), ylim = c(50, 120), 
                               main = 'Fig 3'))
apply(coefcand, 1, function(val) {
  lty <- if(val[3] == 'black') 2 else 1
  lwd <- if(val[3] == 'black') 2 else 1
  abline(val[1], val[2], col = val[3], lty = lty, lwd = lwd)
})
legend('topleft', paste('SSE =', coefcand$sse), 
       col = coefcand$col, lty = c(rep(1, 5), 2), lwd = c(rep(1, 5), 2), cex = 2)
dev.off()

# plot (4): Gaussian distribution
dunif2 <- function(x) ifelse (abs(x) < 3^(1/3), 0.5/3^(1/3), 0)
dlaplace <- function(x) exp(-abs(x)) / 2
png('Plant_fig4.png', width = 960, height = 960)
par(mar = c(7, 7, 7, 2) + 0.1)
curve(dlaplace, xlim = c(-3, 3), ylim = c(0, 0.5), cex.axis = 2, ann = FALSE,
      lty = 1, col = 'red')
curve(dnorm, add = TRUE,
      lty = 3, col = 'black')
curve(dunif2, add = TRUE, 
      lty = 4, col = 'blue')
title(main = 'Fig 4', cex.main = 3)
title(xlab = expression(italic(e / sigma[e])), line = 5, cex.lab = 3, family = 'serif')
title(ylab = '確率密度', line = 4, cex.lab = 3)
legend('topright', c('正規分布', 'ラプラス分布', '一様分布'), 
       col = c('black', 'red', 'blue'), lty = c(3, 1, 4), cex = 2)
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()

dnorm(0)

# plot (5): an example of parameter set
CalculateProbabilityDensityForGrid <- function(
  w0, w1, s, xlim = c(45, 100), ylim = c(50, 120)
) {
  
  xnew <- seq(xlim[1], xlim[2], length.out = 100)
  ynew <- seq(ylim[1], ylim[2], length.out = 100)
  znew <- sapply(xnew, function(xnew0) {
    yhat <- w0 + w1 * xnew0
    dnorm(ynew, mean = yhat, sd = s)
  })
  list(x = xnew, y = ynew, z = t(znew))
  
}

CalculateProbabilityDensity <- function(
  x, y, w0, w1, s
) {
  
  yhat <- w0 + w1 * x
  prob <- mapply(function(y0, yhat0) {
    dnorm(y0, mean = yhat0, sd = s)
  }, y, yhat)
  prod(prob)
  
}

png('Plant_fig5.png', width = 960, height = 960)
w0 <- 91; w1 <- 0; s <- 5
densdat <- CalculateProbabilityDensityForGrid(w0, w1, s)
with(dat_plant, DrawPlot_plant(x, y, xlim = c(45, 100), ylim = c(50, 120),
                               main = 'Fig 5'))
image(densdat, col = hcl.colors(12, "Blues", alpha = 0.5, rev = TRUE), add = TRUE)
with(dat_plant, points(x, y, cex = 3))
abline(w0, w1, col = 'white', lwd = 2)
dev.off()

for (i in 1:nrow(dat_plant)) {
  print(with(dat_plant, CalculateProbabilityDensity(x[i], y[i], w0, w1, s)))
}
with(dat_plant, CalculateProbabilityDensity(x, y, w0, w1, s))

png('Plant_fig6.png', width = 960, height = 960)
w0 <- 45; w1 <- 0.6; s <- 3
densdat <- CalculateProbabilityDensityForGrid(w0, w1, s)
with(dat_plant, DrawPlot_plant(x, y, xlim = c(45, 100), ylim = c(50, 120),
                               main = 'Fig 6'))
image(densdat, col = hcl.colors(12, "Blues", alpha = 0.5, rev = TRUE), add = TRUE)
with(dat_plant, points(x, y, cex = 3))
abline(w0, w1, col = 'white', lwd = 2)
dev.off()
for (i in 1:nrow(dat_plant)) {
  print(with(dat_plant, CalculateProbabilityDensity(x[i], y[i], w0, w1, s)))
}
with(dat_plant, CalculateProbabilityDensity(x, y, w0, w1, s))

png('Plant_fig7.png', width = 960, height = 960)
lmres <- lm(y ~ x, dat_plant)
w0 <- coef(lmres)[1]; w1 <- coef(lmres)[2]; s <- summary(lmres)$sigma
densdat <- CalculateProbabilityDensityForGrid(w0, w1, s)
with(dat_plant, DrawPlot_plant(x, y, xlim = c(45, 100), ylim = c(50, 120),
                               main = 'Fig 7'))
image(densdat, col = hcl.colors(12, "Blues", alpha = 0.5, rev = TRUE), add = TRUE)
with(dat_plant, points(x, y, cex = 3))
abline(w0, w1, col = 'white', lwd = 2)
dev.off()
for (i in 1:nrow(dat_plant)) {
  print(with(dat_plant, CalculateProbabilityDensity(x[i], y[i], w0, w1, s)))
}
with(dat_plant, CalculateProbabilityDensity(x, y, w0, w1, s))
logLik(lmres)

