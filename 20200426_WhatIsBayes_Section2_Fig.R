library(glmnet)


#### Set working directory ####

SetWorkingDirectory <- function() {
  today <- gsub('-', '', Sys.Date())
  output_directory <- paste0('Section2_', today)
  output_path <- file.path('20200329_WhatIsBayes', output_directory)
  if (! output_directory %in% list.files('20200329_WhatIsBayes')) {
    dir.create(output_path)
  }
  print(output_path)
  setwd(output_path)
}
SetWorkingDirectory()



#### Create data ####

# function to draw plot
DrawPlot <- function(
  x, y, main = '', ...
) {
  par(mar = c(7, 7, 7, 2) + 0.1)
  plot(x, y, ann = FALSE, cex = 3, cex.axis = 2, ...)
  title(main = main, cex.main = 3)
  title(xlab = expression(paste(italic(x))), line = 5, cex.lab = 3, family = 'serif')
  title(ylab = expression(italic(t)), line = 4, cex.lab = 3, family = 'serif')
  par(mar = c(5, 4, 4, 2) + 0.1)
}

# function to draw estimated curve by ML
DrawCurve_lm <- function(lmres, x, ...) {
  plotarea <- par()$usr
  x <- seq(plotarea[1], plotarea[2], length.out = 1000)
  y <- predict(lmres, data.frame(x))
  is_in_area <- (plotarea[3] <= y) & (plotarea[4] >= y)
  y[!is_in_area] <- NA
  lines(x, y, ...)
}

# function to draw estimated curve by ridge regression
DrawCurve_ridge <- function(lmres, ...) {
  plotarea <- par()$usr
  x <- seq(plotarea[1], plotarea[2], length.out = 1000)
  x_poly <- poly(x, 9, raw = TRUE)
  y <- predict(ridgeres, x_poly)[, 1]
  is_in_area <- (plotarea[3] <= y) & (plotarea[4] >= y)
  y[!is_in_area] <- NA
  lines(x, y, ...)
}

# true function
TrueFunction <- function(x) {
  5 * (x - 0) * (x - 0.6) * (x - 0.9)
}

# data creation : cubic function
CreatePlantData <- function() {
  set.seed(1984)
  x <- seq(0, 1, length.out = 10)
  n <- length(x)
  e <- rnorm(n, mean = 0, sd = 0.05)
  y <- TrueFunction(x) + e
  data.frame(x = x, y = y)
}
dat <- CreatePlantData()



#### Plot ####

## Data visualization
png('fig1.png', width = 960, height = 960)
DrawPlot(dat$x, dat$y, main = 'Fig 1', 
         xlim = c(-0.05, 1.05), ylim = c(-0.3, 0.5))
dev.off()

## Estimation with ML
lmres <- lm(y ~ poly(x, 9, raw = TRUE), dat)
plotx <- seq(-0.05, 1.05, length.out = 100)
ploty1 <- predict(lmres, data.frame(x = plotx))
coef_lmres <- signif(coef(lmres), 5)
names(coef_lmres) <- NULL; options(scipen = 2); print(coef_lmres)

## Draw curve estimated with ML
png('fig2.png', width = 960, height = 960)
DrawPlot(dat$x, dat$y, main = 'Fig 2', 
         xlim = c(-0.05, 1.05), ylim = c(-0.3, 0.5))
DrawCurve_lm(lmres)
curve(TrueFunction(x), lty = 2, col = 'gray50', add = TRUE)
dev.off()

## Estimation with ridge regression
datx_poly <- poly(dat$x, 9, raw = TRUE)
ridgeres <- cv.glmnet(datx_poly, dat$y, alpha = 0)
ploty2 <- predict(ridgeres, datx_poly)
coef_ridgeres <- signif(coef(ridgeres)[, 1], 5)
names(coef_ridgeres) <- NULL; options(scipen = 2); print(coef_ridgeres)

## Draw curve estimated with ridge regression
png('fig3.png', width = 960, height = 960)
DrawPlot(dat$x, dat$y, main = 'Fig 3', 
         xlim = c(-0.05, 1.05), ylim = c(-0.3, 0.5))
DrawCurve_lm(lmres, col = 'red', lty = 3)
DrawCurve_ridge(ridgeres)
curve(TrueFunction(x), lty = 2, col = 'gray50', add = TRUE)
dev.off()


