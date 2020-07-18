library(glmnet)
library(BGLR)


#### Set working directory ####

SetWorkingDirectory <- function() {
  today <- gsub('-', '', Sys.Date())
  output_directory <- paste0('Section2_', today)
  output_path <- file.path('20200329_WhatIsBayes', output_directory)
  if (!output_directory %in% list.files('20200329_WhatIsBayes')) {
    dir.create(output_path)
  }
  print(output_path)
  setwd(output_path)
}
SetWorkingDirectory()



#### Create data ####

# true function
TrueFunction <- function(x) {
  w <- c(50, 1.3, 0.5, 1, rep(0, 7))
  x <- c(1, x)
  sum(w * x)
}

# data generation
CreateScoreData <- function(n) {
  q <- 10
  
  # generate predictor variable
  set.seed(1984)
  X <- matrix(round(runif(n * q, 60, 90)), ncol = q, nrow = n)
  colnames(X) <- paste0('Subject', 1:q)
  rownames(X) <- paste0('Student', 1:n)
  
  # generate responce variable
  e <- rnorm(n, mean = 0, sd = 3)
  y <- round(apply(X, 1, TrueFunction) + e)
  y <- ifelse(y > 300, 300, y)
  dat <- data.frame(y, X)
  list(Xmat = X, y = y, dat = dat)
  
}
datlist <- CreateScoreData(n = 12)

# function to calculate range of plot
PlotLim <- function(Xmat, y) {
  xrange <- range(Xmat)
  xlim <- c(weighted.mean(xrange, c(1.1, -0.1)),
            weighted.mean(xrange, c(-0.1, 1.1)))
  xrange <- range(y)
  ylim <- c(weighted.mean(xrange, c(1.1, -0.1)),
            weighted.mean(xrange, c(-0.1, 1.1)))
  list(xlim = xlim, ylim = ylim)
  
}

# function to draw scatter plot
DrawScatterPlot <- function(x, y, i, ...) {
  par(mar = c(9, 4, 0, 2) + 0.1)
  plot(x,
       y,
       cex = 1,
       cex.axis = 1,
       ann = FALSE,
       ...)
  title(
    xlab = bquote(italic(x)[.(i)]),
    line = 4,
    cex.lab = 2.5,
    family = 'serif'
  )
  
}

# function to draw true line
DrawRegressionLine <- function(Xmat, coefval, i, ...) {
  Xmat <- cbind(1, Xmat)
  Xmean <- apply(Xmat, 2, mean)
  yval <- sum((Xmean * coefval)[-(i + 1)])
  abline(yval, coefval[i + 1], ...)
  
}



#### Data visualization ####

png('fig1_Visualization.png',
    width = 1200,
    height = 540)
par_default <- par(no.readonly = TRUE)
par(mfrow = c(2, 5), oma = c(0, 0, 5, 0))
with(datlist, {
  q <- ncol(Xmat)
  lims <- PlotLim(Xmat, y)
  for (i in 1:q) {
    DrawScatterPlot(Xmat[, i],
                    y,
                    i = i,
                    xlim = lims$xlim,
                    ylim = lims$ylim)
    DrawRegressionLine(Xmat,
                       coefval = c(50, 1.3, 0.5, 1, rep(0, 7)),
                       i,
                       lty = 2)
    
  }
  
})
mtext(
  'Figure 1',
  line = 2,
  outer = TRUE,
  font = 2,
  cex = 2
)
par(par_default)
dev.off()



#### estimation with ML ####

lmres <- lm(y ~ ., datlist$dat)
print(round(coef(lmres), 2))
print(summary(lmres)$sigma)

png('fig2_MLestimation.png', width = 1200, height = 540)
par_default <- par(no.readonly = TRUE)
par(mfrow = c(2, 5), oma = c(0, 0, 5, 0))
with(datlist, {
  q <- ncol(Xmat)
  lims <- PlotLim(Xmat, y)
  for (i in 1:q) {
    DrawScatterPlot(Xmat[, i],
                    y,
                    i = i,
                    xlim = lims$xlim,
                    ylim = lims$ylim)
    DrawRegressionLine(Xmat,
                       coefval = c(50, 1.3, 0.5, 1, rep(0, 7)),
                       i,
                       lty = 2)
    DrawRegressionLine(Xmat, coefval = coef(lmres),
                       i, col = 'red')
    
  }
  
})
mtext(
  'Figure 2',
  line = 2,
  outer = TRUE,
  font = 2,
  cex = 2
)
par(par_default)
dev.off()



#### estimation with ridge regression ####

lambda_vec <- exp(seq(-8, 8, length.out = 100))
glmres <- cv.glmnet(datlist$Xmat, datlist$y, alpha = 0,
                    grouped = FALSE)
print(round(coef(glmres)[, 1], 2))
print(glmres$lambda.min)

png('fig3_RidgeRegression.png',
    width = 1200,
    height = 540)
par_default <- par(no.readonly = TRUE)
par(mfrow = c(2, 5), oma = c(0, 0, 5, 0))
with(datlist, {
  q <- ncol(Xmat)
  lims <- PlotLim(Xmat, y)
  for (i in 1:q) {
    DrawScatterPlot(Xmat[, i],
                    y,
                    i = i,
                    xlim = lims$xlim,
                    ylim = lims$ylim)
    DrawRegressionLine(Xmat,
                       coefval = c(50, 1.3, 0.5, 1, rep(0, 7)),
                       i,
                       lty = 2)
    DrawRegressionLine(Xmat, coefval = coef(glmres)[, 1],
                       i, col = 'blue')
    
  }
  
})
mtext(
  'Figure 3',
  line = 2,
  outer = TRUE,
  font = 2,
  cex = 2
)
par(par_default)
dev.off()



#### estimation with Bayesian ridge regression ####

bglrres <-
  BGLR(
    datlist$y,
    ETA = list(subject = list(X = datlist$Xmat, model = 'BRR')),
    nIter = 1200,
    burnIn = 200,
    saveAt = 'BGLR_',
    verbose = FALSE
  )
print(round(bglrres$mu, 2))
print(round(bglrres$ETA$subject$b, 2))
print(round(bglrres$varE, 2))
print(round(bglrres$ETA$subject$varB, 2))

png('fig4_BayesianRidgeRegression.png',
    width = 1200,
    height = 540)
par_default <- par(no.readonly = TRUE)
par(mfrow = c(2, 5), oma = c(0, 0, 5, 0))
with(datlist, {
  q <- ncol(Xmat)
  lims <- PlotLim(Xmat, y)
  for (i in 1:q) {
    DrawScatterPlot(Xmat[, i],
                    y,
                    i = i,
                    xlim = lims$xlim,
                    ylim = lims$ylim)
    DrawRegressionLine(Xmat,
                       coefval = c(50, 1.3, 0.5, 1, rep(0, 7)),
                       i,
                       lty = 2)
    DrawRegressionLine(Xmat,
                       coefval = c(bglrres$mu, bglrres$ETA$subject$b),
                       i,
                       col = 'green4')
    
  }
  
})
mtext(
  'Figure 4',
  line = 2,
  outer = TRUE,
  font = 2,
  cex = 2
)
par(par_default)
dev.off()

png('fig5_BayesianRidgeRegression_CoefDistribution.png',
    width = 1200,
    height = 540)
par_default <- par(no.readonly = TRUE)
par(mfrow = c(2, 5), oma = c(0, 0, 5, 0))
for (i in 1:ncol(datlist$Xmat)) {
  
  par(mar = c(9, 4, 0, 2) + 0.1)
  
  # prepare values
  priodis_sd <- sqrt(with(bglrres$ETA$subject, S0 / (df0 - 2)))
  postdis_m <- bglrres$ETA$subject$b[i]
  postdis_sd <- bglrres$ETA$subject$SD.b[i]
  trueval <- c(50, 1.3, 0.5, 1, rep(0, 7))[i + 1]
  dprio <- function(x) dnorm(x, 0, priodis_sd)
  dpost <- function(x) dnorm(x, postdis_m, postdis_sd)
  
  # prepare for polygon()
  polyx <- seq(-3, 3, 0.01)
  polyy0 <- rep(0, length(polyx))
  polyy1 <- dprio(polyx)
  polyy2 <- dpost(polyx)
  
  # plot
  curve(dpost, xlim = c(-3, 3), ylim = c(0, 2.2), ann = FALSE)
  curve(dprio, add = TRUE)
  polygon(c(polyx, rev(polyx)),
          c(polyy1, polyy0),
          col = rgb(1, 0, 0, 0.3),
          border = NA)
  polygon(c(polyx, rev(polyx)),
          c(polyy2, polyy0),
          col = rgb(0, 0, 1, 0.2),
          border = NA)
  abline(v = trueval)
  title(
    xlab = bquote(italic(w)[.(i)]),
    family = 'serif',
    line = 4,
    cex.lab = 2.5
  )
  
}
mtext(
  'Figure 5',
  line = 2,
  outer = TRUE,
  font = 2,
  cex = 2
)
par(par_default)
dev.off()

