library(ggplot2)
library(MASS)
library(reshape2)
library(corrplot)
library(plyr)
library(mgcv)
library(sm)
library(vars)
library(lattice)
library(R2HTML)
library(knitr)
library(IRkernel)
options(repr.plot.width = 7)
options(repr.plot.height = 5)



DataOutback <- Suharti



qplot(DataOutback,
      x = DataOutback$RRS,
      y = DataOutback$TampilanS,
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Suharti",
      xlab = "Tampilan Interior Suharti",
      main = "Hubungan Tampilan Interior dengan Retention Rate")



qplot(y = RRS,
      x = TampilanS,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Suharti",
      xlab = "Tampilan Interior Suharti",
      main = "Hubungan Tampilan Interior dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRS,
      x = MenuS,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      method = "lm",
      ylab = "Retention Rate Suharti",
      xlab = "Menu SuhartI",
      main = "Hubungan Tampilan Menu dengan Retention Rate",
      formula = y ~ x)

qplot(y = RRS,
      x = MenuS,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Suharti",
      xlab = "Menu Sharti",
      main = "Hubungan Tampilan Menu dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRS,
      x = PackagingS,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Suharti",
      xlab = "Menu Suharti",
      main = "Hubungan Tampilan Packaging dengan Retention Rate",
      formula = y ~ x)

qplot(y = RRS,
      x = PackagingS,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Suharti",
      xlab = "Packaging Suharti",
      main = "Hubungan Tampilan Packaging Makanan dengan Retention Rate",
      formula = y ~ x)

qplot(y = RRS,
      x = WifiS,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Suharti",
      xlab = "Wifi Suharti",
      main = "Hubungan Wifi dengan Retention Rate pada Suharti",
      formula = y ~ x)

qplot(y = RRS,
      x = WifiS,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Suharti",
      xlab = "Wifi Suharti",
      main = "Hubungan Wifi dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRS,
      x = PembayaranS,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Suharti",
      xlab = "Pembayaran Suharti",
      main = "Hubungan Servis Permbayaran dengan Retention Rate pada Suharti",
      formula = y ~ x)

qplot(y = RRS,
      x = PembayaranS,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Suhari",
      xlab = "Service Pembayaran Suharti",
      main = "Hubungan pembayaran dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRS,
      x = PelayananS,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Suharti",
      xlab = "Pelayanan Suharti",
      main = "Hubungan Pelayanan dengan Retention Rate pada Suharti",
      formula = y ~ x)




qplot(y = RRS,
      x = PelayananS,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Suharti",
      xlab = "Pelayanan Suharti",
      main = "Hubungan Pelayanan dengan Retention Rate",
      formula = y ~ x)




library(leaps)
regsubsets.out <-regsubsets(RRS ~ TampilanS + MenuS + PackagingS + WifiS + PembayaranS + PelayananS,
                            data = DataOutback,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")
regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)

plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
