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

DataOutback <- Outback




qplot(DataOutback,
      y = DataOutback$RRO,
      x = DataOutback$TampilanO,
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Outback",
      xlab = "Tampilan Interior Outback",
      main = "Hubungan Tampilan Interior dengan Retention Rate")



qplot(y = RRO,
      x = TampilanO,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Outback",
      xlab = "Tampilan Interior Outback",
      main = "Hubungan Tampilan Interior dengan Retention Rate",
      formula = y ~ x)

qplot(y = RRO,
      x = MenuO,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      method = "lm",
      ylab = "Retention Rate Outback",
      xlab = "Menu Outback",
      main = "Hubungan Tampilan Menu dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRO,
      x = MenuO,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Outback",
      xlab = "Menu Outback",
      main = "Hubungan Tampilan Menu dengan Retention Rate",
      formula = y ~ x)

qplot(y = RRO,
      x = PackagingO,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Outback",
      xlab = "Packaging Outback",
      main = "Hubungan Tampilan Packaging dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRO,
      x = PackagingO,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Outback",
      xlab = "Packaging Outback",
      main = "Hubungan Tampilan Packaging Makanan dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRO,
      x = WifiO,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Outback",
      xlab = "Wifi Outback",
      main = "Hubungan Wifi dengan Retention Rate pada Outback",
      formula = y ~ x)

qplot(y = RRO,
      x = WifiO,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Outback",
      xlab = "Wifi Outback",
      main = "Hubungan Wifi dengan Retention Rate",
      formula = y ~ x)



qplot(y = RRO,
      x = PembayaranO,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Outback",
      xlab = "Pembayaran Outback",
      main = "Hubungan Servis Permbayaran dengan Retention Rate pada Outback",
      formula = y ~ x)

qplot(y = RRO,
      x = PembayaranO,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Outback",
      xlab = "Service Pembayaran Outback",
      main = "Hubungan Servis Pembayaran dengan Retention Rate",
      formula = y ~ x)

qplot(y = RRO,
      x = PelayananO,
      data = DataOutback,
      geom = c("point"),
      position = position_jitter(w = 0.1, h = 0.1),
      ylab = "Retention Rate Outback",
      xlab = "Pelayanan Outback",
      main = "Hubungan Pelayanan dengan Retention Rate pada Outback",
      formula = y ~ x)



qplot(y = RRO,
      x = PelayananO,
      data = DataOutback,
      geom = c("point", "smooth"),
      method = "lm",
      ylab = "Retention Rate Outback",
      xlab = "Pelayanan Outback",
      main = "Hubungan Pelayanan dengan Retention Rate",
      formula = y ~ x)

library(leaps)
regsubsets.out <-regsubsets(RRO ~ TampilanO + MenuO + PackagingO + WifiO + PembayaranO + PelayananO,
                            data = DataOutback,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")
regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)

plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
