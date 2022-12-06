# LOADING PACKAGES ############################################################

library(dplyr)
library(lubridate)
library(anytime)
library(ggfortify)
library(stats)
library(strucchange)
library(forecast)
library(tseries)
library(data.table)
library(lmtest)
library(aTSA)

# BREAKPOINTS #################################################################

bp_hh_d <- breakpoints(hh_prices_01 ~ 1, format.times = TRUE)
ci_hh_d <- confint(bp_hh_d )

#henry hub weekly
bp_hh_w <- breakpoints(ts_hh_weekly ~ 1, format.times = TRUE)
ci_hh_w <- confint(bp_hh_w)

plot(ts_hh_weekly ,  xlab="Time", ylab="USD/ MMBtu")
lines(fitted(bp_hh_w, breaks = 5), col = 4)
lines(ci_hh_w)
legend('topleft', legend=c("Henry Hub: Weekly"),
       col=c("black"), lty=1, cex=0.8)
efp_hh_w <- efp(ts_hh_weekly ~ 1)
plot(efp_hh_w)
sctest(efp_hh_w)

#henry hub monthly
bp_hh_m <- breakpoints(ts_hh_monthly ~ 1, format.times = TRUE)
ci_hh_m <- confint(bp_hh_m)
plot(ts_hh_monthly ,  xlab="Time", ylab="USD/ MMBtu", )
lines(fitted(bp_hh_m, breaks = 3), col = "blue")
lines(ci_hh_m)
legend('topleft', legend=c("Henry Hub: Monthly"),
       col=c("black"), lty=1, cex=1.3)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") 

# wti daily
bp_wti_d <- breakpoints(ts_wti_daily ~ 1, format.times = TRUE)
confint(bp_wti_d)

# wti weekly
bp_wti_w <- breakpoints(ts_wti_weekly ~ 1, format.times = TRUE)
ci_wti_w <- confint(bp_wti_w)
plot(ts_wti_weekly,  xlab="Time", ylab="USD/ MMBtu")
lines(fitted(bp_wti_w, breaks = 5), col = 4)
lines(ci_wti_w)
legend('topleft', legend=c("WTI Crude: Weekly"),
       col=c("black"), lty=1, cex=0.8)

# wti monthly
bp_wti_m <- breakpoints(ts_wti_monthly ~ 1, format.times = TRUE)
ci_wti_m <- confint(bp_wti_m)
plot(ts_wti_monthly ,  xlab="Time", ylab="Dollars per Barrel", )
lines(fitted(bp_wti_m, breaks = 5), col = "blue")
lines(ci_wti_m )
legend('topleft', legend=c("WTI Crude: Monthly"),
       col=c("black"), lty=1, cex=1.5)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") 

# NBP
# daily
bp_nbp_d <- breakpoints(nbp_ts_transform ~ 1, format.times = TRUE)
confint(bp_nbp_d )

# weekly
bp_nbp_w <- breakpoints(NBP_ts ~ 1, format.times = TRUE)
ci_nbp_w <- confint(bp_nbp_w)
plot(NBP_ts ,  xlab="Time", ylab="USD/ MMBtu", )
lines(fitted(bp_nbp_w , breaks = 4), col = 4)
lines(ci_nbp_w[4])
legend('topleft', legend=c("NBP: Weekly"),
       col=c("black"), lty=1, cex=1.5)

# monthly
bp_nbp_m <- breakpoints(ts_month_nbp ~ 1, format.times = TRUE)
ci_nbp_m <- confint(bp_nbp_m)
plot(ts_month_nbp ,  xlab="Time", ylab="USD/ MMBtu", )
lines(fitted(bp_nbp_m , breaks = 2), col = "blue")
lines(ci_nbp_m )
legend('topleft', legend=c("NBP: Monthly"),
       col=c("black"), lty=1, cex=1.5)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") 


# JKM
bp_jkm_d <- breakpoints(JK_ts ~ 1, format.times = TRUE)
confint(bp_jkm_d)
bp_jkm_w <- breakpoints(daily_JKM_ts ~ 1, format.times = TRUE)
confint(bp_jkm_w)

bp_jkm_m <- breakpoints(monthly_ts_jkm ~ 1, format.times = TRUE)
confint(bp_jkm_m)
plot(monthly_ts_jkm ,  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_jkm_m  , breaks = 1), col = "blue", lwd = 0.8)
lines(confint(bp_jkm_m), lwd = 1)
legend('topleft', legend=c("JKM: Monthly"),
       col=c("black"), lty=1, cex=1.5)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") 


# BRENT
bp_b_d <- breakpoints(brent_daily_ts ~ 1, format.times = TRUE)
confint(bp_b_d)

bp_b_w <- breakpoints(brent_week_ts ~ 1, format.times = TRUE)
confint(bp_b_w )

bp_b_m <- breakpoints(brent_month_ts ~ 1, format.times = TRUE)
confint(bp_b_m )
plot(brent_month_ts,  xlab="Time", ylab="Dollars per Barrel", col = "black")
lines(fitted(bp_b_m, breaks = 5), col = "blue", lwd = 0.8)
lines(confint(bp_b_m), lwd = 1)
legend('topleft', legend=c("EU Brent: Monthly"),
       col=c("black"), lty=1, cex=1.5)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") 

# TTF
bp_ttf_d <- breakpoints(ttf_daily ~ 1, format.times = TRUE)
confint(bp_ttf_d)

bp_ttf_w <- breakpoints(ttf_dutch_ts ~ 1, format.times = TRUE)
confint(bp_ttf_w)



bp_ttf_m <- breakpoints(ts_month_ttf ~ 1, format.times = TRUE)
confint(bp_ttf_m)
plot(ts_month_ttf,  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_ttf_m, breaks = 1), col = "blue", lwd = 0.8)
#lines(confint(bp_ttf_w), lwd = 1)
legend('topleft', legend=c("TTF: Monthly"),
       col=c("black"), lty=1, cex=1.5)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") 
abline(v = time(ts_month_ttf)[132], lty = 2)



bp_eu_m <- breakpoints(wb_eu_ng ~ 1, format.times = TRUE)
confint(bp_eu_m)
plot(wb_eu_ng,  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_eu_m, breaks = 3), col = "blue", lwd = 0.8)
legend('topleft', legend=c("EU Natural Gas"),
       col=c("black"), lty=1, cex=1.5)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") 
abline(v = time(wb_eu_ng)[104], lty = 2)
abline(v = time(wb_eu_ng)[216], lty = 2)
abline(v = time(wb_eu_ng)[262], lty = 2)

bp_jap_m <- breakpoints(wb_japan_lng ~ 1, format.times = TRUE)
confint(bp_jap_m)
plot(wb_japan_lng ,  xlab="Time", ylab="USD/ MMBtu", col = "black")
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray")
lines(fitted(bp_jap_m, breaks = 5), col = "blue", lwd = 1.2)
lines(confint(bp_jap_m), lwd = 1)
legend('topleft', legend=c("Japan LNG: Monthly"),
       col=c("black"), lty=1, cex=1.5)



