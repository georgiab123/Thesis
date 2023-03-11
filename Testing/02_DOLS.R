# LOADING PACKAGES

library(cointReg)
library(aTSA)
library(lubridate)
library(fBasics)
library(splusTimeSeries)
library(aod)
library(ggplot2)
library(RColorBrewer)

# LOADING IN DATA  #############################################################



# GREGORY HANSEN DOLS ##########################################################

# for all the below we do a DOLS with an intercept and then a DOLS with an intercept
# and a time trend 

# BRENT DOLS GH #################################################################

# the dates are staggered all by one day -- each day represents ht next
# brent 0th period 
# split = 2009-02-05

y <- window(hh_daily_real, start = time(hh_daily)[1] ,  end = time(hh_daily)[4385])   
x <- window(brent_daily_real, start = time(hh_daily)[1], end = time(hh_daily)[4385]) 
n <- length(y)
# no time trend
deter <- cbind(level = rep(1,n))
brent_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC" )
brent_0_gh

# segment 1 
y <- window(hh_daily_real, start = time(hh_daily_log)[4385] ,  end = time(hh_daily_log)[length(hh_daily)])   
x <- window(brent_daily_real, start = time(hh_daily_log)[4385], end = time(hh_daily_log)[length(hh_daily)]) 
n <- length(y)
#no time trend 
deter <- cbind(level = rep(1,n))
brent_1_gh <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
brent_1_gh

# graphing $ THiS IS FOR 
cols <- brewer.pal(9, "Dark2")
cols <- brewer.pal(9, "Spectral")
dev.off()
par(mar=c(5,4,4,5)+.1)
plot(hh_daily_real, ylab = "USD/MMBtu", col = cols[], lwd =2)
par(new = "TRUE")
plot(brent_daily_real, axes = TRUE, yaxt = "n", ylab = "", col = cols[2], lwd = 2)
axis(4)
mtext("Dollars per Barrel", side=4, line=3)
abline(v = time(hh_daily)[4385], lty = 2, col = alpha("black", 0.5), lwd = 2) 
 # Maki samples for WTI model 3 MAKI

par(mar=c(5,4,4,4)+.1)
plot(hh_daily_real, ylab = "USD/MMBtu", col = cols_1[9], lwd =2, xaxs='i',yaxs='i', ylim = c(0,30))
par(new = "TRUE")
plot(wti_daily_real, axes = TRUE, yaxt = "n", ylab = "", col = cols_1[1], lwd = 2, , xaxs='i',yaxs='i', ylim = c(0,200))
axis(4)
mtext("Dollars per Barrel", side=4, line=3)
abline(v = time(nbp_daily_log)[1426], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[2844], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[4324], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[7489], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[8432], lty = 2, col = alpha("black", 0.5), lwd = 2) 
par(xpd=TRUE)
legend("bottomleft", legend = c("Henry Hub", "WTI"),  col = c(cols_1[9], cols_1[1]), lty = c(1,1), lwd = 3, cex=0.9, inset = c(-0.08,-0.2))
par(xpd=FALSE)
rect(xleft = time(nbp_daily_log)[1426], xright = time(nbp_daily_log)[2844], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor(cols_1[9],, alpha = 0.15))
rect(xleft = time(nbp_daily_log)[4324], xright = time(nbp_daily_log)[7489], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor(cols_1[9],, alpha = 0.15))
rect(xleft = time(nbp_daily_log)[8432], xright = time(nbp_daily_log)[9404], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor(cols_1[9],, alpha = 0.15))
text(x = 1999, y = 190, expression(paste(beta, "= 0.13***")), cex = 1.1)
text(x = 2003, y = 190, expression(paste(beta, "=0.15***")), cex = 1.1)
text(x = 2007, y = 190, expression(paste(beta, "=0.03*")), cex = 1.1)
text(x = 2013, y = 190, expression(paste(beta, "=0.02***")), cex = 1.1)
text(x = 2019, y = 190, expression(paste(beta, "=0.01")), cex = 1.1)
text(x = 2021.5, y = 190, expression(paste(beta, "=0.07***")), cex = 1.1)


# WTI DOLS GH #################################################################

#split = 2009-02-05 
y <- window(hh_daily_real, start = time(hh_daily_log)[1],  end = time(hh_daily_log)[4385])   
x <- window(wti_daily_real, start = time(hh_daily_log)[1], end = time(hh_daily_log)[4385]) 
n <- length(y)
# no time trend 
deter <- cbind(level = rep(1,n))
wti_0_gh <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
wti_0_gh
# segment 1
y <- window(hh_daily_real, start = time(hh_daily_log)[4385] ,  end = time(hh_daily_log)[length(hh_daily)])   
x <- window(wti_daily_real, start = time(hh_daily_log)[4385], end = time(hh_daily_log)[length(hh_daily)]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
wti_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4") , info.crit = "BIC"  )
wti_1_gh


# JKM DOLS GH ##################################################################
# 2016-05-24
y <- window(hh_daily_real, start = time(jkm_daily_log)[1] ,  end = time(hh_daily_log)[7049])   
x <- window(jkm_daily_real, start = time(jkm_daily_log)[1], end = time(hh_daily_log)[7049]) 
n <- length(y)
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_0_gh
# segment 1
y <- window(hh_daily_real, start = time(hh_daily_log)[7049] ,  end = time(jkm_daily_log)[length(jkm_daily)])   
x <- window(jkm_daily_real, start = time(hh_daily_log)[7049], end = time(jkm_daily_log)[length(jkm_daily)]) 
n <- length(y)
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_1_gh


# TTF DOLS GH ##################################################################
#2012-10-17
#segment 0 
y <- window(hh_daily_real, start = time(ttf_daily_log)[1] ,  end = time(ttf_daily_log)[1018])   
x <- window(ttf_daily_real, start = time(ttf_daily_log)[1], end = time(ttf_daily_log)[1018]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", info.crit = "BIC")
ttf_0_gh
# segment 1
y <- window(hh_daily_real, start =  time(ttf_daily_log)[1018] ,  end = time(ttf_daily_log)[length(ttf_daily_log)])   
x <- window(ttf_daily_real, start =  time(ttf_daily_log)[1018], end = time(ttf_daily_log)[length(ttf_daily_log)]) 
n <- length(y)
# no trend 
deter <- cbind(level = rep(1,n))
ttf_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_1_gh

# NBP DOLS GH ##################################################################

# 2005-12-01
y <- window(hh_daily_real, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[3224])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[3224]) 
n <- length(y)
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_0_gh
#segment 1
y <- window(hh_daily_real, start =  time(nbp_daily_log)[3224] ,  end = time(nbp_daily_log)[length(nbp_daily_log)])   
x <- window(nbp_daily_real, start =  time(nbp_daily_log)[3224], end = time(nbp_daily_log)[length(nbp_daily_log)]) 
n <- length(y)
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_1_gh


# MAKI TEST 3 BREAKS ###########################################################


# MAKI TEST 5 BREAKS ###########################################################

# BRENT MAKI TEST 5 BREAKS DOLS  ###############################################
y <- window(hh_daily_real, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[1426])   
x <- window(wti_daily_real, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[1426]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
wti_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
wti_0_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[1426] ,  end = time(nbp_daily_log)[2844])   
x <- window(wti_daily_real, start = time(nbp_daily_log)[1426], end = time(nbp_daily_log)[2844]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
wti_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
wti_1_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[2844] ,  end = time(nbp_daily_log)[4324])   
x <- window(wti_daily_real, start = time(nbp_daily_log)[2844], end = time(nbp_daily_log)[4324]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
wti_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
wti_2_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[4324] ,  end = time(nbp_daily_log)[7489])   
x <- window(wti_daily_real, start = time(nbp_daily_log)[4324], end = time(nbp_daily_log)[7489]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
wti_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
wti_3_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[7489] ,  end = time(nbp_daily_log)[8432])   
x <- window(wti_daily_real, start = time(nbp_daily_log)[7489], end = time(nbp_daily_log)[8432]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
wti_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
wti_4_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[8432] ,  end = time(hh_daily_real)[9425])   
x <- window(wti_daily_real, start = time(nbp_daily_log)[8432], end = time(hh_daily_real)[9425]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
wti_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
wti_5_m  

# BRENT


# JKM


# NBP


# TTF 





date_decimal(as.numeric(time(nbp_daily_log)))[3524]

# NBP #
# Breaks


# end  = 2006-09-26
y <- window(hh_daily_real, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[3524])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[3524]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_0_m

# end = 2008-01-27
y <- window(hh_daily_log, start =  time(nbp_daily_log)[3524] ,  end = time(nbp_daily_log)[4011])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[3524], end = time(nbp_daily_log)[4011]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_1_m

# end = 2016-06-03
y <- window(hh_daily_log, start =  time(nbp_daily_log)[4011] ,  end = time(nbp_daily_log)[7059])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[4011], end = time(nbp_daily_log)[7059]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_2_m

# end = 2018-01-01,
y <- window(hh_daily_log, start =  time(nbp_daily_log)[7059] ,  end = time(nbp_daily_log)[7635])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[7059], end = time(nbp_daily_log)[7635]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_3_m

# end = 2021-02-15
y <- window(hh_daily_log, start =  time(nbp_daily_log)[7635] ,  end = time(nbp_daily_log)[8775])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[7635], end = time(nbp_daily_log)[8775]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(nbp_daily_log)[8775] ,  end = time(nbp_daily_log)[length(nbp_daily_log)])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[8775], end = time(nbp_daily_log)[length(nbp_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_5_m


# TTF

date_decimal(as.numeric(time(nbp_daily_log)))[3524]

#TTF 

# end  = 2011-11-24 
y <- window(hh_daily_log, start = time(ttf_daily_log)[1] ,  end = time(ttf_daily_log)[692])   
x <- window(ttf_daily_log, start = time(ttf_daily_log)[1], end = time(ttf_daily_log)[692]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", n.lag = 1, n.lead = 6,  info.crit = "AIC")
ttf_0_m


dev.off()
plot(ttf_0_m_trend, ylim = c(-0.4,0.4))
par(new = TRUE)
plot(ttf_0_m, col = "red", ylim = c(-0.4,0.4))

# end = 2014 - 07 -03 
y <- window(hh_daily_log, start =  time(ttf_daily_log)[692] ,  end = time(ttf_daily_log)[1643])   
x <- window(nbp_daily_log, start =  time(ttf_daily_log)[692], end = time(ttf_daily_log)[1643]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_1_m

# end = 2015-03-01
y <- window(hh_daily_log, start =  time(ttf_daily_log)[1643] ,  end = time(ttf_daily_log)[1884])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[1643], end = time(ttf_daily_log)[1884]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_2_m

# end = 2015-12-13
y <- window(hh_daily_log, start =  time(ttf_daily_log)[1884] ,  end = time(ttf_daily_log)[2171])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[1884], end = time(ttf_daily_log)[2171]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_3_m

# end = 2022-02-01
y <- window(hh_daily_log, start =  time(ttf_daily_log)[2171] ,  end = time(ttf_daily_log)[4411])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[2171], end = time(ttf_daily_log)[4411]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(ttf_daily_log)[4411] ,  end = time(ttf_daily_log)[length(ttf_daily_log)])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[4411], end = time(ttf_daily_log)[length(ttf_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_5_m


#JKM 


date_decimal(as.numeric(time(jkm_daily_log)))[3524]

# end  = 2016-06-02
y <- window(hh_daily_log, start = time(jkm_daily_log)[1] ,  end = time(jkm_daily_log)[672])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[1], end = time(jkm_daily_log)[672]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_0_m

# end = 2020-03-02
y <- window(hh_daily_log, start =  time(jkm_daily_log)[672] ,  end = time(jkm_daily_log)[2040])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[672], end = time(jkm_daily_log)[2040]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_1_m

# end = 2020-10-13
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2040] ,  end = time(jkm_daily_log)[2265])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2040], end = time(jkm_daily_log)[2265]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_2_m

# end = 2021-05-01
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2265] ,  end = time(jkm_daily_log)[2465])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2265], end = time(jkm_daily_log)[2465]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_3_m

# end = 2022-02-06
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2465] ,  end = time(jkm_daily_log)[2746])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2465], end = time(jkm_daily_log)[2746]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2746] ,  end = time(jkm_daily_log)[length(jkm_daily_log)])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2746], end = time(jkm_daily_log)[length(jkm_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_5_m



# BRENT 

date_decimal(as.numeric(time(brent_daily_log)))[2818]

# end  = 2003-02-27
y <- window(hh_daily_log, start = time(brent_daily_log)[1] ,  end = time(brent_daily_log)[2218])   
x <- window(brent_daily_log, start = time(brent_daily_log)[1], end = time(brent_daily_log)[2218]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_0_m

# end = 2004-10-20
y <- window(hh_daily_log, start =  time(brent_daily_log)[2218] ,  end = time(brent_daily_log)[2818])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[2218], end = time(brent_daily_log)[2818]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_1_m

# end = 2007-02-06
y <- window(hh_daily_log, start =  time(brent_daily_log)[2818] ,  end = time(brent_daily_log)[3657])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[2818], end = time(brent_daily_log)[3657]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_2_m

# end = 2018-11-18
y <- window(hh_daily_log, start =  time(brent_daily_log)[3657] ,  end = time(brent_daily_log)[7957])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[3657], end = time(brent_daily_log)[7957]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_3_m

# end = 2021-02-15
y <- window(hh_daily_log, start =  time(brent_daily_log)[7957] ,  end = time(brent_daily_log)[8776])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[7957], end = time(brent_daily_log)[8776]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(brent_daily_log)[8776],  end = time(hh_daily_log)[length(hh_daily_log)])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[8776], end = time(hh_daily_log)[length(hh_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_5_m


# WTI

date_decimal(as.numeric(time(brent_daily_log)))[2818]

# end  = 2003-01-22
y <- window(hh_daily_log, start = time(wti_daily_log)[1] ,  end = time(wti_daily_log)[2182])   
x <- window(wti_daily_log, start = time(wti_daily_log)[1], end = time(wti_daily_log)[2182]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_0_m

# end = 2004-10-20 (SAME)
y <- window(hh_daily_log, start =  time(wti_daily_log)[2182] ,  end = time(brent_daily_log)[2818])   
x <- window(wti_daily_log, start =  time(wti_daily_log)[2182], end = time(brent_daily_log)[2818]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_1_m

# end = 2007-03-16
y <- window(hh_daily_log, start =  time(brent_daily_log)[2818] ,  end = time(wti_daily_log)[3695])   
x <- window(wti_daily_log, start =  time(brent_daily_log)[2818], end = time(wti_daily_log)[3695]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_2_m

# end = 2018-01-15
y <- window(hh_daily_log, start =  time(wti_daily_log)[3695] ,  end = time(wti_daily_log)[7650])   
x <- window(wti_daily_log, start =  time(wti_daily_log)[3695], end = time(wti_daily_log)[7650]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_3_m

# end = 2021-02-15 (SAME)
y <- window(hh_daily_log, start =  time(wti_daily_log)[7650] ,  end = time(brent_daily_log)[8776])   
x <- window(wti_daily_log, start =  time(wti_daily_log)[7650], end = time(brent_daily_log)[8776]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_4_m

# end = end of sample data
y <- matrix(window(hh_daily_log, start =  time(brent_daily_log)[8776],  end = time(hh_daily_log)[length(hh_daily_log)]), ncol = 1)   
x <- matrix(window(wti_daily_log, start =  time(brent_daily_log)[8776], end = time(hh_daily_log)[length(hh_daily_log)]), ncol = 1) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
cointReg:::getLeadLag(x= x, y =y, deter=deter,  max.lag = 10, max.lead = 10, ic = c("BIC"), symmet = TRUE)
wti_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC", n.lead = 1, n.lag =1)
wti_5_m
plot(wti_5_m_trend, ylim=c(-0.7,0.7), col = "blue") # with trend
par(new = TRUE)
plot(wti_5_m, ylim=c(-0.7,0.7), col = "red") # without trend

# WALD TESTS ###############################################################################

wtest <- wald.test(Sigma = wti_1_hj$varmat, b = wti_1_hj$theta.all, Terms = 3, H0 = c(1))

# comparision to manual

lags_x <- tslag(diff(x), -3:3 )
colnames(lags_x)
colnames(lags_x) <- c("lead_3", "lead_2", "lead1", "lag_0", "lag_1", "lag_2", "lag_3")
lags_x <- data.frame(lags_x)


# remove the first observations so aligns with differenced data
dols.fit = lm(y[-1] ~ x[-1] + lags_x$lead_3 +  lags_x$lead_2 + 
                 lags_x$lead1 + lags_x$lag_1 + lags_x$lag_2 + 
                 lags_x$lag_3, na.rm = T) 
summary(dols.fit)
summary(dols.fit,correction="nw", kernel = "ba")

deter <- cbind(level = rep(1,n))

jkm_5 <- cointRegD(x, y, deter, bandwidth = c("nw"),  kernel = "ba", kmax = "k12")

# wald test:
# if less than 0.05, reject the null hypothesis that the coefficient is 
# as specified in H0

wtest <- wald.test(Sigma = jkm_5$varmat, b = jkm_5$theta.all, Terms = 1:2, H0 = c(0,1))
print(wtest, digits = 20)

# 01 - May -2021

# 13 - Oct -2020

# 02 - Marh -2020

# 02-June-2016 

# set up deter matrix

deter <-  cbind(level = rep(1,n), trend = 1:n)

# now running regression
DOLS_result <- cointRegD(x, y, deter, kmax = c("k4"), bandwidth = c("nw"))

result <-coint.test(hh_daily_log, wti_daily_log, d = 0, nlag = 10, output = TRUE)


s# testing regular cointegration: trend <- (1: length(hh_monthly))
model <- lm(hh_daily_log ~ 1 + wti_daily_log + trend)
model
plot(residuals(model), type = "l")
adf(residuals(model))
