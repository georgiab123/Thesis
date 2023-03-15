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
par(xpd=FALSE)
abline(v = time(nbp_daily_log)[1426], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[2844], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[4324], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[7489], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[8432], lty = 2, col = alpha("black", 0.5), lwd = 2) 
par(xpd=TRUE)
legend("bottomleft", legend = c("Henry Hub", "WTI"),  col = c(cols_1[9], cols_1[1]), lty = c(1,1), lwd = 3, cex=0.9, inset = c(-0.07,-0.15))
par(xpd=FALSE)
rect(xleft = time(nbp_daily_log)[1426], xright = time(nbp_daily_log)[2844], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
rect(xleft = time(nbp_daily_log)[4324], xright = time(nbp_daily_log)[7489], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
rect(xleft = time(nbp_daily_log)[8432], xright = time(nbp_daily_log)[9404], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
par(xpd=TRUE)
text(x = 1999, y = 208, expression(paste(beta, "= 0.13***")), cex = 1.4)
text(x = 2003, y = 208, expression(paste(beta, "=0.15***")), cex = 1.4)
text(x = 2007, y = 208, expression(paste(beta, "=0.03*")), cex = 1.4)
text(x = 2013, y = 208, expression(paste(beta, "=0.02***")), cex = 1.4)
text(x = 2019, y = 208, expression(paste(beta, "=0.01")), cex = 1.4)
text(x = 2021.5, y = 208, expression(paste(beta, "=0.07***")), cex = 1.4)


# WTI DOLS GH #################################################################

#split = 2009-02-05 
y <- window(hh_daily_real, start = time(hh_daily_log)[1],  end = time(hh_daily_log)[4385])   
x <- window(wti_daily_real, start = time(hh_daily_log)[1], end = time(hh_daily_log)[4385]) 
n <- length(y)
# no time trend 
deter <- cbind(level = rep(1,n))
wti_0_gh <- cointRegD(y, x, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
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

# WTI MAKI TEST 5 BREAKS DOLS  ###############################################
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
legend("bottomleft", legend = c("Henry Hub", "WTI"),  col = c(cols_1[9], cols_1[1]), lty = c(1,1), lwd = 3, cex=0.9, inset = c(-0.07,-0.15))
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

# BRENT MAKI TEST 5 BREAKS DOLS  #################################################


date_decimal(as.numeric(time(nbp_daily_log)))[1426]
y <- window(hh_daily_real, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[1426])   
x <- window(brent_daily_real, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[1426]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
brent_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
brent_0_m  

#06-Feb-2007,
y <- window(hh_daily_real, start = time(nbp_daily_log)[1426] ,  end = time(nbp_daily_log)[3656])   
x <- window(brent_daily_real, start = time(nbp_daily_log)[1426], end = time(nbp_daily_log)[3656]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
brent_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
brent_1_m  

#09-Jan-2010,
y <- window(hh_daily_real, start = time(nbp_daily_log)[3656] ,  end = time(nbp_daily_log)[4723])   
x <- window(brent_daily_real, start = time(nbp_daily_log)[3656], end = time(nbp_daily_log)[4723]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
brent_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
brent_2_m  

#07-Feb-2014
y <- window(hh_daily_real, start = time(nbp_daily_log)[4723] ,  end = time(nbp_daily_log)[6212])   
x <- window(brent_daily_real, start = time(nbp_daily_log)[4723], end = time(nbp_daily_log)[6212]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
brent_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
brent_3_m  

# 14-Mar-2020
y <- window(hh_daily_real, start = time(nbp_daily_log)[6212] ,  end = time(nbp_daily_log)[8437])   
x <- window(brent_daily_real, start = time(nbp_daily_log)[6212], end = time(nbp_daily_log)[8437]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
brent_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
brent_4_m  

# end
y <- window(hh_daily_real, start = time(nbp_daily_log)[8437] ,  end = time(hh_daily_real)[9425])   
x <- window(brent_daily_real, start = time(nbp_daily_log)[8437], end = time(hh_daily_real)[9425]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
brent_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
brent_5_m  



par(mar=c(5,4,4,4)+.1)
plot(hh_daily_real, ylab = "USD/MMBtu", col = cols_1[9], lwd =2, xaxs='i',yaxs='i', ylim = c(0,30))
par(new = "TRUE")
plot(brent_daily_real, axes = TRUE, yaxt = "n", ylab = "", col = cols_1[2], lwd = 2, , xaxs='i',yaxs='i', ylim = c(0,210))
axis(4)
mtext("Dollars per Barrel", side=4, line=3)
par(xpd=FALSE)
abline(v = time(nbp_daily_log)[1426], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[3656], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[4723], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[6212], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[8437], lty = 2, col = alpha("black", 0.5), lwd = 2) 
par(xpd=TRUE)
legend("bottomleft", legend = c("Henry Hub", "Brent"),  col = c(cols_1[9], cols_1[2]), lty = c(1,1), lwd = 3, cex=0.9, inset = c(-0.07,-0.15))
par(xpd=FALSE)
rect(xleft = time(nbp_daily_log)[1426], xright = time(nbp_daily_log)[3656], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.2))
rect(xleft = time(nbp_daily_log)[4723], xright = time(nbp_daily_log)[6212], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.2))
rect(xleft = time(nbp_daily_log)[8437], xright =  time(hh_daily_real)[9425], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.2))
par(xpd=TRUE)
text(x = 1999, y = 220, expression(paste(beta, "= 0.13***")), cex = 1.4)
text(x = 2004.5, y = 220, expression(paste(beta, "=0.097***")), cex = 1.4)
text(x = 2008.7, y = 220, expression(paste(beta, "=0.08***")), cex = 1.4)
text(x = 2012, y = 220, expression(paste(beta, "=-0.03***")), cex = 1.4)
text(x = 2017, y = 220, expression(paste(beta, "=0.03***")), cex = 1.4)
text(x = 2021.7, y = 220, expression(paste(beta, "=0.07***")), cex = 1.4)
# NBP MAKI TEST 5 BREAKS DOLS  #################################################



# 11-Dec-2000, 30-Aug-2005, 07-Mar-2009, 07-Feb-2014, 06-Apr-2020
y <- window(hh_daily_real, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[1409])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[1409]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_0_m  
plot(x, ylim = c(2,20))
par(new = "TRUE")
plot(y, ylim = c(2,20))
par(new = "TRUE")
plot(0.61539 + 1.13550*x, ylim = c(2,20), col = "red")
RMSE <- sqrt(sum((y - (0.61539 + 1.13550*x))^2)/n)
R2 <- 1 - sum((y - (0.61539 + 1.13550*x))^2)/sum((y - mean(y))^2)
AR2 <- 1 - ( ((1-R2)*(n-1)) / (n-1-1) )



y <- window(hh_daily_real, start = time(nbp_daily_log)[1409] ,  end = time(nbp_daily_log)[3131])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[1409], end = time(nbp_daily_log)[3131]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_1_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[3131] ,  end = time(nbp_daily_log)[4415])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[3131], end = time(nbp_daily_log)[4415]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_2_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[4415] ,  end = time(nbp_daily_log)[6212])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[4415], end = time(nbp_daily_log)[6212]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_3_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[6212] ,  end = time(nbp_daily_log)[8460])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[6212], end = time(nbp_daily_log)[8460]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_4_m  

y <- window(hh_daily_real, start = time(nbp_daily_log)[8460] ,  end = time(nbp_daily_real)[9404])   
x <- window(nbp_daily_real, start = time(nbp_daily_log)[8460], end = time(nbp_daily_real)[9404]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_5_m  

col_green <- brewer.pal(9,"YlGn")
plot(hh_daily_real, ylab = "USD/MMBtu", col = cols_1[9], lwd =2, xaxs='i',yaxs='i', ylim = c(0,80))
par(new = "TRUE")
par(xpd=FALSE)
plot(nbp_daily_real, axes = FALSE, yaxt = "n", ylab = "", col = col_green[8], lwd = 2, , xaxs='i',yaxs='i', ylim = c(0,80))
abline(v = time(nbp_daily_log)[1409], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[3131], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[4415], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[6212], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(nbp_daily_log)[8460], lty = 2, col = alpha("black", 0.5), lwd = 2) 
par(xpd=TRUE)
legend("bottomleft", legend = c("Henry Hub", "NBP"),  col = c(cols_1[9], col_green[8]), lty = c(1,1), lwd = 3, cex=0.9, inset = c(-0.07,-0.15))
par(xpd=FALSE)
rect(xleft = time(nbp_daily_log)[1409], xright = time(nbp_daily_log)[3131], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.2))
rect(xleft = time(nbp_daily_log)[4415], xright = time(nbp_daily_log)[6212], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.2))
rect(xleft = time(nbp_daily_log)[8460], xright = time(nbp_daily_log)[9404], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.2))
par(xpd=TRUE)
text(x = 1999, y = 83, expression(paste(beta, "= 1.14***")), cex = 1.4)
text(x = 2003.5, y = 83, expression(paste(beta, "=0.68***")), cex = 1.4)
text(x = 2007.5, y = 83, expression(paste(beta, "=0.36*")), cex = 1.4)
text(x = 2011.8, y = 83, expression(paste(beta, "=-0.11*")), cex = 1.4)
text(x = 2017.5, y = 83, expression(paste(beta, "=0.32***")), cex = 1.4)
text(x = 2021.6, y = 83, expression(paste(beta, "=0.11***")), cex = 1.4)


# TTF MAKI TEST 5 BREAKS DOLS  #################################################

# 17-Sep-2011, 07-Feb-2014, 03-Jun-2016, 08-Mar-2019, 15-Feb-2021 
date_decimal(as.numeric(time(nbp_daily_log)))[1426]
y <- window(hh_daily_real, start = time(ttf_daily)[1] ,  end = time(ttf_daily)[623])   
x <- window(ttf_daily_real, start = time(ttf_daily)[1], end = time(ttf_daily)[623]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_0_m  

y <- window(hh_daily_real, start = time(ttf_daily)[623] ,  end = time(ttf_daily)[1496])   
x <- window(ttf_daily_real, start = time(ttf_daily)[623], end = time(ttf_daily)[1496]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_1_m  

y <- window(hh_daily_real, start = time(ttf_daily)[1496] ,  end = time(ttf_daily)[2342])   
x <- window(ttf_daily_real, start = time(ttf_daily)[1496], end = time(ttf_daily)[2342]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_2_m  

y <- window(hh_daily_real, start = time(ttf_daily)[2342] ,  end = time(ttf_daily)[3350])   
x <- window(ttf_daily_real, start = time(ttf_daily)[2342], end = time(ttf_daily)[3350]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_3_m  

y <- window(hh_daily_real, start = time(ttf_daily)[3350] ,  end = time(ttf_daily)[4059])   
x <- window(ttf_daily_real, start = time(ttf_daily)[3350], end = time(ttf_daily)[4059]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_4_m  

y <- window(hh_daily_real, start = time(ttf_daily)[4059] ,  end = time(ttf_daily)[4691])   
x <- window(ttf_daily_real, start = time(ttf_daily)[4059], end = time(ttf_daily)[4691]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_5_m  

hh_daily_real_window <- window(hh_daily_real, start = time(ttf_daily)[1] ,  end = time(ttf_daily)[4691])   

col_purp <- brewer.pal(9,"PRGn")
plot(hh_daily_real_window, ylab = "USD/MMBtu", col = cols_1[9], lwd =2, xaxs='i',yaxs='i', ylim = c(0,100))
par(new = "TRUE")
plot(ttf_daily_real, axes = FALSE, yaxt = "n", ylab = "", col = col_purp[2], lwd = 2, , xaxs='i',yaxs='i', ylim = c(0,100))
par(xpd=FALSE)
abline(v = time(ttf_daily)[623], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(ttf_daily)[1496], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(ttf_daily)[2342], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(ttf_daily)[3350], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(ttf_daily)[4059], lty = 2, col = alpha("black", 0.5), lwd = 2) 
par(xpd=TRUE)
legend("bottomleft", legend = c("Henry Hub", "TTF"),  col = c(cols_1[9], col_purp[2]), lty = c(1,1), lwd = 3, cex=0.9, inset = c(-0.05,-0.15))
par(xpd=FALSE)
rect(xleft = time(ttf_daily)[623], xright = time(ttf_daily)[1496], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
rect(xleft = time(ttf_daily)[2342], xright = time(ttf_daily)[3350], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
rect(xleft = time(ttf_daily)[4059], xright = time(ttf_daily)[4691], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
par(xpd=TRUE)
text(x = 2010.8, y = 104, expression(paste(beta,   "= -0.11*")), cex = 1.4)
text(x = 2013, y = 104, expression(paste(beta, "= 0.38***")), cex = 1.4)
text(x = 2015.2, y = 104, expression(paste(beta, "= 0.58***")), cex = 1.4)
text(x = 2017.8, y = 104, expression(paste(beta, "= 0.04*")), cex = 1.4)
text(x = 2020.2, y = 104, expression(paste(beta, "= 0.21***")), cex = 1.4)
text(x = 2022, y = 104, expression(paste(beta, "= 0.08***")), cex = 1.4)


#JKM MAKI 3 BREAKS ######################################################################

#04-Jun-2015 & 20-Jan-2018 & 19-Oct-2021 

#date_decimal(as.numeric(time(nbp_daily_log)))[1426]

y <- window(hh_daily_real, start = time(jkm_daily)[1] ,  end = time(jkm_daily)[307])   
x <- window(jkm_daily_real, start = time(jkm_daily)[1], end = time(jkm_daily)[307]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
jkm_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_0_m  

y <- window(hh_daily_real, start = time(jkm_daily)[307] ,  end = time(jkm_daily)[1268])   
x <- window(jkm_daily_real, start = time(jkm_daily)[307], end = time(jkm_daily)[1268]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
jkm_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_1_m  

y <- window(hh_daily_real, start = time(jkm_daily)[1268] ,  end = time(jkm_daily)[2635])   
x <- window(jkm_daily_real, start = time(jkm_daily)[1268], end = time(jkm_daily)[2635]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
jkm_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_2_m  

y <- window(hh_daily_real, start = time(jkm_daily)[2635] ,  end = time(jkm_daily)[3014])   
x <- window(jkm_daily_real, start = time(jkm_daily)[2635], end = time(jkm_daily)[3014]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
jkm_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_3_m  


col_pink <- brewer.pal(9,"PiYG")
hh_daily_real_window <- window(hh_daily_real, start =time(jkm_daily)[1], end = time(jkm_daily)[3014] )
col_purp <- brewer.pal(9,"PRGn")
plot(hh_daily_real_window, ylab = "USD/MMBtu", col = cols_1[9], lwd =2, xaxs='i',yaxs='i', ylim = c(0,80))
par(new = "TRUE")
plot(jkm_daily_real, axes = FALSE, yaxt = "n", ylab = "", col = col_pink[1], lwd = 2, , xaxs='i',yaxs='i', ylim = c(0,80))
par(xpd=FALSE)
abline(v = time(jkm_daily)[307], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(jkm_daily)[1268], lty = 2, col = alpha("black", 0.5), lwd = 2) 
abline(v = time(jkm_daily)[2635], lty = 2, col = alpha("black", 0.5), lwd = 2) 
par(xpd=TRUE)
legend("bottomleft", legend = c("Henry Hub", "JKM"),  col = c(cols_1[9], col_pink[1]), lty = c(1,1), lwd = 3, cex=0.9, inset = c(-0.05,-0.15))
par(xpd=FALSE)
rect(xleft = time(jkm_daily)[307], xright = time(jkm_daily)[1268], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
rect(xleft = time(jkm_daily)[2635], xright = time(jkm_daily)[3014], ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("grey", alpha = 0.15))
par(xpd=TRUE)
text(x = 2015, y = 83, expression(paste(beta,   "= 0.18***")), cex = 1.4)
text(x = 2016.6, y = 83, expression(paste(beta, "= 0.15***")), cex = 1.4)
text(x = 2020, y = 83, expression(paste(beta, "= 0.15***")), cex = 1.4)
text(x = 2022.4, y = 83, expression(paste(beta, "= 0.04***")), cex = 1.4)


# DOLS PRE SPECIFIED NATURAL GAS BENCHMARKS #####################################

# SPLIT AT February 2016, or jkm_daily[550]

# JKM 

y <- window(hh_daily_real, start = time(jkm_daily)[1] ,  end = time(jkm_daily)[550])   
x <- window(jkm_daily_real, start = time(jkm_daily)[1], end = time(jkm_daily)[550]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
jkm_0_2016 <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_0_2016  

y <- window(hh_daily_real, start = time(jkm_daily)[550] ,  end = time(jkm_daily)[3014])   
x <- window(jkm_daily_real, start = time(jkm_daily)[550], end = time(jkm_daily)[3014]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
jkm_1_2016 <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_1_2016 

# NBP 

y <- window(hh_daily_real, start = time(nbp_daily)[1] ,  end = time(jkm_daily_real)[550])   
x <- window(nbp_daily_real, start = time(nbp_daily)[1], end = time(jkm_daily_real)[550]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_0_2016 <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_0_2016  

y <- window(hh_daily_real, start = time(jkm_daily)[550] ,  end = time(nbp_daily_real)[9404])   
x <- window(nbp_daily_real, start = time(jkm_daily)[550], end = time(nbp_daily_real)[9404]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
nbp_1_2016 <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_1_2016  

# TTF 

y <- window(hh_daily_real, start = time(ttf_daily)[1] ,  end = time(jkm_daily)[550])   
x <- window(ttf_daily_real, start = time(ttf_daily)[1], end = time(jkm_daily)[550]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_0_2016 <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_0_2016  

y <- window(hh_daily_real, start = time(jkm_daily)[550] ,  end = time(ttf_daily_real)[4691])   
x <- window(ttf_daily_real, start = time(jkm_daily)[550], end = time(ttf_daily_real)[4691]) 
n <- length(y)
deter <- cbind(level = rep(1,n))
ttf_1_2016 <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_1_2016  




# WALD TESTS ####################################################################

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
