# LOADING PACKAGES ############################################################

library(strucchange)
library(mbreaks)

# DAILY BREAKPOINTS #############################################################

# the below takes ~ 30 minuts to run
# use the below in GAUSS for NBP, Henry hub, WTI, and Brent
bp_hh_d <- breakpoints(hh_daily_log ~ 1, format.times = TRUE, h = 0.1, breaks = 5)
# breakpoints at levels are just:
# 1159 2168 4367 6532 8483 
# 2000(95) 2003(9) 2009(18) 2014(358) 2020(119) 
# breakpoints at first differences are :
bp_hh_d <- breakpoints(diff(hh_daily_log) ~ 1, format.times = TRUE, h = 0.1, breaks = 5)

#breakpoints including a trend term are:
trend <- 1:length(hh_daily_log)
bp_hh_d <- breakpoints(diff(log(hh_daily)) ~ 1 + trend, format.times = TRUE, h = 0.1, breaks = 5)



bp_jkm_d <- breakpoints(log(jkm_daily) ~ 1, format.times = TRUE, h = 0.1, breaks = 5 ) 
bp_nbp_d <- breakpoints(log(nbp_daily) ~ 1, format.times = TRUE, h = 0.1, breaks = 4 )
bp_ttf_d <- breakpoints(log(ttf_daily) ~ 1, format.times = TRUE, h = 0.1, breaks = 5 )
# the below give spurios results
bp_wti_d <- breakpoints(log(wti_daily) ~ 1, format.times = TRUE, h = 0.1, breaks = 5  )
bp_brent_d <- breakpoints(log(brent_daily) ~ 1, format.times = TRUE, h = 0.1, breaks = 5 )

# GAUSS CARRION-I-SILVESTRE BREAKPOINTS ####################################

# BRENT diff, model 3

ts.plot(brent_daily_log, wti_daily_log)
abline(v = time(brent_daily_log)[1747], col = "red")
abline(v = time(brent_daily_log)[2873], col = "red")
abline(v = time(brent_daily_log)[4324], col = "red")
abline(v = time(brent_daily_log)[6781], col = "red")
abline(v = time(brent_daily_log)[8479], col = "red")

# WTI diff, model 3

plot(wti_daily_log)
abline(v = time(wti_daily_log)[982])
abline(v = time(wti_daily_log)[2242])
abline(v = time(wti_daily_log)[3309])
abline(v = time(wti_daily_log)[6576])
abline(v = time(wti_daily_log)[8445])
982.00000 
2242.0000 
3309.0000 
6576.0000 
8445.0000 points

# HH diff, model 3
dev.off()
plot(hh_daily_log)
abline(v = time(hh_daily_log)[1752.0000 ], col = "blue", lty = 2)
abline(v = time(hh_daily_log)[3131.0000 ], col = "blue", lty =2)
abline(v = time(hh_daily_log)[4625.0000 ], col = "blue", lty = 2)
abline(v = time(hh_daily_log)[6212.0000 ], col = "blue", lty =2 )
abline(v = time(hh_daily_log)[7649.0000], col = "blue", lty = 2)
abline(v = time(hh_daily_log)[1159.0000 ], col = "red")
abline(v = time(hh_daily_log)[2168.0000 ], col = "red")
abline(v = time(hh_daily_log)[4367.0000 ], col = "red")
abline(v = time(hh_daily_log)[6532.0000 ], col = "red")
abline(v = time(hh_daily_log)[8483.0000], col = "red")

# TTF diff, model 3
765.00000 
1242.0000 
2185.0000 
2976.0000 
3790.0000 points

# compare with our calculated breakpoints
dev.off()
plot(ttf_daily_log)
abline(v = time(ttf_daily_log)[765], col = "blue", lty = 2)
abline(v = time(ttf_daily_log)[1242.0000 ], col = "blue", lty =2)
abline(v = time(ttf_daily_log)[2185.0000 ], col = "blue", lty = 2)
abline(v = time(ttf_daily_log)[2976.0000 ], col = "blue", lty =2 )
abline(v = time(ttf_daily_log)[3790.0000], col = "blue", lty = 2)
abline(v = time(ttf_daily_log)[469.0000 ], col = "red")
abline(v = time(ttf_daily_log)[1929.0000 ], col = "red")
abline(v = time(ttf_daily_log)[2861.0000 ], col = "red")
abline(v = time(ttf_daily_log)[3355.0000 ], col = "red")
abline(v = time(ttf_daily_log)[4187.0000], col = "red")


# TTF levels, model 3 (slightly different break points)
765.00000  # same  
1430.0000  # diff of ~ 200 
2444.0000  # diff of ~ 250 
2977.0000  # same 
3790.0000  # same
# compare TTF breakpoints between CI diff and CI levels 
plot(ttf_daily_log)
abline(v = time(ttf_daily_log)[765], col = "blue", lty = 2)
abline(v = time(ttf_daily_log)[1242.0000 ], col = "blue", lty =2)
abline(v = time(ttf_daily_log)[2185.0000 ], col = "blue", lty = 2)
abline(v = time(ttf_daily_log)[2976.0000 ], col = "blue", lty =2 )
abline(v = time(ttf_daily_log)[3790.0000], col = "blue", lty = 2)
abline(v = time(ttf_daily_log)[765 ], col = "red")
abline(v = time(ttf_daily_log)[1430.0000 ], col = "red")
abline(v = time(ttf_daily_log)[2444.0000 ], col = "red")
abline(v = time(ttf_daily_log)[2977.0000 ], col = "red")
abline(v = time(ttf_daily_log)[3790.0000], col = "red")



# BREAKPOINTS WEEKLY AND MONTHLY ###############################################
# below is very quick, < 1 minute
bp_hh_w <- breakpoints(log(hh_weekly) ~ 1, format.times = TRUE, h = 0.1, breaks = 6 )
bp_jkm_w <- breakpoints(log(jkm_weekly) ~ 1, format.times = TRUE, h = 0.1, breaks = 6)
bp_nbp_w <- breakpoints(log(nbp_weekly) ~ 1, format.times = TRUE, h = 0.1, breaks  = 5)
bp_ttf_w <- breakpoints(log(ttf_weekly) ~ 1, format.times = TRUE, h = 0.1, breaks = 6 )
bp_wti_w <- breakpoints(log(wti_weekly) ~ 1, format.times = TRUE, h = 0.1, breaks = 6)
bp_brent_w <- breakpoints(log(brent_weekly) ~ 1, format.times = TRUE, h = 0.1, breaks = 6)


bp_hh_m <- breakpoints(log(hh_monthly) ~ 1, format.times = TRUE, h = 0.1, breaks = 4 )
bp_jkm_m <- breakpoints(log(jkm_monthly) ~ 1, format.times = TRUE, h = 0.1, breaks = 6 )
bp_nbp_m <- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, h = 0.1, breaks = 5)
bp_ttf_m <- breakpoints(log(ttf_monthly) ~ 1, format.times = TRUE, h = 0.1, breaks = 4)
bp_wti_m <- breakpoints(log(wti_monthly) ~ 1, format.times = TRUE, h = 0.1 , breaks = 6)
bp_brent_m <- breakpoints(log(brent_monthly) ~ 1, format.times = TRUE, h = 0.1, breaks = 4 )
bp_eu_m <- breakpoints(log(wb_eu_ng) ~ 1, format.times = TRUE,h = 0.1, breaks = 5)
bp_jap_m <- breakpoints(log(wb_japan_lng) ~ 1, format.times = TRUE, h = 0.1, breaks = 6 )

# SUP F TESTS #################################################################

# conduct sequential supf tests
results_1 <- c()
results_2 <- c()
results_3 <- c()
results_names <- log_ts_names[c(2,3,5,6,8,9,11,12,14,15,17,18)]
for(i in log_ts_names[c(2,3,5,6,8,9,11,12,14,15,17,18)]){
  print(i)
  df <- data.frame(Y=as.matrix(get(i)), date=time(get(i)))
  ts_results_1 <- doseqtests("Y", data = df, prewhit = 0, eps1=0.10, m = 8)
  ts_results_2 <- dosequa("Y", data = df, prewhit = 0, eps1=0.10, signif = 1,  m =8)
  ts_results_3 <- dotest("Y", data = df, prewhit = 0, eps1=0.10, m = 8)
  results_1 <- append(results_1, ts_results_1)
  results_2 <- append(results_2, ts_results_2)
  results_3 <- append(results_3, ts_results_3)
}

# now for daily data, which takes much longer
results_1_d <- c()
results_2_d <- c()
results_3_d <- c()
results_names_d <- log_ts_names[c(1,4,7,10,13,16)]
for(i in results_names_d){
  print(i)
  df <- data.frame(Y=as.matrix(get(i)), date=time(get(i)))
  ts_results_1 <- doseqtests("Y", data = df, prewhit = 0, eps1=0.10, m = 8)
  ts_results_2 <- dosequa("Y", data = df, prewhit = 0, eps1=0.10, signif = 1,  m =8)
  ts_results_3 <- dotest("Y", data = df, prewhit = 0, eps1=0.10, m = 8)
  results_1_d <- append(results_1, ts_results_1)
  results_2_d <- append(results_2, ts_results_2)
  results_3_d <- append(results_3, ts_results_3)
}

df_hh <- data.frame(Y=as.matrix(hh_daily_log), date=time(hh_daily_log))
hh_daily_log_seqtests <- doseqtests("Y", data = df_hh, prewhit = 0, eps1=0.10, m = 8)

df_nbp <- data.frame(Y=as.matrix(nbp_daily_log), date=time(nbp_daily_log))
nbp_daily_log_seqtests <- doseqtests("Y", data = df_nbp, prewhit = 0, eps1=0.10, m = 8)

df_ttf <- data.frame(Y=as.matrix(ttf_daily_log), date=time(ttf_daily_log))
ttf_daily_log_seqtests <- doseqtests("Y", data = df_ttf, prewhit = 0, eps1=0.10, m = 8)

df_jkm <- data.frame(Y=as.matrix(jkm_daily_log), date=time(jkm_daily_log))
jkm_daily_log_seqtests <- doseqtests("Y", data = df_jkm, prewhit = 0, eps1=0.10, m = 8)

df_brent <- data.frame(Y=as.matrix(brent_daily_log), date=time(brent_daily_log))
brent_daily_log_seqtests <- doseqtests("Y", data = df_brent, prewhit = 0, eps1=0.10, m = 8)


df_wti <- data.frame(Y=as.matrix(wti_daily_log), date=time(wti_daily_log))
wti_daily_log_seqtests <- doseqtests("Y", data = df_wti, prewhit = 0, eps1=0.10, m = 8)

# to run
df_eu <- data.frame(Y=as.matrix(log(wb_eu_ng)), date=time(log(wb_eu_ng)))
eu_daily_log_seqtests <- doseqtests("Y", data = df_eu, prewhit = 0, eps1=0.10, m = 8)

df_jap <- data.frame(Y=as.matrix(log(wb_japan_lng)), date=time(log(wb_japan_lng)))
jap_daily_log_seqtests <- doseqtests("Y", data = df_jap, prewhit = 0, eps1=0.10, m = 8)

df_hh_monthly <- data.frame(Y=as.matrix(brent_monthly_log), date=time(brent_monthly_log))
doorder("Y", data = df_hh_monthly, ic ="LWZ")

#UD Max tests for all daily data
tezt_1 <- dotest("Y", data = df_hh, prewhit = 0, eps1=0.10, m = 8)
tezt_2 <- dotest("Y", data = df_nbp, prewhit = 0, eps1=0.10, m = 8)
tezt_3 <- dotest("Y", data = df_ttf, prewhit = 0, eps1=0.10, m = 8)
tezt_4 <- dotest("Y", data = df_jkm, prewhit = 0, eps1=0.10, m = 8)
tezt_5 <- dotest("Y", data = df_brent, prewhit = 0, eps1=0.10, m = 8)
tezt_6 <- dotest("Y", data = df_wti, prewhit = 0, eps1=0.10, m = 8)
# below are monthly results for european and japanese LNG
dotest("Y", data = df_eu, prewhit = 0, eps1=0.10, m = 8)
dotest("Y", data = df_jap, prewhit = 0, eps1=0.10, m = 8)


# TESTING BREAKPOINTS VALIDITY  ###################################################

# lets test out different breakpoints for henry hub monthly
acf(log(hh_daily), lag.max=1000)
plot(diff(log(hh_monthly)))

# allowing the segment size to change
bp_hh_m_1_test <- breakpoints(log(hh_monthly) ~ 1, h = 24, format.times = TRUE)

# allow for more breaks
plot(log(hh_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_hh_m_1_test), col = "blue", lwd = 0.8)
legend('topleft', legend=c("log(HH monthly)"),
       col=c("black"), lty=1, cex=1)

plot(log(hh_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_hh_m_1), col = "blue", lwd = 0.8)
lines(confint(bp_hh_m_1))
legend('topleft', legend=c("log(HH monthly)"),
       col=c("black"), lty=1, cex=1)


bp_hh_m_2 <- breakpoints(log(hh_monthly) ~ 1, h = 0.1, format.times = TRUE)
bp_hh_m_3 <- breakpoints(log(hh_monthly) ~ 1, h = 24, format.times = TRUE, breaks = 3)


bp_hh_m_4 <- breakpoints(log(hh_monthly) ~ 1, h = 24, format.times = TRUE, breaks = 4)
bp_hh_m_5 <- breakpoints(log(hh_monthly) ~ 1, h = 24, format.times = TRUE, breaks = 5)
# 2000(3) 2002(12) 2009(1) 2014(12) 2020(11) with h - 24
# 2000(11) 2004(9) 2008(12) 2014(12)  
bp_hh_m_6 <- breakpoints(log(hh_monthly) ~ 1, h = 24, format.times = TRUE, breaks = 6)

ocus.hh_m_1 <- efp(log(hh_monthly) ~ breakfactor(bp_hh_m_1), type = "OLS-CUSUM")
ocus.hh_m_2 <- efp(log(hh_monthly) ~ breakfactor(bp_hh_m_1), type = "OLS-CUSUM")
ocus.hh_m_3 <- efp(log(hh_monthly) ~ breakfactor(bp_hh_m_3), type = "OLS-CUSUM")
ocus.hh_m_4 <- efp(log(hh_monthly) ~ breakfactor(bp_hh_m_4), type = "OLS-CUSUM")
ocus.hh_m_5 <- efp(log(hh_monthly) ~ breakfactor(bp_hh_m_5), type = "OLS-CUSUM")
ocus.hh_m_6 <- efp(log(hh_monthly) ~ breakfactor(bp_hh_m_6), type = "OLS-CUSUM")

# if it crosses the boundary then there is a structural change at that point
# each successive model takes into account previous breakpoints
par( mfrow= c(3,2), mai = c(0.3, 0.3, 0.3, 0.3))
plot(ocus.hh_m_1)
plot(ocus.hh_m_2)
plot(ocus.hh_m_3)
plot(ocus.hh_m_4)
plot(ocus.hh_m_5)
plot(ocus.hh_m_6)

# fstest
re.seat <- efp(log(hh_monthly) ~ 1, type = "RE")
plot(re.seat)
fs.hh_m_1 <- Fstats(log(hh_monthly) ~ 1)
dev.off()
plot(fs.hh_m_1, main = "supF test")
# testing

fs.seat <- Fstats(log(hh_monthly) ~ 1, from = 0.1)
plot(fs.seat)
re.seat <- efp(log(hh_monthly) ~ 1,  type = "RE")
plot(re.seat)
breakpoints_test <- breakpoints(log(hh_monthly) ~ 1, h = 0.1, format.times = TRUE, breaks = 5)
plot(breakpoints_test)
bp2 <- breakpoints(breakpoints_test , breaks = 5, format.times = TRUE) 

plot(log(hh_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(), col = "blue", lwd = 0.8)

lines(confint(bp_nbp_m_5))
legend('topleft', legend=c("log(NBP monthly)"),
       col=c("black"), lty=1, cex=1)


# now lets test out the sctests after allowing for different breaks

sctest(Fstats(log(hh_monthly) ~ breakfactor(bp_hh_m_1)), type = "supF")
sctest(Fstats(log(hh_monthly) ~ breakfactor(bp_hh_m_2)), type = "supF")
sctest(Fstats(log(hh_monthly) ~ breakfactor(bp_hh_m_3)), type = "supF")
sctest(Fstats(log(hh_monthly) ~ breakfactor(bp_hh_m_4)), type = "supF")
sctest(Fstats(log(hh_monthly) ~ breakfactor(bp_hh_m_5)), type = "supF")
sctest(Fstats(log(hh_monthly) ~ breakfactor(bp_hh_m_1_test)), type = "supF")

sctest(ocus.hh_m_1)

# test for NBP

bp_nbp_m_1 <- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, breaks = 1, h = 0.1 )
bp_nbp_m_2<- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, breaks = 2, h = 0.1)
bp_nbp_m_3 <- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, breaks = 3, h = 0.1)
bp_nbp_m_4 <- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, breaks = 4, h = 0.1)
bp_nbp_m_5 <- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, breaks = 5, h =0.1)
bp_nbp_m_6 <- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, breaks = 6, h =0.1)
bp_nbp_m_A <- breakpoints(log(nbp_monthly) ~ 1, format.times = TRUE, h =0.1)

ocus.nbp_m_1 <- efp(log(nbp_monthly) ~ breakfactor(bp_nbp_m_1), type = "OLS-CUSUM")
ocus.nbp_m_2 <- efp(log(nbp_monthly) ~ breakfactor(bp_nbp_m_1), type = "OLS-CUSUM")
ocus.nbp_m_3 <- efp(log(nbp_monthly) ~ breakfactor(bp_nbp_m_3), type = "OLS-CUSUM")
ocus.nbp_m_4 <- efp(log(nbp_monthly) ~ breakfactor(bp_nbp_m_4), type = "OLS-CUSUM")
ocus.nbp_m_5 <- efp(log(nbp_monthly) ~ breakfactor(bp_nbp_m_5), type = "OLS-CUSUM")
ocus.nbp_m_6 <- efp(log(nbp_monthly) ~ breakfactor(bp_nbp_m_6), type = "OLS-CUSUM")
ocus.nbp_m_A <- efp(log(nbp_monthly) ~ breakfactor(bp_nbp_m_A), type = "OLS-CUSUM")


dev.off()
par( mfrow= c(3,2), mai = c(0.3, 0.3, 0.3, 0.3))
plot(ocus.nbp_m_1)
plot(ocus.nbp_m_2)
plot(ocus.nbp_m_3)
plot(ocus.nbp_m_4)
plot(ocus.nbp_m_5) # this is still crossing boundary? 
plot(ocus.nbp_m_6)
plot(ocus.nbp_m_A)

sctest(Fstats(log(nbp_monthly) ~ breakfactor(bp_nbp_m_1)), type = "supF") # does reject
sctest(Fstats(log(nbp_monthly) ~ breakfactor(bp_nbp_m_2)), type = "supF") # does not reject
sctest(Fstats(log(nbp_monthly) ~ breakfactor(bp_nbp_m_3)), type = "supF") # does not reject
sctest(Fstats(log(nbp_monthly) ~ breakfactor(bp_nbp_m_4)), type = "supF") # does not reject
sctest(Fstats(log(nbp_monthly) ~ breakfactor(bp_nbp_m_5)), type = "supF") # does not reject

plot(log(nbp_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_nbp_m_5 ), col = "blue", lwd = 0.8)
lines(confint(bp_nbp_m_5))
legend('topleft', legend=c("log(NBP monthly)"),
       col=c("black"), lty=1, cex=1)


#SIG TESTS ####################################################################

fs.hh_d <- Fstats(hh_daily ~ 1)
fs.jkm_d <- Fstats(jkm_daily ~ 1)
fs.nbp_d <- Fstats(nbp_daily ~ 1)
fs.ttf_d <- Fstats(ttf_daily ~ 1)
fs.wti_d <- Fstats(wti_daily ~ 1)
fs.brent_d <- Fstats(brent_daily ~ 1)

sctest(fs.hh_d, type="expF")
sctest(fs.jkm_d, type="expF")
sctest(fs.nbp_d, type="expF")
sctest(fs.ttf_d, type="expF")
sctest(fs.wti_d, type="expF")
sctest(fs.brent_d, type="expF")

# weekly

fs.hh_w <- Fstats(hh_weekly ~ 1)
fs.jkm_d <- Fstats(hh_daily ~ 1)
fs.nbp_d <- Fstats(hh_daily ~ 1)
fs.ttf_d <- Fstats(hh_daily ~ 1)
fs.wti_d <- Fstats(hh_daily ~ 1)
fs.brent_d <- Fstats(hh_daily ~ 1)

sctest(fs.hh_w,type ="expF")
sctest(fs.jkm_d, type="expF")
sctest(fs.nbp_d, type="expF")
sctest(fs.ttf_d, type="expF")
sctest(fs.wti_d, type="expF")
sctest(fs.brent_d, type="expF")

#monthly
fs.hh_m <- Fstats(log(hh_monthly) ~ 1, from = 0.1)
sctest(fs.hh_m, type = "supF")
fs.jkm_d <- Fstats(hh_daily ~ 1)
fs.nbp_d <- Fstats(hh_daily ~ 1)
fs.ttf_d <- Fstats(hh_daily ~ 1)
fs.wti_d <- Fstats(hh_daily ~ 1)
fs.brent_d <- Fstats(hh_daily ~ 1)

#BIC TESTING ###################################################
# daily
dev.off()
par( mfrow= c(3,2), mai = c(0.3, 0.3, 0.3, 0.3))
plot(bp_hh_d, main="log(HH daily)", ylab="BIC")
plot(bp_jkm_d,  main="log(JKM daily)") 
plot(bp_nbp_d, main="log(NBP daily)")
plot(bp_ttf_d, main="log(TTF daily)")
plot(bp_wti_d, main="log(WTI daily)")
plot(bp_brent_d, main="log(Brent daily)")



# weekly
plot(bp_hh_w, main="log(HH weekly)")
plot(bp_jkm_w,  main="log(JKM weekly)") 
plot(bp_nbp_w, main="log(NBP weekly)")
plot(bp_ttf_w, main="log(TTF weekly)")
plot(bp_wti_w, main="log(WTI weekly)")
plot(bp_brent_w, main="log(Brent weekly)")

# monthly
par( mfrow= c(4,2), mai = c(0.3, 0.3, 0.3, 0.3))
plot(bp_hh_m, main="log(HH monthly)")
plot(bp_jkm_m,  main="log(JKM monthly)") 
plot(bp_nbp_m, main="log(NBP monthly)")
plot(bp_ttf_m, main="log(TTF monthly)")
plot(bp_wti_m, main="log(WTI monthly)")
plot(bp_brent_m, main="log(Brent monthly)")
plot(bp_eu_m, main="log(WB EU monthly)")
plot(bp_jap_m, main="log(WB Jap LNG monthly)")



# PLOTING BREAKPOINTS ########################################################


par( mfrow= c(4,2), mai = c(0.4, 0.3, 0.1, 0.2))

plot(log(hh_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_hh_m), col = "blue", lwd = 0.8)
lines(confint(bp_hh_m))
legend('topleft', legend=c("log(HH monthly)"),
       col=c("black"), lty=1, cex=1)

plot(log(jkm_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_jkm_m), col = "blue", lwd = 0.8)
lines(confint(bp_jkm_m))
legend('topleft', legend=c("log(JKM monthly)"),
       col=c("black"), lty=1, cex=1)


plot(log(nbp_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_nbp_m), col = "blue", lwd = 0.8)
lines(confint(bp_nbp_m))
legend('topleft', legend=c("log(NBP monthly)"),
       col=c("black"), lty=1, cex=1)


plot(log(ttf_monthly),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_ttf_m), col = "blue", lwd = 0.8)
lines(confint(bp_ttf_m))
legend('topleft', legend=c("log(TTF monthly)"),
       col=c("black"), lty=1, cex=1 )



plot(log(wti_monthly),  xlab="Time", ylab="Dollars per Barrel", col = "black")
lines(fitted(bp_wti_m), col = "blue", lwd = 0.8)
lines(confint(bp_wti_m))
legend('topleft', legend=c("log(WTI monthly)"),
       col=c("black"), lty=1, cex=1)

plot(log(brent_monthly),  xlab="Time", ylab="Dollars per Barrel", col = "black")
lines(fitted(bp_brent_m), col = "blue", lwd = 0.8)
lines(confint(bp_brent_m))
legend('topleft', legend=c("log(Brent monthly)"),
       col=c("black"), lty=1, cex=1)

plot(log(wb_eu_ng),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_eu_m), col = "blue", lwd = 0.8)
lines(confint(bp_eu_m))
legend('topleft', legend=c("log(WB EU monthly)"),
       col=c("black"), lty=1, cex=1)


plot(log(wb_japan_lng),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_jap_m), col = "blue", lwd = 0.8)
lines(confint(bp_jap_m))
legend('topleft', legend=c("log(WB Jap LNG monthly)"),
       col=c("black"), lty=1, cex=1)


# for the daily:
par( mfrow= c(3,2), mai = c(0.4, 0.3, 0.1, 0.2))

plot(log(hh_daily),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_hh_d), col = "blue", lwd = 0.8)
lines(confint(bp_hh_d))
legend('topleft', legend=c("log(HH daily)"),
       col=c("black"), lty=1, cex=1)

plot(log(jkm_daily),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_jkm_d), col = "blue", lwd = 0.8)
lines(confint(bp_jkm_d))
legend('topleft', legend=c("log(JKM daily)"),
       col=c("black"), lty=1, cex=1)

plot(log(nbp_daily),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_nbp_d), col = "blue", lwd = 0.8)
lines(confint(bp_nbp_d))
legend('topleft', legend=c("log(NBP daily)"),
       col=c("black"), lty=1, cex=1)

plot(log(ttf_daily),  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_ttf_d), col = "blue", lwd = 0.8)
lines(confint(bp_ttf_d))
legend('topleft', legend=c("log(TTF daily)"),
       col=c("black"), lty=1, cex=1 )

plot(log(wti_daily),  xlab="Time", ylab="Dollars per Barrel", col = "black")
lines(fitted(bp_wti_d), col = "blue", lwd = 0.8)
lines(confint(bp_wti_d))
legend('topleft', legend=c("log(WTI daily)"),
       col=c("black"), lty=1, cex=1)

plot(log(brent_daily),  xlab="Time", ylab="Dollars per Barrel", col = "black")
lines(fitted(bp_brent_d), col = "blue", lwd = 0.8)
lines(confint(bp_brent_d))
legend('topleft', legend=c("log(Brent daily)"),
       col=c("black"), lty=1, cex=1)

# now for weekly

par( mfrow= c(3,2), mai = c(0.4, 0.3, 0.1, 0.2))
plot(log(hh_weekly),  xlab="Time", ylab="USD/ MMBtu", col = "black") #plot 1
lines(fitted(bp_hh_w), col = "blue", lwd = 0.8)
lines(confint(bp_hh_w))
legend('topleft', legend=c("log(HH weekly)"),
       col=c("black"), lty=1, cex=1)
plot(log(jkm_weekly),  xlab="Time", ylab="USD/ MMBtu", col = "black") #plot 2
lines(fitted(bp_jkm_w), col = "blue", lwd = 0.8)
lines(confint(bp_jkm_w))
legend('topleft', legend=c("log(JKM weekly)"),
       col=c("black"), lty=1, cex=1)
plot(log(nbp_weekly),  xlab="Time", ylab="USD/ MMBtu", col = "black") #plot 3
lines(fitted(bp_nbp_w), col = "blue", lwd = 0.8)
lines(confint(bp_nbp_w))
legend('topleft', legend=c("log(NBP weekly)"),
       col=c("black"), lty=1, cex=1)
plot(log(ttf_weekly),  xlab="Time", ylab="USD/ MMBtu", col = "black") #plot 4
lines(fitted(bp_ttf_w), col = "blue", lwd = 0.8)
lines(confint(bp_ttf_w))
legend('topleft', legend=c("log(TTF weekly)"),
       col=c("black"), lty=1, cex=1 )
plot(log(wti_weekly),  xlab="Time", ylab="Dollars per Barrel", col = "black") # plot 5
lines(fitted(bp_wti_w), col = "blue", lwd = 0.8)
lines(confint(bp_wti_w))
legend('topleft', legend=c("log(WTI weekly)"),
       col=c("black"), lty=1, cex=1)
plot(log(brent_weekly),  xlab="Time", ylab="Dollars per Barrel", col = "black") # plot 6
lines(fitted(bp_brent_w), col = "blue", lwd = 0.8)
lines(confint(bp_brent_w))
legend('topleft', legend=c("log(Brent weekly)"),
       col=c("black"), lty=1, cex=1)


# EXPLORATORY ANALYSIS ########################################################

ts_names

ts_names
vec <- seq(2, 38, 2)
log_ts_names <- ts_names[vec]

lm.model <- hh_monthly ~ 1
fs <- Fstats(lm.model, data = hh_monthly)
plot(fs, pval=TRUE)
plot(fs, aveF=TRUE)
sctest(fs, type="expF")
plot(fs)
dev.off()m

bp_hh_m <- breakpoints(lm.model, format.times = TRUE)
confint(bp_hh_m)
plot(hh_monthly,  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp_hh_m, breaks = 1), col = "blue", lwd = 0.8)
#lines(confint(bp_ttf_w), lwd = 1)

gefp(lm.model)


bh.efp <- gefp(hh_monthly ~ 1,  data = hh_monthly)
plot(bh.efp, aveF = TRUE)
plot(bh.efp, functional = meanL2BB)


#  NEW
ocus.nile <- efp(hh_monthly ~ 1, type = "OLS-CUSUM")
plot(ocus.nile)
# the peak here indiciates where the structural break is, around 2009/2010


#boundaries corresponding to a supF test at the 5% significance level.

fs.nile <- Fstats(hh_daily ~ 1)
dev.off()
plot(fs.nile)
bp.nile <- breakpoints(hh_monthly~1)
plot(bp.nile)
bp1 <- breakpoints(bp.nile, breaks = 1)
fm0.nile <- lm(hh_monthly ~ 1)
nile.fac <- breakfactor(bp1)
fm1.nile <- lm(hh_monthly ~ nile.fac - 1)
coef(fm1.nile)


#

bp.oil <- breakpoints(log(hh_monthly) ~ 1)
bp.oil_2 <- breakpoints(log(hh_weekly) ~ 1)

bp.oil
dev.off()
plot(hh_monthly,  xlab="Time", ylab="USD/ MMBtu", col = "black")
lines(fitted(bp.oil, breaks = 4), col = "blue", lwd = 0.8)


bp3 <- breakpoints(bp.oil, breaks = 5)
bp3 <- breakpoints(bp.oil_2, breaks = 5)
#and then we use the OLS-based CUSUM for checking for additional breaks in the mean.
ocus.oil <- efp(log(hh_monthly) ~ breakfactor(bp3), type = "OLS-CUSUM")
ocus.oil_2 <- efp(log(hh_weekly) ~ breakfactor(bp3), type = "OLS-CUSUM")
plot(ocus.oil_2)
plot(ocus.oil)














