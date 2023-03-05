# LOADING PACKAGES ############################################################

library(fpp2)

# IMPORTING DATA ############################################################

# ttf_daily_copy
# JKM_df
# term_ts_hh
# nbp_ts_og

# PLOTING ####################################################################

# plotting graph of daily prices series for jkm, nbp, hh, and ttf

ttf_plot <- ts(ttf_daily_copy$price, start = c(2010, 3), frequency = 365)
jkm_plot <- ts(JKM_df$price, start = c(2014, 213), frequency = 365)
jkm_plot <- na.interp(jkm_plot, linear = TRUE)
hh_plot <- na.interp(term_ts_hh, linear = TRUE)
nbp_plot <- na.interp(nbp_ts_og, linear = TRUE)
ttf_plot <- na.interp(ttf_plot, linear = TRUE)
# going to plot all graphs
all_ts <- cbind(hh_plot, nbp_plot, ttf_plot, jkm_plot)
t.start <- time(hh_monthly)[220]
window_1 <- window(all_ts, start = t.start)


# first plot shown (Figure 1)
display.brewer.all()
dev.off()
cols <- brewer.pal(4, "Spectral")
ts.plot(window_1, gpars = list(col = cols), xlim=c(2016,2022.6), ylab="USD/MMBtu", lwd=2,  lty = c(1,1,1,1))
legend("topleft", legend = c("Henry Hub", "NBP", "TTF", "JKM"), col = cols, lty = c(1,1,1,1), lwd = 2)
grid(lty = 1)
abline(v = time(hh_monthly)[288], lty = 2,  col = alpha("black", 0.5), lwd = 2)   
abline(v = time(hh_monthly)[302], lty = 2, col = alpha("black", 0.5), lwd = 2) 


# plotting natural gas exports and imports 

# just LNG exports and imports 

ng_imports_USA <- ng_imports_USA[-c(1:2),]
ng_imports_USA <- ng_imports_USA[-c(1:288),]
colnames(ng_imports_USA) <- c("date", "price")
ts_ng_imports <- ts(ng_imports_USA$price, start = c(1997,1), frequency = 12 )

ng_exports_USA <- ng_exports_USA[-c(1:2),]
ng_exports_USA <- ng_exports_USA[-c(1:288),]
colnames(ng_exports_USA) <- c("date", "price")
ts_ng_exports <- ts(ng_exports_USA$price, start = c(1997,1), frequency = 12 )

# all exports (LNG and NG)

exports_USA <- exports_USA[-c(1:2),]
imports_USA <- imports_USA[-c(1:2),]
colnames(imports_USA) <- c("date", "price")
colnames(exports_USA) <- c("date", "price")
ts_exports <- ts(exports_USA$price, start = c(1997,1), frequency = 12 )
ts_imports <- ts(imports_USA$price, start = c(1997,1), frequency = 12 )

# second plot shown (Figure 2)
dev.off()
cols <- brewer.pal(3, "Dark2")
ts_ei <- cbind(ts_ng_exports, ts_ng_imports) 
ts_ei_window <- window(ts_ei, start = time(hh_monthly)[100])
ts.plot(ts_ei, gpars = list(col = cols), ylab = "MMcf", lwd=2)
grid(lty =1)
legend("topleft", legend = c("Natural Gas exports", "Natural Gas imports"), col = cols, lty = 1, lwd=2)
abline(v = time(hh_monthly)[228], lty = 2, col = alpha("black", 0.7)) 
abline(v = time(hh_monthly)[240], lty = 2, col = alpha("black", 0.7)) 
axis(1, at = c(2016), srt=45, col = "black", las = 1, cex.axis = 0.6)
axis(1, at = c(2017), srt=45, col = "black", las = 1, cex.axis = 0.6)

