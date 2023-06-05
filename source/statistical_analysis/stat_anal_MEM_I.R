library(car)
library(dplyr)
# library(lme4)
# install.packages(lme4)
library(lmerTest) # to include p-values


# read averaged data as df
df=read.csv("df_clips_all_processed_gold.csv",stringsAsFactors = TRUE)


####ALLLLL Workers
# # # Convert variables to factor

X= data.frame(
  condition_num=as.factor(df$condition_num),
  condition_alg=as.factor(df$condition_alg),
  Pos_Neg=as.factor(df$mean_positive),
  S_L=as.factor(df$ speech_level),
  Gender=as.factor(df$Gender),
  ClipID=as.factor(df$ClipID),
  SNR=as.factor(df$snr),
  N_E=as.factor(df$noise_events),
  N_T=as.factor(df$noise_type),
  CMOS=as.numeric(df$CMOS),
  DNS_O=as.numeric(df$dnsmos_OVR),
  DNS_S=as.numeric(df$dnsmos_SIG),
  DNS_B=as.numeric(df$dnsmos_BAK),
  ViS=as.numeric(df$visqol_MOS),
  Que_G=as.numeric(df$X3quest_G),
  Que_S=as.numeric(df$X3quest_S),
  Que_N=as.numeric(df$X3quest_N))


lm_dnsSig_pn <- lmer(X$DNS_S ~ 1+CMOS + SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)
lm_dnsBak_pn <- lmer(X$DNS_B ~ 1+CMOS + SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)
lm_dnsOvr_pn <- lmer(X$DNS_O ~ 1+CMOS + SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)
lm_vis_pn <- lmer(X$ViS ~ 1+CMOS + SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)
lm_X3S_pn <- lmer(X$Que_S ~ 1+CMOS + SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)
lm_X3N_pn <- lmer(X$Que_N ~ 1+CMOS + SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)
lm_X3G_pn <- lmer(X$Que_G ~ 1+CMOS + SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)



#
# # quantile(residuals(lm_dnsBak_pn, "pearson", scaled = TRUE))
#
# # residual plot
# plot(lm_X3G_pn, type = c("p", "smooth"))
# #qq plot
# library(lattice)
# qqmath(lm_X3G_pn, id = 0.05)
#
# # #confidence intervals
# # confint(lm_pos_dnsSig)



# ######### Only postive negative
# stargazer(lm_dnsSig_pn, lm_dnsBak_pn, lm_dnsOvr_pn, lm_vis_pn, lm_X3S_pn, lm_X3N_pn, lm_X3G_pn,
#           no.space=TRUE, omit.stat=c("ser", "rsq"))

lmer_cmos = lmer(X$CMOS ~ 1+ SNR + S_L + N_E + N_T+Pos_Neg+Gender+ (1|ClipID), data = X)
