library(dplyr)
library(car)
# read averaged data as df
df=read.csv('df_conditions_pos_neg_gold.csv',stringsAsFactors = TRUE)

# Divide by mean_positive
df_neg = df %>% filter(mean_positive==0)
df_pos = df %>% filter(mean_positive==1)

lm_df_all = lm(df$dnsmos_BAK~df$CMOS, data = df)
summary(lm_df_all)

# # # Convert variables to factor
df_pos$condition_num=as.factor(df_pos$condition_num)
df_pos$condition_alg=as.factor(df_pos$condition_alg)
df_pos$mean_positive=as.factor(df_pos$mean_positive)
df_pos$speech_level=as.factor(df_pos$ speech_level)
df_pos$snr=as.factor(df_pos$snr)
df_pos$noise_events=as.factor(df_pos$noise_events)
df_pos$noise_type=as.factor(df_pos$noise_type)
df_pos$CMOS=as.numeric(df_pos$CMOS)
df_pos$dns_OVR=as.numeric(df_pos$dnsmos_OVR)
df_pos$dns_SIG=as.numeric(df_pos$dnsmos_SIG)
df_pos$dns_BAK=as.numeric(df_pos$dnsmos_BAK)
df_pos$visqol=as.numeric(df_pos$visqol_MOS)
df_pos$X3quest_G=as.numeric(df_pos$X3quest_G)
df_pos$X3quest_S=as.numeric(df_pos$X3quest_S)
df_pos$X3quest_N=as.numeric(df_pos$X3quest_N)

####
lm_pos_dnsSig <- lm(df_pos$dns_SIG ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)
# summary(lm_pos_dnsSig)
lm_pos_dnsBak <- lm(df_pos$dns_BAK ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)
lm_pos_dnsOvr <- lm(df_pos$dns_OVR ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)
lm_pos_vis <- lm(df_pos$visqol ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)
lm_pos_X3S <- lm(df_pos$X3quest_S ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)
lm_pos_X3N <- lm(df_pos$X3quest_N ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)
lm_pos_X3G <- lm(df_pos$X3quest_G ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)




# vif(global.lm)
# cor(df_pos)


################### Negative workers

# # # Convert variables to factor
df_neg$condition_num=as.factor(df_neg$condition_num)
df_neg$condition_alg=as.factor(df_neg$condition_alg)
df_neg$mean_positive=as.factor(df_neg$mean_positive)
df_neg$speech_level=as.factor(df_neg$ speech_level)
df_neg$snr=as.factor(df_neg$snr)
df_neg$noise_events=as.factor(df_neg$noise_events)
df_neg$noise_type=as.factor(df_neg$noise_type)
df_neg$CMOS=as.numeric(df_neg$CMOS)
df_neg$dns_OVR=as.numeric(df_neg$dnsmos_OVR)
df_neg$dns_SIG=as.numeric(df_neg$dnsmos_SIG)
df_neg$dns_BAK=as.numeric(df_neg$dnsmos_BAK)
df_neg$visqol=as.numeric(df_neg$visqol_MOS)
df_neg$X3quest_G=as.numeric(df_neg$X3quest_G)
df_neg$X3quest_S=as.numeric(df_neg$X3quest_S)
df_neg$X3quest_N=as.numeric(df_neg$X3quest_N)

####
lm_neg_dnsSig <- lm(df_neg$dns_SIG ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)
# summary(lm_neg_dnsSig)
lm_neg_dnsBak <- lm(df_neg$dns_BAK ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)
lm_neg_dnsOvr <- lm(df_neg$dns_OVR ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)
lm_neg_vis <- lm(df_neg$visqol ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)
lm_neg_X3S <- lm(df_neg$X3quest_S ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)
lm_neg_X3N <- lm(df_neg$X3quest_N ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)
lm_neg_X3G <- lm(df_neg$X3quest_G ~ CMOS + snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)


############### R table to latex table
# install.packages("stargazer")
library(stargazer)

######### Only postive negative
stargazer(lm_pos_dnsSig, lm_neg_dnsSig, lm_pos_dnsBak, lm_neg_dnsBak, lm_pos_dnsOvr, lm_neg_dnsOvr, lm_pos_vis, lm_neg_vis,
          lm_pos_X3S, lm_neg_X3S, lm_pos_X3N, lm_neg_X3N, lm_pos_X3G, lm_neg_X3G,
          no.space=TRUE, omit.stat=c("ser", "rsq", 'f'))



# ######### Influence of condition parameters
# lm_cmos_p = lm(df_pos$CMOS ~ 1+ snr + speech_level + noise_events + noise_type+condition_alg, data = df_pos)
# lm_cmos_n = lm(df_neg$CMOS ~ 1+ snr + speech_level + noise_events + noise_type+condition_alg, data = df_neg)
#
# stargazer(lm_cmos_p, lm_cmos_n, no.space=TRUE, omit.stat=c("ser", "rsq", 'f'))
