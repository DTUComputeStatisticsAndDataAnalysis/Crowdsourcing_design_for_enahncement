library(car)
library(dplyr)
# library(lme4)
# install.packages(lme4)
library(lmerTest) # to include p-values


# read averaged data as df
df=read.csv("df_clips_all_processed.csv",stringsAsFactors = TRUE)

# Divide by mean_positive
df_neg = df %>% filter(mean_positive==0)
df_pos = df %>% filter(mean_positive==1)

################## Positive workers
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
df_pos$gender = as.factor(df_pos$Gender)

####
lm_pos_dnsSig <- lmer(df_pos$dns_SIG ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_pos)
# summary(lm_pos_dnsSig)
lm_pos_dnsBak <- lmer(df_pos$dns_BAK ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_pos)
lm_pos_dnsOvr <- lmer(df_pos$dns_OVR ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_pos)
lm_pos_vis <- lmer(df_pos$visqol ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_pos)
lm_pos_X3S <- lmer(df_pos$X3quest_S ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_pos)
lm_pos_X3N <- lmer(df_pos$X3quest_N ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_pos)
lm_pos_X3G <- lmer(df_pos$X3quest_G ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_pos)

quantile(residuals(lm_pos_dnsSig, "pearson", scaled = TRUE))

# residual plot
plot(lm_pos_dnsSig, type = c("p", "smooth"))
#qq plot
library(lattice)
qqmath(lm_pos_dnsSig, id = 0.05)

# confidence intervals
confint(lm_pos_dnsSig)


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
df_neg$gender = as.factor(df_neg$Gender)

####
lm_neg_dnsSig <- lmer(df_neg$dns_SIG ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_neg)
# summary(lm_neg_dnsSig)
lm_neg_dnsBak <- lmer(df_neg$dns_BAK ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_neg)
lm_neg_dnsOvr <- lmer(df_neg$dns_OVR ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_neg)
lm_neg_vis <- lmer(df_neg$visqol ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_neg)
lm_neg_X3S <- lmer(df_neg$X3quest_S ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_neg)
lm_neg_X3N <- lmer(df_neg$X3quest_N ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_neg)
lm_neg_X3G <- lmer(df_neg$X3quest_G ~ 1+CMOS + snr + speech_level + noise_events + noise_type + gender + (1|ClipID), data = df_neg)


# ############### R table to latex table
# # install.packages("stargazer")
# library(stargazer)
#
# ######### Only postive negative
# stargazer(lm_pos_dnsSig, lm_neg_dnsSig, lm_pos_dnsBak, lm_neg_dnsBak, lm_pos_dnsOvr, lm_neg_dnsOvr, lm_pos_vis, lm_neg_vis,
#           lm_pos_X3S, lm_neg_X3S, lm_pos_X3N, lm_neg_X3N, lm_pos_X3G, lm_neg_X3G,
#           no.space=TRUE, omit.stat=c("ser", "rsq"))
