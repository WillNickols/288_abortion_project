library(dplyr)

abortion_diffs <- read.csv('data/abortion_diffs.csv')
abortion_diffs <- abortion_diffs[abortion_diffs$TreatedOriginal == 'Ban',]
abortion_diffs <- dplyr::rename(abortion_diffs, Tau_Abortions = Tau)
birth_diffs <- read.csv('data/birth_diffs.csv')
birth_diffs <- birth_diffs[birth_diffs$TreatedOriginal == 'Ban',]
birth_diffs <- dplyr::rename(birth_diffs, Tau_Births = Tau)

abortion_diffs$MonthNum <- (abortion_diffs$Year - 2020) * 12 + abortion_diffs$Month
birth_diffs$MonthNum <- (birth_diffs$Year - 2020) * 12 + birth_diffs$Month

ks <- 1:12
sigmas <- vector(length = length(ks))
for (k in ks) {
  birth_diffs_tmp <- birth_diffs
  birth_diffs_tmp$MonthNum <- birth_diffs_tmp$MonthNum - k
  merged_diffs <- full_join(abortion_diffs[,c("State", "MonthNum", "Tau_Abortions")], 
                            birth_diffs_tmp[,c("State", "MonthNum", "Tau_Births")], 
                            by=c("State", "MonthNum"))
  lmer_out <- lm(Tau_Abortions ~ Tau_Births + Tau_Births.x + Tau_Births.y, merged_diffs)
  summary(lmer_out)
  sigmas[k] <- summary(lmer_out)$sigma
}
k_best <- which.max(sigmas)

birth_diffs_tmp <- birth_diffs
birth_diffs_tmp$MonthNum <- birth_diffs_tmp$MonthNum - k_best
merged_diffs <- full_join(abortion_diffs[,c("State", "MonthNum", "Tau_Abortions")], 
                          birth_diffs_tmp[,c("State", "MonthNum", "Tau_Births")], 
                          by=c("State", "MonthNum"))
merged_diffs <- merged_diffs[!is.na(merged_diffs$Tau_Abortions) & !is.na(merged_diffs$Tau_Births),]

lmer_out <- lm(Tau_Births ~ Tau_Abortions, merged_diffs)
summary(lmer_out)

plot(, merged_diffs$Tau_Births)
abline(a=0, b=1)

plot((merged_diffs$Tau_Births + predict(lmer_out)) / 2, merged_diffs$Tau_Births - predict(lmer_out))























ks <- 1:12
sigmas <- vector(length = length(ks))
for (k in ks) {
  abortion_diffs_tmp <- abortion_diffs
  abortion_diffs_tmp$MonthNum <- abortion_diffs_tmp$MonthNum + k
  merged_diffs <- full_join(birth_diffs[,c("State", "MonthNum", "Tau_Births")], 
                            abortion_diffs_tmp[,c("State", "MonthNum", "Tau_Abortions")], 
                            by=c("State", "MonthNum"))
  abortion_diffs_tmp <- abortion_diffs
  abortion_diffs_tmp$MonthNum <- abortion_diffs_tmp$MonthNum + k + 1
  merged_diffs <- full_join(merged_diffs, 
                            abortion_diffs_tmp[,c("State", "MonthNum", "Tau_Abortions")], 
                            by=c("State", "MonthNum"))
  abortion_diffs_tmp <- abortion_diffs
  abortion_diffs_tmp$MonthNum <- abortion_diffs_tmp$MonthNum + k + 2
  merged_diffs <- full_join(merged_diffs, 
                            abortion_diffs_tmp[,c("State", "MonthNum", "Tau_Abortions")], 
                            by=c("State", "MonthNum"))
  abortion_diffs_tmp <- abortion_diffs
  abortion_diffs_tmp$MonthNum <- abortion_diffs_tmp$MonthNum + k + 3
  merged_diffs <- full_join(merged_diffs, 
                            abortion_diffs_tmp[,c("State", "MonthNum", "Tau_Abortions")], 
                            by=c("State", "MonthNum"))
  merged_diffs <- merged_diffs[!is.na(merged_diffs$Tau_Abortions.x) & !is.na(merged_diffs$Tau_Births),]
  lmer_out <- lm(Tau_Births ~ Tau_Abortions.x + Tau_Abortions.x.x + Tau_Abortions.y + Tau_Abortions.y.y, merged_diffs)
  summary(lmer_out)
  sigmas[k] <- summary(lmer_out)$sigma
}
k_best <- which.max(sigmas)

birth_diffs_tmp <- birth_diffs
birth_diffs_tmp$MonthNum <- birth_diffs_tmp$MonthNum - k_best
merged_diffs <- full_join(abortion_diffs[,c("State", "MonthNum", "Tau_Abortions")], 
                          birth_diffs_tmp[,c("State", "MonthNum", "Tau_Births")], 
                          by=c("State", "MonthNum"))
merged_diffs <- merged_diffs[!is.na(merged_diffs$Tau_Abortions) & !is.na(merged_diffs$Tau_Births),]

lmer_out <- lm(Tau_Births ~ Tau_Abortions, merged_diffs)
summary(lmer_out)

plot(, merged_diffs$Tau_Births)
abline(a=0, b=1)

plot((merged_diffs$Tau_Births + predict(lmer_out)) / 2, merged_diffs$Tau_Births - predict(lmer_out))








