library(dplyr)

abortion_diffs <- read.csv('data/abortion_diffs.csv')
abortion_diffs <- dplyr::rename(abortion_diffs, Tau_Abortions = Tau)
birth_diffs <- read.csv('data/birth_diffs.csv')
birth_diffs <- dplyr::rename(birth_diffs, Tau_Births = Tau)

abortion_diffs$MonthNum <- (abortion_diffs$Year - 2020) * 12 + abortion_diffs$Month
birth_diffs$MonthNum <- (birth_diffs$Year - 2020) * 12 + birth_diffs$Month

abortion_diffs <- abortion_diffs[order(abortion_diffs$MonthNum),]
birth_diffs <- birth_diffs[order(birth_diffs$MonthNum),]

birth_diffs$AbortionEst <- NA
weights <- c(0.35, 0.5, 0.15)
for (i in 1:nrow(birth_diffs)) {
  abortion_counts <- abortion_diffs$Tau_Abortions[abortion_diffs$State == birth_diffs$State[i] & 
                                 abortion_diffs$MonthNum %in% ((birth_diffs$MonthNum[i] - 8): 
                                                                 (birth_diffs$MonthNum[i] - 6))]
  if (length(abortion_counts) == 3) {
    birth_diffs$AbortionEst[i] <- -sum(abortion_counts * weights)
  }
}

custom_palette <- c("Control" = "blue", "Neighbor" = "darkgreen", "Ban" = "red")
plot1 <- ggplot(birth_diffs, aes(x = AbortionEst, y = Tau_Births, fill = TreatedOriginal, size = Population)) + 
  geom_point(stroke = 0.8, shape=21) + 
  scale_fill_manual(values = custom_palette, labels = c('Ban', 'Control', 'Neighbor')) + 
  scale_colour_manual(values=c("white", "black")) +
  theme_bw() +
  labs(color = '', y = TeX('$\\tau$ for births'), x = TeX('Abortion-based predictor of $\\tau$ for births')) + 
  theme(text = element_text(size = 20))

ggsave('figures/fig_3.png', plot1, width = 8, height = 6, dpi = 1000)

# plot((birth_diffs$AbortionEst + birth_diffs$Tau_Births) / 2, (birth_diffs$AbortionEst - birth_diffs$Tau_Births) / 2)
# plot(birth_diffs$AbortionEst, birth_diffs$Tau_Births)

# tmp_abortions <- aggregate(Tau_Abortions * Population ~ Year + Month + MonthNum, abortion_diffs, sum)
# tmp_births <- aggregate(Tau_Births * Population ~ Year + Month + MonthNum, birth_diffs, sum)
# colnames(tmp_abortions) <- c("Year", "Month", "MonthNum", "Tau_Abortions")
# colnames(tmp_births) <- c("Year", "Month", "MonthNum", "Tau_Births")
# 
# tmp_births$AbortionEst <- NA
# for (i in 1:nrow(tmp_births)) {
#   abortion_counts <- tmp_abortions$Tau_Abortions[tmp_births$MonthNum %in% ((tmp_births$MonthNum[i] - 8): 
#                                                                           (tmp_births$MonthNum[i] - 6))]
#   if (length(abortion_counts) == 3) {
#     tmp_births$AbortionEst[i] <- sum(abortion_counts * weights)
#   }
# }
# 
# plot((tmp_births$AbortionEst + tmp_births$Tau_Births) / 2, (tmp_births$AbortionEst - tmp_births$Tau_Births) / 2)
# plot(tmp_births$AbortionEst, tmp_births$Tau_Births)







