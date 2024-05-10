library(dplyr)
library(ggrepel)
abortion_rates <- read.csv('data/abortion_rates.csv')
abortion_rates_gut <- read.csv('data/abortion_rates_gut.csv')
abortion_rates <- dplyr::rename(abortion_rates, Abortions_SFP = AbortionsPerThousandWomen)
abortion_rates_gut <- dplyr::rename(abortion_rates_gut, Abortions_Gut = AbortionsPerThousandWomen)

all_abortion_df <- full_join(abortion_rates, abortion_rates_gut, by = c('State', 'Year', 'Month'))

month_num <- match(all_abortion_df$Month, month.abb)
all_abortion_df$Date <- as.Date(paste(all_abortion_df$Year, month_num, "01", sep = "-"))

all_abortion_df %>%
  mutate(label = if_else(Date == median(Date), as.character(State), NA_character_)) %>%
  ggplot(aes(x = Abortions_SFP, y = Abortions_Gut, color = State)) + 
    geom_point(size = 2) + 
    geom_abline(slope=1, intercept=0, linetype = 'dotted', color = 'red') + 
    theme_bw() + 
    theme(text = element_text(size = 20),
          legend.position = 'none') + 
    labs(x = 'Society for Family Planning', y = 'Guttmacher') + 
    geom_label_repel(aes(label = label),
                     nudge_x = 1,
                     na.rm = TRUE,
                     max.overlaps = 4) + 
  ggtitle("Abortions per 1000 women age 15-49")
