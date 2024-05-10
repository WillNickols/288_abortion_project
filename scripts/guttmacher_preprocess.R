library(reshape2)
library(dplyr)
library(ggrepel)

abortion_rates_gut <- read.csv('data/MonthlyAbortionProvisionMonthly.csv')
abortion_rates_gut <- abortion_rates_gut %>% dplyr::select(state, month, median)
abortion_rates_gut <- abortion_rates_gut[abortion_rates_gut$state != 'US',]
abortion_rates_gut$State <- mapvalues(abortion_rates_gut$state, c(state.abb, 'DC'), c(state.name, 'DC'), warn_missing = F)
abortion_rates_gut$Year <- as.numeric(gsub('.*/', '', abortion_rates_gut$month))
abortion_rates_gut$Month <- month.abb[as.numeric(gsub('/.*', '', abortion_rates_gut$month))]
abortion_rates_gut <- abortion_rates_gut %>% dplyr::select(State, Month, Year, median) %>%
  dplyr::rename(Abortions = median)

# If missing, it's banned
df_addition <- data.frame(expand.grid(state.name, unique(abortion_rates_gut$Month)))
df_addition$Year <- 2023
df_addition$Abortions <- 0
colnames(df_addition) <- colnames(abortion_rates_gut)

abortion_rates_gut <- rbind(abortion_rates_gut, df_addition)
abortion_rates_gut <- abortion_rates_gut[!duplicated(abortion_rates_gut[,c('State', 'Month', 'Year')]),]

# Get childbearing women population numbers
pop_ests <- read.csv('data/sc-est2022-agesex-civ.csv')
pop_ests <- pop_ests[pop_ests$NAME != "United States" & 
                       pop_ests$AGE >= 15 & pop_ests$AGE <= 49 & 
                       pop_ests$SEX == 2,]
pop_ests <- pop_ests[c("NAME", "AGE", "POPEST2020_CIV", "POPEST2021_CIV", "POPEST2022_CIV")]
colnames(pop_ests) <- c("State", "Age", "2020", "2021", "2022")
pop_ests <- reshape2::melt(pop_ests, id.vars = c('State', 'Age'))
colnames(pop_ests) <- c("State", "Age", "Year", "Population")
pop_ests$Year <- as.numeric(as.character(pop_ests$Year))

# Impute 2023 populations for each age in each state with a linear model
lm_fit <- lm(Population ~ State:as.factor(Age) + State:as.factor(Age):Year, pop_ests)
new_data_pop <- pop_ests
new_data_pop$Year <- 2023
new_data_pop$Population <- NULL
new_data_pop <- unique(new_data_pop)
new_data_pop$Population <- predict(lm_fit, newdata = new_data_pop)
pop_ests <- rbind(pop_ests, new_data_pop)

# Sum over all childbearing ages
pop_ests_agg <- pop_ests %>%
  dplyr::group_by(State, Year) %>%
  dplyr::summarise(Population = sum(Population))

# ggplot(pop_ests_agg, aes(x = Year, y = Population, color = State)) + 
#   geom_point()

# Impute all months
# Original estimates are for July 1
pop_ests_agg$Year <- pop_ests_agg$Year + 0.5
lm_fit_months <- lm(Population ~ State + State:Year, pop_ests_agg)
new_data_month <- expand.grid(unique(pop_ests_agg$State), seq(2020, 2024, 1/12))
colnames(new_data_month) <- c("State", "Year")
new_data_month$Population <- predict(lm_fit_months, newdata = new_data_month)

# ggplot(new_data_month, aes(x = Year, y = Population, color = State)) + 
#   geom_point()

new_data_month$Month <- round((new_data_month$Year - floor(new_data_month$Year)) * 12)
new_data_month$Month <- mapvalues(new_data_month$Month, 0:11, month.abb)
new_data_month$Year <- floor(new_data_month$Year)
new_data_month$State <- as.character(new_data_month$State)
new_data_month$State <- ifelse(new_data_month$State == 'District of Columbia', "DC", new_data_month$State)

# Join populations and abortion counts
joined_df <- left_join(abortion_rates_gut, new_data_month, by=c("State", "Year", "Month"))
joined_df$AbortionsPerThousandWomen <- joined_df$Abortions / joined_df$Population * 1000

month_num <- match(joined_df$Month, month.abb)
joined_df$Date <- as.Date(paste(joined_df$Year, month_num, "01", sep = "-"))

joined_df[joined_df$State == 'South Dakota',] %>%
  mutate(label = if_else(Date == max(Date), as.character(State), NA_character_)) %>%
  ggplot(aes(x = Date, y = AbortionsPerThousandWomen, group = State, colour = State)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   max.overlaps = 50)

joined_df$Date <- NULL
write.csv(joined_df, 'data/abortion_rates_gut.csv', row.names = F)








