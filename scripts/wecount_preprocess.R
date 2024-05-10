library(reshape2)
library(dplyr)
library(ggrepel)

text_lines <- readLines('data/abortions.txt')
text_lines <- gsub('District of Columbia', 'DC', text_lines)
text_lines <- gsub('All US state totals', 'All_US', text_lines)
text_lines <- gsub('New Hampshire', 'New_Hampshire', text_lines)
text_lines <- gsub('New Jersey', 'New_Jersey', text_lines)
text_lines <- gsub('New Mexico', 'New_Mexico', text_lines)
text_lines <- gsub('  ', '_', text_lines)
text_lines <- strsplit(text_lines, ' ')[[1]]
text_lines[text_lines == '–'] <- 0
text_lines <- gsub(',', '', text_lines)
abortion_matrix <- matrix(nrow = 53, ncol = 18)
colnames(abortion_matrix) <- text_lines[1:18]
text_lines <- text_lines[-c(1:18)]

next_state <- 1
i <- 1
j <- 1
rownames_vec <- c()
for (text_line_id in 1:length(text_lines)) {
  if ((text_line_id - 1) %% 19 == 0) {
    rownames_vec <- c(rownames_vec, text_lines[text_line_id])
    next_state <- next_state + 1
  } else {
    abortion_matrix[i,j] <- text_lines[text_line_id]
    if (j < 18) {
      j <- j + 1
    } else {
      i <- i + 1
      j <- 1
    }
  }
}

text_lines <- readLines('data/abortions2.txt')
text_lines <- gsub('New York', 'New_York', text_lines)
text_lines <- gsub('North Carolina', 'North_Carolina', text_lines)
text_lines <- gsub('North Dakota', 'North_Dakota', text_lines)
text_lines <- gsub('Rhode Island', 'Rhode_Island', text_lines)
text_lines <- gsub('South Carolina', 'South_Carolina', text_lines)
text_lines <- gsub('South Dakota', 'South_Dakota', text_lines)
text_lines <- gsub('West Virginia', 'West_Virginia', text_lines)
text_lines <- gsub('  ', '_', text_lines)
text_lines <- strsplit(text_lines, ' ')[[1]]
text_lines[text_lines == '–'] <- 0
text_lines <- gsub(',', '', text_lines)
text_lines <- text_lines[-c(1:18)]
for (text_line_id in 1:length(text_lines)) {
  if ((text_line_id - 1) %% 19 == 0) {
    rownames_vec <- c(rownames_vec, text_lines[text_line_id])
    next_state <- next_state + 1
  } else {
    abortion_matrix[i,j] <- text_lines[text_line_id]
    if (j < 18) {
      j <- j + 1
    } else {
      i <- i + 1
      j <- 1
    }
  }
}

text_lines <- readLines('data/abortions3.txt')
text_lines <- gsub('All US state totals', 'Telehealth', text_lines)
text_lines <- gsub('  ', '_', text_lines)
text_lines <- strsplit(text_lines, ' ')[[1]]
text_lines[text_lines == '–'] <- 0
text_lines <- gsub(',', '', text_lines)
text_lines <- text_lines[-c(1:18)]
abortion_matrix[53,] <- text_lines[-1]

abortion_matrix <- apply(abortion_matrix, 2, as.numeric)
rownames(abortion_matrix) <- c(gsub("_", " ", gsub("[^a-zA-Z_]", "", rownames_vec)), 'Telehealth')
abortion_df <- data.frame(abortion_matrix)
abortion_df <- abortion_df[rownames(abortion_df) != 'All US',]
abortion_df$State <- rownames(abortion_df)
abortion_df <- reshape2::melt(abortion_df, id.vars = 'State')
abortion_df$Year <- as.numeric(paste0("20", gsub('.*_', '', abortion_df$variable)))
abortion_df$Month <- gsub('_.*', '', abortion_df$variable)
abortion_df$variable <- NULL
abortion_df <- dplyr::rename(abortion_df, Abortions = value)

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
telehealth_addition <- new_data_month %>% dplyr::group_by(Month, Year) %>% dplyr::summarize(Population = sum(Population))
telehealth_addition$State = 'Telehealth'
new_data_month <- rbind(new_data_month, telehealth_addition)

# Join populations and abortion counts
joined_df <- left_join(abortion_df, new_data_month, by=c("State", "Year", "Month"))
joined_df$AbortionsPerThousandWomen <- joined_df$Abortions / joined_df$Population * 1000

month_num <- match(joined_df$Month, month.abb)
joined_df$Date <- as.Date(paste(joined_df$Year, month_num, "01", sep = "-"))

joined_df[joined_df$State == 'Telehealth',] %>%
  mutate(label = if_else(Date == max(Date), as.character(State), NA_character_)) %>%
  ggplot(aes(x = Date, y = AbortionsPerThousandWomen, group = State, colour = State)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   max.overlaps = 50)

joined_df$Date <- NULL
write.csv(joined_df, 'data/abortion_rates.csv', row.names = F)








