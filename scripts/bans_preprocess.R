library(reshape2)
library(stringr)
library(PanelMatch)

bans <- read.csv('data/bans.csv')
bans <- bans[order(bans$year * 12 + bans$month),]

# 4 years (2020-2023) of 12 months + 4 months of 2024
nrows <- 4 * 12 + 4
treated_status <- data.frame(matrix(nrow = length(unique(bans$state)), ncol = nrows))
rownames(treated_status) <- unique(bans$state)
bans$month_since_2020 <- (bans$year - 2020) * 12 + bans$month

for (i in 1:nrow(bans)) {
  treated_status[bans$state[i], bans$month_since_2020[i]:ncol(treated_status)] <- bans$ban_in[i]
}

treated_status_matrix <- treated_status
treated_status <- data.frame(treated_status)
colnames(treated_status) <- as.character(1:ncol(treated_status))
treated_status$State <- rownames(treated_status)
treated_status <- reshape2::melt(treated_status, id.vars = 'State')
colnames(treated_status) <- c("State", "Month", "Treated")
treated_status$Month <- as.integer(as.character(treated_status$Month))
treated_status <- treated_status[order(treated_status$State),]

plot1 <- DisplayTreatment(unit.id = "State", 
                          time.id = "Month", 
                          legend.position = "none", 
                          xlab = "Months since the beginning of 2020", 
                          ylab = "State", 
                          treatment = "Treated", data = treated_status, x.angle = 90, 
                          title = 'Treatment for abortion reporting', x.size = 7)
plot1

neighbors <- read.csv('data/neighbors.csv')

neighbor_states <- data.frame(matrix(0, nrow = length(unique(bans$state)), ncol = nrows))
rownames(neighbor_states) <- unique(bans$state)

for (i in 1:nrow(treated_status_matrix)) {
  for (j in 1:ncol(treated_status_matrix)) {
    if (treated_status_matrix[i,j] == 1) {
      current_neighbors <- unlist(str_split(neighbors$neighbors[neighbors$state == rownames(treated_status_matrix)[i]], ", "))
      neighbor_states[current_neighbors, j] <- 1
    }
  }
}

treated_and_neighbors <- pmax(neighbor_states, treated_status_matrix * 2)
treated_and_neighbors <- data.frame(treated_and_neighbors)
colnames(treated_and_neighbors) <- as.character(1:ncol(treated_and_neighbors))
treated_and_neighbors$State <- rownames(treated_and_neighbors)
treated_and_neighbors <- reshape2::melt(treated_and_neighbors, id.vars = 'State')
colnames(treated_and_neighbors) <- c("State", "Month", "Treated")
treated_and_neighbors$Month <- as.integer(as.character(treated_and_neighbors$Month))

telehealth_addition <- data.frame('State' = 'Telehealth', 'Month' = unique(treated_and_neighbors$Month))
telehealth_addition <- telehealth_addition[order(telehealth_addition$Month),]
telehealth_addition$Treated <- ifelse(telehealth_addition$Month %in% 
                                        unique(treated_and_neighbors$Month[treated_and_neighbors$Treated == 2]),
                                      1, 0)
treated_and_neighbors <- rbind(treated_and_neighbors, telehealth_addition)

treated_and_neighbors <- treated_and_neighbors[order(-treated_and_neighbors$Treated),]
treated_and_neighbors$State <- factor(treated_and_neighbors$State, levels = rev(unique(treated_and_neighbors$State)))
treated_and_neighbors <- treated_and_neighbors %>%
  mutate(
    Treated = case_when(
      Treated == 0 ~ "Control",
      Treated == 1 ~ "Neighbor",
      Treated == 2 ~ "Ban"
    )
  )

custom_palette <- c("Control" = "blue", "Neighbor" = "darkgreen", "Ban" = "red")
plot2 <- ggplot(treated_and_neighbors, aes(x = Month, y = State, fill = Treated)) +
  geom_tile(width = 0.9, height = 0.9) + 
  scale_fill_manual(values = custom_palette) + 
  theme(text = element_text(size = 20),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), panel.border = element_blank(),
        legend.position = "right",
        panel.background = element_blank(), 
        axis.text.x = element_text(angle=90, size = 10, vjust=0.5),
        axis.text.y = element_text(size = 10, angle = 0),
        plot.title = element_text(hjust = 0.5)) + 
  labs(y = '', x = 'Month from 2020')

plot2

treated_and_neighbors$Year <- 2020 + floor((treated_and_neighbors$Month - 1) / 12)
treated_and_neighbors$Month <- (treated_and_neighbors$Month - 1) %% 12 + 1
treated_and_neighbors$Month <- month.abb[treated_and_neighbors$Month]
write.csv(treated_and_neighbors, 'data/abortion_treatments.csv', row.names = F)

# Ban treatment effect on births: Treatment should take place 6 months after ban since most abortions were <12 weeks

bans <- read.csv('data/bans.csv')
bans$month[bans$year >= 2022] <- bans$month[bans$year >= 2022] + 6
bans$year <- ifelse(bans$month > 12, bans$year + 1, bans$year)
bans$month <- ifelse(bans$month > 12, bans$month - 12, bans$month)
bans <- bans[order(bans$year * 12 + bans$month),]

# 4 years (2020-2023) of 12 months + 4 months of 2024
nrows <- 4 * 12 + 4
treated_status <- data.frame(matrix(nrow = length(unique(bans$state)), ncol = nrows))
rownames(treated_status) <- unique(bans$state)
bans$month_since_2020 <- (bans$year - 2020) * 12 + bans$month

for (i in 1:nrow(bans)) {
  treated_status[bans$state[i], bans$month_since_2020[i]:ncol(treated_status)] <- bans$ban_in[i]
}

treated_status <- data.frame(treated_status)
colnames(treated_status) <- as.character(1:ncol(treated_status))
treated_status$State <- rownames(treated_status)
treated_status <- reshape2::melt(treated_status, id.vars = 'State')
colnames(treated_status) <- c("State", "Month", "Treated")
treated_status$Month <- as.integer(as.character(treated_status$Month))
treated_status <- treated_status[order(treated_status$State),]

treated_status$Year <- 2020 + floor((treated_status$Month - 1) / 12)
treated_status$Month <- (treated_status$Month - 1) %% 12 + 1
treated_status$Month <- month.abb[treated_status$Month]
treated_status$Treated <- ifelse(treated_status$Treated == 0, 'Control', 'Ban')
write.csv(treated_status, 'data/birth_treatments.csv', row.names = F)









