library(dplyr)
library(ggplot2)
library(ggrepel)

# Load the data
abortion_diffs <- read.csv('data/abortion_diffs.csv')
birth_diffs <- read.csv('data/birth_diffs.csv')

# Calculate actual counts from rates
abortion_diffs$AbortionCount <- abortion_diffs$AbortionsPerThousandWomen * abortion_diffs$Population / 1000
birth_diffs$BirthCount <- birth_diffs$BirthsPerThousandWomen * birth_diffs$Population / 1000

# Merge the data
merged_data <- full_join(
  abortion_diffs %>% select(State, Year, Month, AbortionCount, TreatedOriginal),
  birth_diffs %>% select(State, Year, Month, BirthCount),
  by = c("State", "Year", "Month")
)

# Create the plot
plot <- ggplot(merged_data, aes(x = AbortionCount, y = BirthCount, color = TreatedOriginal)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = c("Control" = "blue", "Neighbor" = "darkgreen", "Ban" = "red")) +
  theme_bw() +
  labs(
    x = "Monthly Abortion Count",
    y = "Monthly Birth Count",
    color = "State Type",
    title = "Relationship Between Monthly Abortion and Birth Counts",
    subtitle = "By State Type (2022-2023)"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

# Save the plot
ggsave('figures/birth_abortion_relationship.png', plot, width = 10, height = 8, dpi = 300)

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("Correlation between abortion and birth counts:", 
    cor(merged_data$AbortionCount, merged_data$BirthCount, use = "complete.obs"), "\n")

# Print regression results
model <- lm(BirthCount ~ AbortionCount + TreatedOriginal, data = merged_data)
cat("\nRegression Results:\n")
print(summary(model)) 