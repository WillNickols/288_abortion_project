library(ggplot2)
library(scales)

# Sample data
dates <- seq(as.Date("2021-10-01"), by = "month", length.out = 18)

first_vals <- rnorm(length(dates), 1, 0.05)
df <- data.frame(Date = c(dates), 
                  untreated = first_vals,
                  real = first_vals,
                  type = c(rep('Control', 18)))


first_vals <- rnorm(length(dates)/2, 0.7, 0.05)
df2 <- data.frame(Date = c(dates), 
                 untreated = c(first_vals, 
                           rnorm(length(dates)/2, 0.7, 0.05)),
                 real = c(first_vals,
                                   abs(rnorm(length(dates)/2, 0.1, 0.05))),
                 type = c(rep('Ban', 18)))

first_vals <- rnorm(length(dates)/2, 1.1, 0.05)
df3 <- data.frame(Date = c(dates), 
                  untreated = c(first_vals, 
                           rnorm(length(dates)/2, 1.1, 0.05)),
                  real = c(first_vals,
                                rnorm(length(dates)/2, 1.5, 0.05)),
                  type = c(rep('Neighbor', 18)))

df <- rbind(df, df2, df3)

# Plot
ggplot(df, aes(x = Date, color = type, )) +
  geom_line(aes(y = real, linetype = "solid"), linewidth = 1) +
  geom_line(aes(y = untreated, linetype = "dotted"), linewidth = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b") +
  labs(x = "Month", y = "Abortions per 1000 women age 15-49", title = "") +
  scale_linetype_manual(name = NULL, values = c("solid" = "solid", "dotted" = "dotted"),
                        labels = c("Potential outcomes", "Observed outcomes")) +
  scale_color_manual(name = NULL, values = c("Control" = "blue", "Neighbor" = "darkgreen", "Ban" = "red"),
                        labels = c("Ban", "Control", "Neighbor")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 20),
        legend.position = 'bottom',
        legend.direction = 'vertical') + 
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "black")


dates <- seq(as.Date("2021-10-01"), by = "month", length.out = 18)

first_vals <- rnorm(length(dates), 2.8, 0.05)
df <- data.frame(Date = c(dates), 
                 untreated = first_vals,
                 real = first_vals,
                 type = c(rep('Control', 18)))


first_vals <- rnorm(15, 3.2, 0.05)
df2 <- data.frame(Date = c(dates), 
                  untreated = c(first_vals, 
                                rnorm(3, 3.2, 0.05)),
                  real = c(first_vals,
                           abs(rnorm(3, 3.5, 0.05))),
                  type = c(rep('Ban', 18)))

df <- rbind(df, df2)

# Plot
ggplot(df, aes(x = Date, color = type, )) +
  geom_line(aes(y = real, linetype = "solid"), linewidth = 1) +
  geom_line(aes(y = untreated, linetype = "dotted"), linewidth = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b") +
  scale_y_continuous(limits = c(0, 4)) +
  labs(x = "Month", y = "Births per 1000 women age 15-49", title = "") +
  scale_linetype_manual(name = NULL, values = c("solid" = "solid", "dotted" = "dotted"),
                        labels = c("Potential outcomes", "Observed outcomes")) +
  scale_color_manual(name = NULL, values = c("Control" = "blue", "Neighbor" = "darkgreen", "Ban" = "red"),
                     labels = c("Ban", "Control", "Neighbor")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 20),
        legend.position = 'bottom',
        legend.direction = 'vertical') + 
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "black") + 
  geom_vline(xintercept = as.Date("2022-12-01"), linetype = "dashed", color = "black")











