birth_rates <- read.csv('data/birth_rates.csv')
treatments <- read.csv('data/birth_treatments.csv')

combined_df <- left_join(birth_rates, treatments, by=c("State", "Month", "Year"))

month_num <- match(combined_df$Month, month.abb)
combined_df$Date <- as.Date(paste(combined_df$Year, month_num, "01", sep = "-"))
combined_df$TreatedOriginal <- combined_df$Treated

custom_palette <- c("Control" = "blue", "Neighbor" = "darkgreen", "Ban" = "red")
combined_df_for_plot <- combined_df[order(combined_df$Date, decreasing = TRUE), ] %>%
  mutate(label = if_else(Date == max(Date), as.character(State), NA_character_))
plot1 <- ggplot(combined_df_for_plot, aes(x = Date, y = BirthsPerThousandWomen, group = State, color = TreatedOriginal)) + 
  geom_path() + 
  theme_bw() + 
  scale_color_manual(values = custom_palette, labels = c('Ban', 'Control', 'Neighbor')) + 
  labs(x = 'Month', y = TeX("Births per 1000 women age 15-49"), color = '') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 20),
        legend.position = 'right',
        legend.direction = 'vertical') + 
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "black") + 
  geom_vline(xintercept = as.Date("2022-12-01"), linetype = "dashed", color = "black") + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   max.overlaps = 6)

ggsave('figures/fig_2a.png', plot1, width = 12, height = 6, dpi = 1000)

combined_df$Treated <- ifelse(combined_df$Treated == 'Control', 0, 1)
combined_df$Month <- as.integer((combined_df$Year - 2020) * 12 + as.integer(mapvalues(combined_df$Month, month.abb, 1:12)))
combined_df$eventually_treated <- combined_df$State %in% unique(combined_df$State[combined_df$TreatedOriginal == 'Ban' | combined_df$TreatedOriginal == 'Neighbor'])

# From Stat 286 final
# Create groups based on when the ban first went into effect
combined_df$group_id <- 0
group_counter <- 0
for (month in combined_df$Month) {
  group_counter_update <- TRUE
  if (month == 1) {
    next
  }
  for (state in combined_df$State[combined_df$Month == month]) {
    if (combined_df$Treated[combined_df$State == state & combined_df$Month == month] == 1 & 
        combined_df$Treated[combined_df$State == state & combined_df$Month == month - 1] == 0) {
      if (group_counter_update == TRUE) {
        group_counter <- group_counter + 1
      }
      if (all(combined_df$group_id[combined_df$State == state] == 0)) {
        combined_df$group_id[combined_df$State == state] <- group_counter
      }
      group_counter_update <- FALSE
    }
  }
}
combined_df$group_id <- mapvalues(combined_df$group_id, sort(unique(combined_df$group_id)), 0:(length(unique(combined_df$group_id)) - 1))
combined_df$group_count <- mapvalues(combined_df$group_id, 
                                     as.numeric(names(table(combined_df$group_id[combined_df$Month == 1]))), 
                                     as.numeric(table(combined_df$group_id[combined_df$Month == 1])))

combined_df$Stateid <- as.numeric(as.factor(combined_df$State))
N <- length(unique(combined_df$Stateid))

# Prepare storage vectors
deltas <- matrix(0, nrow = N, ncol = N) # Each column is a delta_i vector
xi <- vector(length = N)

# Turn the full dataframe into a dataframe where columns are states and rows are Months
# cast_df_full is T x N with entries corresponding to expenditures
melted_df_full <- combined_df[, c("Stateid", "BirthsPerThousandWomen", "Month")]
cast_df_full <- reshape2::dcast(melted_df_full, Month ~ Stateid, value.var = 'BirthsPerThousandWomen')
cast_df_full <- cast_df_full[order(cast_df_full$Month),]
rownames(cast_df_full) <- cast_df_full$Month
cast_df_full$Month <- NULL

# Find fit parameters for each state
for (i in sort(unique(combined_df$Stateid))) {
  # Subset only the observations from Months before T_g, consistent with the minimizing
  # equation
  if (all(combined_df$Treated[combined_df$Stateid == i] == 0)) {
    combined_df_sub <- combined_df
  } else {
    combined_df_sub <- combined_df[combined_df$Month < min(combined_df$Month[combined_df$Stateid == i & 
                                                                               combined_df$Treated == 1]),]
  }
  
  # Get the observed Y_it for the state of interest
  Yit <- combined_df_sub$BirthsPerThousandWomen[combined_df_sub$Stateid == i][
    order(combined_df_sub$Month[combined_df_sub$Stateid == i])]
  
  # Get the expenditures for control units and not-yet-treated units
  melted_df <- combined_df_sub[!combined_df_sub$eventually_treated, # == 0, #|
                               #combined_df_sub$group_id > unique(combined_df_sub[
                               #combined_df_sub$Stateid == i,]$group_id),
                               c("Stateid", "BirthsPerThousandWomen", "Month")]
  
  # Create the table of control expenditures (# control states x # Months)
  cast_df <- reshape2::dcast(melted_df,Month ~ Stateid, value.var = 'BirthsPerThousandWomen')
  cast_df <- cast_df[order(cast_df$Month),]
  cast_df$Month <- NULL
  cast_df$Yit <- Yit
  
  # Fit the ridge model and store the coefficients
  ridge_fit <- linearRidge(Yit ~ ., cast_df) # Seems pretty robust but needs to be set
  fit_coefs <- coef(ridge_fit)
  xi[i] <- fit_coefs[1]
  
  # If the state isn't included, a 0 is stored for its coefficient
  deltas[as.numeric(gsub("`", "", names(fit_coefs)[-1])),i] <- fit_coefs[-1]
}

# Months by states
prediction_mat <- matrix(rep(xi, dim(cast_df_full)[1]), 
                         nrow = dim(cast_df_full)[1], 
                         ncol = dim(cast_df_full)[2], byrow = T) + 
  as.matrix(cast_df_full) %*% deltas

# Can't have negative births
prediction_mat[prediction_mat < 0] <- 0

tau_mat <- data.frame(cast_df_full - prediction_mat)

colnames(tau_mat) <- mapvalues(gsub("X", "", colnames(tau_mat)), combined_df$Stateid, combined_df$State, warn_missing = F)
tau_mat$Month <- rownames(tau_mat)
tau_df <- reshape2::melt(tau_mat, id.vars = 'Month')
colnames(tau_df) <- c("Month", "State", "Tau")
tau_df$Month <- as.numeric(tau_df$Month)
combined_df <- left_join(combined_df, tau_df, by= c('Month', 'State'))

combined_df_for_plot <- combined_df[order(combined_df$Date, decreasing = TRUE), ] %>%
  mutate(label = if_else(Date == max(Date), as.character(State), NA_character_))
plot2 <- ggplot(combined_df_for_plot, aes(x = Date, y = Tau, group = State, color = TreatedOriginal)) + 
  geom_path() + 
  theme_bw() + 
  scale_color_manual(values = custom_palette, labels = c('Ban', 'Control', 'Neighbor')) + 
  labs(x = 'Month', y = TeX("$\\tau$ (Change in births per 1000 women)"), color = '') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 20),
        legend.position = 'right',
        legend.direction = 'vertical') + 
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "black") + 
  geom_vline(xintercept = as.Date("2022-12-01"), linetype = "dashed", color = "black") + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   max.overlaps = 6)

ggsave('figures/fig_2b.png', plot2, width = 12, height = 7, dpi = 1000)

#summary(lm(BirthsPerThousandWomen ~ as.factor(State) + as.factor(Date) + Treated, combined_df))

sum((combined_df$Population / 1000 * combined_df$Tau)[combined_df$TreatedOriginal == 'Ban'])

output_df <- combined_df[,c('State', 'Year', 'Month', 'Population', 'BirthsPerThousandWomen', 'TreatedOriginal', 'Tau')]
output_df$Month <- (output_df$Month - 1) %% 12 + 1
write.csv(output_df, 'data/birth_diffs.csv', row.names = F)

#####################
# New bootstrapping #
#####################

set.seed(111)
nboot <- 1000
ban_effect <- vector(length = nboot)
for (B in 1:nboot) {
  new_states <- sample(unique(combined_df$State), 51, replace = T)
  
  combined_df_tmp <- data.frame(matrix(ncol = ncol(combined_df), nrow = 0))
  for (i in 1:length(new_states)) {
    new_state <- new_states[i]
    tmp_df <- combined_df[combined_df$State == new_state,]
    tmp_df$State <- paste0(tmp_df$State, i)
    tmp_df$Stateid <- i
    combined_df_tmp <- rbind(combined_df_tmp, tmp_df)
  }
  combined_df_tmp$Tau <- NULL
  
  N <- length(unique(combined_df_tmp$Stateid))
  
  # Prepare storage vectors
  deltas <- matrix(0, nrow = N, ncol = N) # Each column is a delta_i vector
  xi <- vector(length = N)
  
  # Turn the full dataframe into a dataframe where columns are states and rows are Months
  # cast_df_full is T x N with entries corresponding to expenditures
  melted_df_full <- combined_df_tmp[, c("Stateid", "BirthsPerThousandWomen", "Month")]
  cast_df_full <- reshape2::dcast(melted_df_full, Month ~ Stateid, value.var = 'BirthsPerThousandWomen')
  cast_df_full <- cast_df_full[order(cast_df_full$Month),]
  rownames(cast_df_full) <- cast_df_full$Month
  cast_df_full$Month <- NULL
  
  # Find fit parameters for each state
  for (i in sort(unique(combined_df_tmp$Stateid))) {
    # Subset only the observations from Months before T_g, consistent with the minimizing
    # equation
    if (all(combined_df_tmp$Treated[combined_df_tmp$Stateid == i] == 0)) {
      combined_df_sub <- combined_df_tmp #[combined_df$Month < 8,]
    } else {
      combined_df_sub <- combined_df_tmp[combined_df_tmp$Month < min(combined_df_tmp$Month[combined_df_tmp$Stateid == i & 
                                                                                             combined_df_tmp$Treated == 1]),]
    }
    
    # Get the observed Y_it for the state of interest
    Yit <- combined_df_sub$BirthsPerThousandWomen[combined_df_sub$Stateid == i][
      order(combined_df_sub$Month[combined_df_sub$Stateid == i])]
    
    # Get the expenditures for control units and not-yet-treated units
    melted_df <- combined_df_sub[!combined_df_sub$eventually_treated, #$group_id == 0, #|
                                 #combined_df_sub$group_id > unique(combined_df_sub[
                                 #combined_df_sub$Stateid == i,]$group_id),
                                 c("Stateid", "BirthsPerThousandWomen", "Month")]
    
    # Create the table of control expenditures (# control states x # Months)
    cast_df <- reshape2::dcast(melted_df,Month ~ Stateid, value.var = 'BirthsPerThousandWomen')
    cast_df <- cast_df[order(cast_df$Month),]
    cast_df$Month <- NULL
    cast_df$Yit <- Yit
    
    # Fit the ridge model and store the coefficients
    ridge_fit <- linearRidge(Yit ~ ., cast_df) # Seems pretty robust but needs to be set
    fit_coefs <- coef(ridge_fit)
    xi[i] <- fit_coefs[1]
    
    # If the state isn't included, a 0 is stored for its coefficient
    deltas[as.numeric(gsub("`", "", names(fit_coefs)[-1])),i] <- fit_coefs[-1]
  }
  
  # Months by states
  prediction_mat <- matrix(rep(xi, dim(cast_df_full)[1]), 
                           nrow = dim(cast_df_full)[1], 
                           ncol = dim(cast_df_full)[2], byrow = T) + 
    as.matrix(cast_df_full) %*% deltas
  
  # Can't have negative abortions
  prediction_mat[prediction_mat < 0] <- 0
  
  tau_mat <- data.frame(cast_df_full - prediction_mat)
  
  colnames(tau_mat) <- mapvalues(gsub("X", "", colnames(tau_mat)), combined_df_tmp$Stateid, combined_df_tmp$State, warn_missing = F)
  tau_mat$Month <- rownames(tau_mat)
  tau_df <- reshape2::melt(tau_mat, id.vars = 'Month')
  colnames(tau_df) <- c("Month", "State", "Tau")
  tau_df$Month <- as.numeric(tau_df$Month)
  combined_df_tmp <- left_join(combined_df_tmp, tau_df, by= c('Month', 'State'))
  
  ban_effect[B] <- sum((combined_df$Population / 1000 * combined_df_tmp$Tau)[combined_df_tmp$TreatedOriginal == 'Ban'])

  print(B)
}

quantile(ban_effect, c(0.025, 0.975), na.rm=T)

hist(ban_effect[ban_effect!= 0])






























#################
# Bootstrapping #
#################

# nboot <- 1000
# null_outcomes <- vector(length = nboot)
# for (B in 1:nboot) {
#   combined_df_tmp <- combined_df
#   combined_df_tmp$Tau <- NULL
#   new_state_order <- sample(unique(combined_df_tmp$State))
#   new_outcomes <- combined_df_tmp[,c('State', 'Year', 'Month', 'BirthsPerThousandWomen')]
#   new_outcomes$State <- mapvalues(new_outcomes$State, sort(unique(combined_df_tmp$State)), new_state_order)
#   combined_df_tmp$BirthsPerThousandWomen <- NULL
#   combined_df_tmp <- left_join(combined_df_tmp, new_outcomes, by=c('State', 'Year', 'Month'))
#   
#   N <- length(unique(combined_df_tmp$Stateid))
#   
#   # Prepare storage vectors
#   deltas <- matrix(0, nrow = N, ncol = N) # Each column is a delta_i vector
#   xi <- vector(length = N)
#   
#   # Turn the full dataframe into a dataframe where columns are states and rows are Months
#   # cast_df_full is T x N with entries corresponding to expenditures
#   melted_df_full <- combined_df_tmp[, c("Stateid", "BirthsPerThousandWomen", "Month")]
#   cast_df_full <- reshape2::dcast(melted_df_full, Month ~ Stateid, value.var = 'BirthsPerThousandWomen')
#   cast_df_full <- cast_df_full[order(cast_df_full$Month),]
#   rownames(cast_df_full) <- cast_df_full$Month
#   cast_df_full$Month <- NULL
#   
#   # Find fit parameters for each state
#   for (i in sort(unique(combined_df_tmp$Stateid))) {
#     # Subset only the observations from Months before T_g, consistent with the minimizing
#     # equation
#     combined_df_sub <- combined_df_tmp[combined_df_tmp$Month < min(c(combined_df_tmp$Month[combined_df_tmp$Stateid == i & 
#                                                                                              combined_df_tmp$Treated == 1], max(combined_df_tmp$Month))),]
#     
#     # Get the observed Y_it for the state of interest
#     Yit <- combined_df_sub$BirthsPerThousandWomen[combined_df_sub$Stateid == i][
#       order(combined_df_sub$Month[combined_df_sub$Stateid == i])]
#     
#     # Get the expenditures for control units and not-yet-treated units
#     melted_df <- combined_df_sub[combined_df_sub$group_id == 0 |
#                                    combined_df_sub$group_id > unique(combined_df_sub[
#                                      combined_df_sub$Stateid == i,]$group_id),
#                                  c("Stateid", "BirthsPerThousandWomen", "Month")]
#     
#     # Create the table of control expenditures (# control states x # Months)
#     cast_df <- reshape2::dcast(melted_df,Month ~ Stateid, value.var = 'BirthsPerThousandWomen')
#     cast_df <- cast_df[order(cast_df$Month),]
#     cast_df$Month <- NULL
#     cast_df$Yit <- Yit
#     
#     # Fit the ridge model and store the coefficients
#     ridge_fit <- linearRidge(Yit ~ ., cast_df, lambda = 1) # Seems pretty robust but needs to be set
#     fit_coefs <- coef(ridge_fit)
#     xi[i] <- fit_coefs[1]
#     
#     # If the state isn't included, a 0 is stored for its coefficient
#     deltas[as.numeric(gsub("`", "", names(fit_coefs)[-1])),i] <- fit_coefs[-1]
#   }
#   
#   # Months by states
#   prediction_mat <- matrix(rep(xi, dim(cast_df_full)[1]), 
#                            nrow = dim(cast_df_full)[1], 
#                            ncol = dim(cast_df_full)[2], byrow = T) + 
#     as.matrix(cast_df_full) %*% deltas
#   
#   # Can't have negative births
#   prediction_mat[prediction_mat < 0] <- 0
#   
#   tau_mat <- data.frame(cast_df_full - prediction_mat)
#   
#   colnames(tau_mat) <- mapvalues(gsub("X", "", colnames(tau_mat)), combined_df_tmp$Stateid, combined_df_tmp$State, warn_missing = F)
#   tau_mat$Month <- rownames(tau_mat)
#   tau_df <- reshape2::melt(tau_mat, id.vars = 'Month')
#   colnames(tau_df) <- c("Month", "State", "Tau")
#   tau_df$Month <- as.numeric(tau_df$Month)
#   combined_df_tmp <- left_join(combined_df_tmp, tau_df, by= c('Month', 'State'))
#   
#   null_outcomes[B] <- sum((combined_df_tmp$Population / 1000 * combined_df_tmp$Tau)[combined_df_tmp$TreatedOriginal == 'Ban'])
# }
# 
# 
# 
# 
# 
# 
# 
