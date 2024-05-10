library(PanelMatch)

abortion_rates <- read.csv('data/abortion_rates.csv')
treatments <- read.csv('data/abortion_treatments.csv')

combined_df <- left_join(abortion_rates, treatments, by=c("State", "Month", "Year"))

month_num <- match(combined_df$Month, month.abb)
combined_df$Date <- as.Date(paste(combined_df$Year, month_num, "01", sep = "-"))

# linewidths <- combined_df$Population[!duplicated(combined_df$State)]
# linewidths <- linewidths / mean(linewidths)
# names(linewidths) <- combined_df$State[!duplicated(combined_df$State)]
ggplot(combined_df, aes(x = Date, y = AbortionsPerThousandWoman, group = State, colour = Treated)) + 
  geom_line()

combined_df$Treated <- ifelse(combined_df$Treated == 'Neighbor', 1, 0)
combined_df$Month <- as.integer((combined_df$Year - 2022) * 12 + as.integer(mapvalues(combined_df$Month, month.abb, 1:12)))
combined_df$State <- as.integer(as.factor(combined_df$State))
fake_state_1 <- combined_df[combined_df$State == 1,]
fake_state_1$AbortionsPerThousandWoman <- fake_state_1$Treated <- 0
fake_state_1$State <- 100
fake_state_2 <- combined_df[combined_df$State == 1,]
fake_state_2$AbortionsPerThousandWoman <- max(combined_df$AbortionsPerThousandWoman) * 2
fake_state_2$Treated <- 0
fake_state_2$State <- 101
combined_df <- rbind(combined_df, fake_state_1, fake_state_2)
combined_df$State <- as.integer(combined_df$State)

PM.results.pw <- PanelMatch(lag = 3, time.id = "Month", unit.id = "State", 
                              treatment = "Treated", refinement.method = 'CBPS.weight', 
                              data = combined_df, match.missing = TRUE, size.match = 20, 
                              qoi = "att", outcome.var = "AbortionsPerThousandWoman", lead = 0:11, 
                              covs.formula = ~ I(lag(AbortionsPerThousandWoman,1:3)),
                              forbid.treatment.reversal = FALSE, 
                              use.diagonal.variance.matrix = TRUE)

get_covariate_balance(PM.results.pw$att, data = combined_df, 
                      covariates = c("AbortionsPerThousandWoman"), plot = TRUE,
                      ylim = c(-1,1))

current_weight_scheme <- attr(PM.results.pw$att[[2]], "weights")
current_extracted_vals <- mapvalues(as.integer(names(current_weight_scheme)), combined_df[combined_df$Month == 7,]$State, 
          combined_df[combined_df$Month == 7,]$AbortionsPerThousandWoman, warn_missing = F)
sum(current_weight_scheme * current_extracted_vals)

PE.results <- PanelEstimate(sets = PM.results.pw, 
                            data = combined_df, se.method = "bootstrap", 
                            number.iterations = 1000, confidence.level = .95) 
# View the point estimates 
summary(PE.results)
plot(PE.results)

PM.results.maha<-PanelMatch(lag=4,time.id="year",unit.id="wbcode2", treatment="dem",refinement.method="ps.weight", 
                            data=dem,match.missing=TRUE, covs.formula=~I(lag(tradewb,1:4))+I(lag(y,1:4)), 
                            size.match =5,qoi="att",outcome.var="y", lead=0:4,forbid.treatment.reversal=FALSE, 
                            use.diagonal.variance.matrix=TRUE)

PM.results.maha$att[[12]]

###
# There seems to be a problem that all the weights are the same if two states have bans that go into effect at the same time
# Summing the weights times values doesn't seem to give something comparable to what the non-ban count should be

