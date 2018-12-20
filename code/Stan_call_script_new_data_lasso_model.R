################################################################################
#                                                                              #
# Purpose:       Stan Implementation File  - Sweden School Fires               #
#                                                                              #
# Author:        Mark Kurzeja                                                  #
# Contact:       mtkurzej@umich.edu                                            #
# Client:        Mark Kurzeja                                                  #
#                                                                              #
# Code created:  2018-04-04                                                    #
# Last updated:  2018-04-24                                                    #
#                                                                              #
# Comment:       A file responsible for getting the stan models up and running #
#                                                                              #
################################################################################

rm(list = ls())
setwd("C:/Users/Mark k/Dropbox/Graduate School/05) Courses/Stats 551/551FinalProject/data")
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggcorrplot)
library(GGally)
library(rstan)
options(mc.cores = 8)
rstan_options(auto_write = TRUE)

################################################################################
#                                                                              #
#                                  Data Prep                                   #
#                                                                              #
################################################################################

# Read in the data
mdat <- readxl::read_xlsx("./yearly_joint_data_clean.xlsx")
mdat_full <- mdat

# Remove some of the other columns
varsToRemove <- c("municipality_name", "Year_id", "municipalityType_id", 
				  "municipalityTypeBroad_id", "governing_id")
mdat[,varsToRemove] <- NULL

# Convert Chars to Factors:
mdat$municipalityType %<>% as.factor()
mdat$municipalityTypeBroad %<>% as.factor()
mdat$governing %<>% as.factor()
mdat$municipality_id %<>% as.factor()
mdat$Year %<>% as.factor()

# Scale some of the factors that we are going to use - specify the ones we do not want to scale
mdat[, colnames(mdat) %>% setdiff(c("municipality_id", "Year", 
                                    "municipalityType", "municipalityTypeBroad", 
									"governing", "Fires"))] %<>% scale()

# FOR DEV ONLY - Downsample data to make model run faster
# mdat <- mdat %>% sample_n(200)

################################################################################
#                                                                              #
#                            Build the Stan Object                             #
#                                                                              #
################################################################################

# Grab the fires variable - it disappears after we make the model matrix
Fires <- mdat$Fires

# Make the model matrix to extract the data from:
mdat <- model.matrix(Fires ~., data = mdat)

# Ensure that it is a dataframe so that we can work with the dplyr functions
mdat %<>% data.frame(check.names = F)

# Get the factor matrix of the munipalities
muni_matrix <- mdat %>% dplyr::select(`(Intercept)`, dplyr::contains("municipality_id"))
mdat_temp <- mdat[,setdiff(colnames(mdat), colnames(muni_matrix))]

# Get the factor matrix of the years
year_matrix <- mdat %>% dplyr::select(contains("Year"))
mdat_temp <- mdat_temp[,setdiff(colnames(mdat_temp), colnames(year_matrix))]

# Get the stan object as we need it...
stan_pass <- list(
  nobs = nrow(mdat),
  n_muni = ncol(muni_matrix),
  munis = muni_matrix,
  n_preds = ncol(mdat_temp),
  preds = mdat_temp,
  n_years = ncol(year_matrix),
  years = year_matrix,
  fires = Fires
)

################################################################################
#                                                                              #
#                             Run the Stan Object                              #
#                                                                              #
################################################################################

if(FALSE) {
# Run and save the model
mmod_doublexp <- stan(file = "../code/swed_fires_model_six_new_data_doubleexp.stan", data = stan_pass, 
                      chains = 4, iter = 1200, 
                      warmup = 600, thin = 1, refresh = 1200, 
                      # control = list(max_treedepth = 15), 
                      verbose = F, pars = c("betas", "beta_muni", "beta_year", 
								 "beta_year_sd", "beta_muni_sd", "beta_null"))
  save(mmod_doublexp, file = "../code/full_model_inference_exp.model")
} else {
  load("../code/full_model_inference_exp.model")
}

############################
# shinystan::launch_shinystan(mmod_doublexp)


################################################################################
#                                                                              #
#                                 Data Export                                  #
#                                                                              #
################################################################################
if (FALSE) {
  dd <- extract(mmod_doublexp) %>% data.frame()
  
  result <- list()
  
  # Get the muni_vars
  result[["munis"]] <- dd %>% dplyr::select(contains("beta_muni."))
  colnames(result[["munis"]]) <- c("Intercept", sprintf("Muni.%i", 2:290))
  
  # Get the years vars
  result[["years"]] <- dd %>% dplyr::select(contains("beta_year."))
  colnames(result[["years"]]) <- as.character(1999:2014)
  
  # Get the coefficients
  result[["betas"]] <- dd %>% dplyr::select(contains("betas"))
  colnames(result[["betas"]]) <- colnames(stan_pass$preds)
  
  # Get the last of the parameters
  result[["last"]] <- dd[,c("beta_year_sd", "beta_muni_sd", "beta_null")]
  
  dplyr::bind_cols(result) %>% 
    write.csv("./stan_output_table_exp.csv", row.names = F)
}

################################################################################
#                                                                              #
#                      Simulate the Posterior Predictive                       #
#                                                                              #
################################################################################
dd <- extract(mmod_doublexp) %>% data.frame()

# Matrix multiply two dataframes
mmult <- function(x,y) {
  as.matrix(x) %*% t(as.matrix(y))
}

# This is the function that generates posterior predictions for each of the parameters
post_pred_values <- function(dat_row, n_samp = 200) {
  # Get the data that we are working with
  y = stan_pass$years[dat_row, ]
  m = stan_pass$munis[dat_row, ]
  p = stan_pass$preds[dat_row, ]
  
  year_mat <- dd[, grep(x = colnames(dd), pattern = "beta_year.", fixed = T)]
  muni_mat <- dd[, grep(x = colnames(dd), pattern = "beta_muni.", fixed = T)]
  pred_mat <- dd[, grep(x = colnames(dd), pattern = "betas", fixed = T)]
  beta_null_mat <- dd[, grep(x = colnames(dd), pattern = "beta_null", fixed = T)]
  
  row_choices <- sample(1:nrow(dd), n_samp, replace = T)

  k <- mmult(year_mat[row_choices,], y) + 
    mmult(muni_mat[row_choices,], m) + 
    mmult(pred_mat[row_choices,], p) + as.matrix(beta_null_mat[row_choices], ncol = 1)
  data.frame(yhat = k %>% as.numeric %>% exp %>% rpois(n = length(.) * 10, lambda = .))
}

################################################################################
#                                                                              #
#                                Visualizations                                #
#                                                                              #
################################################################################
 
# --------------------- Plotting the posterior predictive ----------------------
myggsave <- function(name, w = 6, h = 4) {
  ggsave(filename = sprintf("../fig/%s.pdf", name), device = "pdf", width = w, height = h)
}

plotss <- plyr::ldply(seq_len(stan_pass$nobs), function(i) {
  v = post_pred_values(i)
  data.frame(observation_id = i, actual = Fires[i], yhat = v)
}, .progress = plyr::progress_win())

# First save down this dataframe for future :)
if (FALSE) {
  # plotss %>% write.csv("./post_pred_exp.csv", row.names = F)
  save(plotss, file = "../code/plotss_exp.data")
}
load("../code/plotss_exp.data")

# ---------- Plot the confidence intervals for each of the parameters ----------
library(HDInterval)
plotss %>% group_by(actual) %>% 
  summarise(lower = hdi(yhat, credMass = 0.98)[1], 
            upper = hdi(yhat, credMass = 0.98)[2], 
            lowermid = hdi(yhat, credMass = 0.80)[1], 
            uppermid = hdi(yhat, credMass = 0.80)[2],
            count = n() / 2000) %>%
  mutate(pers = count / sum(count), lable = sprintf("%i (%.2f%%)", count, pers * 100)) %>% 
  ungroup() %>% 
  ggplot(.) +
  geom_segment(aes(x = actual, xend = actual, y = lower, yend = upper)) +
  geom_segment(aes(x = actual, xend = actual, y = lowermid, yend = uppermid), size = 1.5, color = "red") +
  geom_abline(intercept = 0, slope= 1, color = "blue") +
  geom_text(aes(actual, upper, label = lable, angle = 90), nudge_y = 10) +
  labs(x = "Actual Number of Fires", y = "Predicted Number of Fires..") +
  ggtitle("80% | 98% Posterior Predictive Credible Intervals", "80% CIs are red, and 98% CIs are black")
myggsave("CI_post_pred_intervals", w = 10, h = 4)

# ---------------- Plot the posterior coverage for First 20 Obs ----------------
plotss %>% filter(observation_id %in% seq(979,length.out = 16)) %>% 
  ggplot(.) +
  geom_bar(aes(yhat)) + 
  facet_wrap(~observation_id, scales = "free", strip.position = "left") +
  geom_vline(aes(xintercept = actual), color = "blue") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(x = "Posterior Predictive Density", y = "Observation ID") + 
  ggtitle("Flexibility of Posterior Predictions for High and Low Observations")
myggsave("post_pred_samples", w = 6, h = 4)

# ----------------------- Predictions for an active muni -----------------------
obsnum = 58
plotss2 <- plyr::ldply(which(mdat_full$municipality_id == obsnum), function(i) {
  v = post_pred_values(i, 5000)
  data.frame(observation_id = i, actual = Fires[i], yhat = v)
}, .progress = plyr::progress_win())

ys <- data.frame(observation_id = which(mdat_full$municipality_id == obsnum), years = 1998:2014 )
plotss2 %>% 
  filter(observation_id == which(mdat_full$municipality_id == obsnum)) %>% 
  left_join(., ys) %>% group_by(years) %>%
  summarize(lower = quantile(yhat, probs = 0.01), 
            upper = quantile(yhat, probs = 0.99), 
            actual = mean(actual),
            mean = mean(yhat)) %>%
  ggplot(.) +
  scale_y_continuous(limits = c(0,55)) +
  geom_point(aes(years, actual)) + 
  geom_line(aes(years, mean), color = "blue") + 
  geom_ribbon(aes(x = years, ymin = lower, ymax = upper), 
              alpha = 0.5, fill = RColorBrewer::brewer.pal(3, "Blues")[3]) + 
  labs(x = "Posterior Predictive Density", y = "Fires") + 
  ggtitle(sprintf("95%% CI for Posterior Predictions for Municipality %i", obsnum))
myggsave("post_pred_samples_ribbon_high", w = 8, h = 3)

# ------------------------ Predictions for a dead muni -------------------------
obsnum = 55
plotss2 <- plyr::ldply(which(mdat_full$municipality_id == obsnum), function(i) {
  v = post_pred_values(i, 2000)
  data.frame(observation_id = i, actual = Fires[i], yhat = v)
}, .progress = plyr::progress_win())

ys <- data.frame(observation_id = which(mdat_full$municipality_id == obsnum), years = 1998:2014 )
plotss2 %>% 
  filter(observation_id == which(mdat_full$municipality_id == obsnum)) %>% 
  left_join(., ys) %>% group_by(years) %>%
  summarize(lower = quantile(yhat, probs = 0.025, type = 2), 
            upper = quantile(yhat, probs = 0.975, type = 2), 
            actual = mean(actual),
            mean = mean(yhat)) %>%
  ggplot(.) +
  geom_point(aes(years, actual)) + 
  geom_line(aes(years, mean), color = "blue") + 
  geom_ribbon(aes(x = years, ymin = lower, ymax = upper), 
              alpha = 0.5, fill = RColorBrewer::brewer.pal(3, "Blues")[3]) + 
  labs(x = "Posterior Predictive Density", y = "Fires") + 
  ggtitle(sprintf("95%% CI for Posterior Predictions for Municipality %i", obsnum))
myggsave("post_pred_samples_ribbon_low", w = 8, h = 3)

# ----------------------- Plotting the Year Multipliers ------------------------
yy <- dd[,c("beta_year.1", "beta_year.2", 
            "beta_year.3", "beta_year.4", "beta_year.5", "beta_year.6", "beta_year.7", 
            "beta_year.8", "beta_year.9", "beta_year.10", "beta_year.11", 
            "beta_year.12", "beta_year.13", "beta_year.14", "beta_year.15", 
            "beta_year.16")]

colnames(yy) <- c(1999:2014)

yy %>% 
  tidyr::gather(factor_key = T) %>% 
  mutate(value = exp(value)) %>% group_by(key) %>%
  summarize(lower = quantile(value, probs = 0.05), 
            upper = quantile(value, probs = 0.95),
            median = median(value)) %>%
  mutate(key = factor(key, levels = rev(levels(key)))) %>%
  ggplot(.) +
  geom_point(aes(median, key), color = "blue") +
  geom_vline(xintercept = 1, color = "blue", alpha = 0.5) +
  geom_segment(aes(x = lower, xend = upper, y = key, yend = key)) + 
  labs(x = "Mean Adjustment Multiplier (Reference = 1998)", y = "Year") + 
  ggtitle("90% CI for Multiplier for Mean Fires by Year")
myggsave("year_multiplier", w = 8, h = 4)

# ----------------------- Plotting the Beta Multipliers ------------------------
dd <- extract(mmod_doublexp) %>% data.frame()

# Get the coefficients
bb <- dd %>% dplyr::select(contains("betas"))
colnames(bb) <- colnames(stan_pass$preds)

# Plot
bb %>% 
  head() %>% 
  tidyr::gather() %>%
  mutate(value = exp(value)) %>% 
  group_by(key) %>% 
  summarize(lower = quantile(value, probs = 0.05), 
            upper = quantile(value, probs = 0.95),
            median = median(value)) %>%
  ggplot(.) + 
  geom_vline(xintercept = 1, col = "red", alpha = 0.5) + 
  geom_segment(aes(x = lower, xend = upper, y = key, yend = key)) + 
  geom_point(aes(median, key), color = "blue") + 
  labs(x = "Multiplier", y = "") +
  ggtitle("90% CI for Multiplier for Mean Fires by Predictor")
  myggsave("beta_multiplier", w = 10, h = 4)  

# ------------------------- Plot Some Munis Fire Rates -------------------------
mdat_full %>% dplyr::filter(municipality_id %in% c(1,5, 35, 104)) %>% 
    dplyr::select(municipality_id, Fires, Year) %>% 
  ggplot(data = ., aes(x = Year, y = Fires)) + 
  geom_bar(stat = "identity", fill = RColorBrewer::brewer.pal(3, "Reds")[3]) + 
  facet_grid(municipality_id~.) +
  theme(legend.position = "none") +
  ggtitle("Fires Per Year in Selected Municipalities")
myggsave("FiresPerMuni",8)

# ---------------------------- Fires Per Year Plot -----------------------------
mdat_full %>% 
  dplyr::select(Fires, Year) %>% 
  group_by(Year) %>% 
  summarize(Fires = sum(Fires)) %>% 
  ggplot(data = ., aes(x = Year, y = Fires)) + 
  geom_bar(stat = "identity", fill = RColorBrewer::brewer.pal(3, "Reds")[3]) + 
  geom_text(aes(Year, Fires, label = Fires),nudge_y = 6) + 
  scale_x_discrete(limits = 1998:2014) +
  theme(legend.position = "none") +
  ggtitle("School Fires Per Year Across Sweden")
myggsave("FiresPerYear", 8)

# ---------------- Plot the color tiles for the number of fires ----------------
mdat_full %>% 
  dplyr::select(municipality_id, Fires, Year) %>% 
  mutate(Fires = as.factor(cut(Fires, breaks = c(0,1,2,3, 4, 5,10, 50), right = F))) %>% 
  ggplot(data = ., aes(x = municipality_id, y = Year)) + 
  geom_tile(aes(fill = Fires)) + 
  scale_fill_brewer(palette = "Blues") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.border = element_blank()) + 
  theme(axis.ticks.x  = element_blank()) + 
  theme(axis.text.x  = element_blank()) + 
  scale_y_continuous(limits = c(1998,2014), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Fires Per Year in All Municipalities") + 
  labs(x = "Municipality ID", y = "Year")
myggsave("FiresPerMuni_tile", 8)

########################## Get the correlation matrix ##########################

mc <- mdat[,c("Foreign_Born_Share", "Gini_Coefficient", 
        "Median_Income", "Population", "Share_65+", "Share_Of_Voters_Who_Voted_Local", 
        "Share_Of_Voters_Who_Voted_National", "Unemployment", "Youth_Unemployment", 
        "foretagsklimatRanking",  "urbanDegree", "asylumCosts", 
         "refugees", 
        "rentalApartments", "snowmobiles", "cars", "tractors", "motorcycles", 
        "fokusRanking", "Fires")] %>% cor()


cormat <- round(cor(mc),1)
ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           lab = TRUE, ggtheme = ggplot2::theme_gray, title = "Correlation Plot")
myggsave("corr_plot", 10, 10)




