# Load necessary libraries
library(simr)
library(lme4)
library(lmerTest)
library(dplyr)


# Data simulation function
simulate_data <- function(n_subjects = 50, n_items = 18) {
  
  # Define fixed effects
  intercept <- 500  # Overall intercept
  b_modality <- -20  # Effect of modality (visual vs. olfactory)
  b_time <- -10 # Effect of time (t2 vs. t1)
  b_interaction <- 5 # Interaction effect
  
  # Define random effects standard deviations
  sd_subject_intercept <- 20
  sd_subject_time <- 12
  sd_item_intercept <- 15
  sd_item_modality <- 20
  sd_item_time <- 10
  
  # Create subjects and items
  subjects_olf <- 1:(n_subjects/2)
  subjects_vis <- (n_subjects/2+1):n_subjects
  items <- 1:n_items
  
  # Generate random effects (u and v)
  u0 <- rnorm(n_subjects, 0, sd_subject_intercept)
  u_time <- rnorm(n_subjects, 0, sd_subject_time)
  v0 <- rnorm(n_items, 0, sd_item_intercept)
  v_modality <- rnorm(n_items, 0, sd_item_modality)
  v_time <- rnorm(n_items, 0, sd_item_time)
  
  # Create the data frame
  data_olfactory <- expand.grid(Subject = subjects_olf, Item = items, Modality = -0.5, Time = c(-0.5, 0.5))
  data_visual <- expand.grid(Subject = subjects_vis, Item = items, Modality = 0.5, Time = c(-0.5, 0.5))
  
  data <- rbind(data_olfactory, data_visual)
  data$rt <- NA 
  
  sigma <- 10
  
  for (i in 1:nrow(data)) {
    mu <- intercept + u0[data$Subject[i]] + v0[data$Item[i]] + # Intercept
      (b_modality + v_modality[data$Item[i]]) * data$Modality[i] + # Modality
      (b_time + u_time[data$Subject[i]] + v_time[data$Item[i]]) * data$Time[i] + # Time
      b_interaction * data$Modality[i] * data$Time[i] # Interaction
    data$rt[i] <- rnorm(1, mu, sigma)
  }
  data$logrt <- log(data$rt)
  
  return(data)
}

simdat <- simulate_data(50, 18)

aggregate(rt ~ Time + Modality, data = simdat, FUN = mean)

# Check variable names
model <- lmer(rt ~ Modality*Time + (1 + Time || Subject) + (1 + Modality + Time || Item), data = simdat)
summary(model)

# Function to perform one simulation and analysis
run_simulation <- function(n_subjects = 50, n_items = 18) {
  data <- simulate_data(n_subjects, n_items)
  model <- lmer(rt ~ Modality*Time + (1 + Time || Subject) + (1 + Modality + Time || Item), data = data)
  return(summary(model)$coefficients)
}

# Function to estimate power for a given sample size
estimate_power <- function(n_subjects = 50, n_items = 18, n_simulations = 10, alpha = 0.05) {
  coefficients <- list()
  p_value_modality <- c()
  p_value_time <- c()
  p_value_interaction <- c()
  for (i in 1:n_simulations) {
    coefficients[[i]] <- run_simulation(n_subjects, n_items)
    p_value_modality[i] <- coefficients[[i]]["Modality", "Pr(>|t|)"]
    p_value_time[i] <- coefficients[[i]]["Time", "Pr(>|t|)"]
    p_value_interaction[i] <- coefficients[[i]]["Modality:Time", "Pr(>|t|)"]
  }
  p_values <- list(p_value_modality,
                   p_value_time,
                   p_value_interaction)

  power <- c(mean(p_values[[1]] < alpha),
             mean(p_values[[2]] < alpha),
             mean(p_values[[3]] < alpha))
  return(power)
}

power_res <- estimate_power(30, 18, 100, 0.05)


## Try with multiple participant numbers

# Define sample sizes to test
sample_sizes <- seq(20, 200, by = 20)

# Estimate power for each sample size
power_results <- sapply(sample_sizes, estimate_power)

# Combine results into a data frame
power_df <- data.frame(sample_size = sample_sizes,
                       power = c(power_results),
                       effect = rep(c("Modality", "Time", "Interaction"),
                                    times = length(sample_sizes)))

# Print results
print(power_df)

# Plot power curve
plot(sample_sizes,
     power_df$power[power_df$effect == "Modality"], 
     xlab = "Sample Size", ylab = "Power",
     type = "l", lwd = 2, col = "red", ylim = c(0, 1),
     main = "Power Analysis for Experimental Design")
lines(sample_sizes,
      power_df$power[power_df$effect == "Time"],
      col = "blue", lty = 3, lwd = 2)
lines(sample_sizes,
      power_df$power[power_df$effect == "Interaction"],
      col = "orange", lty = 4, lwd = 2)
abline(h = 0.80, lty = 2)
legend("bottomright", legend = c("Modality", "Time", "Interaction"),
       col = c("red", "blue", "orange"), lwd = 2, lty = c(1, 3, 4))


