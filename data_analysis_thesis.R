library(lme4)
library(GGally)
library(ggplot2)
library("lavaan")
library(semPlot)
#install.packages("R.matlab")
library(R.matlab)
library(dplyr)
library(reshape2)
library(knitr)
library(kableExtra)
library(tidyr)
library(broom)
library(stringr)



### experiment 1
data <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/data1.csv')
mu1exp1 <- data$mu1
mu1exp1 <- as.data.frame(mu1exp1)
musexp1 <- data[c("mu1","mu2")]
value_counts <- table(data$choice)

# Print the counts
print(value_counts)

write.csv(musexp1, '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/musexp1.csv', row.names=TRUE)

### experiment 2
data2 <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/data2.csv')
musexp2 <- data2[c("mu1","mu2", "choice", "reward")]
data2$choice <- data2$choice - 1
value_counts <- table(data2$choice)

# Print the counts
print(value_counts)

write.csv(data2, '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/exp2data.csv', row.names=TRUE)

# data analysis

# Read the data from the CSV file (replace "data_file.csv" with the actual file name)
sim_results <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/experiment2.csv')

#calculate sd of relevant columns
cumulative_reward_sd <- sd(sim_results$cumulative_reward)
cumulative_optimal_reward_sd <- sd(sim_results$cumulative_optimal_reward)
cumulative_regret_sd <- sd(sim_results$cumulative_regret)

# Create a ggplot object on cumulative reward/regret with all lines and shaded 95% confidence intervals
ggplot(sim_results, aes(x = trial)) +
  geom_line(aes(y = cumulative_reward, color = "Cumulative Reward")) +
  geom_line(aes(y = cumulative_optimal_reward, color = "Cumulative Optimal Reward")) +
  geom_line(aes(y = cumulative_regret, color = "Cumulative Regret")) +
  geom_ribbon(aes(ymin = cumulative_reward - 1.96 * cumulative_reward_sd, 
                  ymax = cumulative_reward + 1.96 * cumulative_reward_sd,
                  fill = "Cumulative Reward"), alpha = 0.2) +
  geom_ribbon(aes(ymin = cumulative_optimal_reward - 1.96 * cumulative_optimal_reward_sd, 
                  ymax = cumulative_optimal_reward + 1.96 * cumulative_optimal_reward_sd,
                  fill = "Cumulative Optimal Reward"), alpha = 0.2) +
  geom_ribbon(aes(ymin = cumulative_regret - 1.96 * cumulative_regret_sd, 
                  ymax = cumulative_regret + 1.96 * cumulative_regret_sd,
                  fill = "Cumulative Regret"), alpha = 0.2) +
  labs(title = "Cumulative Rewards and Regret",
       x = "Trial Number",
       y = "Reward/Regret") +
  scale_color_manual(name = "Lines",
                     values = c("Cumulative Reward" = "blue",
                                "Cumulative Optimal Reward" = "green",
                                "Cumulative Regret" = "red")) +
  scale_fill_manual(name = "Confidence Intervals",
                    values = c("Cumulative Reward" = "lightblue",
                               "Cumulative Optimal Reward" = "lightgreen",
                               "Cumulative Regret" = "pink")) +
  theme_minimal()

# plot reward/regret
reward_sd <- sd(sim_results$reward)
optimal_reward_sd <- sd(sim_results$optimal_reward)
regret_sd <- sd(sim_results$regret)

# Create a ggplot object with all lines and shaded 95% confidence intervals
ggplot(sim_results, aes(x = trial)) +
  geom_line(aes(y = reward, color = "Reward")) +
  geom_line(aes(y = optimal_reward, color = "Optimal Reward")) +
  geom_line(aes(y = regret, color = "Regret")) +
  geom_ribbon(aes(ymin = reward - 1.96 * reward_sd, 
                  ymax = reward + 1.96 * reward_sd,
                  fill = "Reward"), alpha = 0.2) +
  geom_ribbon(aes(ymin = optimal_reward - 1.96 * optimal_reward_sd, 
                  ymax = optimal_reward + 1.96 * optimal_reward_sd,
                  fill = "Optimal Reward"), alpha = 0.2) +
  geom_ribbon(aes(ymin = regret - 1.96 * regret_sd, 
                  ymax = regret + 1.96 * regret_sd,
                  fill = "Regret"), alpha = 0.2) +
  labs(title = "Rewards and Regret",
       x = "Trial Number",
       y = "Reward/Regret") +
  scale_color_manual(name = "Lines",
                     values = c("Reward" = "blue",
                                "Optimal Reward" = "green",
                                "Regret" = "red")) +
  scale_fill_manual(name = "Confidence Intervals",
                    values = c("Reward" = "lightblue",
                               "Optimal Reward" = "lightgreen",
                               "Regret" = "pink")) +
  theme_minimal()

#### plot the means of arms ####
data2$count <- 1:nrow(data2)


# Create the line plot
ggplot(data2, aes(x = count)) +
  geom_smooth(aes(y = mu1, color = "Arm 1")) +
  geom_smooth(aes(y = mu2, color = "Arm 2")) +
  labs(title = "Mean Reward of Both Arms Over Trials",
       x = "Trial",
       y = "Mean",
       color = "Arm") +
  theme_minimal()

#### analyse the whole data for all hyperparameter values ####
#import the big dataframe
data <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/simulation_results-18.csv')



# Calculate the regret rate per 100 trials
data$trial_mod <- (data$trial - 1) %/% 100 + 1
regret_rates <- data %>%
  group_by(trial_mod, eta, decay) %>%
  summarize(cumulative_regret = sum(cumulative_regret),
            num_trials = n()) %>%
  mutate(regret_rate = cumulative_regret / num_trials)

# Calculate the mean and standard deviation of the regret rate for each eta and decay combination
regret_stats <- regret_rates %>%
  group_by(eta, decay) %>%
  summarize(mean_regret_rate = mean(regret_rate),
            sd_regret_rate = sd(regret_rate),
            num_trials = n())

# Convert data to long format for plotting
regret_stats_long <- melt(regret_stats, id.vars = c("eta", "decay", "num_trials"))

data$eta <- factor(data$eta)
data$decay <- factor(data$decay)

#### Plot the line plot ####
ggplot(data, aes(x = trial, y = regret, group = interaction(eta, decay), color = interaction(eta, decay))) +
  geom_smooth(se = FALSE) +
  #geom_ribbon(aes(ymin = mean_regret_rate - sd_regret_rate, ymax = mean_regret_rate + sd_regret_rate, fill = interaction(eta, decay)), alpha = 0.2) +
  labs(title = "Regret per number of trials per eta-decay value pair",
       x = "Number of Trials",
       y = "Regret",
       color = "eta and decay") +
  theme_minimal()

#### which eta decay pair has highest overall reward? ####
cumulative_rewards_by_eta_decay <- data %>%
  group_by(eta, decay) %>%
  summarize(cumulative_reward = sum(rewards))
cumulative_human_reward = sum(data2$reward)

# Print the cumulative reward for each eta-decay pair
print(cumulative_rewards_by_eta_decay, n = 121)

### beautiful plot that shows best overall reward for eta decay pairs
cumulative_rewards_by_eta_decay$eta = as.factor(cumulative_rewards_by_eta_decay$eta)
cumulative_rewards_by_eta_decay$decay = as.factor(cumulative_rewards_by_eta_decay$decay)

ggplot(cumulative_rewards_by_eta_decay, aes(x = eta, y = cumulative_reward, group = decay, color = decay)) +
  geom_line() +
  labs(title = "Cumulative Reward by Eta-Memory Pair",
       x = "Eta",
       y = "Cumulative Reward",
       color = "Memory") +
  theme_minimal()

#### mean regret of eta and decay levels seperately ####
data$eta = as.factor(data$eta)
data$decay = as.factor(data$decay)

### eta
df_eta <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/fixeddecayalleta.csv')
df_eta$eta = as.factor(df_eta$eta)
ggplot(df_eta, aes(x = trial, y = regret, colour = eta, group = eta)) +
  geom_smooth() +
  labs(title = "Regret by Eta",
       x = "trial",
       y = "Regret") +
  theme_minimal()

mean_regret_by_eta <- data %>%
  group_by(eta) %>%
  summarize(mean_regret = mean(regret))

ggplot(mean_regret_by_eta, aes(x = eta, y = mean_regret)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  labs(title = "Mean Regret by Eta",
       x = "Eta",
       y = "Mean Regret") +
  theme_minimal()

### decay
ggplot(data, aes(x = trial, y = regret, colour = decay, group = decay)) +
  geom_smooth() +
  labs(title = "Regret by Decay",
       x = "trial",
       y = "Regret") +
  theme_minimal()

mean_regret_by_decay <- data %>%
  group_by(decay) %>%
  summarize(mean_regret = mean(regret))

ggplot(mean_regret_by_decay, aes(x = decay, y = mean_regret)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  labs(title = "Mean Regret by Decay",
       x = "Decay",
       y = "Mean Regret") +
  theme_minimal()

######
eta_value <- 0.5
decay_value <- 0.1
filtered_data <- data %>%
  filter(eta == eta_value, decay == decay_value)

ggplot(filtered_data, aes(x = trial)) +
  geom_smooth(aes(y = posterior_mean_arm0, color = "Arm 1")) +
  geom_smooth(aes(y = posterior_mean_arm1, color = "Arm 2")) +
  #geom_point(aes(y = ifelse(chosen_arm == 0, posterior_mean_arm0, posterior_mean_arm1), color = factor(chosen_arm)), size = 3, shape = 21) +
  labs(title = paste("Estimated Posterior Mean for eta =", eta_value, "and decay =", decay_value),
       x = "Trial",
       y = "Estimated Posterior Mean") +
  #scale_color_manual(values = c("red", "blue"), labels = c("Arm 1", "Arm 2")) +
  theme_minimal()

# Compute the mean posterior mean of each arm
mean_posterior_mean_arm0 <- data %>%
  summarise(mean_posterior_mean_arm0 = mean(posterior_mean_arm0))

mean_posterior_mean_arm1 <- data %>%
  summarise(mean_posterior_mean_arm1 = mean(posterior_mean_arm1))

# Compute the mean true mean of each arm
mean_true_mean_arm0 <- data %>%
  summarise(mean_true_mean_arm0 = mean(true_dist_mean0))

mean_true_mean_arm1 <- data %>%
  summarise(mean_true_mean_arm1 = mean(true_dist_mean1))

# Combine the results into a single dataframe
result_summary <- cbind(mean_posterior_mean_arm0, mean_posterior_mean_arm1, mean_true_mean_arm0, mean_true_mean_arm1)

print(result_summary)

#### run t-tests ####
# Create an empty data frame to store the t-test results
t_test_results <- data.frame(eta = numeric(),
                             decay = numeric(),
                             p_value = numeric(),
                             significant = character())
human_choice <- musexp2$choice

# Loop through each unique eta-decay combination
for (eta_val in unique(data$eta)) {
  for (decay_val in unique(data$decay)) {
    # Subset data for the specific eta-decay combination
    subset_data <- data[data$eta == eta_val & data$decay == decay_val, ]
    chosen_arms <- subset_data$chosen_arm
    
    
    # Perform the t-test
    t_test_result <- t.test(human_choice, chosen_arms)
    
    # Store the results in the t_test_results data frame
    t_test_results <- rbind(t_test_results, 
                            data.frame(eta = eta_val,
                                       decay = decay_val,
                                       p_value = t_test_result$p.value,
                                       significant = ifelse(t_test_result$p.value < 0.05, "Yes", "No")))
  }
}

# Print the t-test results
print(t_test_results)

#### plot only significant eta decay combinations ####
# Define the significance level
significance_level <- 0.05

# Perform t-tests and create t_test_results data frame as described before

# Filter the t_test_results to include only significant correlations
significant_results <- t_test_results[t_test_results$p_value < significance_level, ]

# Subset the data to include only significant eta and decay combinations
significant_data <- data[data$eta %in% significant_results$eta & data$decay %in% significant_results$decay, ]

# Plot the significant data
ggplot(significant_data, aes(x = trial, y = regret, group = interaction(eta, decay), color = interaction(eta, decay))) +
  geom_smooth(se = FALSE) +
  labs(title = "Regret per number of trials per significant eta-decay value pair",
       x = "Number of Trials",
       y = "Regret Rate",
       color = "eta and decay") +
  theme_minimal()


#### the whole procedure again, but with chi-square tests ####
# Define the significance level
significance_level <- 0.05

# Perform chi-square tests and create chi_square_results data frame
chi_square_results <- data.frame(eta = numeric(),
                                 decay = numeric(),
                                 p_value = numeric(),
                                 significant = character())

# Loop through each unique eta-decay combination
for (eta_val in unique(data$eta)) {
  for (decay_val in unique(data$decay)) {
    # Subset data for the specific eta-decay combination
    subset_data <- data[data$eta == eta_val & data$decay == decay_val, ]
    
    # Create a 2x2 contingency table for chi-square test
    contingency_table <- table(human_choice, subset_data$chosen_arm)
    
    # Perform the chi-square test
    chi_square_result <- chisq.test(contingency_table)
    
    # Store the results in the chi_square_results data frame
    chi_square_results <- rbind(chi_square_results, 
                                data.frame(eta = eta_val,
                                           decay = decay_val,
                                           p_value = chi_square_result$p.value,
                                           significant = ifelse(chi_square_result$p.value < significance_level, "Yes", "No")))
  }
}

# Filter the chi_square_results to include only significant correlations
significant_data <- merge(data, chi_square_results, by = c("eta", "decay"))

# Subset the data to include only significant eta and decay combinations
#significant_data <- significant_data[significant_data$significant == "Yes", ]


# Plot the significant data
ggplot(significant_data, aes(x = trial, y = regret, group = interaction(eta, decay), color = interaction(eta, decay))) +
  geom_smooth(se = FALSE) +
  labs(title = "Regret per number of trials per significant eta-decay value pair",
       x = "Number of Trials",
       y = "Regret Rate",
       color = "eta and decay") +
  theme_minimal()

#### which eta-decay pairs lead to significant correlation of human and active inference choice? ####

# Create a binary variable to indicate significant correlations (1 for "Yes", 0 for "No")
data$significant_binary <- ifelse(significant_data$significant == "Yes", 1, 0)

# Perform logistic regression
logistic_model <- glm(significant_binary ~ eta * decay, data = data, family = binomial)

# Summarize the logistic regression results
summary(logistic_model)

# Extract the coefficients and p-values
coefficients <- tidy(logistic_model) %>%
  select(term, estimate, p.value)

# Print the results
print(coefficients)


#### likelihood fitting ####
kf_sm_lik <- function(par,data) {
  gamma <- par[1]
  choice <- data$deck
  reward <- data$payoff
  m <- kalman_filter(choice,reward,4,0,1000,16,16)$m
  p <- softmax_choice_prob(m,gamma)
  pchoice <- p[cbind(1:nrow(data),choice)]
  lik <- prod(pchoice)
  return(lik)
}

#### likelohood analyses ####
# read in thompson sampling
df_thompson <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/optimization_thompson.csv')
colnames(df_thompson)[colnames(df_thompson) == "Optimal.Parameters"] <- "decay"
write.csv(df_thompson, '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/df_thompson.csv', row.names=TRUE)


#read in active inference agent
df_ai_eta_decay <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/optimization_ai_eta_decay.csv')
write.csv(df_ai_eta_decay, '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/df_ai_eta_decay.csv', row.names=TRUE)

#read in ucb algorithm
df_ucb <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/optimization_ucb.csv')
write.csv(df_ucb, '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/df_ucb.csv', row.names=TRUE)

# read in random agent
df_random <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/optimization_random.csv')
write.csv(df_random, '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/df_optimization_random.csv', row.names=TRUE)

likelihood_results <- cbind(df_ai_eta_decay$Participant, df_ai_eta_decay$eta, df_ai_eta_decay$decay, df_ai_eta_decay$Optimal.Likelihood, df_ai_eta_decay$AIC_ai ,df_ucb$ucb, df_ucb$decay, df_ucb$Optimal.Likelihood, df_ucb$AIC_ucb,df_thompson$decay, 
                            df_thompson$Optimal.Likelihood, df_thompson$AIC_thompson,df_random$decay, df_random$Optimal.Likelihood, df_random$AIC_random)
likelihood_results <- as.data.frame(likelihood_results)
colnames(likelihood_results) <- c("Participant", "eta", "AImemory", "AILikelihood", "AIAIC","beta", "ucbMemory", "ucbLikelihood", "ucbAIC","thompsonMemory", "thompsonLikelihood", "thompsonAIC","randomMemory", "randomLikelihood", "randomAIC")

write.csv(likelihood_results, "/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/likelihood_results.csv", row.names = FALSE)

#### write csv with likelihood and AIC values ####
# Create a vector of likelihood column names
likelihood_columns <- colnames(likelihood_results)[grep("Likelihood", colnames(likelihood_results))]

# Create a vector of AIC column names corresponding to the likelihood columns
aic_columns <- sub("Likelihood", "AIC", likelihood_columns)

# Select the columns in the desired order
selected_columns <- c("Participant", likelihood_columns, aic_columns)

# Create a new dataframe with the selected columns
selected_data <- likelihood_results[selected_columns]
write.csv(selected_data, "/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/likelihood_and_aic_data.csv", row.names = FALSE)


# Plotting AI Likelihood
ggplot(likelihood_results, aes(x = `AI memory`, y = `AI likelihood`)) +
  geom_smooth() +
  labs(x = "AI Memory", y = "AI Likelihood", title = "AI Likelihood vs. Memory")

# Plotting UCB Likelihood
ggplot(likelihood_results, aes(x = `ucb memory`, y = `ucb likelihood`)) +
  geom_smooth() +
  labs(x = "UCB Memory", y = "UCB Likelihood", title = "UCB Likelihood vs. Memory")

# Plotting Thompson Likelihood
ggplot(likelihood_results, aes(x = `thompson memory`, y = `thompson likelihood`)) +
  geom_smooth() +
  labs(x = "Thompson Memory", y = "Thompson Likelihood", title = "Thompson Likelihood vs. Memory")

# Plotting Random Likelihood
ggplot(likelihood_results, aes(x = `random memory`, y = `random likelihood`)) +
  geom_smooth() +
  labs(x = "Random Memory", y = "Random Likelihood", title = "Random Likelihood vs. Memory")




# Compute the mean and standard deviation for each column (excluding "Participant")
summary_data <- likelihood_results %>%
  summarise(across(-Participant, list(mean = mean, sd = sd)))

#### Check if any likelihood entry is lower than AILikelihood ####
lower_likelihood_than_ai <- sapply(likelihood_columns, function(likelihood_col) {
  any(selected_data[[likelihood_col]] < selected_data$AILikelihood)
})

# Print the result for each likelihood column
for (i in seq_along(likelihood_columns)) {
  likelihood_col <- likelihood_columns[i]
  result <- ifelse(lower_likelihood_than_ai[i], "Yes", "No")
  cat("Does", likelihood_col, "have an entry lower than AILikelihood?", result, "\n")
}

#### Check if any AIC entry is lower than AILikelihood ####
lower_aic_than_ai <- sapply(aic_columns, function(aic_col) {
  any(selected_data[[aic_col]] < selected_data$AILikelihood)
})

# Print the result for each AIC column
for (i in seq_along(aic_columns)) {
  aic_col <- aic_columns[i]
  result <- ifelse(lower_aic_than_ai[i], "Yes", "No")
  cat("Does", aic_col, "have an entry lower than AILikelihood?", result, "\n")
}

# Print the summary data
print(summary_data)

## calculate AICs
df_ai_eta_decay$AIC_ai <- 2 * df_ai_eta_decay$Optimal.Likelihood + 2 * 2
df_thompson$AIC_thompson <- 2 * df_thompson$Optimal.Likelihood + 2 * 1
df_ucb$AIC_ucb <- 2 * df_ucb$Optimal.Likelihood + 2 * 2
df_random$AIC_random <- 2 * df_random$Optimal.Likelihood + 2 * 2


AIC_ai = mean(df_ai_eta_decay$AIC_ai)
sd_AIC_ai = sd(df_ai_eta_decay$AIC_ai)
print(AIC_ai)

AIC_ucb = mean(df_ucb$AIC_ucb)
sd_AIC_ucb = sd(df_ucb$AIC_ucb)

print(AIC_ucb)

AIC_thompson = mean(df_thompson$AIC_thompson)
sd_AIC_thompson = sd(df_thompson$AIC_thompson)

print(AIC_thompson)

AIC_random = mean(df_random$AIC_random)
sd_AIC_random = sd(df_random$AIC_random)


print(AIC_random)

## calculate relative likelihood between active inference and UCB

min_AIC = min(AIC_ucb, AIC_ai)

B_ai = exp((min_AIC-AIC_ai)/2)
B_ucb = exp((min_AIC-AIC_ucb)/2)

p_ai = B_ai/(B_ai + B_ucb)
p_ucb = B_ucb/(B_ai + B_ucb)

## calculate relative likelihood between active inference and thompson

min_AIC = min(AIC_thompson, AIC_ai)

B_ai = exp((min_AIC-AIC_ai)/2)
B_thompson = exp((min_AIC-AIC_thompson)/2)

p_ai = B_ai/(B_ai + B_thompson)
p_thompson = B_thompson/(B_ai + B_thompson)

## calculate relative likelihood between thompson and ucb

min_AIC = min(AIC_thompson, AIC_ucb)

B_ucb = exp((min_AIC-AIC_ucb)/2)
B_thompson = exp((min_AIC-AIC_thompson)/2)

p_ucb = B_ucb/(B_ucb + B_thompson)
p_thompson = B_thompson/(B_ucb + B_thompson)

## calculate relative likelihood between random and Active inference

min_AIC = min(AIC_random, AIC_ai)

B_ai = exp((min_AIC-AIC_ai)/2)
B_random = exp((min_AIC-AIC_random)/2)

p_ai = B_ai/(B_ai + B_random)
p_random = B_random/(B_ai + B_random)

## calculate relative likelihood between random and ucb

min_AIC = min(AIC_random, AIC_ucb)

B_ucb = exp((min_AIC-AIC_ucb)/2)
B_random = exp((min_AIC-AIC_random)/2)

p_ucb = B_ucb/(B_ucb + B_random)
p_random = B_random/(B_ucb + B_random)

## calculate relative likelihood between thompson and random

min_AIC = min(AIC_thompson, AIC_random)

B_random = exp((min_AIC-AIC_random)/2)
B_thompson = exp((min_AIC-AIC_thompson)/2)

p_random = B_random/(B_random + B_thompson)
p_thompson = B_thompson/(B_random + B_thompson)

#### plot distribution of parameters ####
data_frames <- list(df_random, df_ai_eta_decay, df_thompson, df_ucb)

# Plot the distribution of "decay" column for each data frame
for (i in 1:length(data_frames)) {
  current_df <- data_frames[[i]]
  
  # Create a ggplot
  gg <- ggplot(current_df, aes(x = decay)) +
    geom_histogram(binwidth = 0.02, fill = "blue", color = "black") +
    labs(title = paste("Distribution of Decay (DataFrame", i, ")"),
         x = "Decay", y = "Frequency") +
    theme_minimal()
  
  # Print the ggplot
  print(gg)
}

#### calculate the distribution of parameters ####
# Create a list of data frames and corresponding algorithm names
data_frames <- list(df_random, df_ai_eta_decay, df_thompson, df_ucb)
algorithm_names <- c("Random", "AI Eta Decay", "Thompson", "UCB")

# Loop through data frames and algorithm names
for (i in 1:length(data_frames)) {
  current_df <- data_frames[[i]]
  algorithm_name <- algorithm_names[i]
  
  # Calculate the distribution of the "decay" column
  decay_mean <- mean(current_df$decay)
  decay_sd <- sd(current_df$decay)
  
  # Print the distribution
  print(decay_mean)
  print(decay_sd)
}


#### simulation results based on optimized parameters ####
# ucb
df_sim_ucb <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/simulation_results_ucb_per_participant.csv')
# thompson
df_sim_thompson <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/simulation_results_thompson.csv')
# active inference
df_sim_ai <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/simulation_results_ai-2.csv')
# random
df_sim_random <- read.csv(file = '/Users/marvinmathony/Documents/UCL/LagnadoThesis/data/simulation_results_random.csv')

# human cumulative reward
data2$cumulative_reward <- cumsum(data2$reward)

#cumulative reward plot
cumulative_rewards <- data.frame(data2$subject, data2$cumulative_reward, df_sim_ai$cumulative_rewards, df_sim_thompson$cumulative_rewards, df_sim_ucb$cumulative_rewards, df_sim_ai$cumulative_optimal_rewards)
col_names <- c("subject", "human_cumulative_reward", "ai_cumulative_reward", "thompson_cumulative_reward", "ucb_cumulative_reward", "optimal_rewards")
colnames(cumulative_rewards) <- col_names
cumulative_rewards$count <- 1:nrow(cumulative_rewards)

# Plot cumulative rewards using ggplot2
ggplot(cumulative_rewards, aes(x = count)) +
  geom_line(aes(y = human_cumulative_reward, color = "Human")) +
  geom_line(aes(y = ai_cumulative_reward, color = "AI")) +
  geom_line(aes(y = thompson_cumulative_reward, color = "Thompson")) +
  geom_line(aes(y = ucb_cumulative_reward, color = "UCB")) +
  geom_line(aes(y = optimal_rewards, color = "Optimal")) +
  labs(x = "Trial Number", y = "Cumulative Reward", title = "Cumulative Rewards") +
  scale_color_manual(values = c("Human" = "blue", "AI" = "red", "Thompson" = "green", "UCB" = "purple", "Optimal" = "orange")) +
  theme_minimal()

# do the same for rewards with regret 
data2$regret <- data2$reward - df_sim_ai$optimal_reward
rewards <- data.frame(data2$subject, data2$reward, data2$regret, df_sim_ai$rewards, df_sim_ai$regret, df_sim_thompson$rewards, df_sim_thompson$regret, df_sim_ucb$rewards, df_sim_ucb$regret,df_sim_random$rewards,df_sim_ai$optimal_reward)
col_names <- c("subject", "human_reward", "human_regret", "ai_reward", "ai_regret", "thompson_reward", "thompson_regret", "ucb_reward", "ucb_regret", "random_reward","optimal_rewards")
colnames(rewards) <- col_names
rewards$count <- 1:nrow(cumulative_rewards)

rewards <- rewards %>%
  group_by(subject) %>%
  mutate(trial = row_number())

#### reward distributions ####
optimal_mean = mean(rewards$optimal_rewards)
optimal_sd = sd(rewards$optimal_rewards)

random_mean = mean(rewards$random_reward)
random_sd = sd(rewards$random_reward)

human_mean = mean(rewards$human_reward)
human_sd = sd(rewards$human_reward)

AI_mean = mean(rewards$ai_reward)
AI_sd = sd(rewards$ai_reward)

thompson_mean = mean(rewards$thompson_reward)
thompson_sd = sd(rewards$thompson_reward)

ucb_mean = mean(rewards$ucb_reward)
ucb_sd = sd(rewards$ucb_reward)

##### pairwise t-tests #####

t.test(rewards_long$reward ~ rewards_long$group, var.equal = FALSE)

#### plot mean distribution of arms ####
mean_dist <- data2 %>%
  group_by(subject) %>%
  mutate(trial_nr = row_number())

mean_true_rewards <- mean_dist %>%
  group_by(trial_nr) %>%
  summarize(mean_arm1 = mean(mu1),
            mean_arm2 = mean(mu2),
            mean_of_arms = (mean_arm1 + mean_arm2) / 2)

ggplot(mean_true_rewards, aes(x = trial_nr)) +
  geom_smooth(aes(y = mean_arm1, color = "Arm1"), linetype = "solid") +
  geom_smooth(aes(y = mean_arm2, color = "Arm2"), linetype = "solid") +
  geom_smooth(aes(y = mean_of_arms, color = "Mean of Arms"), linetype = "dashed") +
  labs(x = "Trial Number", y = "Mean Reward", title = "Mean True Reward per Trial") +
  scale_color_manual(values = c("Arm1" = "blue", "Arm2" = "red", "Mean of Arms" = "green"),
                     labels = c("Arm1", "Arm2", "Mean of both Arms")) +
  scale_linetype_manual(values = c("Arms" = "solid", "Mean of Arms" = "dashed")) +
  theme_minimal()

#### plot the mean performance of each algorithm for each trial, averaged over all participants ####
mean_rewards <- rewards %>%
  group_by(trial) %>%
  summarize(mean_human_reward = mean(human_reward),
            mean_ai_reward = mean(ai_reward),
            mean_thompson_reward = mean(thompson_reward),
            mean_ucb_reward = mean(ucb_reward),
            mean_optimal_reward = mean(optimal_rewards),
            mean_random_reward = mean(random_reward))

ggplot(mean_rewards, aes(x = trial)) +
  geom_smooth(aes(y = mean_human_reward, color = "Human"), linetype = "solid") +
  geom_smooth(aes(y = mean_ai_reward, color = "Active Inference"), linetype = "dashed") +
  geom_smooth(aes(y = mean_thompson_reward, color = "Thompson"), linetype = "dashed") +
  geom_smooth(aes(y = mean_ucb_reward, color = "UCB"), linetype = "dashed") +
  geom_smooth(aes(y = mean_optimal_reward, color = "Optimal"), linetype = "dashed") +
  geom_smooth(aes(y = mean_random_reward, color = "Random"), linetype = "dashed") +
  labs(x = "Trial Number", y = "Mean Reward", title = "Mean Reward per Algorithm per Trial") +
  scale_color_manual(values = c("Human" = "blue", "Active Inference" = "red", "Thompson" = "green", "UCB" = "purple", "Optimal" = "orange", "Random" = "yellow"),
                     labels = c("Human Performance", "Active Inference", "Thompson", "UCB", "Optimal", "Random")) +
  scale_linetype_manual(values = c("Human" = "solid", "Algorithm" = "dashed")) +
  theme_minimal()

## plot only active inference and human
ggplot(mean_rewards, aes(x = trial)) +
  geom_line(aes(y = mean_human_reward, color = "Human"), linetype = "solid") +
  geom_line(aes(y = mean_ai_reward, color = "Active Inference"), linetype = "dashed") +
  labs(x = "Trial Number", y = "Mean Reward", title = "Mean Reward per Algorithm per Trial") +
  scale_color_manual(values = c("Human" = "blue", "Active Inference" = "red"))+
  theme_minimal()

## plot human and thompson
ggplot(mean_rewards, aes(x = trial)) +
  geom_line(aes(y = mean_human_reward, color = "Human"), linetype = "solid") +
  geom_line(aes(y = mean_thompson_reward, color = "Thompson"), linetype = "dashed") +
  labs(x = "Trial Number", y = "Mean Reward", title = "Mean Reward per Algorithm per Trial") +
  scale_color_manual(values = c("Human" = "blue", "Thompson" = "red"))+
  theme_minimal()

## plot human and random
  

# Print the modified dataframe
print(data)

# Plot rewards and regrets per trials
ggplot(rewards, aes(x = count)) +
  geom_smooth(aes(y = human_reward, color = "Human", linetype = "Reward"), se = F) +
  geom_smooth(aes(y = human_regret, color = "Human", linetype = "Regret"), se = F) +
  geom_smooth(aes(y = ai_reward, color = "AI", linetype = "Reward"), se = F) +
  geom_smooth(aes(y = ai_regret, color = "AI", linetype = "Regret"), se = F) +
  geom_smooth(aes(y = thompson_reward, color = "Thompson", linetype = "Reward"), se = F) +
  geom_smooth(aes(y = thompson_regret, color = "Thompson", linetype = "Regret"), se = F) +
  geom_smooth(aes(y = ucb_reward, color = "UCB", linetype = "Reward"), se = F) +
  geom_smooth(aes(y = ucb_regret, color = "UCB", linetype = "Regret"), se = F) +
  geom_smooth(aes(y = optimal_rewards, color = "optimal", linetype = "Reward"), se = F)+
  labs(x = "Trial Number", y = "Reward/Regret", title = "Rewards and Regrets") +
  scale_color_manual(values = c("Human" = "blue", "AI" = "red", "Thompson" = "green", "UCB" = "purple", "optimal" = "orange")) +
  scale_linetype_manual(values = c("Reward" = "solid", "Regret" = "dashed")) +
  theme_minimal()

# however, this creates the illusion of learning over participants, so 
# lets plot a random participant
# Filter data for participant 10
participant_10 <- rewards[rewards$subject == 10,]

# Plot rewards and regrets per trials
plot1 <- ggplot(participant_10, aes(x = count)) +
  geom_smooth(aes(y = human_reward, color = "Human", linetype = "human"), se = F) +
  geom_smooth(aes(y = ai_reward, color = "AI", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = thompson_reward, color = "Thompson", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = ucb_reward, color = "UCB", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = optimal_rewards, color = "optimal", linetype = "algorithm"), se = F)+
  geom_smooth(aes(y = random_reward, color = "Random", linetype = "algorithm"), se = F) +
  
  labs(x = "Trial Number", y = "Mean Reward", title = "Participant 10") +
  scale_color_manual(values = c("Human" = "blue", "AI" = "red", "Thompson" = "green", "UCB" = "purple", "optimal" = "orange", "Random" = "yellow")) +
  theme_minimal()


participant_29 <- rewards[rewards$subject == 29,]

# Plot rewards and regrets per trials
plot2 <- ggplot(participant_29, aes(x = count)) +
  geom_smooth(aes(y = human_reward, color = "Human", linetype = "human"), se = F) +
  geom_smooth(aes(y = ai_reward, color = "AI", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = thompson_reward, color = "Thompson", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = ucb_reward, color = "UCB", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = optimal_rewards, color = "optimal", linetype = "algorithm"), se = F)+
  geom_smooth(aes(y = random_reward, color = "Random", linetype = "algorithm"), se = F) +
  
  labs(x = "Trial Number", y = "Mean Reward", title = "Participant 29") +
  scale_color_manual(values = c("Human" = "blue", "AI" = "red", "Thompson" = "green", "UCB" = "purple", "optimal" = "orange", "Random" = "yellow")) +
  theme_minimal()


participant_5 <- rewards[rewards$subject == 5,]

# Plot rewards and regrets per trials
plot3 <- ggplot(participant_5, aes(x = count)) +
  geom_smooth(aes(y = human_reward, color = "Human", linetype = "human"), se = F) +
  geom_smooth(aes(y = ai_reward, color = "AI", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = thompson_reward, color = "Thompson", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = ucb_reward, color = "UCB", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = optimal_rewards, color = "optimal", linetype = "algorithm"), se = F)+
  geom_smooth(aes(y = random_reward, color = "Random", linetype = "algorithm"), se = F) +
  
  labs(x = "Trial Number", y = "Mean Reward", title = "Participant 5") +
  scale_color_manual(values = c("Human" = "blue", "AI" = "red", "Thompson" = "green", "UCB" = "purple", "optimal" = "orange", "Random" = "yellow")) +
  theme_minimal()

participant_16 <- rewards[rewards$subject == 16,]

# Plot rewards and regrets per trials
plot4 <- ggplot(participant_16, aes(x = count)) +
  geom_smooth(aes(y = human_reward, color = "Human", linetype = "human"), se = F) +
  geom_smooth(aes(y = ai_reward, color = "AI", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = thompson_reward, color = "Thompson", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = ucb_reward, color = "UCB", linetype = "algorithm"), se = F) +
  geom_smooth(aes(y = optimal_rewards, color = "optimal", linetype = "algorithm"), se = F)+
  geom_smooth(aes(y = random_reward, color = "Random", linetype = "algorithm"), se = F) +
  
  labs(x = "Trial Number", y = "Mean Reward", title = "Participant 16") +
  scale_color_manual(values = c("Human" = "blue", "AI" = "red", "Thompson" = "green", "UCB" = "purple", "optimal" = "orange", "Random" = "yellow")) +
  theme_minimal()

## create one plot
# Combine plots into a 2x2 grid
# Remove the legends from individual plots
plot1 <- plot1 + theme(legend.position = "none")
plot2 <- plot2 + theme(legend.position = "none")
plot3 <- plot3 + theme(legend.position = "none")

# Create a common legend
grid <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)



#### count exploration ####

agents <- list(df_sim_ai, df_sim_thompson, df_sim_ucb)

count_lower_mean <- function(data) {
  return(sum(data$chosen_arm == (data$posterior_mean_arm0 > data$posterior_mean_arm1)))
}

count_lower_ambiguity <- function(data) {
  return(sum(data$chosen_arm == (data$posterior_mean_arm0 > data$posterior_mean_arm1) &
               (data$ambiguity_arm0 < data$ambiguity_arm1)))
}

lower_ambiguity_count <- count_lower_ambiguity(df_sim_ai)
cat("Lower ambiguity count during exploration for df_sim_ai:", lower_ambiguity_count, "\n")

#### plot true mean distribution for random participant ####
participant_26 <- data2[data2$subject == 26,]
participant_26$Trial <- 1:nrow(participant_26)

ggplot(participant_26, aes(x = Trial)) +
  geom_line(aes(y = mu1, color = "mean1")) +
  geom_line(aes(y = mu2, color = "mean2")) +
  labs(x = "Trial Number", y = "mean", title = "True Reward Distribution Means per Arm for a Random Participant") +
  scale_color_manual(values = c("mean1" = "blue", "mean2" = "red")) +
  theme_minimal()

  
  






