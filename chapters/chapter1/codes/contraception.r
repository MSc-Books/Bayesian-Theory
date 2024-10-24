# Import library for plots
library(ggplot2)

# Define the parameters
n <- 20  # Total number of pregnancies
k <- 4   # Number of pregnancies in the treatment group
p_values <- seq(0.1, 0.9, by = 0.1)  # Possible values of p

# Define prior probabilities
prior <- c(0.06, 0.06, 0.06, 0.06, 0.52, 0.06, 0.06, 0.06, 0.06)

# Calculate likelihood for each model (p)
likelihood <- dbinom(k, size = n, prob = p_values)

# Calculate the unnormalized posterior
unnormalized_posterior <- prior * likelihood

# Normalize the posterior
posterior <- unnormalized_posterior / sum(unnormalized_posterior)

# Create a data frame for plotting
results <- data.frame(
  p = p_values,
  Prior = prior,
  Likelihood = likelihood,
  Posterior = posterior
)

# Reshape data for ggplot
results_long <- data.frame(
  p = rep(p_values, 3),  # Repeat p_values for each distribution
  Distribution = factor(rep(c("Prior", "Likelihood", "Posterior"), each = length(p_values)), 
                        levels = c("Prior", "Likelihood", "Posterior")),
  Probability = c(prior, likelihood, posterior)
)

# Create the plot
p <- ggplot(results_long, aes(x = p, y = Probability, fill = Distribution)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Distribution, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1)) +  # Set x-axis breaks
  theme_minimal() +
  labs(
    title = "Bayesian Analysis of Contraceptive Efficacy",
    x = "Parameter Values Representing Contraceptive Success (p)",
    y = "Probability Mass Function"
  ) +
  scale_fill_manual(values = c("Prior" = "blue", "Likelihood" = "red", "Posterior" = "green")) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# Save plot as PNG
ggsave("bayesian_analysis.png", p, width = 8, height = 10, dpi = 300, bg = "white")

# Print results
print(results)