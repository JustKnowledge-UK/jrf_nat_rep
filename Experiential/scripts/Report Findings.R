
#Import data and libraries

library(tidyverse)
setwd("~/repos/JRF Nat Rep")
data <- read_csv("./Experiential/data/2025-06-30 - clean_data_jrf_experiential.csv")

data$Ethnicity_Collapsed <- relevel(factor(data$Ethnicity_Collapsed), ref = "White British")
data$Sex <- relevel(factor(data$Sex), ref = "Male")
data$Region <- relevel(factor(data$Region), ref = "London")
data$income_group <- relevel(factor(data$income_group), ref = "Mid")

# Report Structure

# report being paid less than if they were in-house
library(crosstable) 
library(flextable)

Paid_Less_Cross <- crosstable(data%>% 
                                select(Pros_And_Cons_Pay, Sex, Age, Ethnicity, Ethnicity_Collapsed, Region, income_group, Education_Band, BORNUK_binary), 
                              by = Pros_And_Cons_Pay,
                              total = "both",
                              showNA = "no", 
                              funs = c("median", "mean", "std dev" = "sd"),
                              percent_digits = 2, 
                              percent_pattern = "{n} ({p_row})") %>%
  as_flextable()

Paid_Less_Cross

# Preference for in-house
work_pref_table <- data %>%
  count(Work_Preference) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 1),
    Display = paste0(n, " (", Percentage, "%)")
  ) %>%
  select(Work_Preference, n, Percentage) %>%
  flextable() %>%
  set_header_labels(
    Work_Preference = "Work Preference",
    n = "Count",
    Percentage = "Percentage"
  ) %>%
  theme_vanilla()

work_pref_table


## Why are outsourced workers in their current role?

# Create a summary of reasons why outsourced workers are in their current role
why_job_reasons <- data.frame(
  Reason = c(
    "I like doing this kind of work",
    "My job is in a convenient location", 
    "I can work flexibly in a way which suits me",
    "The pay is good",
    "I like my colleagues",
    "I like the workplace culture",
    "It is helping me develop skills and experience I need to progress",
    "This was the best job available to me",
    "I do not have the formal qualifications I need to do another job I would prefer",
    "I can do the job alongside managing my health conditions",
    "I can do the job alongside childcare or caring for others"
  ),
  Variable = c(
    "Why_Job_Like", "Why_Job_Convinient", "Why_Job_Flexibility", "Why_Job_Pay",
    "Why_Job_Collegues", "Why_Job_Culture", "Why_Job_Progress", "Why_Job_BestAvailable",
    "Why_Job_NotQualified", "Why_Job_Health", "Why_Job_Carer"
  )
)

# Count responses for each reason (excluding NAs)
why_job_counts <- data.frame(
  Reason = character(0),
  Count = numeric(0),
  Percentage = numeric(0)
)

for(i in 1:nrow(why_job_reasons)) {
  var_name <- why_job_reasons$Variable[i]
  reason_text <- why_job_reasons$Reason[i]
  
  # Count non-NA responses
  count <- sum(!is.na(data[[var_name]]))
  total_responses <- nrow(data)
  percentage <- round(count / total_responses * 100, 1)
  
  why_job_counts <- rbind(why_job_counts, data.frame(
    Reason = reason_text,
    Count = count,
    Percentage = percentage
  ))
}

# Sort by count (descending)
why_job_counts <- why_job_counts[order(why_job_counts$Count, decreasing = TRUE), ]

# Create table
why_job_table <- why_job_counts %>%
  flextable() %>%
  set_header_labels(
    Reason = "Reason for Current Role",
    Count = "Count",
    Percentage = "Percentage"
  ) %>%
  theme_vanilla() %>%
  autofit()

why_job_table

# Create plot

why_job_plot <- ggplot(why_job_counts, aes(x = Percentage, y = reorder(Reason, Percentage))) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            hjust = -0.05, size = 3) +
  labs(
    title = "Reasons Why Outsourced Workers Are in Their Current Role",
    x = "Percentage of Total Sample",
    y = "Reason"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

why_job_plot

## Pros and Cons of Outsourced Work

# Function to categorize pros and cons responses
categorize_response <- function(response) {
  if (is.na(response)) {
    return(NA)
  }
  
  response <- as.character(response)
  
  # Don't know category
  if (grepl("Don't know", response, ignore.case = TRUE)) {
    return("Don't know")
  }
  
  # Neither/No impact category
  if (grepl("Neither|no impact", response, ignore.case = TRUE)) {
    return("No impact / Neither")
  }
  
  # Less/Worse category
  if (grepl("less|worse|harder", response, ignore.case = TRUE)) {
    return("Less / Worse")
  }
  
  # More/Better category  
  if (grepl("more|better|easier", response, ignore.case = TRUE)) {
    return("More / Better")
  }
  
  # Default to Neither for other responses
  return("No impact / Neither")
}


# Define variable labels
pros_cons_labels <- data.frame(
  Variable = c("Pros_And_Cons_Flexibility", "Pros_And_Cons_Pay", "Pros_And_Cons_Hours", 
               "Pros_And_Cons_Holiday", "Pros_And_Cons_Terms", "Pros_And_Cons_Promotion",
               "Pros_And_Cons_Training", "Pros_And_Cons_Security", "Pros_And_Cons_Treatment",
               "Pros_And_Cons_Specialisation", "Pros_And_Cons_Connection", "Pros_And_Cons_FeelInvested",
               "Pros_And_Cons_Rights", "Pros_And_Cons_HealthSafety"),
  Label = c("Get to work flexibly", "Pay", "Access to secure working hours",
            "Holiday leave", "Terms and conditions", "Opportunity to progress / promotion",
            "Access to training / development", "Access to job security", "Treatment compared to in-house colleagues",
            "Opportunity to specialise in role or industry", "Feeling connected to people I work with",
            "Feeling invested in my role and my work", "Ease of asserting rights at work",
            "Protection of health and safety at work")
)

# Process each pros and cons variable
pros_cons_data <- data.frame()

for (i in 1:nrow(pros_cons_labels)) {
  var_name <- pros_cons_labels$Variable[i]
  label <- pros_cons_labels$Label[i]
  
  # Categorize responses
  categorized <- sapply(data[[var_name]], categorize_response)
  
  # Count categories
  counts <- table(categorized, useNA = "no")
  total <- sum(counts)
  
  # Calculate percentages
  for (category in names(counts)) {
    percentage <- round(counts[category] / total * 100, 0)
    pros_cons_data <- rbind(pros_cons_data, data.frame(
      Variable = label,
      Category = category,
      Count = as.numeric(counts[category]),
      Percentage = percentage
    ))
  }
}

# Calculate Less/Worse percentage for ordering
less_worse_pct <- pros_cons_data %>%
  filter(Category == "Less / Worse") %>%
  select(Variable, Percentage) %>%
  rename(LessWorse_Pct = Percentage)

# Order by Less/Worse percentage (descending)
variable_order <- less_worse_pct[order(less_worse_pct$LessWorse_Pct, decreasing = TRUE), "Variable"]

# Set factor levels for proper ordering
pros_cons_data$Variable <- factor(pros_cons_data$Variable, levels = variable_order)
pros_cons_data$Category <- factor(pros_cons_data$Category, 
                                  levels = c("Don't know", "No impact / Neither", "More / Better", "Less / Worse"))

# Create the stacked bar chart

pros_cons_plot <- ggplot(pros_cons_data, aes(x = Percentage, y = Variable, fill = Category)) +
  geom_col(position = "stack") +
  geom_text(aes(label = ifelse(Percentage >= 5, paste0(Percentage, "%"), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Less / Worse" = "#e74c3c", 
                               "More / Better" = "#27ae60", 
                               "No impact / Neither" = "#3498db", 
                               "Don't know" = "#95a5a6")) +
  labs(
    title = "Reflections on the potential benefits and drawbacks of outsourced work",
    subtitle = "Compared to if you were an in-house / non-outsourced worker",
    x = "Percentage",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    legend.position = "top",
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), 
                     breaks = seq(0, 100, 10),
                     expand = c(0, 0))

pros_cons_plot


# Function to transform categorical responses to numeric values
recode_pros_cons <- function(x) {
  case_when(
    grepl("more|better|easier", tolower(x)) ~ 1,
    grepl("less|harder|worse", tolower(x)) ~ -1,
    grepl("neither|no impact", tolower(x)) ~ 0,
    grepl("don't know", tolower(x)) ~ NA_real_,
    # Default case
    TRUE ~ NA_real_
  )
}

# Process the data using the same approach as the Quarto file
data <- data %>%
  mutate(across(starts_with("Pros_And_Cons"), 
                ~ recode_pros_cons(as.character(.)),
                .names = "{.col}_numeric"))

## Analysis of Negative Outcomes per Worker (using Quarto approach)

# Count negative outcomes for each respondent using the same method as Quarto file
data<- data %>%
  mutate(
    total = rowSums(select(., contains("_numeric")), na.rm = TRUE),
    num_neg = rowSums(select(., contains("_numeric")) == -1, na.rm = TRUE)
  ) %>%
  # Create meaningful categories (same as Quarto file)
  mutate(neg_category = case_when(
    num_neg == 0 ~ "0",
    num_neg == 1 ~ "1",
    num_neg == 2 ~ "2",
    num_neg == 3 ~ "3",
    num_neg == 4 ~ "4",
    num_neg >= 5 ~ "5+"
  )) %>%
  # Create readable categories for display
  mutate(
    negative_outcomes_category = case_when(
      num_neg == 0 ~ "No negative impacts",
      num_neg %in% 1:2 ~ "1-2 negative outcomes", 
      num_neg %in% 3:4 ~ "3-4 negative outcomes",
      num_neg >= 5 ~ "5+ negative outcomes",
      TRUE ~ NA_character_
    )
  )

# Create summary table using the same approach as Quarto file
negative_outcomes_summary <- data %>%
  count(negative_outcomes_category) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 1),
    Display = paste0(n, " (", Percentage, "%)")
  ) %>%
  filter(!is.na(negative_outcomes_category)) %>%
  # Reorder for logical presentation
  mutate(negative_outcomes_category = factor(negative_outcomes_category, 
                                           levels = c("No negative impacts", 
                                                     "1-2 negative outcomes",
                                                     "3-4 negative outcomes", 
                                                     "5+ negative outcomes"))) %>%
  arrange(negative_outcomes_category)

# Create table using flextable
negative_outcomes_table <- negative_outcomes_summary %>%
  select(negative_outcomes_category, n, Percentage) %>%
  flextable() %>%
  set_header_labels(
    negative_outcomes_category = "Number of Negative Outcomes",
    n = "Count",
    Percentage = "Percentage"
  ) %>%
  theme_vanilla() %>%
  autofit()

negative_outcomes_table

# Create visualization
negative_outcomes_plot <- ggplot(negative_outcomes_summary, 
                                aes(x = Percentage, y = fct_rev(negative_outcomes_category))) +
  geom_col(fill = c("#27ae60", "#f39c12", "#e67e22", "#e74c3c"), alpha = 0.8) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            hjust = -0.05, size = 4, fontweight = "bold") +
  labs(
    title = "Distribution of Negative Outcomes Among Outsourced Workers",
    subtitle = "Number of negative impacts compared to in-house work",
    x = "Percentage of Workers",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

negative_outcomes_plot

#Percentage of people reportring they are paid less and number of negative outcomes

# Filter for people paid less and create negative categories
paid_less_group <- data %>%
  filter(Pros_And_Cons_Pay == 'I get paid less') %>%
  count(neg_category) %>%
  mutate(percent = (n / sum(n)) * 100)

# Create the bar plot for paid less group
ggplot(paid_less_group, aes(x = neg_category, y = percent, fill = neg_category)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5) +
  labs(title = "Percentage of People Who Say They Are Paid Less Reporting Negative Responses",
       x = "Number of Negative Responses (-1s)",
       y = "Percentage of People Paid Less") +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal()

## Statistical Analysis of Demographic Differences in Negative Outcomes

# Load required libraries
library(broom)
library(MASS)


# 4. Mean number of negatives by group
means_by_group <- data %>%
  dplyr::select(income_group, Ethnicity_Collapsed, Sex, Education_Band, BORNUK_binary, num_neg) %>%
  pivot_longer(cols = -num_neg, names_to = "demographic", values_to = "group") %>%
  filter(!is.na(group)) %>%
  group_by(demographic, group) %>%
  summarise(
    mean_negatives = round(mean(num_neg, na.rm = TRUE), 2),
    median_negatives = median(num_neg, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Create visualization of means
means_plot <- means_by_group %>%
  ggplot(aes(x = group, y = mean_negatives, fill = demographic)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = mean_negatives), vjust = -0.5, size = 3) +
  facet_wrap(~ demographic, scales = "free_x") +
  labs(
    title = "Mean Number of Negative Outcomes by Demographic Group",
    x = "",
    y = "Mean Number of Negatives"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

means_plot

## Regression Analysis of Demographic Differences

# Ensure we have complete data for regression
regression_data <- data %>%
  filter(!is.na(num_neg), !is.na(income_group), !is.na(Ethnicity_Collapsed),
         !is.na(Sex), !is.na(Education_Band), !is.na(BORNUK_binary),
         !is.na(Region), !is.na(OutsourcedNonOL), !is.na(Age))

demographics <- c("income_group", "Ethnicity_Collapsed", "Sex", "Education_Band", "BORNUK_binary", "Region", "Age", "OutsourcedNonOL")

# 1.1 Univariate Models
univariate_models <- list()
model_summaries <- list()

for (demo in demographics) {
  # Create formula dynamically
  formula_str <- paste("num_neg ~", demo)
  formula_obj <- as.formula(formula_str)
  
  # Poisson regression
  poisson_model <- glm(formula_obj, 
                      data = regression_data, 
                      family = poisson)
  
  # Linear regression for comparison
  linear_model <- lm(formula_obj, 
                    data = regression_data)
  
  # Store models
  univariate_models[[demo]] <- list(
    poisson = poisson_model,
    linear = linear_model
  )
  
  # Extract summary statistics
  poisson_summary <- tidy(poisson_model, conf.int = TRUE)
  linear_summary <- tidy(linear_model, conf.int = TRUE)
  
  model_summaries[[demo]] <- list(
    demographic = demo,
    poisson_aic = AIC(poisson_model),
    linear_r_squared = summary(linear_model)$r.squared,
    poisson_deviance = poisson_model$deviance,
    linear_rmse = sqrt(mean(residuals(linear_model)^2))
  )
}

# 1.2 Multivariate Model
# Poisson regression with all demographics
full_poisson <- glm(num_neg ~ income_group + Ethnicity_Collapsed + Sex + 
                   Education_Band + BORNUK_binary + Age + Region + OutsourcedNonOL, 
                   data = regression_data, 
                   family = poisson)
summary(full_poisson)

# Linear regression with all demographics  
full_linear <- lm(num_neg ~ income_group + Ethnicity_Collapsed + Sex + 
                 Education_Band + BORNUK_binary + Age + Region + OutsourcedNonOL, 
                 data = regression_data)
summary(full_linear)

# Check for overdispersion in Poisson model
overdispersion_ratio <- full_poisson$deviance / full_poisson$df.residual
cat("Overdispersion ratio:", round(overdispersion_ratio, 3), "\n")
cat("If > 1.5, consider negative binomial or quasi-Poisson\n")

# If overdispersed, fit negative binomial
if(overdispersion_ratio > 1.5) {
  full_negbin <- glm.nb(num_neg ~ income_group + Ethnicity_Collapsed + Sex + 
                       Education_Band + BORNUK_binary + Age + Region + OutsourcedNonOL, 
                       data = regression_data)
  cat("Negative binomial model fitted due to overdispersion\n")
}

# 1.3 Model Comparison Table
model_comparison <- data.frame(
  Model = c("Full Poisson", "Full Linear", if(exists("full_negbin")) "Full Negative Binomial"),
  AIC = c(AIC(full_poisson), AIC(full_linear), if(exists("full_negbin")) AIC(full_negbin) else NA),
  Deviance = c(full_poisson$deviance, NA, if(exists("full_negbin")) full_negbin$deviance else NA),
  R_Squared = c(NA, summary(full_linear)$r.squared, NA),
  RMSE = c(sqrt(mean(residuals(full_poisson)^2)), 
           sqrt(mean(residuals(full_linear)^2)), 
           if(exists("full_negbin")) sqrt(mean(residuals(full_negbin)^2)) else NA)
) %>%
  filter(!is.na(Model))

# Create model comparison table
model_comparison_table <- model_comparison %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  flextable() %>%
  set_header_labels(
    Model = "Model Type",
    AIC = "AIC",
    Deviance = "Deviance", 
    R_Squared = "R-Squared",
    RMSE = "RMSE"
  ) %>%
  theme_vanilla() %>%
  autofit()

model_comparison_table

# 1.4 Coefficient Analysis (using best model)
best_model <- if(exists("full_negbin")) full_negbin else full_poisson

# Extract coefficients with confidence intervals
coefficients_df <- tidy(best_model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # For Poisson/NegBin: exp(estimate) gives rate ratios
    rate_ratio = exp(estimate),
    rate_ratio_lower = exp(conf.low),
    rate_ratio_upper = exp(conf.high),
    significant = p.value < 0.05
  )

# Create coefficients table
coefficients_table <- coefficients_df %>%
  dplyr::select(term, estimate, std.error, rate_ratio, conf.low, conf.high, p.value, significant) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  flextable() %>%
  set_header_labels(
    term = "Variable",
    estimate = "Coefficient",
    std.error = "Std Error",
    rate_ratio = "Rate Ratio",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper", 
    p.value = "P-Value",
    significant = "Significant"
  ) %>%
  theme_vanilla() %>%
  autofit()

coefficients_table

# 1.5 Forest Plot of Coefficients
forest_plot <- coefficients_df %>%
  mutate(
   term_clean = str_replace_all(term, "BORNUK_binary", ""),
   term_clean = str_replace_all(term_clean, "_", " ")
  ) %>%
  ggplot(aes(x = rate_ratio, y = reorder(term_clean, rate_ratio))) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbarh(aes(xmin = rate_ratio_lower, xmax = rate_ratio_upper), 
                 height = 0.2, color = "darkblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Rate Ratios for Number of Negative Outcomes",
    subtitle = "Showing 95% confidence intervals",
    x = "Rate Ratio (>1 = higher rate of negatives)",
    y = "",
    caption = "Reference: Male, White British, Mid income, etc."
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

forest_plot

# 1.6 Predicted Values by Group

# Check which variables are actually in the regression data
cat("Variables in regression_data:\n")
print(names(regression_data))
cat("\nVariables in best_model:\n")
print(names(best_model$model))

# Create prediction data with proper error handling
predicted_data <- expand_grid(
  income_group = levels(factor(regression_data$income_group)),
  Ethnicity_Collapsed = "White British",  # Hold constant
  Sex = "Male",  # Hold constant
  Education_Band = if("Education_Band" %in% names(regression_data)) levels(factor(regression_data$Education_Band))[1] else "High",
  BORNUK_binary = if("BORNUK_binary" %in% names(regression_data)) levels(factor(regression_data$BORNUK_binary))[1] else "Yes",
  Region = if("Region" %in% names(regression_data)) names(sort(table(regression_data$Region), decreasing = TRUE))[1] else "London",
  OutsourcedNonOL = if("OutsourcedNonOL" %in% names(regression_data)) names(sort(table(regression_data$OutsourcedNonOL), decreasing = TRUE))[1] else "Outsourced",
  Age = median(regression_data$Age, na.rm = TRUE)  # Hold constant
)

# Ensure all variables are factors where needed
predicted_data <- predicted_data %>%
  mutate(
    income_group = factor(income_group, levels = levels(factor(regression_data$income_group))),
    Ethnicity_Collapsed = factor(Ethnicity_Collapsed, levels = levels(factor(regression_data$Ethnicity_Collapsed))),
    Sex = factor(Sex, levels = levels(factor(regression_data$Sex))),
    Education_Band = if("Education_Band" %in% names(regression_data)) factor(Education_Band, levels = levels(factor(regression_data$Education_Band))) else Education_Band,
    BORNUK_binary = if("BORNUK_binary" %in% names(regression_data)) factor(BORNUK_binary, levels = levels(factor(regression_data$BORNUK_binary))) else BORNUK_binary,
    Region = if("Region" %in% names(regression_data)) factor(Region, levels = levels(factor(regression_data$Region))) else Region,
    OutsourcedNonOL = if("OutsourcedNonOL" %in% names(regression_data)) factor(OutsourcedNonOL, levels = levels(factor(regression_data$OutsourcedNonOL))) else OutsourcedNonOL
  ) %>%
  mutate(
    predicted_negatives = predict(best_model, newdata = ., type = "response")
  )

# Plot predicted values
predicted_plot <- predicted_data %>%
  ggplot(aes(x = income_group, y = predicted_negatives)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = round(predicted_negatives, 2)), vjust = -0.5) +
  labs(
    title = "Predicted Number of Negative Outcomes by Income Group",
    subtitle = "Holding other demographics constant",
    x = "Income Group",
    y = "Predicted Number of Negatives"
  ) +
  theme_minimal()

predicted_plot

## Analysis of Work Conditions Variables

# Variables to analyze:
work_variables <- c("Guaranteed_Hours", "Notice_Of_Working_Hours", "Notice_Of_Cancelled_Shifts", 
                   "Cancelled_Shift_Pay", "Sick_Pay")

# Examine the response categories for each variable
cat("Response categories for each work variable:\n")
cat("==========================================\n")

for (var in work_variables) {
  cat("\n", var, ":\n")
  response_counts <- table(data[[var]], useNA = "ifany")
  print(response_counts)
  cat("Total responses:", sum(response_counts), "\n")
}

# Create percentage summaries for each variable
work_conditions_summary <- list()

for (var in work_variables) {
  # Calculate counts and percentages
  counts <- table(data[[var]], useNA = "no")  # Exclude NAs from percentage calculation
  total_responses <- sum(counts)
  
  # Create summary dataframe
  summary_df <- data.frame(
    Response = names(counts),
    Count = as.numeric(counts),
    Percentage = round(as.numeric(counts) / total_responses * 100, 1),
    stringsAsFactors = FALSE
  )
  
  # Store in list
  work_conditions_summary[[var]] <- summary_df
}

# Display tables for each variable
library(flextable)

# Variable labels for better presentation
variable_labels <- c(
  "Guaranteed_Hours" = "Guaranteed Hours",
  "Notice_Of_Working_Hours" = "Notice of Working Hours", 
  "Notice_Of_Cancelled_Shifts" = "Notice of Cancelled Shifts",
  "Cancelled_Shift_Pay" = "Cancelled Shift Pay",
  "Sick_Pay" = "Sick Pay"
)

# Create tables for each variable
work_tables <- list()

for (var in work_variables) {
  table_data <- work_conditions_summary[[var]] %>%
    arrange(desc(Percentage))  # Order by percentage descending
  
  work_table <- table_data %>%
    flextable() %>%
    set_header_labels(
      Response = variable_labels[var],
      Count = "Count",
      Percentage = "Percentage (%)"
    ) %>%
    theme_vanilla() %>%
    autofit()
  
  work_tables[[var]] <- work_table
  
  # Print table
  cat("\n", variable_labels[var], "Distribution:\n")
  print(work_table)
}

# Create visualizations for each variable
work_plots <- list()

for (var in work_variables) {
  plot_data <- work_conditions_summary[[var]] %>%
    arrange(Percentage) %>%  # Order for horizontal bar chart
    mutate(Response = factor(Response, levels = Response))  # Preserve order
  
  work_plot <- ggplot(plot_data, aes(x = Percentage, y = Response)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = paste0(Percentage, "%")), 
              hjust = -0.05, size = 3) +
    labs(
      title = paste("Distribution of Responses:", variable_labels[var]),
      x = "Percentage of Respondents",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 9),
      axis.title.x = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  
  work_plots[[var]] <- work_plot
  
  # Print plot
  print(work_plot)
}

# Create a combined summary showing key statistics
work_summary_stats <- data.frame(
  Variable = variable_labels[work_variables],
  Total_Responses = sapply(work_variables, function(var) {
    sum(table(data[[var]], useNA = "no"))
  }),
  Missing_Values = sapply(work_variables, function(var) {
    sum(is.na(data[[var]]))
  }),
  Most_Common_Response = sapply(work_variables, function(var) {
    summary_data <- work_conditions_summary[[var]]
    summary_data$Response[which.max(summary_data$Count)]
  }),
  Most_Common_Percentage = sapply(work_variables, function(var) {
    summary_data <- work_conditions_summary[[var]]
    paste0(max(summary_data$Percentage), "%")
  }),
  stringsAsFactors = FALSE
)

# Create summary table
work_summary_table <- work_summary_stats %>%
  flextable() %>%
  set_header_labels(
    Variable = "Work Condition Variable",
    Total_Responses = "Total Responses",
    Missing_Values = "Missing Values",
    Most_Common_Response = "Most Common Response",
    Most_Common_Percentage = "Percentage"
  ) %>%
  theme_vanilla() %>%
  autofit()

cat("\nSummary of Work Conditions Variables:\n")
work_summary_table

## Cross-tabulation: Notice of Working Hours by Income Group

# Create simplified categorization for Notice of Working Hours
data <- data %>%
  mutate(Notice_Of_Working_Hours_Simplified = case_when(
    Notice_Of_Working_Hours %in% c("Less than 24 hours", "1-3 days", "4-6 days") ~ "Less than a week",
    TRUE ~ Notice_Of_Working_Hours
  ))

# Cross-tabulation with simplified variable
notice_pay_crosstab <- crosstable(data, 
                                  cols = Notice_Of_Working_Hours_Simplified, 
                                  by = income_group,
                                  total = "both",
                                  percent_pattern = "{n} ({p_col})",
                                  percent_digits = 2) %>%
  as_flextable() %>%
  set_caption("Notice of Working Hours by Income Group")

notice_pay_crosstab

## Notice of Working Hours by Ethnicity
notice_ethnicity_crosstab <- crosstable(data, 
                                        cols = Notice_Of_Working_Hours_Simplified, 
                                        by = Ethnicity_Collapsed,
                                        total = "both",
                                        percent_pattern = "{n} ({p_col})",
                                        percent_digits = 2) %>%
  as_flextable() %>%
  set_caption("Notice of Working Hours by Ethnicity")

notice_ethnicity_crosstab


# Statistical analysis of notice of working hours by ethnicity
# First check chi-square assumptions
chi_results <- chisq.test(table(data$Notice_Of_Working_Hours_Simplified, 
                                data$Ethnicity_Collapsed))

min_expected <- min(chi_results$expected)
cat("Chi-square assumption check - Minimum expected frequency:", round(min_expected, 2))
if(min_expected >= 5) {
  cat(" ✓ Chi-square assumptions met\n")
} else {
  cat(" ✗ Chi-square assumptions violated - using Fisher's exact test\n")
}

# Create contingency table for analysis
contingency_table <- table(data$Notice_Of_Working_Hours_Simplified, 
                          data$Ethnicity_Collapsed)

cat("\nContingency Table:\n")
print(contingency_table)

# Fisher's exact test (appropriate when chi-square assumptions violated)
fisher_results <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)

# Calculate Cramér's V for effect size
library(vcd)
cramers_v <- assocstats(contingency_table)$cramer

# Create comprehensive results summary
fisher_summary <- data.frame(
  Test = "Fisher's exact test",
  `p-value` = ifelse(fisher_results$p.value < 0.001, "< 0.001", 
                     round(fisher_results$p.value, 4)),
  `Cramers V` = round(cramers_v, 3),
  `Effect Size` = case_when(
    cramers_v < 0.1 ~ "Negligible",
    cramers_v < 0.3 ~ "Small",
    cramers_v < 0.5 ~ "Medium", 
    TRUE ~ "Large"
  ),
  Significance = ifelse(fisher_results$p.value < 0.05, "Significant", "Not significant"),
  stringsAsFactors = FALSE
)

# Display results
cat("\nFisher's Exact Test Results:\n")
print(fisher_summary)

# Post-hoc analysis assessment
cat("\nPost-hoc Analysis Assessment:\n")
if(fisher_results$p.value < 0.05) {
  cat("✓ Overall association is significant\n")
  
  # Check if post-hoc tests are warranted
  ethnicity_levels <- length(unique(data$Ethnicity_Collapsed[!is.na(data$Ethnicity_Collapsed)]))
  notice_levels <- length(unique(data$Notice_Of_Working_Hours_Simplified[!is.na(data$Notice_Of_Working_Hours_Simplified)]))
  
  if(ethnicity_levels > 2 || notice_levels > 2) {
    cat("✓ Multiple categories present - post-hoc pairwise comparisons recommended\n")
    
    # Perform pairwise Fisher's exact tests between ethnicity groups
    cat("\nPairwise Fisher's Exact Tests (Bonferroni corrected):\n")
    
    ethnicity_groups <- unique(data$Ethnicity_Collapsed[!is.na(data$Ethnicity_Collapsed)])
    pairwise_results <- list()
    
    # Get all pairwise combinations
    combinations <- combn(ethnicity_groups, 2, simplify = FALSE)
    
    for(i in seq_along(combinations)) {
      group1 <- combinations[[i]][1]
      group2 <- combinations[[i]][2]
      
      # Create pairwise table
      subset_data <- data[data$Ethnicity_Collapsed %in% c(group1, group2), ]
      subset_table <- table(subset_data$Notice_Of_Working_Hours_Simplified, 
                           subset_data$Ethnicity_Collapsed, useNA = "no")
      subset_table <- subset_table[rowSums(subset_table) > 0, colSums(subset_table) > 0, drop = FALSE]
      
      if(nrow(subset_table) > 1 && ncol(subset_table) > 1) {
        # Fisher's exact test
        pairwise_fisher <- fisher.test(subset_table, simulate.p.value = TRUE, B = 5000)
        pairwise_cramers <- assocstats(subset_table)$cramer
        
        # Calculate directional interpretation with safe indexing
        direction <- tryCatch({
          # Use safer indexing with existence checks
          row_idx <- which(rownames(subset_table) == "Less than a week")
          col1_idx <- which(colnames(subset_table) == group1)
          col2_idx <- which(colnames(subset_table) == group2)
          
          if(length(row_idx) > 0 && length(col1_idx) > 0 && length(col2_idx) > 0) {
            group1_count <- subset_table[row_idx, col1_idx]
            group2_count <- subset_table[row_idx, col2_idx]
            
            group1_total <- sum(subset_table[, col1_idx])
            group2_total <- sum(subset_table[, col2_idx])
            
            group1_pct <- round(group1_count / group1_total * 100, 1)
            group2_pct <- round(group2_count / group2_total * 100, 1)
            
            if(group1_pct > group2_pct) {
              paste0(group1, " higher (", group1_pct, "% vs ", group2_pct, "%)")
            } else if(group2_pct > group1_pct) {
              paste0(group2, " higher (", group2_pct, "% vs ", group1_pct, "%)")
            } else {
              paste0("Similar (", group1_pct, "% vs ", group2_pct, "%)")
            }
          } else {
            "Cannot calculate - missing categories"
          }
        }, error = function(e) {
          "Error in calculation"
        })
        
        pairwise_results[[i]] <- data.frame(
          Comparison = paste(group1, "vs", group2),
          p_value = pairwise_fisher$p.value,
          cramers_v = round(pairwise_cramers, 3),
          less_than_week_direction = direction,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Combine results and apply Bonferroni correction
    if(length(pairwise_results) > 0) {
      pairwise_df <- do.call(rbind, pairwise_results)
      pairwise_df$p_adjusted = p.adjust(pairwise_df$p_value, method = "bonferroni")
      pairwise_df$significant_adjusted = pairwise_df$p_adjusted < 0.05
      
      # Add effect size interpretation
      pairwise_df$effect_size = case_when(
        pairwise_df$cramers_v < 0.1 ~ "Negligible",
        pairwise_df$cramers_v < 0.3 ~ "Small",
        pairwise_df$cramers_v < 0.5 ~ "Medium", 
        TRUE ~ "Large"
      )
      
      # Round p-values for display
      pairwise_df$p_value = round(pairwise_df$p_value, 4)
      pairwise_df$p_adjusted = round(pairwise_df$p_adjusted, 4)
      
      # Reorder columns for better readability
      pairwise_df <- pairwise_df[, c("Comparison", "p_value", "p_adjusted", "significant_adjusted", 
                                   "cramers_v", "effect_size", "less_than_week_direction")]
      
      print(pairwise_df)
      
      # Highlight significant findings
      significant_pairs <- pairwise_df[pairwise_df$significant_adjusted, ]
      if(nrow(significant_pairs) > 0) {
        cat("\nSignificant pairwise differences (after Bonferroni correction):\n")
        for(i in 1:nrow(significant_pairs)) {
          cat("•", significant_pairs$Comparison[i], 
              "(p =", significant_pairs$p_adjusted[i], 
              ", Cramér's V =", significant_pairs$cramers_v[i], ")\n")
          cat("  Direction:", significant_pairs$less_than_week_direction[i], "\n")
        }
      } else {
        cat("\nNo significant pairwise differences after Bonferroni correction.\n")
      }
    }
    
  } else {
    cat("→ Only 2 categories in each variable - no post-hoc tests needed\n")
  }
} else {
  cat("→ Overall association not significant - post-hoc tests not recommended\n")
}
  