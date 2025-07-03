#Import data and libraries

library(tidyverse)
setwd("~/repos/JRF Nat Rep")
data <- read_csv("./Experiential/data/2025-06-30 - clean_data_jrf_experiential.csv")

data$Ethnicity_Collapsed <- relevel(factor(data$Ethnicity_Collapsed), ref = "White British")
data$Sex <- relevel(factor(data$Sex), ref = "Male")
data$Region <- relevel(factor(data$Region), ref = "London")
data$income_group <- relevel(factor(data$income_group), ref = "Mid")

# Create filtered dataset excluding income outliers for income-related analyses
# income_drop: 1 = outlier, 0 = not outlier (from data_cleaning.R)
data_no_income_outliers <- data %>%
  filter(income_drop == 0)

cat("Data quality check - Income outlier filtering:\n")
cat("Original dataset:", nrow(data), "rows\n")
cat("After removing income outliers:", nrow(data_no_income_outliers), "rows\n")
cat("Income outliers removed:", nrow(data) - nrow(data_no_income_outliers), 
    "(", round((nrow(data) - nrow(data_no_income_outliers))/nrow(data)*100, 1), "%)\n\n")

# Report Structure

# report being paid less than if they were in-house
library(crosstable) 
library(flextable)

Paid_Less_Cross <- crosstable(data_no_income_outliers %>% 
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

# Apply the same transformations to the filtered dataset
data_no_income_outliers <- data_no_income_outliers %>%
  mutate(across(starts_with("Pros_And_Cons"), 
                ~ recode_pros_cons(as.character(.)),
                .names = "{.col}_numeric")) %>%
  mutate(
    total = rowSums(select(., contains("_numeric")), na.rm = TRUE),
    num_neg = rowSums(select(., contains("_numeric")) == -1, na.rm = TRUE)
  ) %>%
  mutate(neg_category = case_when(
    num_neg == 0 ~ "0",
    num_neg == 1 ~ "1",
    num_neg == 2 ~ "2",
    num_neg == 3 ~ "3",
    num_neg == 4 ~ "4",
    num_neg >= 5 ~ "5+"
  )) %>%
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
means_by_group <- data_no_income_outliers %>%
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
regression_data <- data_no_income_outliers %>%
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

# Apply same simplification to filtered dataset
data_no_income_outliers <- data_no_income_outliers %>%
  mutate(Notice_Of_Working_Hours_Simplified = case_when(
    Notice_Of_Working_Hours %in% c("Less than 24 hours", "1-3 days", "4-6 days") ~ "Less than a week",
    TRUE ~ Notice_Of_Working_Hours
  ))

# Cross-tabulation with simplified variable (using filtered dataset for income analysis)
notice_pay_crosstab <- crosstable(data_no_income_outliers, 
                                  cols = Notice_Of_Working_Hours_Simplified, 
                                  by = income_group,
                                  total = "both",
                                  percent_pattern = "{n} ({p_col})",
                                  percent_digits = 2) %>%
  as_flextable() %>%
  set_caption("Notice of Working Hours by Income Group")

notice_pay_crosstab

## Cross-tabulation: Guaranteed Hours by Income Group

guaranteed_hours_income_crosstab <- crosstable(data_no_income_outliers, 
                                              cols = Guaranteed_Hours, 
                                              by = income_group,
                                              total = "both",
                                              percent_pattern = "{n} ({p_col})",
                                              percent_digits = 2) %>%
  as_flextable() %>%
  set_caption("Guaranteed Hours by Income Group")

guaranteed_hours_income_crosstab

## Cross-tabulation: Sick Pay by Income Group

sick_pay_income_crosstab <- crosstable(data_no_income_outliers, 
                                      cols = Sick_Pay, 
                                      by = income_group,
                                      total = "both",
                                      percent_pattern = "{n} ({p_col})",
                                      percent_digits = 2) %>%
  as_flextable() %>%
  set_caption("Sick Pay by Income Group")

sick_pay_income_crosstab

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

## Cross-tabulation and Statistical Analysis: Guaranteed Hours by Ethnicity

# Cross-tabulation table
guaranteed_hours_ethnicity_crosstab <- crosstable(data, 
                                                 cols = Guaranteed_Hours, 
                                                 by = Ethnicity_Collapsed,
                                                 total = "both",
                                                 percent_pattern = "{n} ({p_col})",
                                                 percent_digits = 2) %>%
  as_flextable() %>%
  set_caption("Guaranteed Hours by Ethnicity")

guaranteed_hours_ethnicity_crosstab

# Statistical analysis of guaranteed hours by ethnicity
# First check chi-square assumptions
chi_results_hours <- chisq.test(table(data$Guaranteed_Hours, 
                                     data$Ethnicity_Collapsed))

min_expected_hours <- min(chi_results_hours$expected)
cat("Chi-square assumption check - Minimum expected frequency:", round(min_expected_hours, 2))
if(min_expected_hours >= 5) {
  cat(" ✓ Chi-square assumptions met\n")
} else {
  cat(" ✗ Chi-square assumptions violated - using Fisher's exact test\n")
}

# Create contingency table for analysis
contingency_table_hours <- table(data$Guaranteed_Hours, 
                                data$Ethnicity_Collapsed)

cat("\nContingency Table:\n")
print(contingency_table_hours)

# Fisher's exact test (appropriate when chi-square assumptions violated)
fisher_results_hours <- fisher.test(contingency_table_hours, simulate.p.value = TRUE, B = 10000)

# Calculate Cramér's V for effect size
cramers_v_hours <- assocstats(contingency_table_hours)$cramer

# Create comprehensive results summary
fisher_summary_hours <- data.frame(
  Test = "Fisher's exact test",
  `p-value` = ifelse(fisher_results_hours$p.value < 0.001, "< 0.001", 
                     round(fisher_results_hours$p.value, 4)),
  `Cramers V` = round(cramers_v_hours, 3),
  `Effect Size` = case_when(
    cramers_v_hours < 0.1 ~ "Negligible",
    cramers_v_hours < 0.3 ~ "Small",
    cramers_v_hours < 0.5 ~ "Medium", 
    TRUE ~ "Large"
  ),
  Significance = ifelse(fisher_results_hours$p.value < 0.05, "Significant", "Not significant"),
  stringsAsFactors = FALSE
)

# Display results
cat("\nFisher's Exact Test Results:\n")
print(fisher_summary_hours)

# Post-hoc analysis assessment
cat("\nPost-hoc Analysis Assessment:\n")
if(fisher_results_hours$p.value < 0.05) {
  cat("✓ Overall association is significant\n")
  
  # Check if post-hoc tests are warranted
  ethnicity_levels_hours <- length(unique(data$Ethnicity_Collapsed[!is.na(data$Ethnicity_Collapsed)]))
  hours_levels <- length(unique(data$Guaranteed_Hours[!is.na(data$Guaranteed_Hours)]))
  
  if(ethnicity_levels_hours > 2 || hours_levels > 2) {
    cat("✓ Multiple categories present - post-hoc pairwise comparisons recommended\n")
    
    # Perform pairwise Fisher's exact tests between ethnicity groups
    cat("\nPairwise Fisher's Exact Tests (Bonferroni corrected):\n")
    
    ethnicity_groups_hours <- unique(data$Ethnicity_Collapsed[!is.na(data$Ethnicity_Collapsed)])
    pairwise_results_hours <- list()
    
    # Get all pairwise combinations
    combinations_hours <- combn(ethnicity_groups_hours, 2, simplify = FALSE)
    
    for(i in seq_along(combinations_hours)) {
      group1 <- combinations_hours[[i]][1]
      group2 <- combinations_hours[[i]][2]
      
      # Create pairwise table
      subset_data <- data[data$Ethnicity_Collapsed %in% c(group1, group2), ]
      subset_table <- table(subset_data$Guaranteed_Hours, 
                           subset_data$Ethnicity_Collapsed, useNA = "no")
      subset_table <- subset_table[rowSums(subset_table) > 0, colSums(subset_table) > 0, drop = FALSE]
      
      if(nrow(subset_table) > 1 && ncol(subset_table) > 1) {
        # Fisher's exact test
        pairwise_fisher <- fisher.test(subset_table, simulate.p.value = TRUE, B = 5000)
        pairwise_cramers <- assocstats(subset_table)$cramer
        
        # Calculate directional interpretation with safe indexing
        # Focus on "35+ hours" as the key category (full-time equivalent)
        direction <- tryCatch({
          # Use safer indexing with existence checks
          row_idx <- which(rownames(subset_table) == "35+ hours")
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
              paste0(group1, " higher 35+ hours (", group1_pct, "% vs ", group2_pct, "%)")
            } else if(group2_pct > group1_pct) {
              paste0(group2, " higher 35+ hours (", group2_pct, "% vs ", group1_pct, "%)")
            } else {
              paste0("Similar 35+ hours (", group1_pct, "% vs ", group2_pct, "%)")
            }
          } else {
            "Cannot calculate - missing categories"
          }
        }, error = function(e) {
          "Error in calculation"
        })
        
        pairwise_results_hours[[i]] <- data.frame(
          Comparison = paste(group1, "vs", group2),
          p_value = pairwise_fisher$p.value,
          cramers_v = round(pairwise_cramers, 3),
          hours_35plus_direction = direction,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Combine results and apply Bonferroni correction
    if(length(pairwise_results_hours) > 0) {
      pairwise_df_hours <- do.call(rbind, pairwise_results_hours)
      pairwise_df_hours$p_adjusted = p.adjust(pairwise_df_hours$p_value, method = "bonferroni")
      pairwise_df_hours$significant_adjusted = pairwise_df_hours$p_adjusted < 0.05
      
      # Add effect size interpretation
      pairwise_df_hours$effect_size = case_when(
        pairwise_df_hours$cramers_v < 0.1 ~ "Negligible",
        pairwise_df_hours$cramers_v < 0.3 ~ "Small",
        pairwise_df_hours$cramers_v < 0.5 ~ "Medium", 
        TRUE ~ "Large"
      )
      
      # Round p-values for display
      pairwise_df_hours$p_value = round(pairwise_df_hours$p_value, 4)
      pairwise_df_hours$p_adjusted = round(pairwise_df_hours$p_adjusted, 4)
      
      # Reorder columns for better readability
      pairwise_df_hours <- pairwise_df_hours[, c("Comparison", "p_value", "p_adjusted", "significant_adjusted", 
                                               "cramers_v", "effect_size", "hours_35plus_direction")]
      
      print(pairwise_df_hours)
      
      # Highlight significant findings
      significant_pairs_hours <- pairwise_df_hours[pairwise_df_hours$significant_adjusted, ]
      if(nrow(significant_pairs_hours) > 0) {
        cat("\nSignificant pairwise differences (after Bonferroni correction):\n")
        for(i in 1:nrow(significant_pairs_hours)) {
          cat("•", significant_pairs_hours$Comparison[i], 
              "(p =", significant_pairs_hours$p_adjusted[i], 
              ", Cramér's V =", significant_pairs_hours$cramers_v[i], ")\n")
          cat("  Direction:", significant_pairs_hours$hours_35plus_direction[i], "\n")
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

## Rights Violations Analysis

# Create mapping of rights violations variables to readable labels
rights_violations_mapping <- data.frame(
  Label = c(
    "Not being paid on time",
    "Not being paid the full amount I am entitled to for the work I have completed", 
    "Not being given time off that I am entitled to",
    "Not being paid for paid leave that I am entitled to",
    "Not being given pay that I am entitled to while being off sick",
    "Not being provided with a pay slip that shows how much I am being paid over a certain period of time",
    "Not having adequate health and safety protections"
  ),
  Variable = c(
    "RightsViolations_Paid_On_Time",
    "RightsViolations_Paid_Correct_Amount", 
    "RightsViolations_Leave_Entitlement",
    "RightsViolations_Holiday_Pay",
    "RightsViolations_Sick_Pay", 
    "RightsViolations_Pay_Slip",
    "RightsViolations_Health_Safety"
  )
)

# Count responses for each rights violation (excluding NAs)
rights_violations_counts <- data.frame(
  Label = character(0),
  Count = numeric(0),
  Percentage = numeric(0)
)

for(i in 1:nrow(rights_violations_mapping)) {
  var_name <- rights_violations_mapping$Variable[i]
  label_text <- rights_violations_mapping$Label[i]
  
  # Count non-NA responses (people who experienced this violation)
  count <- sum(!is.na(data[[var_name]]))
  total_responses <- nrow(data)
  percentage <- round(count / total_responses * 100, 0)  # Round to whole numbers like reference
  
  rights_violations_counts <- rbind(rights_violations_counts, data.frame(
    Label = label_text,
    Count = count,
    Percentage = percentage
  ))
}

# Sort by percentage (descending order)
rights_violations_counts <- rights_violations_counts[order(rights_violations_counts$Percentage, decreasing = TRUE), ]

# Create the horizontal bar chart
rights_violations_plot <- ggplot(rights_violations_counts, aes(x = Percentage, y = reorder(Label, Percentage))) +
  geom_col(fill = "#8B4A6B", alpha = 0.9) +  # Dark purple/maroon color like reference
  geom_text(aes(label = paste0(Percentage, "%")), 
            hjust = -0.05, size = 3.5, color = "white", fontface = "bold") +
  labs(
    title = "Proportion of outsourced workers who report having\ngone without key entitlements",
    subtitle = "\"Some workers don't receive everything that they are entitled to from their\nemployer. In your current role, have you experienced any of the following –\nplease tick all that apply.\"",
    x = "Percentage of outsourced workers",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 20)),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", size = 0.5)
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), 
                     breaks = seq(0, 60, 10), 
                     expand = expansion(mult = c(0, 0.1))) +
  coord_cartesian(xlim = c(0, max(rights_violations_counts$Percentage) + 5))

rights_violations_plot

# Create summary table
rights_violations_table <- rights_violations_counts %>%
  flextable() %>%
  set_header_labels(
    Label = "Rights Violation",
    Count = "Count",
    Percentage = "Percentage (%)"
  ) %>%
  theme_vanilla() %>%
  autofit()

rights_violations_table


# Create a custom function to recode experiences
# Regular rights violations are coded as -1 (negative)
# "None" responses are coded as +1 (positive)
# NA values coded as 0 (not experienced)
recode_rights_experiences <- function(x, is_none_column = FALSE) {
  if (is_none_column) {
    # For RightsViolations_None column
    case_when(
      is.na(x) ~ 0,          # NA means not answered
      TRUE ~ 1               # "None" response is positive (+1)
    )
  } else {
    # For all other rights columns
    case_when(
      is.na(x) ~ 0,          # NA means not experienced (0)
      TRUE ~ -1              # Any non-NA value means negative experience (-1)
    )
  }
}

# DIAGNOSTIC: Check raw rights violation variables first
cat("\n=== DIAGNOSTIC: Raw Rights Violations Data ===\n")
rights_vars <- c("RightsViolations_Paid_On_Time", "RightsViolations_Paid_Correct_Amount", 
                 "RightsViolations_Leave_Entitlement", "RightsViolations_Holiday_Pay",
                 "RightsViolations_Sick_Pay", "RightsViolations_Pay_Slip", 
                 "RightsViolations_Health_Safety", "RightsViolations_None")

for(var in rights_vars) {
  if(var %in% names(data)) {
    non_na_count <- sum(!is.na(data[[var]]))
    total_count <- nrow(data)
    pct <- round(non_na_count / total_count * 100, 1)
    cat(var, "- Non-NA responses:", non_na_count, "(", pct, "%)\n")
    
    # Show unique values for first few variables
    if(var %in% rights_vars[1:3]) {
      cat("  Unique values:", paste(unique(data[[var]]), collapse = ", "), "\n")
    }
  }
}

# Quick comparison with earlier method
cat("\n=== COMPARISON: Simple Counting Method ===\n")
simple_counts <- data.frame(
  Variable = rights_vars[1:7],  # Exclude "None"
  Count = numeric(7),
  Percentage = numeric(7)
)

for(i in 1:7) {
  var_name <- rights_vars[i]
  if(var_name %in% names(data)) {
    count <- sum(!is.na(data[[var_name]]))
    percentage <- round(count / nrow(data) * 100, 1)
    simple_counts[i, "Count"] <- count
    simple_counts[i, "Percentage"] <- percentage
  }
}
print(simple_counts)
cat("\n")

# Process the data - create numeric columns for rights variables
rights_data <- data %>%
  # Recode regular rights violations
  mutate(across(starts_with("Rights") & !contains("None"), 
                ~ recode_rights_experiences(as.character(.)),
                .names = "{.col}_numeric")) %>%
  # Special handling for None column
  mutate(RightsViolations_None_numeric = 
           recode_rights_experiences(RightsViolations_None, is_none_column = TRUE)) %>%
  # Select both original and numeric columns plus demographic variables
  dplyr::select(
    starts_with("Rights"),
    contains("_numeric"),
    Sex, Age, Ethnicity, Ethnicity_Collapsed, Region, Education_Band,
    OutsourcedNonOL, BORNUK
  )

# DIAGNOSTIC: Check the recoding results
cat("=== DIAGNOSTIC: After Recoding ===\n")
numeric_rights_vars <- paste0(rights_vars[1:7], "_numeric")
for(var in numeric_rights_vars) {
  if(var %in% names(rights_data)) {
    neg_ones <- sum(rights_data[[var]] == -1, na.rm = TRUE)
    zeros <- sum(rights_data[[var]] == 0, na.rm = TRUE)
    ones <- sum(rights_data[[var]] == 1, na.rm = TRUE)
    cat(var, "- (-1s):", neg_ones, ", (0s):", zeros, ", (1s):", ones, "\n")
  }
}

# Show sample of original vs recoded for first variable
cat("\nSample comparison (first 10 rows, first rights variable):\n")
sample_comparison <- rights_data[1:10, c("RightsViolations_Paid_On_Time", "RightsViolations_Paid_On_Time_numeric")]
print(sample_comparison)

# COMPARISON: How do our results compare to the earlier bar chart analysis?
cat("\n=== COMPARISON: Current Method vs Earlier Analysis ===\n")
cat("Earlier horizontal bar chart percentages (from lines 1265-1268):\n")
for(i in 1:7) {
  var_name <- rights_vars[i]
  if(var_name %in% names(data)) {
    earlier_count <- sum(!is.na(data[[var_name]]))
    earlier_pct <- round(earlier_count / nrow(data) * 100, 1)
    
    # Current method count (people with -1 in this variable)
    numeric_var <- paste0(var_name, "_numeric")
    if(numeric_var %in% names(rights_data)) {
      current_count <- sum(rights_data[[numeric_var]] == -1, na.rm = TRUE)
      current_pct <- round(current_count / nrow(rights_data) * 100, 1)
      cat(var_name, "\n")
      cat("  Earlier method:", earlier_count, "(", earlier_pct, "%)\n")
      cat("  Current method:", current_count, "(", current_pct, "%)\n")
    }
  }
}
cat("\n")

# Calculate negative experiences count and categorize
cat("Rights violations analysis - Data quality check:\n")
cat("Total respondents in dataset:", nrow(rights_data), "\n")

rights_summary <- rights_data %>%
  # Select only the violation columns, excluding the "None" column
  dplyr::select(contains("_numeric") & !contains("None_numeric")) %>% 
  # Remove rows where all rights variables are NA
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  {cat("Respondents with at least one rights violation response:", nrow(.), "\n"); .} %>%
  # Count negative responses per person
  mutate(
    num_neg = rowSums(. == -1, na.rm = TRUE)
  ) %>%
  # DIAGNOSTIC: Show distribution of num_neg before categorizing
  {cat("\nRaw distribution of num_neg:\n"); print(table(.$num_neg)); cat("\n"); .} %>%
  # Categorize by number of negative experiences
  mutate(neg_category = case_when(
    num_neg == 0 ~ "0",
    num_neg == 1 ~ "1",
    num_neg == 2 ~ "2",
    num_neg == 3 ~ "3",
    num_neg == 4 ~ "4",
    num_neg >= 5 ~ "5+"
  )) %>%
  # Count respondents in each category and calculate percentages
  count(neg_category) %>%
  mutate(percent = (n / sum(n)) * 100)

# Visualize the distribution of negative rights experiences
ggplot(rights_summary, aes(x = neg_category, y = percent, fill = neg_category)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5) +
  labs(title = "Distribution of Negative Responses on Rights",
       x = "Number of Negative Responses (-1s)",
       y = "Percentage of Respondents") +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal()

## Discrimination Analysis (Multiple Variables)

# Define the valid discrimination response categories
valid_discrimination_responses <- c("Never", "Rarely", "Sometimes", "Often")

# Define demographic groups structure (reusable across all discrimination variables)
demographic_groups <- list(
  list(var = NULL, value = NULL, name = "All outsourced workers"),
  list(var = "Ethnicity_Collapsed", value = "Black/African/Caribbean/Black British", name = "Black workers"),
  list(var = "Ethnicity_Collapsed", value = "Asian/Asian British", name = "Asian workers"), 
  list(var = "Sex", value = "Female", name = "Female workers"),
  list(var = "BORNUK_binary", value = "Not born in UK", name = "Workers born outside of the UK")
)

# Create master function for discrimination analysis
create_discrimination_analysis <- function(discrimination_var, discrimination_type, data, demographic_groups) {
  cat("\n=== ", toupper(discrimination_type), " DISCRIMINATION ANALYSIS ===\n")
  # Check data availability first
  total_responses <- sum(!is.na(data[[discrimination_var]]) & 
                        data[[discrimination_var]] %in% valid_discrimination_responses)
  
  cat("Total valid responses for", discrimination_var, ":", total_responses, "\n")
  
  # Only proceed if we have sufficient data
  if (total_responses < 10) {
    cat("Insufficient data for", discrimination_type, "discrimination analysis (< 10 responses)\n")
    return(NULL)
  }
  
  # Helper function to calculate discrimination percentages by demographic group  
  calculate_discrimination_by_group <- function(data, group_var, group_value = NULL, group_name, discrim_var) {
    
    # Filter data based on group
    if (!is.null(group_var)) {
      filtered_data <- data %>% filter(!!sym(group_var) == group_value)
    } else {
      filtered_data <- data  # For "All outsourced workers"
    }
    
    # Calculate percentages for each discrimination response
    discrimination_counts <- filtered_data %>%
      filter(!!sym(discrim_var) %in% valid_discrimination_responses) %>%
      count(!!sym(discrim_var)) %>%
      mutate(
        Group = group_name,
        Total = sum(n),
        Percentage = round((n / Total) * 100, 0)
      ) %>%
      rename(Response = !!sym(discrim_var))
    
    return(discrimination_counts)
  }
  
  # Calculate discrimination percentages for each demographic group
  discrimination_results <- bind_rows(
    lapply(demographic_groups, function(group) {
      calculate_discrimination_by_group(data, group$var, group$value, group$name, discrimination_var)
    })
  )
  
  # Remove groups with no data
  discrimination_results <- discrimination_results %>% filter(Total > 0)
  
  if (nrow(discrimination_results) == 0) {
    cat("No valid data found for", discrimination_type, "discrimination analysis\n")
    return(NULL)
  }
  
  # Ensure proper ordering of response categories
  discrimination_results$Response <- factor(discrimination_results$Response, 
                                          levels = c("Never", "Rarely", "Sometimes", "Often"))
  
  # Create color palette for response categories (light to dark)
  response_colors <- c("Never" = "#E8F4F8", "Rarely" = "#9ECAE1", "Sometimes" = "#4292C6", "Often" = "#084594")
  
  # Create the faceted horizontal bar chart
  discrimination_plot <- ggplot(discrimination_results, aes(x = Percentage, y = Response, fill = Response)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = paste0(Percentage, "%")), 
              hjust = -0.1, size = 3, color = "black") +
    facet_wrap(~ Group, ncol = 2, scales = "free_x") +
    scale_fill_manual(values = response_colors) +
    labs(
      title = paste("Experiences of", discrimination_type, "discrimination from in-house workers"),
      subtitle = "Percentage of workers reporting each level of discrimination experience",
      x = "Percentage of workers",
      y = "Discrimination experience"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 15)),
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", size = 0.3)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  
  print(discrimination_plot)
  
  # Create summary table
  discrimination_table <- discrimination_results %>%
    dplyr::select(Group, Response, n, Percentage) %>%
    rename(
      "Demographic Group" = Group,
      "Response Category" = Response,
      "Count" = n,
      "Percentage (%)" = Percentage
    ) %>%
    flextable() %>%
    add_header_lines(paste(toupper(discrimination_type), "DISCRIMINATION SUMMARY")) %>%
    theme_vanilla() %>%
    autofit()
  
  print(discrimination_table)
  
  # Return results for potential further analysis
  return(list(
    plot = discrimination_plot,
    table = discrimination_table,
    data = discrimination_results
  ))
}

# Define discrimination variables to analyze
discrimination_vars <- list(
  list(var = "Inhouse_Discrimination_Sex", type = "sex-based"),
  list(var = "Inhouse_Discrimination_Ethnicity", type = "ethnicity-based"),
  list(var = "Inhouse_Discrimination_Age", type = "age-based"),
  list(var = "Inhouse_Discrimination_Disability", type = "disability-based"),
  list(var = "Inhouse_Discrimination_Nationality", type = "nationality-based")
)

# Run analysis for each discrimination variable
discrimination_analyses <- list()
for (i in seq_along(discrimination_vars)) {
  discrim <- discrimination_vars[[i]]
  cat("\n", rep("=", 60), "\n")
  
  result <- create_discrimination_analysis(
    discrimination_var = discrim$var,
    discrimination_type = discrim$type,
    data = data,
    demographic_groups = demographic_groups
  )
  
  if (!is.null(result)) {
    discrimination_analyses[[discrim$type]] <- result
  }
}

## CLARITY QUESTIONS ANALYSIS

cat("\n=== CLARITY QUESTIONS ANALYSIS ===\n")

# Define the clarity questions with their descriptions
clarity_questions <- data.frame(
  Variable = paste0("Clarity", 1:11),
  Question = c(
    "Clear about who to speak to about pay problems",
    "Clear about who to speak to about other rights/entitlements", 
    "Straightforward to understand who approves time off",
    "Clear about who to speak to about promotion",
    "Clear communication between organizations",
    "Clear about role responsibilities",
    "Confident can communicate improvements to right people",
    "Confident opinion will be respected about improvements",
    "Management prevents discrimination",
    "Management takes bullying complaints seriously",
    "Management takes racism complaints seriously"
  ),
  stringsAsFactors = FALSE
)

# Prepare data for visualization - convert to long format
clarity_long <- data %>%
  dplyr::select(all_of(paste0("Clarity", 1:11))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  left_join(clarity_questions, by = "Variable") %>%
  mutate(
    # Ensure consistent response order
    Response = factor(Response, levels = c(
      "Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
      "Somewhat agree", "Strongly agree"
    )),
    # Create shorter question labels for plotting
    Question_Short = case_when(
      Variable == "Clarity1" ~ "Pay problems contact",
      Variable == "Clarity2" ~ "Rights/entitlements contact", 
      Variable == "Clarity3" ~ "Time off approval",
      Variable == "Clarity4" ~ "Promotion contact",
      Variable == "Clarity5" ~ "Organizational communication",
      Variable == "Clarity6" ~ "Role responsibilities",
      Variable == "Clarity7" ~ "Can suggest improvements",
      Variable == "Clarity8" ~ "Opinion will be respected",
      Variable == "Clarity9" ~ "Management prevents discrimination",
      Variable == "Clarity10" ~ "Management handles bullying",
      Variable == "Clarity11" ~ "Management handles racism"
    )
  )

# Calculate percentages for each question and response
clarity_summary <- clarity_long %>%
  group_by(Question_Short, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Question_Short) %>%
  mutate(
    total = sum(count),
    percentage = round(count / total * 100, 1)
  ) %>%
  ungroup()

# Create horizontal bar chart
clarity_plot <- ggplot(clarity_summary, aes(x = percentage, y = reorder(Question_Short, desc(Question_Short)), fill = Response)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c(
      "Strongly disagree" = "#d73027",
      "Somewhat disagree" = "#fc8d59", 
      "Neither agree nor disagree" = "#fee08b",
      "Somewhat agree" = "#91bfdb",
      "Strongly agree" = "#4575b4"
    ),
    name = "Response"
  ) +
  labs(
    title = "Clarity and Confidence at Work: Response Distribution",
    subtitle = "How clearly do outsourced workers understand workplace processes and feel confident about communication?",
    x = "Percentage",
    y = "Question Areas",
    caption = "Data: JRF Experiential Survey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

print(clarity_plot)

# Create summary table
clarity_table <- clarity_long %>%
  group_by(Question_Short) %>%
  summarise(
    Total_Responses = n(),
    `Strongly Agree %` = round(mean(Response == "Strongly agree", na.rm = TRUE) * 100, 1),
    `Somewhat Agree %` = round(mean(Response == "Somewhat agree", na.rm = TRUE) * 100, 1),
    `Neither %` = round(mean(Response == "Neither agree nor disagree", na.rm = TRUE) * 100, 1),
    `Somewhat Disagree %` = round(mean(Response == "Somewhat disagree", na.rm = TRUE) * 100, 1),
    `Strongly Disagree %` = round(mean(Response == "Strongly disagree", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(`Strongly Agree %`))

clarity_table_formatted <- clarity_table %>%
  flextable() %>%
  set_header_labels(
    Question_Short = "Question Area",
    Total_Responses = "Total Responses",
    `Strongly Agree %` = "Strongly Agree (%)",
    `Somewhat Agree %` = "Somewhat Agree (%)",
    `Neither %` = "Neither (%)",
    `Somewhat Disagree %` = "Somewhat Disagree (%)",
    `Strongly Disagree %` = "Strongly Disagree (%)"
  ) %>%
  theme_vanilla() %>%
  autofit()

cat("\nClarity Questions Summary Table:\n")
clarity_table_formatted

## CLARITY BY INCOME GROUP ANALYSIS

cat("\n=== CLARITY QUESTIONS BY INCOME GROUP ===\n")

# Prepare data with income group information
clarity_income_long <- data_no_income_outliers %>%
  dplyr::filter(!is.na(income_group)) %>%
  dplyr::select(all_of(paste0("Clarity", 1:11)), income_group) %>%
  pivot_longer(
    cols = starts_with("Clarity"),
    names_to = "Variable",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  left_join(clarity_questions, by = "Variable") %>%
  mutate(
    Response = factor(Response, levels = c(
      "Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
      "Somewhat agree", "Strongly agree"
    )),
    Question_Short = case_when(
      Variable == "Clarity1" ~ "Pay problems contact",
      Variable == "Clarity2" ~ "Rights/entitlements contact", 
      Variable == "Clarity3" ~ "Time off approval",
      Variable == "Clarity4" ~ "Promotion contact",
      Variable == "Clarity5" ~ "Organizational communication",
      Variable == "Clarity6" ~ "Role responsibilities",
      Variable == "Clarity7" ~ "Can suggest improvements",
      Variable == "Clarity8" ~ "Opinion will be respected",
      Variable == "Clarity9" ~ "Management prevents discrimination",
      Variable == "Clarity10" ~ "Management handles bullying",
      Variable == "Clarity11" ~ "Management handles racism"
    )
  )

# Calculate percentages by income group for selected key questions
key_clarity_questions <- c("Clarity1", "Clarity2", "Clarity3", "Clarity4", "Clarity5",
                           "Clarity6", "Clarity7", "Clarity8","Clarity9", "Clarity10", "Clarity11")
key_question_labels <- c(
  "Clarity1" = "Pay problems contact",
  "Clarity2" = "Rights/entitlements contact", 
  "Clarity3" ~ "Time off approval",
  "Clarity4" = "Promotion contact",
  "Clarity5" ~ "Organizational communication",
  "Clarity6" ~ "Role responsibilities",
  "Clarity7" = "Can suggest improvements",
  "Clarity8" = "Opinion will be respected",
  "Clarity9" ~ "Management prevents discrimination",
  "Clarity10" ~ "Management handles bullying",
  "Clarity11" ~ "Management handles racism"
)



clarity_income_summary <- clarity_income_long %>%
  dplyr::filter(Variable %in% key_clarity_questions) %>%
  group_by(income_group, Question_Short, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(income_group, Question_Short) %>%
  mutate(
    total = sum(count),
    percentage = round(count / total * 100, 1)
  ) %>%
  ungroup()

# Create faceted plot by income group
clarity_income_plot <- ggplot(clarity_income_summary, aes(x = percentage, y = income_group, fill = Response)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~Question_Short, ncol = 2, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "Strongly disagree" = "#d73027",
      "Somewhat disagree" = "#fc8d59", 
      "Neither agree nor disagree" = "#fee08b",
      "Somewhat agree" = "#91bfdb",
      "Strongly agree" = "#4575b4"
    ),
    name = "Response"
  ) +
  labs(
    title = "Workplace Clarity by Income Group",
    subtitle = "Key clarity questions showing differences across income levels",
    x = "Percentage",
    y = "Income Group",
    caption = "Data: JRF Experiential Survey (excluding income outliers)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

print(clarity_income_plot)

# Create summary comparison table
clarity_by_income_comparison <- clarity_income_long %>%
  filter(Variable %in% key_clarity_questions) %>%
  group_by(income_group, Question_Short) %>%
  summarise(
    Total_Responses = n(),
    `Agree %` = round(mean(Response %in% c("Strongly agree", "Somewhat agree"), na.rm = TRUE) * 100, 1),
    `Disagree %` = round(mean(Response %in% c("Strongly disagree", "Somewhat disagree"), na.rm = TRUE) * 100, 1),
    `Neither %` = round(mean(Response == "Neither agree nor disagree", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(Question_Short, desc(`Agree %`))

clarity_income_table <- clarity_by_income_comparison %>%
  flextable() %>%
  set_header_labels(
    income_group = "Income Group",
    Question_Short = "Question Area",
    Total_Responses = "Total",
    `Agree %` = "Agree (%)",
    `Disagree %` = "Disagree (%)",
    `Neither %` = "Neither (%)"
  ) %>%
  theme_vanilla() %>%
  autofit()

cat("\nClarity by Income Group Summary:\n")
clarity_income_table

## Clarity regression
Clarity.fit1<- (lm(Clarity_Overall_Mean ~ 
                     Age +
                     Sex +
                     Ethnicity_Collapsed +
                     BORNUK_binary +
                     Region +
                     income_group, weights = Outsourced, data_no_income_outliers)) 

summary(Clarity.fit1) # some significant

## CLARITY REGRESSION RESULTS VISUALIZATION

cat("\n=== CLARITY REGRESSION FOREST PLOT ===\n")

# Load broom for tidy regression results
library(broom)

# Extract regression results
clarity_results <- tidy(Clarity.fit1, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # Create clean variable labels for non-technical audience
    Variable_Clean = case_when(
      term == "Age" ~ "Age (per year)",
      term == "SexFemale" ~ "Female (vs Male)",
      term == "Ethnicity_CollapsedAsian/Asian British" ~ "Asian/Asian British (vs White British)",
      term == "Ethnicity_CollapsedBlack/African/Caribbean/Black British" ~ "Black/African/Caribbean (vs White British)",
      term == "Ethnicity_CollapsedMixed/Multiple ethnic groups" ~ "Mixed/Multiple ethnicities (vs White British)",
      term == "Ethnicity_CollapsedWhite Other" ~ "White Other (vs White British)",
      term == "Ethnicity_CollapsedArab" ~ "Arab (vs White British)",
      term == "Ethnicity_CollapsedOther ethnic group" ~ "Other ethnic group (vs White British)",
      term == "BORNUK_binaryNot born in UK" ~ "Not born in UK (vs Born in UK)",
      term == "BORNUK_binaryPrefer not to say" ~ "Prefer not to say: Birth country",
      str_starts(term, "Region") ~ str_replace(term, "Region", "Region: "),
      term == "income_groupLow" ~ "Low income (vs Mid income)",
      term == "income_groupHigh" ~ "High income (vs Mid income)",
      TRUE ~ term
    ),
    # Create significance indicators
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ "†",
      TRUE ~ ""
    ),
    # Round estimates and confidence intervals
    estimate_round = round(estimate, 3),
    conf.low_round = round(conf.low, 3),
    conf.high_round = round(conf.high, 3)
  ) %>%
  arrange(desc(abs(estimate)))

# Create forest plot
forest_plot <- ggplot(clarity_results, aes(x = estimate, y = reorder(Variable_Clean, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    size = 0.8,
    color = "#2c3e50",
    fill = "#3498db",
    shape = 21,
    stroke = 0.5
  ) +
  geom_text(
    aes(x = conf.high + 0.02, label = paste0(estimate_round, Significance)),
    hjust = 0,
    size = 3,
    color = "#2c3e50"
  ) +
  labs(
    title = "Factors Associated with Workplace Clarity and Confidence",
    subtitle = "Regression coefficients showing how different factors relate to overall clarity scores",
    x = "Effect on Clarity Score (95% Confidence Interval)",
    y = "Factors",
    caption = "*** p<0.001, ** p<0.01, * p<0.05, † p<0.1\nData: JRF Experiential Survey. Reference groups: Male, White British, Born in UK, Mid income, London region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0)
  )

print(forest_plot)

# Create summary table of key results
key_results <- clarity_results %>%
  filter(p.value < 0.05) %>%  # Only significant 
  dplyr::select(Variable_Clean, estimate_round, conf.low_round, conf.high_round, p.value, Significance) %>%
  arrange(p.value)

if(nrow(key_results) > 0) {
  key_results_table <- key_results %>%
    mutate(
      `P-value` = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ as.character(round(p.value, 3))
      )
    ) %>%
   dplyr::select(-p.value) %>%
    flextable() %>%
    set_header_labels(
      Variable_Clean = "Factor",
      estimate_round = "Effect",
      conf.low_round = "95% CI Lower",
      conf.high_round = "95% CI Upper",
      Significance = "Sig.",
      `P-value` = "P-value"
    ) %>%
    theme_vanilla() %>%
    autofit()
  
  cat("\nStatistically Significant Results (p < 0.1):\n")
  key_results_table
} else {
  cat("\nNo statistically significant results found at p < 0.1 level.\n")
}

# =============================================================================
# WORK PREFERENCE BY INCOME GROUP ANALYSIS
# =============================================================================

cat("\n" %>% rep(3) %>% paste(collapse=""))
cat("=================================================================\n")
cat("WORK PREFERENCE BY INCOME GROUP ANALYSIS\n") 
cat("=================================================================\n\n")

# Create work preference by income group cross-tabulation
work_pref_crosstab <- crosstable(
  data_no_income_outliers %>% 
    dplyr::select(Work_Preference, income_group),
  by = income_group,
  total = "both",
  showNA = "no",
  percent_digits = 1,
  percent_pattern = "{n} ({p_col})"
) %>%
  as_flextable() %>%
  theme_vanilla() %>%
  set_header_labels(
    label = "Work Preference",
    Low = "Low Income",
    Mid = "Mid Income", 
    High = "High Income",
    Total = "Total"
  ) %>%
  autofit()

cat("Work Preference by Income Group Cross-tabulation:\n")
work_pref_crosstab

# Prepare data for visualization
work_pref_data <- data_no_income_outliers %>%
  filter(!is.na(Work_Preference) & !is.na(income_group)) %>%
  count(income_group, Work_Preference) %>%
  group_by(income_group) %>%
  mutate(
    total = sum(n),
    percentage = round(n / total * 100, 1)
  ) %>%
  ungroup()

# Create stacked bar chart
work_pref_plot <- ggplot(work_pref_data, aes(x = percentage, y = income_group, fill = Work_Preference)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c(
      "I would strongly prefer to be an in-house worker" = "#2166ac",
      "I would prefer to be an in-house worker" = "#4393c3", 
      "I have no preference" = "#f7f7f7",
      "Not sure" = "#d1d1d1",
      "I would prefer to be an outsourced worker" = "#fdbf6f",
      "I would strongly prefer to be an outsourced worker" = "#ff7f00"
    ),
    name = "Work Preference"
  ) +
  labs(
    title = "Work Preferences by Income Group",
    subtitle = "Percentage distribution within each income group",
    x = "Percentage (%)",
    y = "Income Group",
    caption = "Source: JRF Experiential Survey 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "gray60", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0))

# Display the plot
print(work_pref_plot)

# Statistical test for association
cat("\n=== Statistical Analysis: Work Preference by Income Group ===\n")

# Create contingency table for statistical testing
work_pref_table_test <- table(data_no_income_outliers$Work_Preference, data_no_income_outliers$income_group)

# Remove rows/columns with all zeros if any
work_pref_table_test <- work_pref_table_test[rowSums(work_pref_table_test) > 0, colSums(work_pref_table_test) > 0]

# Fisher's exact test (use simulation for large tables)
if(nrow(work_pref_table_test) > 2 || ncol(work_pref_table_test) > 2) {
  work_pref_fisher <- fisher.test(work_pref_table_test, simulate.p.value = TRUE, B = 10000)
  cat("Fisher's Exact Test (simulated):\n")
  cat("p-value:", format.pval(work_pref_fisher$p.value, digits = 4), "\n")
} else {
  work_pref_fisher <- fisher.test(work_pref_table_test)
  cat("Fisher's Exact Test:\n")
  cat("p-value:", format.pval(work_pref_fisher$p.value, digits = 4), "\n")
}

# Effect size (Cramér's V)
if(require(vcd, quietly = TRUE)) {
  work_pref_assoc <- vcd::assocstats(work_pref_table_test)
  cat("Cramér's V:", round(work_pref_assoc$cramer, 3), "\n")
  
  # Interpretation of effect size
  cramer_v <- work_pref_assoc$cramer
  if(cramer_v < 0.1) {
    effect_size <- "negligible"
  } else if(cramer_v < 0.3) {
    effect_size <- "small"
  } else if(cramer_v < 0.5) {
    effect_size <- "medium"
  } else {
    effect_size <- "large"
  }
  cat("Effect size interpretation:", effect_size, "\n")
}

# Summary statistics
cat("\n=== Summary Statistics ===\n")
work_pref_summary <- data_no_income_outliers %>%
  filter(!is.na(Work_Preference) & !is.na(income_group)) %>%
  count(income_group, Work_Preference) %>%
  group_by(income_group) %>%
  mutate(
    total = sum(n),
    percentage = round(n / total * 100, 1)
  ) %>%
  ungroup()

# Print key findings
cat("Key findings:\n")
for(income_grp in c("Low", "Mid", "High")) {
  grp_data <- work_pref_summary %>% filter(income_group == income_grp)
  inhouse_total <- sum(grp_data$percentage[grepl("in-house", grp_data$Work_Preference)])
  outsourced_total <- sum(grp_data$percentage[grepl("outsourced", grp_data$Work_Preference)])
  
  cat(sprintf("- %s income: %.1f%% prefer in-house, %.1f%% prefer outsourced\n", 
              income_grp, inhouse_total, outsourced_total))
}

cat("\n=================================================================\n")
cat("END OF ANALYSIS\n")
cat("=================================================================\n")

