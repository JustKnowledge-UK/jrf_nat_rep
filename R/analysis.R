# Analysis of nationally representative data
# January 2024
# Jolyon Miles-Wilson

# Questions of interest are
# - Question 1s: these are the indicator questions (Q3 previously)
# - Question 2s: this is the long-term/short-term question (Q5 previously)
# - Question 3s: this is the blanket statement "I am outsourced/agency" (Q7/8 previously)

rm(list = ls())

library(haven)
# data
data <- read_sav("./Data/cleaned_full_data.sav")

#########################################
### Q3: Outsourced/agency self-report ###
#########################################

# 7.6% are sure they're outsourced, 7.1% think they might be outsourced
data %>%
  group_by(Q3v3a) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  )

# Of those who are sure they're outsourced, 20.7% are sure they're also an agency 
# worker and 11% think they might also be an agency worker
data %>%
  filter(Q3v3a == 1) %>%
  group_by(Q3v3b) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  )

# Of those who think they might be outsourced, 17.9% are sure they are also an
# agency worker and 29.5% think they might also be an agency worker
data %>%
  filter(Q3v3a == 2) %>%
  group_by(Q3v3c) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  )

# Of those who are not outsourced, 4.0% are sure they're an agency worker
# and 1.6% think they might be an agency worker
data %>%
  filter(Q3v3a == 3) %>%
  group_by(Q3v3d) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  )

