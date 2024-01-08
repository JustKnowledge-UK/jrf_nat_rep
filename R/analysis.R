# Analysis of nationally representative data
# January 2024
# Jolyon Miles-Wilson

# Questions of interest are
# - Question 1s: these are the indicator questions (Q3 previously)
# - Question 2s: this is the long-term/short-term question (Q5 previously)
# - Question 3s: this is the blanket statement "I am outsourced/agency" (Q7/8 previously)

rm(list = ls())

library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)

# data
data <- read_sav("Data/UK23626 Workers sample data with nat rep and graduates weight.sav")
# View(data)

# Apply labels to variables
data <- data %>%
  as_factor()

# Drop unnecessary vars
# 14:26 = quals below degree, if we think this would add value we should include them
# 28:36 = age of children, doesnt seem necessary

data <- data[, -c(2,14:26,28,36)]

#################################
##### Identifying variables #####
#################################
# Q3 is the straight up self report outsourced/agency questions
# There are multiple versions. The following code shows that all versions apart
# from the 'Q3v3' ones are empty, so these are the ones to use.
# Q3 <- data %>%
#   select(starts_with("Q3")) %>%
#   map(., ~sum(!is.na(.)))

data <- data %>%
  select(-c(starts_with("Q3v1"), starts_with("Q3v2"), starts_with("Q3v4")))

#change column names
data <- data %>%
  rename(
    ID = MIProRspId,
    Sex = D1_Gender_C,
    Age = D2_Age,
    Region = D4_Region_C,
    Employment_Status = D3_employment,
    Is_an_employee = E2,
    Consent_1_Ethnicity = SCD_Opt_In_1, 
    Consent_2_TU = SCD_Opt_In_2,                
    Consent_3_Health = SCD_Opt_In_3,
    Has_Degree = D6_educ1,
    Has_Second_Job = E1,
    Who_Pays_You = E3,
    Job_Security = E5,
    Work_Circumstance_Agency = E6_1,
    Work_Circumstance_Contract = E6_2,
    Work_Circumstance_Seasonal = E6_3,                       
    Work_Circumstance_Trainee = E6_4,
    Work_Circumstance_Other = E6_5,                       
    Work_Circumstance_Other_TEXT = E6_5_other,
    Org_Size = E7A,
    Is_Supervisor = E7B,
    Job_Title_TEXT = E8A,
    Main_Activity_TEXT = E8B,
    Employer_Activity_TEXT = E9A,
    Employer_Activity_SIC = E9B, #need to check that these were SIC and translate to digits if so
    Biz_Type = E10A,
    Non_Private_Biz_Type = E10B,
    Non_Private_Biz_Type_TEXT = E10B_9_other
  )

#### save csv ####
write_sav(data, "./Data/uncleaned_full_data.sav")

data <- read_sav("./Data/uncleaned_full_data.sav")

#########################################
### Q3: Outsourced/agency self-report ###
#########################################

# 7.6% are sure they're outsourced, 7.1% think they might be outsourced
data %>%
  haven::as_factor(.) %>%
  group_by(Q3v3a) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q3v3a, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Of those who are sure they're outsourced, 20.7% are sure they're also an agency 
# worker and 11% think they might also be an agency worker
data %>%
  filter(Q3v3a == 1) %>%
  haven::as_factor() %>%
  group_by(Q3v3b) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q3v3b, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Of those who think they might be outsourced, 17.9% are sure they are also an
# agency worker and 29.5% think they might also be an agency worker
data %>%
  filter(Q3v3a == 2) %>%
  haven::as_factor() %>%
  group_by(Q3v3c) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q3v3c, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Of those who are not outsourced, 4.0% are sure they're an agency worker
# and 1.6% think they might be an agency worker
data %>%
  filter(Q3v3a == 3) %>%
  haven::as_factor() %>%
  group_by(Q3v3d) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q3v3d, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))


################################
##### Bolstered outsourced #####
################################

Q1_labels <- c("I am paid by one organisation but I do work for a different organisation", 
               "The organisation I’m paid by is a ‘third party’ organisation which other organisations hire to do work for them, rather than doing that work themselves in-house",
               "My  employer / agency provides people to do work for other organisations (i.e. they might provide people to do cleaning, security, administration, IT)",
               "On a day-to-day basis, I’m paid by one organisation but I get given tasks or instructions by people who are paid by   a different organisation",
               "I am paid by one organisation, but I work in a space which has the logo or branding of a different organisation",
               "I wear a uniform which has the logo or branding of my employer / agency, and which marks me out as being paid by a different organisation to some of the other people that I work with"
)

data %>% 
  filter(Q3v3a == 2) %>%
  filter(Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1) %>%
  select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# Calculate the number of people who said TRUE to each indicator item as a proportion
# of all people in the sample
data %>%
  select(c(ID, Q3v3a, starts_with("Q1"))) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  filter(Q3v3a == 2) %>%
  group_by(Q1, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  ) %>%
  # group_by(Q1) %>%
  # mutate(
  #   check = sum(n)
  # ) %>%
  filter(Q1_True_False == 1) %>%
  ggplot(., aes(Q1, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Calculate the number of people who said TRUE to each indicator item as a proportion
# of all people who said they 'might be' outsourced
data %>%
  select(c(ID, Q3v3a, starts_with("Q1"))) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  filter(Q3v3a == 2) %>%
  group_by(Q1, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  # group_by(Q1) %>%
  # mutate(
  #   check = sum(n)
  # ) %>%
  filter(Q1_True_False == 1) %>%
  ggplot(., aes(Q1, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))


#####################
### Likely agency ###
#####################

# Defined as 'Anyone who says they are sure they are agency, combined with 
# long-term work'
data %>% 
  filter(Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1 ) %>%
  # filter(Q2 == 1) %>%
  group_by(Q2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  ) %>%
  ggplot(., aes(Q2, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

#########################
### High # indicators ###
#########################

# Defined as 'People who tick any 5 or 6 of the 6 key indicators (even if they 
# don’t say they’re outsourced or agency?)'

# invert response function for summing
invert_response <- function(x){
  x <- 2 + (-1*x)
}

# data %>%
#   select(starts_with("Q1"))

data %>% 
  mutate(across(starts_with("Q1"), ~invert_response(.x))) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6)
  ) %>%
  group_by(sum_true) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(sum_true, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%"))) +
  scale_x_continuous(breaks = seq(0,6,1))

###############################
### Reasonable # indicators ###
###############################

# Defined as people who tick a) at least one of the first three and 
# b) at least one of the others, in combination with ‘long term work’.

data %>% 
  filter(Q2 == 1) %>%
  filter(Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1) %>%
  filter(Q1_4 == 1| Q1_5 == 1 | Q1_6 == 1) %>%
  select(Q2, starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# Could also develop this to give percentage breakdown of each indicator as 
# proportion of all long-term workers