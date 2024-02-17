# Analysis of nationally representative data
# January 2024
# Jolyon Miles-Wilson

# Questions of interest are
# - Question 1s: these are the indicator questions (Q3 previously)
# - Question 2s: this is the long-term/short-term question (Q5 previously)
# - Question 3s: this is the blanket statement "I am outsourced/agency" (Q7/8 previously)

rm(list = ls())

library(haven)
library(poLCA)
library(dplyr)
library(ggplot2)
library(tidyr)


# data
# data <- read_sav("Data/UK23626 Workers sample data with nat rep and graduates weight.sav")
# # View(data)
# 
# # Apply labels to variables
# data <- data %>%
#   as_factor()
# 
# # Drop unnecessary vars
# # 14:26 = quals below degree, if we think this would add value we should include them
# # 28:36 = age of children, doesnt seem necessary
# 
# data <- data[, -c(2,14:26,28,36)]
# 
# #################################
# ##### Identifying variables #####
# #################################
# # Q3 is the straight up self report outsourced/agency questions
# # There are multiple versions. The following code shows that all versions apart
# # from the 'Q3v3' ones are empty, so these are the ones to use.
# # Q3 <- data %>%
# #   select(starts_with("Q3")) %>%
# #   map(., ~sum(!is.na(.)))
# 
# data <- data %>%
#   select(-c(starts_with("Q3v1"), starts_with("Q3v2"), starts_with("Q3v4")))
# 
# #change column names
# data <- data %>%
#   rename(
#     ID = MIProRspId,
#     Sex = D1_Gender_C,
#     Age = D2_Age,
#     Region = D4_Region_C,
#     Employment_Status = D3_employment,
#     Is_an_employee = E2,
#     Consent_1_Ethnicity = SCD_Opt_In_1, 
#     Consent_2_TU = SCD_Opt_In_2,                
#     Consent_3_Health = SCD_Opt_In_3,
#     Has_Degree = D6_educ1,
#     Has_Second_Job = E1,
#     Who_Pays_You = E3,
#     Job_Security = E5,
#     Work_Circumstance_Agency = E6_1,
#     Work_Circumstance_Contract = E6_2,
#     Work_Circumstance_Seasonal = E6_3,                       
#     Work_Circumstance_Trainee = E6_4,
#     Work_Circumstance_Other = E6_5,                       
#     Work_Circumstance_Other_TEXT = E6_5_other,
#     Org_Size = E7A,
#     Is_Supervisor = E7B,
#     Job_Title_TEXT = E8A,
#     Main_Activity_TEXT = E8B,
#     Employer_Activity_TEXT = E9A,
#     Employer_Activity_SIC = E9B, #need to check that these were SIC and translate to digits if so
#     Biz_Type = E10A,
#     Non_Private_Biz_Type = E10B,
#     Non_Private_Biz_Type_TEXT = E10B_9_other
#   )
# 
# #### save csv ####
# write_sav(data, "./Data/uncleaned_full_data.sav")
rm(list = ls())
data <- read_sav("./Data/uncleaned_full_data.sav")

#########################################
### Q3: Outsourced/agency self-report ###
#########################################

# 7.6% are sure they're outsourced, 7.1% think they might be outsourced
data %>%
  haven::as_factor() %>%
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

### Total MIGHT BE outsourced ###

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

### SURE outsourced ###

data %>% 
  filter(Q3v3a == 1) %>%
  filter(Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1) %>%
  select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# test difference
test_data <- as.table(cbind(c(585, 10155-585),
                       c(610, 10155-610)))

chisq.test(test_data)

prop.test(x = c(585, 610), n = c(10155, 10155))

# Calculate the number of people who said TRUE to each indicator item as a proportion
# of all people who said they are sure they are outsourced
data %>%
  select(c(ID, Q3v3a, starts_with("Q1"))) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  filter(Q3v3a == 1) %>%
  group_by(Q1, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    # sum = sum(n),
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

### Both SURE and MIGHT BE outsourced ###
##### overall #####
data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>%
  filter(Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1) %>%
  select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

##### for each indicator #####
data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>% 
  # think this mutate is redundant
  mutate(
    outsourced = labelled(ifelse(Q3v3a == 1, 1, 0), c("Sure outsourced" = 1, "Might be outsourced" = 0)) # make grouping variable
  ) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False, outsourced) %>%
  select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_overall, fill = outsourced)) +
  geom_col(position = "dodge")

# sure or outsourced LONG TERM
data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>% 
  # think this mutate is redundant
  mutate(
    outsourced = labelled(ifelse(Q3v3a == 1, 1, 0), c("Sure outsourced" = 1, "Might be outsourced" = 0)) # make grouping variable
  ) %>%
  # pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(outsourced, Q2) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  as_factor() %>%
  ggplot(., aes(outsourced, perc_within, fill = Q2)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(round(perc_within, 2),"%")), position = position_dodge(width = 0.9))
  
temp_tab <- data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>% 
  # think this mutate is redundant
  mutate(
    outsourced = labelled(ifelse(Q3v3a == 1, 1, 0), c("Sure outsourced" = 1, "Might be outsourced" = 0)) # make grouping variable
  ) %>%
  # pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(outsourced, Q2) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  )

chisq_tab <- as.table(cbind(temp_tab$n[1],
                        temp_tab$n[4]))
chisq.test(chisq_tab)


chisq_tab <- as.table(cbind(temp_tab$n[2],
                            temp_tab$n[5]))

chisq.test(chisq_tab)

## 17 jan ## 
data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>% 
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q2, Q1, Q1_True_False) %>%
  select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_within, fill = Q2)) +
  geom_col(position = "dodge")
  # geom_label(inherit.aes = T, aes(label = paste0(round(perc_within, 2),"%")), position = position_dodge(width = 0.9))

temp_tab <- data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>% 
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q2, Q1, Q1_True_False) %>%
  select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  )  %>%
  filter(Q1_True_False == 1)

again <- as.table(cbind(temp_tab$n[1:6],
                    temp_tab$n[7:12]))

chisq.test(again)

temp_sub_4 <- temp_tab %>%
  filter(Q1 != "Q1_4")
again_2 <- as.table(cbind(temp_sub_4$n[1:5],
                     temp_sub_4$n[6:10]))

chisq.test(again_2)

### NOT outsourced, each indicator
data %>% 
  filter(Q3v3a == 3) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_overall)) +
  geom_col(position = "dodge")

# overall how many NOT outsourced said yes to at least 1 indicator
data %>% 
  filter(Q3v3a == 3) %>%
  filter(Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1 |Q1_4 == 1 | Q1_5 == 1 |Q1_6 == 1) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# do a plot - agreement to indicator by outsourcing status
data %>% 
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q3v3a, Q1, Q1_True_False) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  # group_by(Q3v3a)
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_within, fill = Q3v3a)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(round(perc_within, 2),"%")), position = position_dodge(width = 0.9))

# v2
data %>% 
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False, Q3v3a) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  # group_by(Q3v3a)
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_within, fill = Q3v3a)) +
  geom_col(position = "dodge")

### HERE ####

## MIGHT BE outsourced and MIGHT BE agency ##
data %>%
  filter(Q3v3c == 2) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False, Q2) %>%
  summarise(n = n()) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_overall, fill = Q2)) +
  geom_col(position = "dodge")


#####################
### Likely agency ###
#####################
# Defined as 'Anyone who says they are sure they are agency, combined with 
# long-term work'

### Overall sure agency ####
data %>% 
  filter(Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1 ) %>%
  # filter(Q2 == 1) %>%
  group_by(Q2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

### Overall MIGHT BE agency ####
data %>% 
  filter(Q3v3b == 2 | Q3v3c == 2 | Q3v3d == 2 ) %>%
  # filter(Q2 == 1) %>%
  #group_by(Q2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )


### SURE agency ###
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


### MIGHT BE agency ###
data %>% 
  filter(Q3v3b == 2 | Q3v3c == 2 | Q3v3d == 2 ) %>%
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



### MAYBE agency and LONG-TERM ###

# Indicators
data %>% 
  filter(Q3v3b == 2 | Q3v3c == 2 | Q3v3d == 2) %>%
  filter(Q2 == 1) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_within = 100 * (n / sum(n)),
    perc_total = 100 * (n / nrow(data))
  ) %>%
  filter(Q1_True_False == 1) %>%
  ggplot(., aes(Q1, perc_total)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc_total,2),"%")))


# SURE
data %>% 
  filter(Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1) %>%
  filter(Q2 == 1) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_within = 100 * (n / sum(n)),
    perc_total = 100 * (n / nrow(data))
  ) %>%
  filter(Q1_True_False == 1) %>%
  ggplot(., aes(Q1, perc_within)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc_within,2),"%")))

##################################
### Assigning people to groups ###
##################################

# invert response function for summing
invert_response <- function(x){
  x <- 2 + (-1*x)
}

## NOTE: Need to check that these groupings are mutually exclusive
data <- data %>%
  mutate(
    outsourced = ifelse(Q3v3a == 1, 1, 0),
    bolstered_outsourced = ifelse(Q3v3a == 2 & (Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1), 1, 0)
    )

data <- data %>%
  mutate(
    likely_agency = ifelse(outsourced == 0 & bolstered_outsourced == 0 & Q2 == 1 & (Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1), 1, 0)
    ) %>%
  mutate(
    likely_agency = ifelse(is.na(likely_agency), 0, likely_agency)
  )

# checks
vars <- data.frame(data$outsourced, 
                   data$bolstered_outsourced, 
                   data$likely_agency)

# Count to see if same as previously calculated
sapply(vars, function(x) sum(x, na.rm = T))

# check that they're mutually exclusive
sum(data$outsourced + data$bolstered_outsourced + data$likely_agency == 2, na.rm = T)

# Now for just indicators
indicator_data <- data %>% 
  select(ID, starts_with("Q1")) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x)), .by = ID) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6)
  ) %>% 
  select(-starts_with("Q1"))

temp <- data %>%
  select(ID, starts_with("Q1")) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x))) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6)
  )

# check sum true the same across dfs
sum(temp$sum_true == indicator_data$sum_true) == nrow(data)
# Check ID the same
sum(data$ID == indicator_data$ID) == nrow(data)

data <- left_join(data, indicator_data, by = "ID")

#########################
### High # indicators ###
#########################

# Defined as 'People who tick any 5 or 6 of the 6 key indicators (even if they 
# don’t say they’re outsourced or agency?)'

# data %>%
#   select(starts_with("Q1"))

# Total n and %
data %>% 
  filter(outsourced == 0 & bolstered_outsourced == 0 & likely_agency == 0) %>%
  filter(sum_true >= 5) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# Allocate to group
data <- data %>% 
  mutate(
    high_indicators = ifelse((outsourced == 0 & bolstered_outsourced == 0 & likely_agency == 0) & sum_true >= 5, 1, 0) 
  )

# By # indicators agreed
data %>% 
  filter(outsourced == 0 & bolstered_outsourced == 0 & likely_agency == 0) %>%
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
  filter(outsourced == 0 & bolstered_outsourced == 0 & likely_agency == 0 & high_indicators == 0) %>%
  filter(Q2 == 1) %>%
  filter(Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1) %>%
  filter(Q1_4 == 1| Q1_5 == 1 | Q1_6 == 1) %>%
  # select(Q2, starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )


# Allocate to group
data <- data %>% 
  mutate(
    reasonable_indicators = ifelse((outsourced == 0 & bolstered_outsourced == 0 & likely_agency == 0 & high_indicators == 0) & 
                                      Q2 == 1 &
                                      (Q1_1 == 1 | Q1_2 == 1 | Q1_3 == 1) &
                                      (Q1_4 == 1| Q1_5 == 1 | Q1_6 == 1), 1, 0) 
  )

total_counts <- data.frame(data$outsourced,
                         data$bolstered_outsourced,
                         data$likely_agency,
                         data$high_indicators,
                         data$reasonable_indicators)

sapply(total_counts, sum)


#### extras 17 Jan

# bolsetered group LT ST

data %>%
  filter(bolstered_outsourced == 1) %>%
  group_by(Q2) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q2, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%"))) +
  scale_x_continuous(breaks = seq(0,6,1))

data %>%
  filter(bolstered_outsourced == 1) %>%
  filter(Q1_3 == 1 & Q1_1 == 2 & Q1_2 == 2) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    perc_total = 100 * (n / nrow(data)),
    perc_bolstered = 100 * (n / 610)
  )
  

data %>%
  filter(high_indicators == 1) %>%
  group_by(Q2) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q2, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%"))) +
  scale_x_continuous(breaks = seq(0,6,1))

# REAOSAONBLE
data %>%
  filter(reasonable_indicators == 1) %>%
  filter((Q1_1 == 1 | Q1_2 == 1) & sum_true >= 3) %>%
  group_by(Q2) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q2, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%"))) +
  scale_x_continuous(breaks = seq(0,6,1))

1# #bolstered yes to 3 but not 1 or 2


# Could also develop this to give percentage breakdown of each indicator as 
# proportion of all long-term workers

# Q3 co-occurrence matrix
# Analysis of question 3 co-occurrences. this code calculates the percentage of respondents who said TRUE to each
# statement, as a function of having said TRUE to another statement, and plots it.

# invert question 3 responses so that 1 = TRUE and 0 = NOT TRUE
invert_response <- function(x){
  x <- 2 + (-1*x)
}

data_2 <- data %>%
  mutate(across(starts_with("Q1"), invert_response))

# Get co-occurrences

temp_df <- data_2 %>% dplyr::select(starts_with("Q1"))

# Initialize an empty matrix for co-occurrence counts
co_occurrence_matrix <- matrix(0, nrow = ncol(temp_df), ncol = ncol(temp_df))

# Loop through columns and calculate co-occurrence
for (i in 1:ncol(temp_df)) {
  for (j in 1:ncol(temp_df)) {
    # Count the co-occurrence of 1s between columns i and j
    co_occurrence_matrix[i, j] <- sum(temp_df[[i]] == 1 & temp_df[[j]] == 1)
  }
}

# Convert the matrix to a data frame for better readability
co_occurrence_df <- as.data.frame(co_occurrence_matrix)

# Display the co-occurrence matrix as a data frame
print(co_occurrence_df)

# get percentages
co_occurrence_perc <- 100 * (co_occurrence_matrix / nrow(data_2)) %>%
  as.data.frame()

# Use a heatmap - much better for visualisation
# function to get rid of repeated values above diagonal
get_lower_triangle <- function(matrix) {
  n <- nrow(matrix)
  lower_triangle <- matrix(NA, n, n)
  lower_triangle[lower.tri(lower_triangle, diag = T)] <- matrix[lower.tri(matrix, diag = T)]
  return(lower_triangle)
}

# Get the lower triangular co-occurrence matrix
lower_triangle_matrix <- get_lower_triangle(co_occurrence_perc)

lower_triangle_matrix %>%
  reshape2::melt() %>% # melt basically pivots longer
  ggplot(., aes(x = as.factor(Var1), y = as.factor(Var2), fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", na.value = "white") +
  theme_minimal() +
  labs(
    title = "Co-occurrence Heatmap",
    x = "Items",
    y = "Items",
    fill = "Co-occurrence"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  geom_text(aes(label = ifelse(!is.na(value), round(value,2), "")), vjust = 1)  # Add value labels


# LCA
# prepare LCA

f1 <- as.formula(cbind(Q1_1,
                       Q1_2,
                       Q1_3,
                       Q1_4,
                       Q1_5,
                       Q1_6) ~ 1)

# 1 class
LCA_1 <- poLCA(f1, data, nclass = 1)

# 2 classes
LCA_2 <- poLCA(f1, data, nclass = 2)
plot(LCA_2)

# save 2 class to use later:
# saveRDS(LCA_2, file = "./outputs/Q3_LCA_2.Rds")

# 3 classes
LCA_3 <- poLCA(f1, data, nclass = 3)
plot(LCA_3)

# 4 classes
LCA_4 <- poLCA(f1, data, nclass = 4)
plot(LCA_3)

ics <- data.frame("classes" = seq(1,4,1),
                  "ic" = c(rep("aic", 4),
                           rep("bic", 4)),
                  "value" = c(LCA_1[["aic"]],
                              LCA_2[["aic"]],
                              LCA_3[["aic"]],
                              LCA_4[["aic"]],
                              LCA_1[["bic"]],
                              LCA_2[["bic"]],
                              LCA_3[["bic"]],
                              LCA_4[["bic"]])
)

# 'elbow' at 2 classes suggests additional classes beyond 2 don't explain much
ggplot(ics, aes(classes, value, colour = ic, group = ic)) +
  geom_point() +
  geom_line()

# change from scientific notation
options(scipen = 999)

# get the class probabilities
lca2_class_probs <- as.data.frame(t(as.data.frame(LCA_2[["probs"]])[,c(TRUE, FALSE)])) %>%
  mutate_all(~round(., 4))

lca3_class_probs <- as.data.frame(t(as.data.frame(LCA_3[["probs"]])[,c(TRUE, FALSE)])) %>%
  mutate_all(~round(., 4))

# add predicted class variable from LCA
data <- data %>%
  mutate(
    predicted_class_2 = LCA_2[["predclass"]],
    predicted_class_3 = LCA_3[["predclass"]]
  )

# quick summary of predicted memberships
data %>%
  group_by(predicted_class_2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n)),
  )  %>%
  ggplot(., aes(predicted_class_2, perc)) +
  geom_col(position = "dodge", colour = "black") +
  scale_y_continuous(breaks = seq(0,60,10)) +
  geom_label(aes(label = round(perc,2)))

# 2 classes come out overall - one with high loadings on all, and one with low loadings
# but relatively high on 3 and 6. suggests these indicators are more broadly applciable
# beyond outsrouced work7
 
# should do an lca with q3v3a added as a covariate, to see how gruopigns cahnge depending
# on what people reported fro this question. or will this just tell us the same as before?



## chi-

## 20 Jan ##

# MIGHT BE outsourced and LONG TERM work
# sure or outsourced LONG TERM
data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>% 
  # think this mutate is redundant
  mutate(
    outsourced = labelled(ifelse(Q3v3a == 1, 1, 0), c("Sure outsourced" = 1, "Might be outsourced" = 0)) # make grouping variable
  ) %>%
  # pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(outsourced, Q2) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  as_factor() %>%
  ggplot(., aes(outsourced, perc_within, fill = Q2)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(round(perc_within, 2),"%")), position = position_dodge(width = 0.9))


temp_df <- data %>% 
  filter(Q3v3a == 1 | Q3v3a == 2) %>% 
  # think this mutate is redundant
  mutate(
    outsourced = labelled(ifelse(Q3v3a == 1, 1, 0), c("Sure outsourced" = 1, "Might be outsourced" = 0)) # make grouping variable
  ) %>%
  # pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(outsourced, Q2) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  as_factor()

# check sig diff for LONG TERM

test_data <- as.table(cbind(c(508, (508+201+11) - 508),
                            c(615, (615+150+7)-615)))

chisq.test(test_data)

prop.test(x = test_data[,'A'], n = c(sum(test_data['A',]), sum(test_data['B',])))

# check sig diff for SHORT TERM

test_data <- as.table(cbind(c(201, (508+201+11) - 201),
                            c(150, (615+150+7) - 150)))

chisq.test(test_data)

prop.test(x = test_data[,'A'], n = c(sum(test_data['A',]), sum(test_data['B',])))


# outsrouced/agency crossover

data %>%
  filter(Q3v3a == 1) %>%
  group_by(Q3v3b) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100  * (n / sum(n))
  )

# people who are SURE they are agency and do LONG TERM work

data %>%
  filter(outsourced == 0) %>%
  filter(Q2 == 1 & (Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1)) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100  * (n / sum(n))
  )

data %>%
  filter(outsourced == 0 & bolstered_outsourced == 0) %>%
  filter(Q2 == 1 & (Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1)) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100  * (n / sum(n))
  )


# no to both outsoruced and agency and say yes to each indicator and 1-6 of them
### NOT outsourced, each indicator
data %>% 
  filter(Q3v3a == 3 & Q3v3d == 3) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_within)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9))


# overall how many NOT outsourced said yes to at least 1 indicator
data %>% 
  filter(Q3v3a == 3 & Q3v3d == 3) %>%
  group_by(sum_true) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  as_factor() %>%
  ggplot(., aes(sum_true, perc_within)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9))


# not outsoruced/agency and true 5+
data %>% 
  filter(Q3v3a == 3 & Q3v3d == 3 & sum_true >= 5) %>%
  group_by(sum_true) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  )

# not outsoruced/agency and true 5+ and LONGTERM
data %>% 
  filter(Q3v3a == 3 & Q3v3d == 3 & sum_true >= 5 & Q2 == 1) %>%
  group_by(sum_true) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  )

# assign these groupings
data <- data %>%
  mutate(
    not_outsourced_agency_5plus = ifelse(Q3v3a == 3 & Q3v3d == 3 & sum_true >= 5, 1, 0),
    not_outsourced_agency_LT_5plus = ifelse(Q3v3a == 3 & Q3v3d == 3 & sum_true >= 5 & Q2 == 1, 1, 0)
  )

data %>%
  summarise(
    mean = mean(INCOME_OPEN_1, na.rm = T),
    median = median(INCOME_OPEN_1, na.rm = T)
  )

jobs_outsourced_agency_5plus <- data %>%
  filter(not_outsourced_agency_5plus == 1) %>%
  select(Job_Title_TEXT, Main_Activity_TEXT, Employer_Activity_TEXT, Employer_Activity_SIC, INCOME_OPEN_1, INCOME_CLOSED_ANNUAL) %>%
  haven::as_factor()

jobs_outsourced_agency_5plus %>%
  summarise(
    mean = mean(INCOME_OPEN_1, na.rm = T),
    median = median(INCOME_OPEN_1, na.rm = T)
  )

jobs_outsourced_agency_5plus %>%
  group_by(Employer_Activity_SIC) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  arrange(desc(n)) %>%
  write.csv(file = "./outputs/SIC_not_out_agency_5plus.csv")

write.csv(jobs_outsourced_agency_5plus, file = "./outputs/data/jobs_outsourced_agency_5plus.csv")

jobs_outsourced_agency_LT_5plus <- data %>%
  filter(not_outsourced_agency_LT_5plus == 1) %>%
  select(Job_Title_TEXT, Main_Activity_TEXT, Employer_Activity_TEXT, Employer_Activity_SIC, INCOME_OPEN_1, INCOME_CLOSED_ANNUAL) %>%
  haven::as_factor()

jobs_outsourced_agency_LT_5plus %>%
  summarise(
    mean = mean(INCOME_OPEN_1, na.rm = T),
    median = median(INCOME_OPEN_1, na.rm = T)
  )

jobs_outsourced_agency_LT_5plus %>%
  group_by(Employer_Activity_SIC) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  arrange(desc(n)) %>%
  write.csv(file = "./outputs/SIC_not_out_agency_LT_5plus.csv")


write.csv(jobs_outsourced_agency_LT_5plus, file = "./outputs/data/jobs_outsourced_agency_LT_5plus.csv")

# indicator breakdown

data %>%
  group_by(sum_true) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  )

# JUST LONG-TERM
data %>%
  filter(Q2 == 1) %>%
  group_by(sum_true) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  )

sum(data$high_indicators)

data %>%
  filter(high_indicators == 1) %>%
  group_by(Q2) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q2, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%"))) +
  scale_x_continuous(breaks = seq(0,6,1))

data %>%
  filter(Q3v3a == 3 & Q3v3d == 3 & sum_true >= 5) %>%
  group_by(Q2) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Q2, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%"))) +
  scale_x_continuous(breaks = seq(0,6,1))

data %>%
  filter(Q3v3a == 3 & Q3v3d == 3) %>%
  group_by(sum_true) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc = 100 * (n / sum(n))
  ) 

data %>%
  filter(Q3v3a == 3 & Q3v3d == 3) %>%
  filter(Q2 == 1) %>%
  group_by(sum_true) %>%
  summarise(
    n  = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc = 100 * (n / sum(n))
  ) 

#### 22 Jan #####

# new bolstered

data <- data %>%
  mutate(
    bolstered_outsourced_2 = ifelse(Q3v3a == 2 & Q2 == 1, 1, 0)
  )

# AGENCY + LONG-TERM, EXCL SURE OUTSROUCED + MIGHT BE OUTSOURCED + LT

data <- data %>%
  mutate(
    agency_LT = ifelse((Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1) & Q2 == 1 & outsourced == 0 & bolstered_outsourced_2 == 0, 1, 0)
  )%>%
  mutate(
    agency_LT = ifelse(is.na(agency_LT), 0, agency_LT)
  )

data %>%
  group_by(outsourced, agency_LT) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  )

data %>%
  filter(outsourced == 0 & bolstered_outsourced_2 == 0) %>%
  filter(sum_true >= 5) %>%
  group_by(Q2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / 10155)
  )

data %>%
  filter(Q3v3a == 3 & Q3v3d == 3) %>%
  filter(sum_true >= 5) %>%
  group_by(Q2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / 10155)
  )

data %>%
  filter(outsourced == 0 & bolstered_outsourced_2 == 0) %>%
  filter(Q3v3d == 1) %>%
  filter(sum_true >= 5) %>%
  filter(Q2 == 1) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / 10155)
  )

data %>%
  filter(Q3v3a == 3 & Q3v3d == 3) %>%
  filter(sum_true >= 5) %>%
  filter(Q3v3d == 1) %>%
  group_by(Q2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / 10155)
  )

### 28 Jan ####
### Sum-true 5+ distribution ###

data %>%
  filter(outsourced == 0)
  filter(sum_true == 5) %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(Q1, Q1_True_False) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == 1) %>%
  as_factor() %>%
  ggplot(., aes(Q1, perc_within)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9))

  #############
  ### 4 Feb ###
  #############
  
  
  