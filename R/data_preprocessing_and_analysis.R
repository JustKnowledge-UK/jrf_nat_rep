rm(list = ls())
# Load libraries
library(haven)
library(tidyverse)
library(skimr)

# Read data
data <- read_sav("Data/UK23626 Workers sample data with nat rep and graduates weight.sav")
# View(data)

# Apply labels to variables
data <- data %>%
  as_factor()

# Drop unnecessary vars
# 14:26 = quals below degree, if we think this would add value we should include them
# 28:36 = age of children, doesnt seem necessary
#78-86 are all empty rows

data <- data[, -c(2,14:26,28,36,78:86)]

#################################
##### Identifying variables #####
#################################
# Q3 is the straight up self report outsourced/agency questions
# There are multiple versions. The following code shows that all versions apart
# from the 'Q3v3' ones are empty, so these are the ones to use.

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
    Non_Private_Biz_Type_TEXT = E10B_9_other,
    Paid_One_Work_Other = Q1_1,
    Third_Party = Q1_2,
    Employer_Is_Agency = Q1_3,
    Instructed_By_Other = Q1_4,
    Work_In_Other = Q1_5,
    Diff_Uniform = Q1_6,
    Short_Long_Employ = Q2,
    Short_Long_Employ_TEXT = Q2_3_other,
    I_Am_Outsourced = Q3v3a,
    Outsourced_And_Agency = Q3v3b,
    Might_Be_Outsourced_And_Agency = Q3v3c,
    Not_Outsourced_And_Agency = Q3v3d,
    Disability = D7_Disability1,
    Disability_Impact = D8_Disability2,
    #some form of multiplication between INCOME_FREQ and INCOME_OPEN_1 is necessary to create equivalence to an annual salary
  )

#fix column types
str(data)
#age to numeric

#check data
skim(data)

#####################ANALYSIS###############################

#########################################
### Q3: Outsourced/agency self-report ###
#########################################

# 7.6% are sure they're outsourced, 7.1% think they might be outsourced
data %>%
  haven::as_factor() %>%
  group_by(I_Am_Outsourced) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(I_Am_Outsourced, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Of those who are sure they're outsourced, 20.7% are sure they're also an agency 
# worker and 11% think they might also be an agency worker
data %>%
  filter(I_Am_Outsourced == "I am sure I’m an outsourced worker") %>%
  haven::as_factor() %>%
  group_by(Outsourced_And_Agency) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Outsourced_And_Agency, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Of those who think they might be outsourced, 17.9% are sure they are also an
# agency worker and 29.5% think they might also be an agency worker
data %>%
  filter(I_Am_Outsourced == "I think I might be an outsourced worker") %>%
  haven::as_factor() %>%
  group_by(Might_Be_Outsourced_And_Agency) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Might_Be_Outsourced_And_Agency, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Of those who are not outsourced, 4.0% are sure they're an agency worker
# and 1.6% think they might be an agency worker
data %>%
  filter(I_Am_Outsourced == "I am not an outsourced worker") %>%
  haven::as_factor() %>%
  group_by(Not_Outsourced_And_Agency) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(Not_Outsourced_And_Agency, perc)) +
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
  filter(I_Am_Outsourced == "I think I might be an outsourced worker") %>%
  filter(Paid_One_Work_Other == "True" | Third_Party == "True" | Employer_Is_Agency == "True") %>%
  select(Paid_One_Work_Other,
         Third_Party,
         Employer_Is_Agency,
         Instructed_By_Other,
         Work_In_Other,
         Diff_Uniform) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# Calculate the number of people who said TRUE to each indicator item as a proportion
# of all people in the sample
data %>%
  select(c(ID, 
           I_Am_Outsourced, 
           Paid_One_Work_Other,
           Third_Party,
           Employer_Is_Agency,
           Instructed_By_Other,
           Work_In_Other,
           Diff_Uniform)) %>%
  pivot_longer(cols=c(Paid_One_Work_Other,
                      Third_Party,
                      Employer_Is_Agency,
                      Instructed_By_Other,
                      Work_In_Other,
                      Diff_Uniform), values_to = "Q1_True_False") %>%
  filter(I_Am_Outsourced == "I think I might be an outsourced worker") %>%
  group_by(name, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  ) %>%
  filter(Q1_True_False == "True") %>%
  ggplot(., aes(name, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

# Calculate the number of people who said TRUE to each indicator item as a proportion
# of all people who said they 'might be' outsourced
data %>%
  select(c(ID, 
           I_Am_Outsourced, 
           Paid_One_Work_Other,
           Third_Party,
           Employer_Is_Agency,
           Instructed_By_Other,
           Work_In_Other,
           Diff_Uniform)) %>%
  pivot_longer(cols=c(Paid_One_Work_Other,
                      Third_Party,
                      Employer_Is_Agency,
                      Instructed_By_Other,
                      Work_In_Other,
                      Diff_Uniform), values_to = "Q1_True_False") %>%
  filter(I_Am_Outsourced == "I think I might be an outsourced worker") %>%
  group_by(name, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == "True") %>%
  ggplot(., aes(name, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

### SURE outsourced ###

data %>% 
  filter(I_Am_Outsourced == "I am sure I’m an outsourced worker") %>%
  filter(Paid_One_Work_Other == "True" | Third_Party == "True" | Employer_Is_Agency == "True") %>%
  select(Paid_One_Work_Other,
         Third_Party,
         Employer_Is_Agency,
         Instructed_By_Other,
         Work_In_Other,
         Diff_Uniform) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# Calculate the number of people who said TRUE to each indicator item as a proportion
# of all people who said they are sure they are outsourced
data %>%
  select(c(ID, 
           I_Am_Outsourced, 
           Paid_One_Work_Other,
           Third_Party,
           Employer_Is_Agency,
           Instructed_By_Other,
           Work_In_Other,
           Diff_Uniform)) %>%
  pivot_longer(cols=c(Paid_One_Work_Other,
                      Third_Party,
                      Employer_Is_Agency,
                      Instructed_By_Other,
                      Work_In_Other,
                      Diff_Uniform), values_to = "Q1_True_False") %>%
  filter(I_Am_Outsourced == "I am sure I’m an outsourced worker") %>%
  group_by(name, Q1_True_False) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  filter(Q1_True_False == "True") %>%
  ggplot(., aes(name, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

### Both SURE and MIGHT BE outsourced ###
data %>% 
  filter(I_Am_Outsourced == "I am sure I’m an outsourced worker" | I_Am_Outsourced == "I think I might be an outsourced worker") %>%
  filter(Paid_One_Work_Other == "True" | Third_Party == "True" | Employer_Is_Agency == "True") %>%
  select(Paid_One_Work_Other,
         Third_Party,
         Employer_Is_Agency,
         Instructed_By_Other,
         Work_In_Other,
         Diff_Uniform) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

#####################
### Likely agency ###
#####################
# Defined as 'Anyone who says they are sure they are agency, combined with 
# long-term work'

### Overall sure agency ####
data %>% 
  filter(Outsourced_And_Agency == "I am sure that I’m also an agency worker" | Might_Be_Outsourced_And_Agency == "I am sure that I’m also an agency worker" | Not_Outsourced_And_Agency == "I am sure that I’m an agency worker" ) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

### Overall MIGHT BE agency ####
data %>% 
  filter(Outsourced_And_Agency == "I think I might also be an agency worker" | Might_Be_Outsourced_And_Agency == "I think I might also be an agency worker" | Not_Outsourced_And_Agency == "I think I might be an agency worker" ) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

### SURE agency ###
data %>% 
  filter(Outsourced_And_Agency == "I am sure that I’m also an agency worker" | Might_Be_Outsourced_And_Agency == "I am sure that I’m also an agency worker" | Not_Outsourced_And_Agency == "I am sure that I’m an agency worker" ) %>%
  group_by(Short_Long_Employ) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  ) %>%
  ggplot(., aes(Short_Long_Employ, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))


### MIGHT BE agency ###
data %>% 
  filter(Outsourced_And_Agency == "I think I might also be an agency worker" | Might_Be_Outsourced_And_Agency == "I think I might also be an agency worker" | Not_Outsourced_And_Agency == "I think I might be an agency worker" ) %>%
  group_by(Short_Long_Employ) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  ) %>%
  ggplot(., aes(Short_Long_Employ, perc)) +
  geom_col() +
  geom_label(aes(label = paste0(round(perc,2),"%")))

##################################
### Assigning people to groups ###
##################################
### in the previous code factors were assigned on the labels
# invert response function for summing
invert_response <- function(x){
  x <- 2 + (-1*x)
}

## NOTE: Need to check that these groupings are mutually exclusive
data <- data %>%
  mutate(
    outsourced = ifelse(I_Am_Outsourced == 1, 1, 0),
    bolstered_outsourced = ifelse(I_Am_Outsourced == 2 & (Paid_One_Work_Other == 1 | Third_Party == 1 | Employer_Is_Agency == 1), 1, 0),
    likely_agency = ifelse(Short_Long_Employ == 1 & (Outsourced_And_Agency == 1 | Might_Be_Outsourced_And_Agency == 1 | Not_Outsourced_And_Agency == 1), 1, 0)
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
sum(data$outsourced + data$bolstered_outsourced + data$likely_agency == ncol(vars), na.rm = T)

#HERE####
# Now for just indicators
indicator_data <- data %>% 
  select(ID, 
         Paid_One_Work_Other,
         Third_Party,
         Employer_Is_Agency,
         Instructed_By_Other,
         Work_In_Other,
         Diff_Uniform) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x)), .by = ID) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Paid_One_Work_Other, Third_Party, Employer_Is_Agency, Instructed_By_Other, Work_In_Other, Diff_Uniform)
  ) %>% 
  select(-starts_with("Q1"))

temp <- data %>%
  select(ID, starts_with("Q1")) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x))) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Paid_One_Work_Other, Third_Party, Employer_Is_Agency, Instructed_By_Other, Work_In_Other, Diff_Uniform)
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
  filter(outsourced == 0 & bolstered_outsourced == 0 & likely_agency == 0) %>%
  filter(Short_Long_Employ == 1) %>%
  filter(Paid_One_Work_Other == 1 | Third_Party == 1 | Employer_Is_Agency == 1) %>%
  filter(Instructed_By_Other == 1| Work_In_Other == 1 | Diff_Uniform == 1) %>%
  # select(Q2, starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / nrow(data))
  )

# Could also develop this to give percentage breakdown of each indicator as 
# proportion of all long-term workers


#### save csv ####
write_sav(data, "./data/uncleaned_full_data.sav")
# write_sav(data_subset, "./data/cleaned_complete_case_data.sav")

# Remove everything apart from data_subset (ready for next)
rm(list = setdiff(ls(), "data_subset"))
