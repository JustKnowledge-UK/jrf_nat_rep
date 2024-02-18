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

####fix column types
str(data)
#age to numeric
# Remove 'under 16' level
data$Age <- droplevels(data$Age, exclude = "Under 16")

# Merge 'over 80' with '80' 2 responses
levels(data$Age)[levels(data$Age) == "Over 80"] <- "80"

# Convert 'Age' to numeric
data$Age <- as.numeric(as.character(data$Age))

#gsub on agency var to remove full stop
data$Short_Long_Employ <- gsub("\\.", "", data$Short_Long_Employ)


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
  dplyr::select(Paid_One_Work_Other,
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
  dplyr::select(c(ID, 
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
  dplyr::select(c(ID, 
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
  dplyr::select(Paid_One_Work_Other,
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
  dplyr::select(c(ID, 
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
  dplyr::select(Paid_One_Work_Other,
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
data <- data %>%
  mutate(
    # SURE outsourced + LONGTERM or MIGHT BE outsourced + LONGTERM
    outsourced_LT = ifelse((I_Am_Outsourced == "I am sure I’m an outsourced worker" & 
                            Short_Long_Employ == "I’m hired to do work which an organisation needs doing on a long-term or ongoing basis.") | 
                           (I_Am_Outsourced == "I think I might be an outsourced worker" & 
                            Short_Long_Employ == "I’m hired to do work which an organisation needs doing on a long-term or ongoing basis"), 1, 0),
    # NOT outsourced, SURE agency, and LONG-TERM
    likely_agency = ifelse(outsourced_LT == 0 & 
                           Short_Long_Employ == "I’m hired to do work which an organisation needs doing on a long-term or ongoing basis" &
                          (Outsourced_And_Agency == "I am sure that I’m also an agency worker"|
                           Might_Be_Outsourced_And_Agency == "I am sure that I’m also an agency worker" |
                           Not_Outsourced_And_Agency == "I am sure that I’m an agency worker"), 1, 0),
    likely_agency = ifelse(is.na(likely_agency), 0, likely_agency),
    # Compute sum_true
    sum_true = rowSums(data[, c("Paid_One_Work_Other", "Third_Party", "Employer_Is_Agency", "Instructed_By_Other", "Work_In_Other", "Diff_Uniform")] == "True", na.rm = TRUE),
    # NOT outsourced, NOT likely agency, & 5 or more indicators
    high_indicators = ifelse(outsourced_LT == 0 & 
                             likely_agency == 0 & 
                             sum_true == 6, 1, 0),
    # NOT outsourced, NOT likely agency, 6 or more indicators, & LONGTERM
    high_indicators_LT = ifelse(outsourced_LT == 0 & 
                                likely_agency == 0 & 
                                (Short_Long_Employ ==  "I’m hired to do work which an organisation needs doing on a long-term or ongoing basis" & 
                                sum_true == 6), 1, 0)
  )

# count the groupings
lapply(list(data$outsourced_LT,
            data$likely_agency,
            data$high_indicators,
            data$high_indicators_LT), sum)

# Flatten these groupings into a single variable

data <- data %>%
  mutate(
    outsourcing_group = factor(case_when(outsourced_LT == 1 ~ 'outsourced',
                                         likely_agency == 1 ~ 'likely_agency',
                                         high_indicators_LT == 1 ~ 'high_indicators',
                                         TRUE ~ 'not_outsourced'), 
                               levels = c("not_outsourced",
                                          "outsourced",
                                          "likely_agency",
                                          "high_indicators")
    )
  )

########LCA######

#create functional income var
#data quality here is very poor clearly lots of either junk answers or miselection of frequency
# Assuming 'data' is your dataset
data$Income <- ifelse(data$INCOME_FREQ == "Annually / per year",
                      data$INCOME_OPEN_1,
                      ifelse(data$INCOME_FREQ == "Monthly",
                             data$INCOME_OPEN_1 * 12,
                             ifelse(data$INCOME_FREQ == "Weekly",
                                    data$INCOME_OPEN_1 * 52,
                                    ifelse(data$INCOME_FREQ == "Hourly",
                                           data$INCOME_OPEN_1 * data$HOURS * 52,  
                                           NA  # Handle cases where INCOME_FREQ doesn't match any condition
                                    )
                             )
                      )
)

# Print out the result to verify
print(data$Income)
##LCA DATA SET
LCA_data<- data %>% 
  dplyr::select(Sex, 
         Age, 
         Region,
         Employment_Status,
         Is_an_employee,
         Ethnicity,
         Has_Degree,
         Has_Second_Job,
         Who_Pays_You,
         Job_Security,
         Org_Size,
         Is_Supervisor,
         Paid_One_Work_Other,
         Third_Party,
         Employer_Is_Agency,
         Instructed_By_Other,
         Work_In_Other,
         Diff_Uniform,
         Short_Long_Employ,
         I_Am_Outsourced,
         Disability,
         HOURS,
         Income,
         BORNUK)

# make Age catergorical
mean_age <- mean(LCA_data$Age)

# Create a new categorical variable based on the mean age
LCA_data <- LCA_data %>%
  mutate(Age_Category = ifelse(Age < mean_age, "Younger", "Older")) %>%
  mutate(Age_Category = factor(Age_Category, levels = c("Younger", "Older")))

#change short/long term employement status to catergorical
LCA_data <- LCA_data %>%
  mutate(Short_Long_Employ = factor(Short_Long_Employ, 
                                    levels = c("I’m hired to do work which an organisation needs doing on a long-term or ongoing basis", 
                                               "I’m hired to do work which an organisation needs doing on a short-term or temporary basis",
                                               "Other ")))

#reduce ethnicity to race
LCA_data <- LCA_data %>%
  mutate(Ethnicity_Reduced = case_when(
    # Combine similar White backgrounds
    Ethnicity %in% c("English / Welsh / Scottish / Northern Irish / British", "Irish", "Any other White background") ~ "White",
    # Combine similar Asian backgrounds
    Ethnicity %in% c("White and Asian", "Indian", "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background") ~ "Asian",
    # Combine similar Black backgrounds
    Ethnicity %in% c("African", "Caribbean", "Any other Black, Black British, or Caribbean background", "White and Black Caribbean", "White and Black African") ~ "Black",
    # Group other ethnicities
    Ethnicity %in% c("Roma", "Gypsy or Irish Traveller", "Any other ethnic group", "Any other Mixed / Multiple ethnic background", "Don’t think of myself as any of these", "Prefer not to say","Arab") ~ "Other/Prefer not to say",
    # Keep the original Ethnicity for any remaining categories
    TRUE ~ Ethnicity
  ))

LCA_data <- LCA_data %>%
  mutate(Ethnicity_Reduced = factor(Ethnicity_Reduced, 
                                    levels = c("Asian", 
                                               "Black",
                                               "Other/Prefer not to say",
                                               "White")))

#reduce bornuk
LCA_data <- LCA_data %>%
  mutate(BORNUK_Reduced = case_when(
    # Combine under 10 year time frame
    BORNUK %in% c("Within the last year", "Within the last 3 years", "Within the last 5 years", "Within the last 10 years") ~ "Recent Arrival",
    # Combine over 10 year time frame
    BORNUK %in% c("Within the last 15 years", "Within the last 20 years", "Within the last 30 years", "More than 30 years ago") ~ "Not Recent Arrival",
    # Keep the original remaining categories
    TRUE ~ BORNUK
  ))

LCA_data <- LCA_data %>%
  mutate(BORNUK_Reduced = factor(BORNUK_Reduced, 
                                    levels = c("I was born in the UK",
                                               "Recent Arrival", 
                                               "Not Recent Arrival",
                                               "Prefer not to say")))
#reduce org size
LCA_data <- LCA_data %>%
  mutate(Org_Size_Reduced = case_when(
    # Combine under 10 year time frame
    Org_Size %in% c( "1-10", "11-19", "20-24", "Don't know but under 25", "25-49", "50-249", "250-499", "Don't know but between 25 and 499") ~ "Small",
    # Combine over 10 year time frame
    Org_Size %in% c("500 or more") ~ "Big",
    # Keep the original remaining categories
    TRUE ~ Org_Size
  ))

LCA_data <- LCA_data %>%
  mutate(Org_Size_Reduced = factor(Org_Size_Reduced, 
                                 levels = c("Small",
                                            "Big")))
#LCA
library(poLCA)

f = cbind(Sex, Employment_Status, Ethnicity_Reduced,
          Has_Degree, Has_Second_Job, Who_Pays_You, Job_Security,
          Org_Size_Reduced, Is_Supervisor, Paid_One_Work_Other, Third_Party,
          Employer_Is_Agency, Instructed_By_Other, Work_In_Other,
          Diff_Uniform, Short_Long_Employ, I_Am_Outsourced, Disability,
          BORNUK_Reduced) ~1
LCA1<-poLCA(f, LCA_data, nclass = 2, maxiter = 1000, nrep = 10)
LCA2<-poLCA(f, LCA_data, nclass = 3, maxiter = 1000, nrep = 10)
LCA3<-poLCA(f, LCA_data, nclass = 4, maxiter = 1000, nrep = 10)
LCA4<-poLCA(f, LCA_data, nclass = 5, maxiter = 1000, nrep = 10)
LCA5<-poLCA(f, LCA_data, nclass = 6, maxiter = 1000, nrep = 10)

LCA1

anova(LCA1,LCA2,LCA3,LCA4,LCA5)
plot(LCA4)

AIC(2): 20929.75
AIC(3): 20704.29
AIC(4): 20512.32
AIC(5): 20425.05
AIC(6): 20359.45
#####HERE####
f2 = cbind(Sex, Ethnicity_Reduced, Paid_One_Work_Other, Third_Party,
            Employer_Is_Agency, Instructed_By_Other, Work_In_Other,
            Diff_Uniform, Short_Long_Employ, I_Am_Outsourced,BORNUK_Reduced)~1


LCA1<-poLCA(f2, LCA_data, nclass = 2, maxiter = 1000, nrep = 10)
LCA2<-poLCA(f2, LCA_data, nclass = 3, maxiter = 1000, nrep = 10)
LCA3<-poLCA(f2, LCA_data, nclass = 4, maxiter = 1000, nrep = 10)
LCA4<-poLCA(f2, LCA_data, nclass = 5, maxiter = 1000, nrep = 10)
LCA5<-poLCA(f2, LCA_data, nclass = 6, maxiter = 1000, nrep = 10)




#### save csv ####
write_sav(data, "./data/uncleaned_full_data.sav")
# write_sav(data_subset, "./data/cleaned_complete_case_data.sav")

# Remove everything apart from data_subset (ready for next)
rm(list = setdiff(ls(), "data_subset"))
