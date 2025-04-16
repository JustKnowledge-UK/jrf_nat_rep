library(readr)
library(weights)
library(dplyr)
## to reduce dataset further perform chi squared test on all variables by income_group
data<-read_csv("data/2025-04-16 - clean_data_jrf_experiential.csv")
#turn off scientific notation
options(scipen=99)

#filter income group outliers
data<-data %>% 
  filter(income_drop==0)

#####CHI SQUARED TESTS TO ELIMINATE VARIABLES#####

#perform chi squared tests on all variables by income_group

# List of columns to loop through (excluding the 'Income_Group' column)
columns_to_test <- c("Type_Of_Org", 
                     "Specific_Type_Of_Org",
                     "OutsourcedNonOL",
                     "TradeUnion", 
                     "TUCOV", 
                     "Occupation_Catergory",
                     "Organisation_Catergory", 
                     "Guaranteed_Hours", 
                     "Notice_Of_Working_Hours",
                     "Notice_Of_Cancelled_Shifts", 
                     "Cancelled_Shift_Pay", 
                     "Sick_Pay",
                     #"RightsViolations_Paid_On_Time", 
                     #"RightsViolations_Paid_Correct_Amount",
                     #"RightsViolations_Leave_Entitlement",
                     #"RightsViolations_Holiday_Pay",
                     #"RightsViolations_Sick_Pay",
                     #"RightsViolations_Pay_Slip",
                     #"RightsViolations_Health_Safety",
                     #"RightsViolations_None",
                     "Pay_Comparison",
                     "Training_Induction",
                     "Treatment_At_Work_InHouse",
                     "Treatment_At_Work_Managers",
                     "Inhouse_Discrimination_Age",      
                     "Inhouse_Discrimination_Disability",
                     "Inhouse_Discrimination_Nationality",
                     "Inhouse_Discrimination_Ethnicity",
                     "Inhouse_Discrimination_Sex",
                     "Client_Discrimination_Age",
                     "Client_Discrimination_Disability",
                     "Client_Discrimination_Nationality", 
                     "Client_Discrimination_Ethnicity",
                     "Client_Discrimination_Sex",
                     #"Why_Job_Like",
                     #"Why_Job_Convinient",
                    # "Why_Job_Flexibility",
                    # "Why_Job_Pay",
                    # "Why_Job_Collegues",
                     #"Why_Job_Culture", 
                    # "Why_Job_Progress",
                    # "Why_Job_BestAvailable",
                    # "Why_Job_NotQualified",
                    # "Why_Job_Health",
                   #  "Why_Job_Carer",
                     "Pros_And_Cons_Flexibility",
                     "Pros_And_Cons_Pay", 
                     "Pros_And_Cons_Hours",
                     "Pros_And_Cons_Holiday",
                     "Pros_And_Cons_Terms",
                     "Pros_And_Cons_Promotion",
                     "Pros_And_Cons_Training",
                     "Pros_And_Cons_Security",
                     "Pros_And_Cons_Treatment",
                     "Pros_And_Cons_Specialisation",
                     "Pros_And_Cons_Connection",
                     "Pros_And_Cons_FeelInvested",
                     "Pros_And_Cons_Rights",
                     "Pros_And_Cons_HealthSafety",
                     "Work_Preference"
                    # "What_Improvements_Pay",
                    # "What_Improvements_MoreHours",
                    # "What_Improvements_LessHours",
                    # "What_Improvements_MoreConsistency",
                    # "What_Improvements_MoreFlexibility",
                    # "What_Improvements_PartofInhouse",
                    # "What_Improvements_PartofOutsourced",
                    # "What_Improvements_Treatment",
                     #"What_Improvements_BetterTerms",
                    # "What_Improvements_EnforceRights",
                    # "What_Improvements_Management",
                    # "What_Improvements_Autonomy",
                    # "What_Improvements_Union",
                    # "What_Improvements_Other",
                    # "What_Improvements_None"
                   )  # Replace with your actual column names

# Initialize an empty data frame to store summary results
summary_df <- data.frame(Variable = character(), Chi_Square = numeric(), Degrees_Freedom = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through the columns to test
for (col in columns_to_test) {
  # Ensure the column exists in the data and handle any potential errors
  if (col %in% names(data)) {
    # Print column being tested (for debugging purposes)
    print(paste("Running Chi-square test for:", col))
    
    # Remove rows with missing values before running the test
    clean_data <- na.omit(data[, c("Income_Group", col, "Outsourced")])
    
    # Run the weighted chi-square test
    chi_sq_result <- wtd.chi.sq(clean_data$Income_Group, clean_data[[col]], 
                                weight = clean_data$Outsourced, 
                                na.rm = TRUE, 
                                drop.missing.levels = TRUE, 
                                mean1 = TRUE)
    
    # Index into the result vector, assuming the first element is Chi-Square, 
    # second is Degrees of Freedom, and third is the P-value.
    chi_square_value <- chi_sq_result[1]    # First element: Chi-Square value
    degree_of_freedom <- chi_sq_result[2]   # Second element: Degrees of Freedom
    p_value <- chi_sq_result[3]             # Third element: P-value
    
    # Append the results to the summary data frame
    summary_df <- rbind(summary_df, data.frame(Variable = col, 
                                               Chi_Square = chi_square_value, 
                                               Degrees_Freedom = degree_of_freedom, 
                                               P_Value = p_value))
  } else {
    # If the column doesn't exist, print a message
    print(paste("Column", col, "does not exist in the data frame. Skipping."))
  }
}

# View the summary data frame
print(summary_df) 


#show all p-values above 0.05
non_sig_vars<-summary_df %>% 
  filter(P_Value > 0.05) %>% 
  arrange(P_Value)

#filter these variables from the dataset
data<-data %>% 
  select(-all_of(non_sig_vars$Variable))

##relevling for regressions
data$Income_Group<-relevel(factor(data$Income_Group), ref = "Not low")
data$Ethnicity_Collapsed<-relevel(factor(data$Ethnicity_Collapsed), ref = "White")
data$Sex<-relevel(factor(data$Sex), ref = "Male")

#drop sex categories with low counts
data <- data %>% 
  filter(!Sex %in% c("Other", "Prefer not to say"))

#drop ethnicities with low counts
data <- data %>% 
  filter(!Ethnicity_Collapsed %in% c("Other", "Prefer not to say", "Arab"))

#########RIGHTS VIOLATIONS COUNT########
# Define the variables to check for non-empty responses (excluding 'RightsViolations_None')
rights_violations_vars <- c(
  "RightsViolations_Paid_On_Time", 
  "RightsViolations_Paid_Correct_Amount",
  "RightsViolations_Leave_Entitlement",
  "RightsViolations_Holiday_Pay",
  "RightsViolations_Sick_Pay",
  "RightsViolations_Pay_Slip",
  "RightsViolations_Health_Safety"
)

# Create a new column 'score' in the data
data$Rights_Violations_Score <- apply(data, 1, function(row) {
  
  # Check if 'RightsViolations_None' has a response (i.e., non-empty)
  if (!is.na(row["RightsViolations_None"]) && row["RightsViolations_None"] != "") {
    return(0)  # If there is a response in 'RightsViolations_None', set score to 0
  } else {
    # Count the number of non-empty responses across the specified rights violations variables
    return(sum(!is.na(row[rights_violations_vars]) & row[rights_violations_vars] != ""))
  }
})

# View the updated data with the score column
table(data$Rights_Violations_Score)

#calclate the percentage of participants with no rights violations
data %>% 
  filter(Rights_Violations_Score==0) %>% 
  nrow()/nrow(data) #57%
# equal to or more than 2 rights violations
data %>% 
  filter(Rights_Violations_Score>=2) %>% 
  nrow()/nrow(data) #16%

# Count non-empty, non-NA responses for each column
response_counts <- sapply(rights_violations_vars, function(var) {
  sum(!is.na(data[[var]]) & data[[var]] != "")
})

# View the counts for each variable
response_counts

## change NA response to NO and non-NA to YES for all rights violations vars
# Check original values
head(data[rights_violations_vars])

# Apply the mutation
data <- data %>% 
  mutate(across(all_of(rights_violations_vars), 
                ~ ifelse(is.na(.) | . == "", "No", "Yes")))


# Check updated values
head(data[rights_violations_vars])



##simple regressions

summary(lm(data$Rights_Violations_Score ~ data$Income_Group)) #not significant
summary(lm(data$Rights_Violations_Score ~ data$Ethnicity_Collapsed*data$Income_Group)) # Black people more likely to experience rights violations
summary(lm(data$Rights_Violations_Score ~ data$Sex*data$Income_Group)) #no effect of low income
summary(lm(data$Rights_Violations_Score ~ data$Sex*data$Ethnicity_Collapsed*data$Income_Group)) #no effect of low income


##### WHat Improvements ####
# Define the variables to check for non-empty responses (excluding None and OTher)
What_Improvements_vars <- c("What_Improvements_Pay",
                            "What_Improvements_MoreHours",
                            "What_Improvements_LessHours",
                            "What_Improvements_MoreConsistency",
                            "What_Improvements_MoreFlexibility",
                            "What_Improvements_PartofInhouse",
                            "What_Improvements_PartofOutsourced",
                            "What_Improvements_Treatment",
                            "What_Improvements_BetterTerms",
                            "What_Improvements_EnforceRights",
                            "What_Improvements_Management",
                            "What_Improvements_Autonomy",
                            "What_Improvements_Union"
)

# Create a new column 'score' in the data
data$What_Improvements_Score <- apply(data, 1, function(row) {
  
  # Check if "What_Improvements_None" has a response (i.e., non-empty)
  if (!is.na(row["What_Improvements_None"]) && row["What_Improvements_None"] != "") {
    return(0)  # If there is a response in "What_Improvements_None", set score to 0
  } else {
    # Count the number of non-empty responses across the specified rights violations variables
    return(sum(!is.na(row[What_Improvements_vars]) & row[What_Improvements_vars] != ""))
  }
})

# View the updated data with the score column
table(data$What_Improvements_Score)

#calclate the percentage of participants at least 1 improvement
data %>% 
  filter(What_Improvements_Score>=1) %>% 
  nrow()/nrow(data) #88%

# equal to or more than 2 improvements
data %>% 
  filter(What_Improvements_Score>=2) %>% 
  nrow()/nrow(data) #60%

# Count non-empty, non-NA responses for each column
response_counts <- sapply(What_Improvements_vars, function(var) {
  sum(!is.na(data[[var]]) & data[[var]] != "")
})

# View the counts for each variable
response_counts

## change NA response to NO and non-NA to YES for all rights violations vars
# Check original values
head(data[What_Improvements_vars])

# Apply the mutation
data <- data %>% 
  mutate(across(all_of(What_Improvements_vars), 
                ~ ifelse(is.na(.) | . == "", "No", "Yes")))


# Check updated values
head(data[What_Improvements_vars])

####Regressions##########
library(jtools)

###### what improvements ####
What_Improvements_Score.fit1<- (lm(What_Improvements_Score ~ 
                                    Age +
                                    Sex +
                                    Ethnicity_Collapsed +
                                    BORNUK +
                                    Income_Group, weights= Outsourced, data)) 

summary(What_Improvements_Score.fit1) #significant
export_summs(What_Improvements_Score.fit1, scale=TRUE, robust=TRUE)
plot_summs(What_Improvements_Score.fit1, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "What Improvements Score")

##what improvements##
What_Improvements_Score.fit2<- (lm(What_Improvements_Score ~ 
                                    Age +
                                    Sex *
                                    Ethnicity_Collapsed +
                                    BORNUK +
                                    Income_Group, weights= Outsourced,data)) 

summary(What_Improvements_Score.fit2) #significant
export_summs(What_Improvements_Score.fit2, scale=TRUE, robust=TRUE)
plot_summs(What_Improvements_Score.fit2, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "What Improvements Score")
##what improvements##
What_Improvements_Score.fit3<- (lm(What_Improvements_Score ~ 
                                     Age +
                                     Sex *
                                     Ethnicity_Collapsed *
                                     Income_Group +
                                     BORNUK, weights= Outsourced, data)) 

summary(What_Improvements_Score.fit3) #significant

anova(What_Improvements_Score.fit1, What_Improvements_Score.fit2, What_Improvements_Score.fit3) #interactions improve model

#multivariate multiple regression model for what improvements

library(forcats)

data <- data %>%
  mutate(
    What_Improvements_Pay = as_factor(What_Improvements_Pay),
    What_Improvements_MoreHours = as_factor(What_Improvements_MoreHours),
    What_Improvements_LessHours = as_factor(What_Improvements_LessHours),
    What_Improvements_MoreConsistency = as_factor(What_Improvements_MoreConsistency),
    What_Improvements_MoreFlexibility = as_factor(What_Improvements_MoreFlexibility),
    What_Improvements_PartofInhouse = as_factor(What_Improvements_PartofInhouse),
    What_Improvements_PartofOutsourced = as_factor(What_Improvements_PartofOutsourced),
    What_Improvements_Treatment = as_factor(What_Improvements_Treatment),
    What_Improvements_BetterTerms = as_factor(What_Improvements_BetterTerms),
    What_Improvements_EnforceRights = as_factor(What_Improvements_EnforceRights),
    What_Improvements_Management = as_factor(What_Improvements_Management),
    What_Improvements_Autonomy = as_factor(What_Improvements_Autonomy),
    What_Improvements_Union = as_factor(What_Improvements_Union)
  )

What_Improvements_Score.fitM <- lm(cbind(What_Improvements_Pay,
                                         What_Improvements_MoreHours,
                                         What_Improvements_LessHours,
                                         What_Improvements_MoreConsistency,
                                         What_Improvements_MoreFlexibility,
                                         What_Improvements_PartofInhouse,
                                         What_Improvements_PartofOutsourced,
                                         What_Improvements_Treatment,
                                         What_Improvements_BetterTerms,
                                         What_Improvements_EnforceRights,
                                         What_Improvements_Management,
                                         What_Improvements_Autonomy,
                                         What_Improvements_Union) ~
                                     Age +
                                     Sex +
                                     Ethnicity_Collapsed +
                                     BORNUK +
                                     Income_Group, weights = Outsourced,family = "binomial", data) 



summary(What_Improvements_Score.fitM) # some sig results here

##dig into specific improvements##
# Pay
What_Improvements_Score.fitPay <- glm(What_Improvements_Pay ~
                                        Age +
                                        Sex +
                                        Ethnicity_Collapsed +
                                        BORNUK +
                                        Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitPay) # Black people more likely to suggest pay improvements

# More Hours
What_Improvements_Score.fitMoreHours <- glm(What_Improvements_MoreHours ~
                                              Age +
                                              Sex +
                                              Ethnicity_Collapsed +
                                              BORNUK +
                                              Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitMoreHours) # Age and Sex(female) associated with lower likelihood of suggesting more hours. People who aaroved in the last year are much more likely

# Less Hours
What_Improvements_Score.fitLessHours <- glm(What_Improvements_LessHours ~
                                              Age +
                                              Sex +
                                              Ethnicity_Collapsed +
                                              BORNUK +
                                              Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitLessHours) # Low pay associated with lower likelihood of suggesting less hours

# More Consistency
What_Improvements_Score.fitMoreConsistency <- glm(What_Improvements_MoreConsistency ~
                                                    Age +
                                                    Sex +
                                                    Ethnicity_Collapsed +
                                                    BORNUK +
                                                    Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitMoreConsistency) # Age associated with lower likelihood of suggesting more consistency. Being Black associated with higher likelihood (no other ethnic groups)

# More Flexibility
What_Improvements_Score.fitMoreFlexibility <- glm(What_Improvements_MoreFlexibility ~
                                                    Age +
                                                    Sex +
                                                    Ethnicity_Collapsed +
                                                    BORNUK +
                                                    Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitMoreFlexibility) # Age associated with lower likelihood of suggesting more flexibility. Being Asian associated with higher likelihood

# Part of In-house
What_Improvements_Score.fitPartofInhouse <- glm(What_Improvements_PartofInhouse ~
                                                  Age +
                                                  Sex +
                                                  Ethnicity_Collapsed +
                                                  BORNUK +
                                                  Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitPartofInhouse) # Sex (female) associated with lower likelihood of wanting to be part of InHouse

# Part of Outsourced
What_Improvements_Score.fitPartofOutsourced <- glm(What_Improvements_PartofOutsourced ~
                                                     Age +
                                                     Sex +
                                                     Ethnicity_Collapsed +
                                                     BORNUK +
                                                     Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitPartofOutsourced) #NA - although within last 20 years is sig

# Treatment
What_Improvements_Score.fitTreatment <- glm(What_Improvements_Treatment ~
                                              Age +
                                              Sex +
                                              Ethnicity_Collapsed +
                                              BORNUK +
                                              Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitTreatment) # NA

# Better Terms
What_Improvements_Score.fitBetterTerms <- glm(What_Improvements_BetterTerms ~
                                                Age +
                                                Sex +
                                                Ethnicity_Collapsed +
                                                BORNUK +
                                                Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitBetterTerms) # NA

# Enforce Rights
What_Improvements_Score.fitEnforceRights <- glm(What_Improvements_EnforceRights ~
                                                  Age +
                                                  Sex +
                                                  Ethnicity_Collapsed +
                                                  BORNUK +
                                                  Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitEnforceRights) # Age associated with lower likelihood of suggesting enforcing rights.

# Management
What_Improvements_Score.fitManagement <- glm(What_Improvements_Management ~
                                               Age +
                                               Sex +
                                               Ethnicity_Collapsed +
                                               BORNUK +
                                               Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitManagement) # Age and Low pay associated with lower likelihood of suggesting management improvements

# Autonomy
What_Improvements_Score.fitAutonomy <- glm(What_Improvements_Autonomy ~
                                             Age +
                                             Sex +
                                             Ethnicity_Collapsed +
                                             BORNUK +
                                             Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitAutonomy) # Age and Sex associated with lower likelihood of suggesting autonomy improvements

# Union
What_Improvements_Score.fitUnion <- glm(What_Improvements_Union ~
                                          Age +
                                          Sex +
                                          Ethnicity_Collapsed +
                                          BORNUK +
                                          Income_Group, weights = Outsourced, family = "binomial", data = data) 

summary(What_Improvements_Score.fitUnion) # Sex associated with lower likelihood of suggesting union improvements


######rights violations#####
Rights_Violations_Score.fit1<- (lm(Rights_Violations_Score ~ 
                                     Age +
                                     Sex +
                                     Ethnicity_Collapsed +
                                     BORNUK +
                                     Income_Group, weights = Outsourced, data)) 

summary(Rights_Violations_Score.fit1) # some significant
export_summs(Rights_Violations_Score.fit1, scale=TRUE, robust=TRUE)
plot_summs(Rights_Violations_Score.fit1, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "Rights Violations Score")

#fit 2
Rights_Violations_Score.fit2<- (lm(Rights_Violations_Score ~ 
                                     Age +
                                     Sex *
                                     Ethnicity_Collapsed +
                                     BORNUK +
                                     Income_Group, weights = Outsourced, data)) 

summary(Rights_Violations_Score.fit2) #some 

#fit 3
Rights_Violations_Score.fit3<- (lm(Rights_Violations_Score ~ 
                                     Age +
                                     Sex *
                                     Ethnicity_Collapsed *
                                     Income_Group +
                                     BORNUK, weights = Outsourced, data)) 

summary(Rights_Violations_Score.fit3) #significant

anova(Rights_Violations_Score.fit1, Rights_Violations_Score.fit2, Rights_Violations_Score.fit3) #interactions not useful improvement to fit

#multivariate multiple regression model for rights violations

data <- data %>%
  mutate(
    RightsViolations_Paid_On_Time = as_factor(RightsViolations_Paid_On_Time),
    RightsViolations_Paid_Correct_Amount = as_factor(RightsViolations_Paid_Correct_Amount),
    RightsViolations_Leave_Entitlement = as_factor(RightsViolations_Leave_Entitlement),
    RightsViolations_Holiday_Pay = as_factor(RightsViolations_Holiday_Pay),
    RightsViolations_Sick_Pay = as_factor(RightsViolations_Sick_Pay),
    RightsViolations_Pay_Slip = as_factor(RightsViolations_Pay_Slip),
    RightsViolations_Health_Safety = as_factor(RightsViolations_Health_Safety)
  )

# Paid On Time
Rights_Violations_Score.fitPaidOnTime <- glm(RightsViolations_Paid_On_Time ~
                                               Age +
                                               Sex +
                                               Ethnicity_Collapsed +
                                               BORNUK +
                                               Income_Group, 
                                             family = binomial, 
                                             weights = Outsourced,
                                             data = data)

summary(Rights_Violations_Score.fitPaidOnTime)

# Paid Correct Amount
Rights_Violations_Score.fitPaidCorrectAmount <- glm(RightsViolations_Paid_Correct_Amount ~
                                                      Age +
                                                      Sex +
                                                      Ethnicity_Collapsed +
                                                      BORNUK +
                                                      Income_Group, 
                                                    family = binomial, 
                                                    weights = Outsourced,
                                                    data = data)

summary(Rights_Violations_Score.fitPaidCorrectAmount)

# Leave Entitlement
Rights_Violations_Score.fitLeaveEntitlement <- glm(RightsViolations_Leave_Entitlement ~
                                                     Age +
                                                     Sex +
                                                     Ethnicity_Collapsed +
                                                     BORNUK +
                                                     Income_Group, 
                                                   family = binomial, 
                                                   weights = Outsourced,
                                                   data = data)

summary(Rights_Violations_Score.fitLeaveEntitlement)

# Holiday Pay
Rights_Violations_Score.fitHolidayPay <- glm(RightsViolations_Holiday_Pay ~
                                               Age +
                                               Sex +
                                               Ethnicity_Collapsed +
                                               BORNUK +
                                               Income_Group, 
                                             family = binomial, 
                                             weights = Outsourced,
                                             data = data)

summary(Rights_Violations_Score.fitHolidayPay)

# Sick Pay
Rights_Violations_Score.fitSickPay <- glm(RightsViolations_Sick_Pay ~
                                            Age +
                                            Sex +
                                            Ethnicity_Collapsed +
                                            BORNUK +
                                            Income_Group, 
                                          family = binomial, 
                                          weights = Outsourced,
                                          data = data)

summary(Rights_Violations_Score.fitSickPay)

# Pay Slip
Rights_Violations_Score.fitPaySlip <- glm(RightsViolations_Pay_Slip ~
                                            Age +
                                            Sex +
                                            Ethnicity_Collapsed +
                                            BORNUK +
                                            Income_Group, 
                                          family = binomial, 
                                          weights = Outsourced,
                                          data = data)

summary(Rights_Violations_Score.fitPaySlip)

# Health Safety
Rights_Violations_Score.fitHealthSafety <- glm(RightsViolations_Health_Safety ~
                                                 Age +
                                                 Sex +
                                                 Ethnicity_Collapsed +
                                                 BORNUK +
                                                 Income_Group, 
                                               family = binomial, 
                                               weights = Outsourced,
                                               data = data)

summary(Rights_Violations_Score.fitHealthSafety)

#### CLarity ######

Clarity.fit1<- (lm(Clarity_Overall_Mean ~ 
                                     Age +
                                     Sex +
                                     Ethnicity_Collapsed +
                                     BORNUK +
                                     Income_Group, weights = Outsourced, data)) 

summary(Clarity.fit1) # some significant
#summ(What_Improvements_Score.fit)

#fit 2
Clarity.fit2<- (lm(Clarity_Overall_Mean ~ 
                                     Age +
                                     Sex *
                                     Ethnicity_Collapsed +
                                     BORNUK +
                                     Income_Group, weights = Outsourced, data)) 

summary(Clarity.fit2) #some 

export_summs(Clarity.fit2, scale=TRUE, robust=TRUE)
plot_summs(Clarity.fit2, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "Clarity Overall Mean")

#fit 3
Clarity.fit3<- (lm(Clarity_Overall_Mean ~ 
                                     Age +
                                     Sex *
                                     Ethnicity_Collapsed *
                                     Income_Group +
                                     BORNUK, weights = Outsourced, data)) 

summary(Clarity.fit3) #significant

anova(Clarity.fit1, Clarity.fit2, Clarity.fit3) #model 2 is better


#multivariate multiple regression model for clarity
#TBC


###Chi Squared Tests for Ethnicity####

# Initialize an empty data frame to store summary results
summary_df <- data.frame(Variable = character(), Chi_Square = numeric(), Degrees_Freedom = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through the columns to test
for (col in columns_to_test) {
  # Ensure the column exists in the data and handle any potential errors
  if (col %in% names(data)) {
    # Print column being tested (for debugging purposes)
    print(paste("Running Chi-square test for:", col))
    
    # Remove rows with missing values before running the test
    clean_data <- na.omit(data[, c("Ethnicity_Collapsed", col, "Outsourced")])
    
    # Run the weighted chi-square test
    chi_sq_result <- wtd.chi.sq(clean_data$Ethnicity_Collapsed, clean_data[[col]], 
                                weight = clean_data$Outsourced, 
                                na.rm = TRUE, 
                                drop.missing.levels = TRUE, 
                                mean1 = TRUE)
    
    # Index into the result vector, assuming the first element is Chi-Square, 
    # second is Degrees of Freedom, and third is the P-value.
    chi_square_value <- chi_sq_result[1]    # First element: Chi-Square value
    degree_of_freedom <- chi_sq_result[2]   # Second element: Degrees of Freedom
    p_value <- chi_sq_result[3]             # Third element: P-value
    
    # Append the results to the summary data frame
    summary_df <- rbind(summary_df, data.frame(Variable = col, 
                                               Chi_Square = chi_square_value, 
                                               Degrees_Freedom = degree_of_freedom, 
                                               P_Value = p_value))
  } else {
    # If the column doesn't exist, print a message
    print(paste("Column", col, "does not exist in the data frame. Skipping."))
  }
}

# View the summary data frame
print(summary_df)


###explore discrimination questions#### 

data <- data %>%
  mutate(across(c(Client_Discrimination_Age, 
                  Client_Discrimination_Disability, 
                  Client_Discrimination_Nationality, 
                  Client_Discrimination_Ethnicity, 
                  Client_Discrimination_Sex), 
                ~ case_when(
                  . == "Never" ~ 0,
                  . == "Rarely" ~ 1,
                  . == "Sometimes" ~ 2,
                  . == "Often" ~ 3,
                  . == "Don’t know" ~ NA_real_,
                  . == "NA - I don't interact with clients or customers in my role" ~ NA_real_,
                  . == "Prefer not to say" ~ NA_real_,
                  TRUE ~ NA_real_  # Added to handle any unexpected values
                )))



#client discrimination Age
client_discrimination_age.fit<- (lm(Client_Discrimination_Age ~ 
                     Age +
                     Sex +
                     Ethnicity_Collapsed +
                     Income_Group +
                     BORNUK, weights = Outsourced,data))

summary(client_discrimination_age.fit) 
export_summs(client_discrimination_age.fit, scale=TRUE, robust=TRUE)
plot_summs(client_discrimination_age.fit, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "Age Discrimination")


#disability
Client_Discrimination_Disability.fit<- (lm(Client_Discrimination_Disability ~ 
                                      Age +
                                      Sex +
                                      Ethnicity_Collapsed +
                                      Income_Group +
                                      BORNUK,weights = Outsourced, data)) 

summary(Client_Discrimination_Disability.fit) 
export_summs(Client_Discrimination_Disability.fit, scale=TRUE, robust=TRUE)
plot_summs(Client_Discrimination_Disability.fit, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "Disability")
#nationality
Client_Discrimination_Nationality.fit<- (lm(Client_Discrimination_Nationality ~ 
                                             Age +
                                             Sex +
                                             Ethnicity_Collapsed +
                                             Income_Group +
                                             BORNUK, weights = Outsourced,data)) 

summary(Client_Discrimination_Nationality.fit) 
export_summs(Client_Discrimination_Nationality.fit, scale=TRUE, robust=TRUE)
plot_summs(Client_Discrimination_Nationality.fit, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "Nationality")
#Ethnicity
Client_Discrimination_Ethnicity.fit<- (lm(Client_Discrimination_Ethnicity ~ 
                                              Age +
                                              Sex +
                                              Ethnicity_Collapsed +
                                              Income_Group +
                                              BORNUK, weights = Outsourced,data)) 

summary(Client_Discrimination_Ethnicity.fit) 
export_summs(Client_Discrimination_Ethnicity.fit, scale=TRUE, robust=TRUE)
plot_summs(Client_Discrimination_Ethnicity.fit, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "Ethnicity")
#Sex
Client_Discrimination_Sex.fit<- (lm(Client_Discrimination_Sex ~ 
                                            Age +
                                            Sex +
                                            Ethnicity_Collapsed +
                                            Income_Group +
                                            BORNUK, weights = Outsourced,data)) 

summary(Client_Discrimination_Sex.fit) #not sig!?
export_summs(Client_Discrimination_Sex.fit, scale=TRUE, robust=TRUE)
plot_summs(Client_Discrimination_Sex.fit, plot.distributions = TRUE,
           rescale.distributions = TRUE, scale = TRUE, robust = TRUE, title = "Sex")


###Chi Squared Tests for Sex####

# Initialize an empty data frame to store summary results
summary_df <- data.frame(Variable = character(), Chi_Square = numeric(), Degrees_Freedom = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through the columns to test
for (col in columns_to_test) {
  # Ensure the column exists in the data and handle any potential errors
  if (col %in% names(data)) {
    # Print column being tested (for debugging purposes)
    print(paste("Running Chi-square test for:", col))
    
    # Remove rows with missing values before running the test
    clean_data <- na.omit(data[, c("Sex", col, "Outsourced")])
    
    # Run the weighted chi-square test
    chi_sq_result <- wtd.chi.sq(clean_data$Sex, clean_data[[col]], 
                                weight = clean_data$Outsourced, 
                                na.rm = TRUE, 
                                drop.missing.levels = TRUE, 
                                mean1 = TRUE)
    
    # Index into the result vector, assuming the first element is Chi-Square, 
    # second is Degrees of Freedom, and third is the P-value.
    chi_square_value <- chi_sq_result[1]    # First element: Chi-Square value
    degree_of_freedom <- chi_sq_result[2]   # Second element: Degrees of Freedom
    p_value <- chi_sq_result[3]             # Third element: P-value
    
    # Append the results to the summary data frame
    summary_df <- rbind(summary_df, data.frame(Variable = col, 
                                               Chi_Square = chi_square_value, 
                                               Degrees_Freedom = degree_of_freedom, 
                                               P_Value = p_value))
  } else {
    # If the column doesn't exist, print a message
    print(paste("Column", col, "does not exist in the data frame. Skipping."))
  }
}

# View the summary data frame
print(summary_df)


### guranteed houes etc####

#order factor levels
data$Guaranteed_Hours<-factor(data$Guaranteed_Hours, levels = c("No guaranteed hours (zero hours)",
                                                                "1-8 hours", 
                                                                "9-15 hours",
                                                                "16-24 hours",
                                                                "25-35 hours",
                                                                "35+ hours"))
                                                              
library(MASS)
Guaranteed_Hours.fit<- (polr(Guaranteed_Hours ~ 
                                      Age +
                                      Sex +
                                      Ethnicity_Collapsed +
                                      Income_Group +
                                      BORNUK, data, Hess=TRUE)) 

summary(Guaranteed_Hours.fit) 

(ctable <- coef(summary(Guaranteed_Hours.fit)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

##write csv
write.csv(data, "data/postx2_data.csv")
