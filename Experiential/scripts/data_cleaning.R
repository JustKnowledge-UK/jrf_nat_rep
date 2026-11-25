#libraries
library(haven)
library(plyr) # load plyr first because otherwise it interferes with dplyr
library(tidyverse)
library(skimr)
library(readxl)
library(janitor)

# clear the environment
rm(list=ls())

#import data
raw_data <- read_sav("./Experiential/data/UK23626 - JRF - Outsourced workers - Experiential data excluding firm names.sav")

raw_data <- haven::as_factor(raw_data)
skim(raw_data)


#drop unneeded columns
raw_data <- raw_data %>%
  select(-c(Q1_B_1, 
            Q1_B_2, 
            Q1_B_3, 
            Q1_B_4, 
            Q1_B_5, 
            Q1_B_6,
            InfoNotOutsourced, 
            SCD_Opt_In_1,
            SCD_Opt_In_2, 
            Q11_RIGHTSVIOLATIONS_8)) %>%
  select(-starts_with("D7"))

#rename vars
raw_data <- raw_data %>%
  dplyr::rename(
    ID = respid_2,
    Sex = D1_Gender_C,
    Age = D2_Age,
    Region = D4_Region_C,
    Employment_Status = D3_employment,
    Is_an_employee = E2,
    Paid_One_Work_Other = Q1_A_1, #outsourced def q1
    Third_Party = Q1_A_2, #outsourced def q2
    Employer_Is_Agency = Q1_A_3, #outsourced def q3
    Instructed_By_Other = Q1_A_4,#outsourced def q4
    Work_In_Other = Q1_A_5,#outsourced def q5
    Diff_Uniform = Q1_A_6,#outsourced def q6
    Short_Long_Employ = Q2,
    Short_Long_Employ_TEXT = Q2_3_other,
    I_Am_Outsourced = Q3a,
    Outsourced_And_Agency = Q3b,
    Might_Be_Outsourced_And_Agency = Q3c,
    I_Am_Agency = Q3d,
    Type_Of_Org = E10a,
    Specific_Type_Of_Org = E10b,
    Type_Of_Org_TEXT = E10b_9_other,
    Has_Degree = D6_educ1,
    Education_Band = ed_bands,
    TUCOV = TUCOV, #pay and conditions of employment directly affected by agreements between your employer and any trade union?
    Who_Pays_You = E3,
    Perm_Temp_Cas = E5, #permanent, temporary or casual
    Temp_Explanation_Agency = E6_1,
    Temp_Explanation_Contract = E6_2,
    Temp_Explanation_Seasonal = E6_3,
    Temp_Explanation_Apprenticeship = E6_4,
    Temp_Explanation_TEXT = E6_5_other,
    Job_Title_TEXT = E8a,
    What_Do_You_Do_TEXT = E8b,
    What_Does_Your_Employer_Do_TEXT = E9a,
    Occupation_Catergory = E9b,
    Organisation_Catergory = E9c,
    Income_Frequency = INCOME_FREQ,
    Hours_Per_Week = HOURS,
    Guaranteed_Hours = Q2_GUARENTEEDHOURS,
    Notice_Of_Working_Hours = Q4_NOTICE,
    Notice_Of_Cancelled_Shifts = Q5a_CANCELLATION,
    Cancelled_Shift_Pay = Q5b,
    Sick_Pay = Q6_SICKPAY,
    TUPE = Q8_TUPE,
    TermsChanging_Pay = Q9_TERMSCHANGING_1,
    TermsChanging_Additional_Pay = Q9_TERMSCHANGING_2,
    TermsChanging_Leave = Q9_TERMSCHANGING_3,
    TermsChanging_Other_Leave = Q9_TERMSCHANGING_4,
    TermsChanging_Sick_Pay = Q9_TERMSCHANGING_5,
    TermsChanging_Flexible_Working = Q9_TERMSCHANGING_6,
    TermsChanging_Hours = Q9_TERMSCHANGING_7,
    TermsChanging_Contract = Q9_TERMSCHANGING_8,
    TermsChanging_Pension = Q9_TERMSCHANGING_9,
    TermsChanging_TEXT = Q10_CONTRACTCHANGEHOW,
    RightsViolations_Paid_On_Time = Q11_RIGHTSVIOLATIONS_1,
    RightsViolations_Paid_Correct_Amount = Q11_RIGHTSVIOLATIONS_2,
    RightsViolations_Leave_Entitlement = Q11_RIGHTSVIOLATIONS_3,
    RightsViolations_Holiday_Pay = Q11_RIGHTSVIOLATIONS_4,
    RightsViolations_Sick_Pay = Q11_RIGHTSVIOLATIONS_5,
    RightsViolations_Pay_Slip = Q11_RIGHTSVIOLATIONS_6,
    RightsViolations_Health_Safety = Q11_RIGHTSVIOLATIONS_7,
    RightsViolations_None = Q11_RIGHTSVIOLATIONS_9,
    RightsViolations_TEXT = Q11_RIGHTSVIOLATIONS_8_other,
    Job_Satisfaction = Q12_JOBSATISFACTION,
    Pay_Comparison = Q15_PAYCOMPARISON1,
    Pay_Rise_Outsourced = Q16_PAYCOMPARISON2a_1,
    Pay_Rise_InHouse = Q16_PAYCOMPARISON2b_1,
    Who_Got_More = Q16_PAYCOMPARISON3, # And which group do you think got a bigger pay rise as a percentage of their pay?
    Tasks_Instructions = Q17_TASKSANDINSTRUCTIONS,
    Uniform = Q18a,
    Uniform_Identification = Q18_UNIFORMA,
    Uniform_Preference = Q18_UNIFORMb,
    Uniform_Treatment_Externals = Q18_UNIFORMc_1,
    Uniform_Treatment_Same_Building = Q18_UNIFORMc_1_1,
    Uniform_Treatment_InHouse = Q18_UNIFORMc_2_1,
    Access_to_Amenities = Q19_DIFFERENTSPACESA,
    Access_to_Amenities_Feelings = Q19_DIFFERENTSPCAESB, #You told us that you have different access to spaces to rest or eat in, compared to in-house staff. How do you feel about this?
    Connection_Belonging = Q20_BELONGINGANDCONNECTION,
    PartOftheTeam_Social_Events = Q21_PARTOFTEAM_1,
    PartOftheTeam_Lunch_Break = Q21_PARTOFTEAM_2,
    PartOftheTeam_Friends = Q21_PARTOFTEAM_3,
    PartOftheTeam_Know_Names = Q21_PARTOFTEAM_4,
    Training_Induction = Q22_TRAININGANDINDUCTION,
    Treatment_At_Work_Overall = Q23_TREATMENTATWORKQUALITY,
    Treatment_At_Work_Managers = Q24_TREATMENTATWORKCOMPARISON_1,
    Treatment_At_Work_InHouse = Q24_TREATMENTATWORKCOMPARISON_2,
    Treatment_At_Work_Customers = Q24_TREATMENTATWORKCOMPARISON_3,
    Inhouse_Discrimination_Age = Q25A_WORKPLACEDISCRIMINATION_1,
    Inhouse_Discrimination_Disability = Q25A_WORKPLACEDISCRIMINATION_2,
    Inhouse_Discrimination_Nationality = Q25A_WORKPLACEDISCRIMINATION_3,
    Inhouse_Discrimination_Ethnicity = Q25A_WORKPLACEDISCRIMINATION_4,
    Inhouse_Discrimination_Sex = Q25A_WORKPLACEDISCRIMINATION_5,
    Client_Discrimination_Age = Q25B_WORKPLACEDISCRIMINATION_1,
    Client_Discrimination_Disability = Q25B_WORKPLACEDISCRIMINATION_2,
    Client_Discrimination_Nationality = Q25B_WORKPLACEDISCRIMINATION_3,
    Client_Discrimination_Ethnicity = Q25B_WORKPLACEDISCRIMINATION_4,
    Client_Discrimination_Sex = Q25B_WORKPLACEDISCRIMINATION_5,
    Complaints = Q26_COMPLAINTSANDPROBLEMS, #How far do you agree or disagree with the following statement? If I have a problem at work (for example, a problem with my pay or other entitlements) and I raise a complaint then the problem will be resolved quickly.
    Clarity1 = Q27_CLARITYANDCONFIDENCE_1, #I feel clear about who in my work to speak to if I have a problem with my pay
    Clarity2 = Q27_CLARITYANDCONFIDENCE_2, #I feel clear about who to speak to if I have another problem with accessing my rights and entitlements at work (not including pay)
    Clarity3 = Q27_CLARITYANDCONFIDENCE_3, #It is straightforward to understand who is responsible for issues like approving time off
    Clarity4 = Q27_CLARITYANDCONFIDENCE_4, #I feel clear about who I can speak to if I want to get a promotion
    Clarity5 = Q27_CLARITYANDCONFIDENCE_5, #There is clear communication between me, the organisation who pays me, and the organisation that I do work for
    Clarity6 = Q27_CLARITYANDCONFIDENCE_6, # I am clear about the responsibilities of my role at work
    Clarity7 = Q27_CLARITYANDCONFIDENCE_7, #If I think something could be improved on at work (such as a process or way of doing things), I am confident that I can communicate this to the right people
    Clarity8 = Q27_CLARITYANDCONFIDENCE_8, #If I think something could be improved on at work (such as a process or way of doing things), I am confident my opinion will be respected and taken seriously
    Clarity9 = Q27_CLARITYANDCONFIDENCE_9, #Management is good at preventing discrimination
    Clarity10 = Q27_CLARITYANDCONFIDENCE_10, # Management takes complaints about bullying seriously
    Clarity11 = Q27_CLARITYANDCONFIDENCE_11, #Management takes complaints about racism seriously
    Why_Job_Like = Q29_WHYJOBA_1,
    Why_Job_Convinient = Q29_WHYJOBA_2,
    Why_Job_Flexibility = Q29_WHYJOBA_3,
    Why_Job_Pay = Q29_WHYJOBA_4,
    Why_Job_Collegues = Q29_WHYJOBA_5,
    Why_Job_Culture = Q29_WHYJOBA_6,
    Why_Job_Progress = Q29_WHYJOBA_7,
    Why_Job_BestAvailable = Q29_WHYJOBA_8,
    Why_Job_NotQualified = Q29_WHYJOBA_9,
    Why_Job_Health = Q29_WHYJOBA_10,
    Why_Job_Carer = Q29_WHYJOBA_11,
    Why_Job_TEXT = Q29_WHYJOBA_12_other,
    Why_Job_MostImportant = Q29_WHYJOBB,
    Pros_And_Cons_Flexibility = Q30_1,
    Pros_And_Cons_Pay = Q30_2,
    Pros_And_Cons_Hours = Q30_3,
    Pros_And_Cons_Holiday = Q30_4,
    Pros_And_Cons_Terms = Q30_5,
    Pros_And_Cons_Promotion = Q30_6,
    Pros_And_Cons_Training = Q30_7,
    Pros_And_Cons_Security = Q30_8,
    Pros_And_Cons_Treatment = Q30_9,
    Pros_And_Cons_Specialisation = Q30_10,
    Pros_And_Cons_Connection = Q30_11,
    Pros_And_Cons_FeelInvested = Q30_12,
    Pros_And_Cons_Rights = Q30_13,
    Pros_And_Cons_HealthSafety = Q30_14,
    Why_Outsourced_SaveMoney = Q31_WHY_OUTSOURCED_1,
    Why_Outsourced_LowerPay = Q31_WHY_OUTSOURCED_2,
    Why_Outsourced_IncreasePay = Q31_WHY_OUTSOURCED_3,
    Why_Outsourced_WorseTerms = Q31_WHY_OUTSOURCED_4,
    Why_Outsourced_BetterTerms = Q31_WHY_OUTSOURCED_5,
    Why_Outsourced_Specialisation = Q31_WHY_OUTSOURCED_6,
    Why_Outsourced_MoreProgression = Q31_WHY_OUTSOURCED_7,
    Why_Outsourced_Flexibility = Q31_WHY_OUTSOURCED_8,
    Why_Outsourced_Redundancies = Q31_WHY_OUTSOURCED_9,
    Why_Outsourced_TEXT = Q31_WHY_OUTSOURCED_10_other,
    What_Improvements_Pay = Q32_IMPROVEMENTS_1,
    What_Improvements_MoreHours = Q32_IMPROVEMENTS_2,
    What_Improvements_LessHours = Q32_IMPROVEMENTS_3,
    What_Improvements_MoreConsistency = Q32_IMPROVEMENTS_4,
    What_Improvements_MoreFlexibility = Q32_IMPROVEMENTS_5,
    What_Improvements_PartofInhouse = Q32_IMPROVEMENTS_6,
    What_Improvements_PartofOutsourced = Q32_IMPROVEMENTS_7,
    What_Improvements_Treatment = Q32_IMPROVEMENTS_8,
    What_Improvements_BetterTerms = Q32_IMPROVEMENTS_9,
    What_Improvements_EnforceRights = Q32_IMPROVEMENTS_10,
    What_Improvements_Management = Q32_IMPROVEMENTS_11,
    What_Improvements_Autonomy = Q32_IMPROVEMENTS_12,
    What_Improvements_Union = Q32_IMPROVEMENTS_13,
    What_Improvements_Other = Q32_IMPROVEMENTS_14,
    What_Improvements_None = Q32_IMPROVEMENTS_15,
    What_Improvements_Other_TEXT = Q32_IMPROVEMENTS_14_other,
    Work_Preference = Q33_PREFERENCES)
    

##coallesce OutsourcedNonOL_2 etc into one variable 'outsource definition group'
raw_data <- raw_data %>%
  mutate(OutsourcedNonOL = coalesce(OutsourcedNonOL_2, OutsourcedNonOL_3, OutsourcedNonOL_4))

#  Delete the original variables
raw_data <- raw_data %>%
  select(-OutsourcedNonOL_2, -OutsourcedNonOL_3, -OutsourcedNonOL_4)

#show how many people are working every single hour of the week
raw_data %>%
  filter(Hours_Per_Week >= 168) %>%
  nrow() #= 11

##fix hours per week var so that anything above 168 is set to NA
raw_data <- raw_data %>%
  mutate(Hours_Per_Week = case_when(
    Hours_Per_Week > 168 ~ NA_real_,
    TRUE ~ Hours_Per_Week
  ))

#show how many people are working above working time directive = 48 hours
raw_data %>%
  filter(Hours_Per_Week > 48) %>%
  nrow() #= 73

####################
#### INCOME VAR ####
####################

# work out min holiday entitlement and use this to calculate working weeks
weeks_in_year <- 365 / 7 # if we're being pedantic (this is 52.14 weeks)
min_holiday_entitlement <- 28
non_working_weeks <- min_holiday_entitlement/5
working_weeks <- weeks_in_year - non_working_weeks

##create income var based on previous code used in Nat Rep
# annual income
raw_data <- raw_data %>%
  mutate(Self_Report_Annual_Salary = case_when(
    Income_Frequency == "Annually / per year" ~ INCOME_OPEN_1,
    Income_Frequency == "Monthly" ~ INCOME_OPEN_1 * 12,
    Income_Frequency == "Weekly" ~ INCOME_OPEN_1 * working_weeks,
    Income_Frequency == "Hourly" ~ INCOME_OPEN_1 * Hours_Per_Week * working_weeks, # Use Hours_Per_Week variable here
    TRUE ~ NA  # Handle cases where INCOME_FREQ is not recognized
  ))

# weekly income
raw_data <- raw_data %>%
  mutate(Self_Report_Weekly_Salary = case_when(
    Income_Frequency == "Annually / per year" ~ INCOME_OPEN_1 / working_weeks,
    Income_Frequency == "Monthly" ~ (INCOME_OPEN_1 * 12) / working_weeks,
    Income_Frequency == "Weekly" ~ INCOME_OPEN_1,
    Income_Frequency == "Hourly" ~ INCOME_OPEN_1 * Hours_Per_Week, # Use Hours_Per_Week variable here
    TRUE ~ NA  # Handle cases where INCOME_FREQ is not recognized
  ))

# Visual check of the new variable
summary(raw_data$Self_Report_Annual_Salary)

#make income correct vars numeric
raw_data <- raw_data %>%
  mutate(
    INCOME_CORRECT_2_other = as.numeric(INCOME_CORRECT_2_other),
    INCOME_CORRECT_3_other = as.numeric(INCOME_CORRECT_3_other),
    INCOME_CORRECT_4_other = as.numeric(INCOME_CORRECT_4_other),
    INCOME_CORRECT_5_other = as.numeric(INCOME_CORRECT_5_other)
  )

## Income correct vars correction
# annual income
raw_data <- raw_data %>%
  mutate(Corrected_Self_Report_Annual_Salary = case_when(
    Income_Frequency == "Annually / per year" ~ INCOME_CORRECT_2_other,
    Income_Frequency == "Monthly" ~ INCOME_CORRECT_3_other * 12,
    Income_Frequency == "Weekly" ~ INCOME_CORRECT_4_other * working_weeks,
    Income_Frequency == "Hourly" ~ INCOME_CORRECT_5_other * Hours_Per_Week * working_weeks, # Use Hours_Per_Week variable here
    TRUE ~ NA  # Handle cases where Income Correct is not recognized
  )
  )

summary(raw_data$Corrected_Self_Report_Annual_Salary)

# weekly income
raw_data <- raw_data %>%
  mutate(Corrected_Self_Report_Weekly_Salary = case_when(
    Income_Frequency == "Annually / per year" ~ INCOME_CORRECT_2_other / working_weeks,
    Income_Frequency == "Monthly" ~ (INCOME_CORRECT_3_other * 12) / working_weeks,
    Income_Frequency == "Weekly" ~ INCOME_CORRECT_4_other,
    Income_Frequency == "Hourly" ~ INCOME_CORRECT_5_other * Hours_Per_Week, # Use Hours_Per_Week variable here
    TRUE ~ NA  # Handle cases where Income Correct is not recognized
  ))

#merge corrected and self reported income and self report income so that corrected has precedents 
## but where it doesn't exist we use self report
raw_data <- raw_data %>%
  mutate(
    Self_Report_Annual_Salary = coalesce(Corrected_Self_Report_Annual_Salary, Self_Report_Annual_Salary),
    Self_Report_weekly_Salary = coalesce(Corrected_Self_Report_Weekly_Salary, Self_Report_Weekly_Salary)
    )

summary(raw_data$Self_Report_Annual_Salary)
summary(raw_data$Self_Report_Weekly_Salary)

##################################
#### combine closed with open ####
##################################

convert_to_numeric <- function(data) {
  # Select the closed income columns. We only want the labelled ones.
  matching_columns <- grep("CLOSED", colnames(data), ignore.case = FALSE)
  matching_column_names <- colnames(data)[matching_columns]
  
  print(paste("Matching columns:", paste(matching_column_names, collapse = ", ")))  # Debug: Check matching columns
  
  # Iterate through the closed variables
  for (column in matching_column_names) {
    print(paste("Processing column:", column))  # Debug: Indicate which column is being processed
    
    # Attempt to split the column values
    temp_df <- data %>%
      tidyr::separate_wider_delim(column, names = c("min_income", "max_income"), 
                                  delim = " up to £", too_few = "align_start")
    
    print(head(temp_df))  # Debug: Check the result after splitting
    
    # Ensure 'min_income' and 'max_income' exist
    if (!"min_income" %in% colnames(temp_df)) temp_df$min_income <- NA
    if (!"max_income" %in% colnames(temp_df)) temp_df$max_income <- NA
    
    # Clean and convert the extracted values
    temp_df <- temp_df %>%
      mutate(
        min_income = ifelse(
          grepl("\\d", min_income), 
          gsub(",", "", str_extract(min_income, "\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?")), 
          NA
        ),
        max_income = ifelse(
          grepl("\\d", max_income), 
          gsub(",", "", str_extract(max_income, "\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?")), 
          NA
        )
      ) %>%
      mutate(
        numeric_min_income = as.numeric(min_income),
        numeric_max_income = as.numeric(max_income)
      )
    
    # Debug: Check numeric conversion
    print(head(temp_df %>% select(numeric_min_income, numeric_max_income)))
    
    # Handle midpoints, ensuring no non-numeric values cause errors
    temp_df <- temp_df %>%
      mutate(midpoint = case_when(
        !is.na(numeric_min_income) & !is.na(numeric_max_income) ~ (numeric_min_income + numeric_max_income) / 2,
        !is.na(numeric_min_income) ~ numeric_min_income,
        TRUE ~ NA_real_
      ))
    
    print(head(temp_df))  # Debug: Check midpoint calculations
    
    # Add the new variable into the data
    data <- data %>%
      mutate(midpoint = temp_df$midpoint, .after = column)
    
    # Rename the added column sensibly
    colnames(data)[colnames(data) == "midpoint"] <- paste0(column, "_midpoint")
  }
  
  return(data)
}

raw_data <- convert_to_numeric(raw_data) 


raw_data <- raw_data %>%
  mutate(
    # make all annual incomes. Note this assumes 52 working weeks!
    income_annual_closed = case_when(Income_Frequency == "Annually / per year" ~ INCOME_CLOSED_ANNUAL_midpoint,
                                     Income_Frequency == "Monthly" ~ INCOME_CLOSED_MONTHLY_midpoint*12,
                                     Income_Frequency == "Weekly" ~ INCOME_CLOSED_WEEKLY_midpoint * working_weeks,
                                     Income_Frequency == "Hourly" ~ INCOME_CLOSED_HOURLY_midpoint * Hours_Per_Week * working_weeks,
                                     TRUE ~ NA),
    income_weekly_closed = case_when(Income_Frequency == "Annually / per year" ~ INCOME_CLOSED_ANNUAL_midpoint / working_weeks,
                                     Income_Frequency == "Monthly" ~ (INCOME_CLOSED_MONTHLY_midpoint*12) / working_weeks,
                                     Income_Frequency == "Weekly" ~ INCOME_CLOSED_WEEKLY_midpoint,
                                     Income_Frequency == "Hourly" ~ INCOME_CLOSED_HOURLY_midpoint * Hours_Per_Week,
                                     TRUE ~ NA)
  )

# merge open with closed
raw_data <- raw_data %>%
  mutate(
    Annual_Income = case_when(!is.na(Self_Report_Annual_Salary) ~ Self_Report_Annual_Salary,
                              !is.na(income_annual_closed) ~ income_annual_closed,
                              TRUE ~ NA),
    Weekly_Income = case_when(!is.na(Self_Report_Weekly_Salary) ~ Self_Report_Weekly_Salary,
                              !is.na(income_weekly_closed) ~ income_weekly_closed,
                              TRUE ~ NA)
  )


#remove corrected income vars and other income vars
raw_data <- raw_data %>%
  select(-c(INCOME_CORRECT_2_other, INCOME_CORRECT_3_other, INCOME_CORRECT_4_other, INCOME_CORRECT_5_other, Corrected_Self_Report_Annual_Salary))

raw_data <- raw_data %>%
  select(-c(INCOME_OPEN_1, INCOME_OPEN_2, INCOME_CORRECT_1,
            INCOME_CORRECT_2, INCOME_CORRECT_3, INCOME_CORRECT_4, INCOME_CORRECT_5,
            Income_Frequency,
            income_annual_closed))

#############
#### END ####
#############

################################
#### remove income outliers ####
################################

library(Hmisc)
x <- raw_data

# calculate the mean and sd, and 3x sd
mean = Hmisc::wtd.mean(x$Weekly_Income, weights=x$Outsourced, na.rm=T) # NOTE on assumption that 'Outsourced' is the weighting variable
std = sqrt(Hmisc::wtd.var(x$Weekly_Income,weights=x$Outsourced,na.rm = T))
Tmin = mean-(3*std)
Tmax = mean+(3*std)

# identify the first set of outliers
outliers <- which(x$Weekly_Income < Tmin | x$Weekly_Income > Tmax)
outlier_count <- length(outliers) # count how many removed this iteration
all_outlier_ids <- x$ID[outliers] # list of ids removed (to be removed from data later)

# initialise some variables, i.e. iteration 0
count <- 0 # iteration counter
total_cases_removed <- 0 # total cases removed

cat("Iteration ", count, ": ", outlier_count, " outliers\n")

# Take a look at the distribution
par(mfrow = c(1, 3)) # this lets us plot three plots in a row
hist(x$Weekly_Income, main = "Histogram") 
# mark the mean and sds for the histogram
abline(v = mean, col='red', lwd = 3)
abline(v = Tmin, col='blue', lwd = 3)
abline(v = Tmax, col='blue', lwd = 3)
boxplot(x$Weekly_Income, main = "Boxplot")
qqnorm(x$Weekly_Income, main = "Normal Q-Q plot")
mtext(paste0("Iteration ", count, ": ", outlier_count, " outlier(s)"), side = 1, line = -2, outer = TRUE)

# Remove outliers
# While there are still outliers detected, remove the outliers and recalculate 
# mean, sd, and 3*sd and remove the outliers based on these new figures.
while(length(outliers) != 0){
  count <- count + 1
  x <- x[-outliers,] # remove the outliers identified in the previous iteration
  
  # recalculate
  mean = Hmisc::wtd.mean(x$Weekly_Income, weights=x$Outsourced, na.rm=T)
  std = sqrt(Hmisc::wtd.var(x$Weekly_Income,weights=x$Outsourced,na.rm = T))
  Tmin = mean - (3 * std)
  Tmax = mean + (3 * std)
  outliers <- which(x$Weekly_Income < Tmin | x$Weekly_Income > Tmax)
  outlier_ids <- x$ID[outliers] # get outlier ids
  all_outlier_ids <- append(all_outlier_ids, outlier_ids) # add removed outliers to outlier list
  total_cases_removed <- total_cases_removed + outlier_count # count total
  
  outlier_count <- length(outliers) # count how many removed this iteration
  cat("Iteration ", count, ": ", outlier_count, " outliers\n")
  
  # Replot distributions to see how they've changed
  par(mfrow = c(1, 3))
  hist(x$Weekly_Income, main = "Histogram") 
  abline(v = mean, col='red', lwd = 3)
  abline(v = Tmin, col='blue', lwd = 3)
  abline(v = Tmax, col='blue', lwd = 3)
  boxplot(x$Weekly_Income, main = "Boxplot")
  qqnorm(x$Weekly_Income, main = "Normal Q-Q plot")
  mtext(paste0("Iteration ", count, ": ", outlier_count, " outlier(s)"), side = 1, line = -2, outer = TRUE)
}

# Drop the cases identified as outliers from data
raw_data <- raw_data %>%
  mutate(
    income_drop = ifelse(ID %in% all_outlier_ids, 1, 0 )
  )

# Check this looks right
sum(raw_data$income_drop, na.rm = T) # this should be equal to all_outlier_ids

# test that income_drop does what it should
# This should look the same as the last iteration above#
x <- raw_data %>%
  filter(income_drop == 0)

# calculate the mean and sd, and 3x sd
mean = Hmisc::wtd.mean(x$Weekly_Income, weights=x$Outsourced, na.rm=T) # NOTE on assumption that 'Outsourced' is the weighting variable
std = sqrt(Hmisc::wtd.var(x$Weekly_Income,weights=x$Outsourced,na.rm = T))
Tmin = mean-(3*std)
Tmax = mean+(3*std)

# Take a look at the distribution
par(mfrow = c(1, 3)) # this lets us plot three plots in a row
hist(x$Weekly_Income, main = "Histogram") 
# mark the mean and sds for the histogram
abline(v = mean, col='red', lwd = 3)
abline(v = Tmin, col='blue', lwd = 3)
abline(v = Tmax, col='blue', lwd = 3)
boxplot(x$Weekly_Income, main = "Boxplot")
qqnorm(x$Weekly_Income, main = "Normal Q-Q plot")
mtext(paste0("Iteration ", count, ": ", outlier_count, " outlier(s)"), side = 1, line = -2, outer = TRUE)

cat("Total removed: ", total_cases_removed, "(", round(100*(total_cases_removed/nrow(x)),2),"%)")

# discounting these cases removes 183 respondents = 11.22%

#############
#### END ####
#############

#####################################################
#### Work out threshold on which to split income ####
#####################################################

# In Nat Rep we use four-fifths of the ASHE median. Here we check how the ASHE data
# compare with this data to see if that is still an appropriate threshold
# TLDR it is still appropriate; the 4/5 threshold is less than the sample median,
# which itself is closer to the 4/5 treshold than it is to the ASHE median 

#### ! Update 25/09/2024 ! #####
#### We have moved to using an alternative threshold based on
#### 4/5 the regional medians. See next section.

# get ashe data
# can be downloaded from https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/allemployeesashetable1/2023provisional/ashetable12023provisional.zip
# then extract to 'data' folder
# better option is to write the code to download it - add here when there's time
# ashe_data <- readxl::read_xls("./data/ashetable12023provisional/PROV - Total Table 1.7a   Annual pay - Gross 2023.xls",
#                               sheet = "All", skip = 4, n_max = 1) %>%
#   select(Median, "10":"90") %>%
#   rename(
#     `50` = Median # rename median to 50th percentile
#   ) %>%
#   pivot_longer(everything(), names_to = "Decile", values_to = "Value") %>%
#   mutate(
#     Decile = as.numeric(Decile)
#   ) %>%
#   arrange(Decile)
# 
# # make a smaller table with just 25, 50 and 75 percentiles - for plotting
# ashe_data_2 <- ashe_data %>%
#   subset(Decile %in% c(25, 50, 75)) %>%
#   mutate(
#     Label = case_when(Decile == 25 ~ "ASHE 25th percentile",
#                       Decile == 50 ~ "ASHE median",
#                       Decile == 75 ~ "ASHE 75th percentile",
#                       TRUE ~ NA)
#   )
# 
# # make tibble of possible pay thresholds
# low_pay_thresholds <- tibble("Threshold" = c("Two thirds median", 
#                                              "Four fifths median"),
#                              "Value" = c(29669 * (2/3), 29669 * 0.8)
# )
# 
# # visualise ashe data with our income data
# 
# income_statistics <- raw_data %>%
#   filter(income_drop == 0 & !is.na(Annual_Income)) %>%
#   summarise(
#     mean = weighted.mean(Annual_Income, w = Outsourced, na.rm = T),
#     median = wtd.quantile(Annual_Income, w = Outsourced, probs = c(.5), na.rm = T),
#     min = wtd.quantile(Annual_Income, w = Outsourced, probs = c(0), na.rm = T),
#     max = wtd.quantile(Annual_Income, w = Outsourced, probs = c(1), na.rm = T),
#     stdev = sqrt(wtd.var(Annual_Income, w = Outsourced, na.rm = T)),
#     n = n()
#   )
# 
# 
# par(mar = c(2, 2, 2, 2))
# # plot the distribution of income for the two groups
# raw_data %>%
#   filter(income_drop == 0 & !is.na(Annual_Income)) %>%
#   ggplot(., aes(x="",y=Annual_Income)) + 
#   geom_violin() +
#   geom_boxplot(width = 0.3) +
#   geom_text(inherit.aes=F, data=income_statistics, aes(x="",y = 6e+04), label=paste0("Mean = ", round(income_statistics$mean,0),"\n", "Median = ", income_statistics$median), nudge_x = 0.1, hjust=0) +
#   coord_cartesian(xlim=c(1,2.5)) +
#   theme_minimal() +
#   xlab("Outsourcing status") + ylab("Annual income") +
#   coord_cartesian(ylim = c(plyr::round_any(min(income_statistics$min), 5000, f = floor),plyr::round_any(max(income_statistics$max),5000, f = ceiling))) +
#   scale_y_continuous(breaks = seq(plyr::round_any(min(income_statistics$min), 5000, f = ceiling), plyr::round_any(max(income_statistics$max),5000, f = ceiling), 10000)) +
#   geom_hline(data = ashe_data_2, aes(yintercept = Value)) +
#   geom_text(inherit.aes = F, data = ashe_data_2, aes(x = 1.5, y = Value,label = Label), nudge_y = -2000) +
#   geom_hline(data = low_pay_thresholds, aes(yintercept = Value, colour = Threshold)) +
#   theme(plot.margin = unit(c(0,4,0,4), "inches"))
# 
# # the threshold looks right (at least it's the same picture painted in the Nat Rep)
# # so we proceed with 4/5 treshold
# 
# # create the grouping variable
# raw_data <- raw_data %>%
#   mutate(
#     Income_Group = case_when(Annual_Income < low_pay_thresholds$Value[which(low_pay_thresholds$Threshold == "Four fifths median")]  ~ "Low",
#                              Annual_Income >= low_pay_thresholds$Value[which(low_pay_thresholds$Threshold == "Four fifths median")] ~ "Not low",
#                              TRUE ~ NA),
#     Income_Group = factor(Income_Group, levels = c("Not low","Low"), exclude=NA)
#   ) %>%
#   ungroup()
# 
# # visualise it to check
# raw_data %>%
#   filter(income_drop == 0 & !is.na(Annual_Income)) %>%
#   dplyr::group_by(Income_Group) %>%
#   dplyr::summarise(
#     n = sum(Outsourced)
#   ) %>%
#   mutate(
#     perc = 100 * (n / sum(n))
#   ) %>%
#   ggplot(aes("", perc, fill=Income_Group)) +
#   geom_col() +
#   theme_minimal() +
#   theme(plot.margin = unit(c(0,4,0,4), "inches")) +
#   geom_label(aes(label=paste0(round(perc,2),"%")))

#############
#### END ####
#############

#####################################################
#### Defining Low Income - regional medians ####
#####################################################
# get the ashe data
# from https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/earningsandhoursworkedukregionbyagegroup
url <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/earningsandhoursworkedukregionbyagegroup/2023provisional/ashetableregionbyage2023provisional.zip'

filename <- basename(url) # this takes the filename from the url
filepath <- paste0("../Data/", filename)
output_dir <- substr(filepath,start = 1, stop=nchar(filepath) - 4)

# check if the file exists. if it doesn't, download and unzip
if(!file.exists(filepath)){ 
  cat("Downloading data\n")
  download.file(url, destfile = filepath, mode = "wb")
  unzip(filepath,exdir=output_dir)
} else{
  cat("Data already in directory. Loading it.\n")
}

# Specify the directory
# Use list.files to get all CSV files in the directory
files <- list.files(path = output_dir, pattern = '* Weekly pay - Gross 2023.xls$', full.names = TRUE)

ashe_data <- readxl::read_excel(files[1], sheet = 'All', skip = 4) %>%
  filter(!is.na(Code)) %>%
  #select(-c(last_col(offset=2):last_col(), contains('change'))) %>% # if we want some other variables
  janitor::clean_names() %>%
  rename(
    #jobs_thousands = thousand,
    Region = description,
    Region_median_income = median
  ) %>%
  select(Region, Region_median_income) %>% # if we just want the median
  # rename problematic regions
  mutate(
    Region = case_when(Region == "East" ~ "East of England",
                       Region == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
                       TRUE ~ Region),
    Region_median_income = as.numeric(Region_median_income)
  )

# join to data
raw_data <- raw_data %>%
  left_join(., ashe_data, by = "Region")

# calcualte 4/5 thresholds
raw_data <- raw_data %>%
  mutate(
    Region_four_fifths =  Region_median_income * .8,
    Region_two_thirds = Region_median_income * (2/3),
    UK_median = ashe_data %>% filter(Region == "United Kingdom") %>% pull(Region_median_income),
    UK_four_fifths = UK_median * 0.8,
    Region_four_thirds = Region_median_income * (4/3)
  )


# stats by region
income_statistics_regional <- raw_data %>%
  filter(income_drop == 0 & !is.na(Weekly_Income)) %>%
  group_by(Region) %>%
  summarise(
    mean = weighted.mean(Weekly_Income, w = Outsourced, na.rm = T),
    median = wtd.quantile(Weekly_Income, w = Outsourced, probs = c(.5), na.rm = T),
    min = wtd.quantile(Weekly_Income, w = Outsourced, probs = c(0), na.rm = T),
    max = wtd.quantile(Weekly_Income, w = Outsourced, probs = c(1), na.rm = T),
    stdev = sqrt(wtd.var(Weekly_Income, w = Outsourced, na.rm = T)),
    n = n()
  )

# let's take a look at two-thrids too 
# here blue lines are 2/3, red lines are 4/5
raw_data %>%
  filter(income_drop == 0 & !is.na(Weekly_Income)) %>%
  ggplot(., aes(x="",y=Weekly_Income)) + 
  facet_wrap(~Region) + 
  geom_violin() +
  geom_boxplot(width = 0.3) +
  geom_hline(aes(yintercept = Region_four_fifths), colour = "red") +
  geom_hline(aes(yintercept = Region_two_thirds), colour = "blue") +
  theme_minimal()

#use 2/3 becuase that's what OECD use and what JRF have used in past
# https://data.oecd.org/earnwage/wage-levels.htm

# create the grouping variable based on Region_two_thirds
# low pay is less than or equal to 2/3 regional median earnigns
raw_data <- raw_data %>%
  mutate(
    income_group = case_when(Weekly_Income <= Region_two_thirds  ~ "Low",
                             Weekly_Income <= Region_four_thirds ~ "Mid",
                             Weekly_Income > Region_four_thirds ~ "High",
                                 TRUE ~ NA),
    income_group = factor(income_group, levels = c("Mid", "Low","High"), exclude=NA)
  ) %>%
  ungroup()


# visualise it to check
# 26.35% of people are low paid
raw_data %>%
  filter(income_drop == 0 & !is.na(Weekly_Income)) %>%
  dplyr::group_by(income_group) %>%
  dplyr::summarise(
    n = sum(Outsourced)
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(aes("", perc, fill=income_group)) +
  geom_col() +
  theme_minimal() +
  #theme(plot.margin = unit(c(0,4,0,4), "inches")) +
  geom_label(aes(label=paste0(round(perc,2),"%")))

#############
#### END ####
#############

##change job satisfaction to completely numeric 1 bad 7 good, change don't know to NA
raw_data <- raw_data %>%
  dplyr::mutate(Job_Satisfaction = case_when(
    Job_Satisfaction == "1 – completely dissatisfied" ~ 1,
    Job_Satisfaction == "2 – mostly dissatisfied" ~ 2,
    Job_Satisfaction == "3 – somewhat dissatisfied" ~ 3,
    Job_Satisfaction == "4 – neither satisfied nor dissatisfied" ~ 4,
    Job_Satisfaction == "5 – somewhat satisfied" ~ 5,
    Job_Satisfaction == "6 – mostly satisfied" ~ 6,
    Job_Satisfaction == "7 – completely satisfied" ~ 7,
    Job_Satisfaction == "Don't know" ~ NA
  ))

##create a Ethniicty collapsed var
raw_data <- raw_data %>%
  dplyr::mutate(
    Ethnicity_Collapsed = case_when(
      # White British
      Ethnicity %in% c("English / Welsh / Scottish / Northern Irish / British") ~ "White British",
      # White Other
      Ethnicity %in% c("Irish", 
                       "Gypsy or Irish Traveller", 
                       "Any other White background",
                       "Roma") ~ "White Other",
      # Grouping Asian ethnicities
      Ethnicity %in% c("Indian", 
                       "Pakistani", 
                       "Bangladeshi",
                       "Any other Asian background",
                       "Chinese") ~ "Asian/Asian British",
      # Grouping Black ethnicities
      Ethnicity %in% c("African", 
                       "Caribbean", 
                       "Any other Black, Black British, or Caribbean background") ~ "Black/African/Caribbean/Black British",
      # Grouping Mixed ethnicities
      Ethnicity %in% c("White and Black Caribbean", 
                       "White and Black African",
                       "White and Asian",
                       "Any other Mixed / Multiple ethnic background") ~ "Mixed/Multiple ethnic groups",
      # Grouping Other ethnicities
      Ethnicity %in% c("Arab") ~ "Arab",
      # Handling missing or ambiguous categories
      Ethnicity %in% c("Any other ethnic group")  ~ "Other ethnic group",
      #prefer not to say
      Ethnicity %in% c("Prefer not to say") ~ "Prefer not to say",
      # Dont think of myself as...
      Ethnicity %in% c("Don't think of myself as any of these") ~ "Don't think of myself as any of these",
      # Default case for any unmatched entries
      TRUE ~ NA
    )
  )

#make white the reference category
raw_data$Ethnicity_Collapsed <- relevel(factor(raw_data$Ethnicity_Collapsed), ref = "White British")

raw_data <- raw_data %>%
  mutate(
    Has_Degree = factor(Has_Degree, levels = c("Yes", "No", "Don't know"))
  )

# make binary born uk var
categories <- as.vector(unique(raw_data$BORNUK))
non_categories <- categories[!(categories %in% c("I was born in the UK","Prefer not to say"))]

# Will throw NA warning. I think this OK but investigate how to avoid the problem
raw_data <- raw_data %>%
  mutate(
    BORNUK_binary = forcats::fct_collapse(BORNUK,
                                          "Born in UK" = "I was born in the UK",
                                          "Not born in UK" = non_categories,
                                          "Prefer not to say" = "Prefer not to say")
  ) 

# make binary ethnicity var
# but keep don't think of myself and prefer not to say as separate
ethnicities <- as.vector(unique(raw_data$Ethnicity_Collapsed))
white_ethnicities <- ethnicities[ethnicities %in% c("White British", "White other")]
non_categories <- c("Don't think of myself as any of these","Prefer not to say")
non_white_ethnicities <- ethnicities[!(ethnicities %in% white_ethnicities) & !(ethnicities %in% non_categories)]

# Will throw NA warning. I think this OK but investigate how to avoid the problem
raw_data <- raw_data %>%
  mutate(
    Ethnicity_binary = forcats::fct_collapse(Ethnicity_Collapsed,
                                             "White" = c("White British","White other"),
                                             "Non-White" = non_white_ethnicities,
                                             "Don't think of myself as any of these" =  "Don't think of myself as any of these",
                                             "Prefer not to say" = "Prefer not to say")
  )

raw_data <- raw_data %>%
  mutate(
    Gender = factor(Sex, levels = c("Male","Female","Other","Prefer not to say"))
  )

## change complaints to completely numeric 1-5, change don't know to NA
raw_data <- raw_data %>%
  dplyr::mutate(Complaints_numeric = case_when(
    Complaints == "Strongly disagree" ~ 1,
    Complaints == "Somewhat disagree" ~ 2,
    Complaints == "Neither agree nor disagree" ~ 3,
    Complaints == "Somewhat agree" ~ 4,
    Complaints == "Strongly agree" ~ 5,
    Complaints == "Not sure" ~ NA
  ))

# change treated well at work overall to completely numeric 1-5, change don't know to NA
raw_data <- raw_data %>%
  dplyr::mutate(Treatment_At_Work_Overall_numeric = case_when(
    Treatment_At_Work_Overall == "Always disrespected or treated poorly" ~ 1,
    Treatment_At_Work_Overall == "Usually disrespected or treated poorly" ~ 2,
    Treatment_At_Work_Overall == "Sometimes well, sometimes poorly" ~ 3,
    Treatment_At_Work_Overall == "Usually respected and treated well" ~ 4,
    Treatment_At_Work_Overall == "Always respected and treated well" ~ 5,
    Treatment_At_Work_Overall == "Don't know" ~ NA
  ))

# change all clarity questions to completely numeric 1-5, change don't know to NA

raw_data <- raw_data %>%
  dplyr::mutate(across(
    starts_with("Clarity"), 
    ~ case_when(
      . == "Strongly disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Strongly agree" ~ 5,
      . == "Not Sure" ~ NA
    ), 
    .names = "{.col}_numeric"  # Creates new variables with a "_numeric" suffix
  ))


# Perform Cronbach's alpha on the new Clarity numeric questions
library(psych)

# Select only the new Clarity numeric variables
clarity <- raw_data %>%
  dplyr::select(matches("^Clarity.*_numeric$")) # Selects columns that start with "Clarity" and end with "_numeric"

# Check Cronbach's alpha
## v. good 0.93
psych::alpha(clarity, check.keys = TRUE)

# Create the mean of the new Clarity numeric questions
raw_data <- raw_data %>%
  mutate(Clarity_Overall_Mean = rowMeans(select(., matches("^Clarity.*_numeric$")), na.rm = TRUE))


#fix age var
raw_data <- raw_data %>%
  # Trim whitespace from the Age column
  mutate(Age = trimws(Age)) %>%
  # Remove rows where Age is "Under 16" or "Over 80"
  filter(!Age %in% c("Under 16", "Over 80")) %>%
  # Convert the remaining Age values to numeric 
  mutate(Age = as.numeric(Age))

##drop Is an employee var since 100% reported as yes
raw_data <- raw_data %>%
  select(-Is_an_employee)

## drop short long employ var since 100% reported long-term
raw_data <- raw_data %>%
  select(-Short_Long_Employ)

##drop E6_5 due to extremely low response rate
raw_data <- raw_data %>%
  select(-E6_5)

#change column types
raw_data <- raw_data %>%
  mutate(
    Age = as.numeric(Age), #make sure that top and bottom make sense
    Job_Satisfaction = as.numeric(Job_Satisfaction),
    Treatment_At_Work_Overall_numeric = as.numeric(Treatment_At_Work_Overall_numeric),
    Complaints_numeric = as.numeric(Complaints_numeric),
    Clarity_Overall_Mean = as.numeric(Clarity_Overall_Mean),
    Ethnicity_Collapsed = as_factor(Ethnicity_Collapsed))

##drop final unneeded vars
raw_data <- raw_data %>%
  select(-c(StartDate,
            Short_Long_Employ_TEXT,
            Type_Of_Org_TEXT,
            Temp_Explanation_TEXT,
            INCOME_CLOSED_ANNUAL,                
            INCOME_CLOSED_ANNUAL_midpoint,      
            INCOME_CLOSED_MONTHLY,              
            INCOME_CLOSED_MONTHLY_midpoint,     
            INCOME_CLOSED_WEEKLY,               
            INCOME_CLOSED_WEEKLY_midpoint,       
            INCOME_CLOSED_HOURLY,                
            INCOME_CLOSED_HOURLY_midpoint))

#final skim
skim(raw_data)


#####SUBSET TO ONLY VARS OF INTERESTED####

#create subset of data
raw_data<- raw_data %>%
  select(Sex, Age, Ethnicity, Ethnicity_Collapsed, Ethnicity_binary,
         Region, Type_Of_Org, Specific_Type_Of_Org,
         Education_Band, BORNUK, BORNUK_binary, Annual_Income, 
         Weekly_Income,
         income_drop, income_group,OutsourcedNonOL,
         TradeUnion, 
         TUCOV, 
         Occupation_Catergory,
         Organisation_Catergory, 
         Guaranteed_Hours, 
         Notice_Of_Working_Hours,
         Notice_Of_Cancelled_Shifts, 
         Cancelled_Shift_Pay, 
         Sick_Pay,
         RightsViolations_Paid_On_Time, 
         RightsViolations_Paid_Correct_Amount,
         RightsViolations_Leave_Entitlement,
         RightsViolations_Holiday_Pay,
         RightsViolations_Sick_Pay,
         RightsViolations_Pay_Slip,
         RightsViolations_Health_Safety,
         RightsViolations_None,
         RightsViolations_TEXT,
         Job_Satisfaction,
         Pay_Comparison,
         Training_Induction,
         Treatment_At_Work_InHouse,
         Treatment_At_Work_Managers,
         Treatment_At_Work_Overall,
         Inhouse_Discrimination_Age,      
         Inhouse_Discrimination_Disability,
         Inhouse_Discrimination_Nationality,
         Inhouse_Discrimination_Ethnicity,
         Inhouse_Discrimination_Sex,
         Client_Discrimination_Age,
         Client_Discrimination_Disability,
         Client_Discrimination_Nationality, 
         Client_Discrimination_Ethnicity,
         Client_Discrimination_Sex,
         Clarity1,
         Clarity2,
         Clarity3,
         Clarity4,
         Clarity5,
         Clarity6,
         Clarity7,
         Clarity8,
         Clarity9,
         Clarity10,
         Clarity11,
         Clarity_Overall_Mean,
         Why_Job_Like,
         Why_Job_Convinient,
         Why_Job_Flexibility,
         Why_Job_Pay,
         Why_Job_Collegues,
         Why_Job_Culture, 
         Why_Job_Progress,
         Why_Job_BestAvailable,
         Why_Job_NotQualified,
         Why_Job_Health,
         Why_Job_Carer,
         Pros_And_Cons_Flexibility,
         Pros_And_Cons_Pay, 
         Pros_And_Cons_Hours,
         Pros_And_Cons_Holiday,
         Pros_And_Cons_Terms,
         Pros_And_Cons_Promotion,
         Pros_And_Cons_Training,
         Pros_And_Cons_Security,
         Pros_And_Cons_Treatment,
         Pros_And_Cons_Specialisation,
         Pros_And_Cons_Connection,
         Pros_And_Cons_FeelInvested,
         Pros_And_Cons_Rights,
         Pros_And_Cons_HealthSafety,
         Work_Preference,
         What_Improvements_Pay,
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
         What_Improvements_Union,
         What_Improvements_Other,
         What_Improvements_None,
         What_Improvements_Other_TEXT,
         Complaints,
         Outsourced
  )

#####output clean data and save####
save_check <- readline("Do you want to save the data? (y/n): ")
  
if(save_check == "y"){
  
  # create a check so we don't overwrite if we don't want to
  out_file <- paste0("./Experiential/data/", Sys.Date() ," - clean_data_jrf_experiential.csv")
  rds_out <- paste0("./Experiential/data/", Sys.Date() ," - clean_data_jrf_experiential.rds")
    
  if(!file.exists(out_file)){ 
    cat(paste0("Saving data as: ", out_file, "\n"))
    write_csv(raw_data, out_file)
    saveRDS(raw_data, file = rds_out)
  } else{
    # prompt function to confirm overwrite
    fun <- function(){
      check <- readline("This file already exists, overwrite? (y/n)")
      
      if(check=="y"){
        write_csv(raw_data, out_file)
        saveRDS(raw_data, file = rds_out)
        return("File saved")
      }
      else{
        return("File not saved")
      }
    }
    fun()
  }
} else{
  print("File not saved")
}



