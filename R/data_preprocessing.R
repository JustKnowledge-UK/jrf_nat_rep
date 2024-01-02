
# Load libraries
library(haven)
library(tidyverse)
library(skimr)

# Read data
# omnibus data
library(haven)
data <- read_sav("Data/UK23626 Workers sample data with nat rep and graduates weight.sav")
View(data)

# Apply labels to variables
data <- data %>%
  as_factor()

# Drop unnecessary vars
# 14:26 = quals below degree, if we think this would add value we should include them
# 28:36 = age of children, doesnt seem necessary

data <- data[, -c(2,14:26,28,36)]

#change column names
data <- data %>%
  rename(
    ID = MIProRspId,
    Sex = D1_Gender_C,
    Age = D2_Age,
    Region = D4_Region,
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
    
    
  ) #TO DO

#####################HERE###############################






# Join and clean constituency columns
data <- unite(data, col='Constituency', c('Con_EM', 'Con_East', 'Con_Lon',
                                          'Con_NE', 'Con_NW', 'Con_Scot',
                                          'Con_Scot','Con_SE', 'Con_SW',
                                          'Con_Wal','Con_WM','Con_York' ))
vars <- c('Con_EM', 'Con_East', 'Con_Lon',
          'Con_NE', 'Con_NW', 'Con_Scot',
          'Con_Scot','Con_SE', 'Con_SW',
          'Con_Wal','Con_WM','Con_York' )

# none of the above vars are in the data
vars %in% colnames(data)

data$Constituency %<>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("\\bNA\\b", "") %>%
  str_replace_all("\\s", "")

###########################################################################
# Make inverted versions of Q3 responses so that 1 = TRUE and 0 = NOT TRUE
###########################################################################
invert_response <- function(x){
  x <- 2 + (-1*x)
}

inverted_q3 <- data %>%
  dplyr::select(MIProRspId, starts_with("Q3")) %>%
  mutate(across(starts_with("Q3"), as.numeric)) %>% # make numeric so inversion works
  mutate(across(starts_with("Q3"), invert_response)) %>%
  rename_with(.cols = starts_with("Q3"), ~ paste0(.x, "_inverted")) # add suffix to vars

# join inverted responses back to to data
data <- data %>%
  left_join(., inverted_q3, by = "MIProRspId")

#########################################################################
#########################################################################



vars <- c('MIProRspId',
          'D1_Gender_C',
          'D2_Age_C',
          'D4_Region_C',
          'pol_atten',
          'emp_quota',
          'D5_socialgrade',
          'ed_bands',
          'D8_Tenure',
          'NSSECQ1',
          'Q1',
          'Q2',
          'Q3_1',
          'Q3_2',
          'Q3_3',
          'Q3_4',
          'Q3_5',
          'Q3_6',
          'Q4_1',
          'Q4_2',
          'Q4_3',
          'Q4_4',
          'Q4_5',
          'Q4_6',
          'Q4_7',
          'Q4A_1',
          'Q4A_2',
          'Q4A_3',
          'Q4A_4',
          'Q4A_5',
          'Q4A_6',
          'Q5',
          'Q6',
          'Q7_1',
          'Q7_2',
          'Q7_3',
          'Q7_4',
          'Q7_5',
          'Q8_1',
          'Q8_2',
          'Q8_3',
          'Q8_4',
          'Q8_5')

# only 9% of the variables that we manipulated previously are in the data
sum(vars %in% colnames(data)) / length(vars)
# get the names of old columnms that are present in the new data
colnames(data[,which(vars %in% colnames(data))])

# Fix col types
data$ID <- as.factor(data$ID)
data$Age <- as.numeric(gsub("[^0-9]+", "", gsub("\\s", "", as.character(data$Age))))
data$Pol_Atten <- as.numeric(gsub("[^0-9]+", "", gsub("\\s", "", as.character(data$Pol_Atten))))

#### Check for missing data ####
# By row
missing_percentage_per_row <- rowMeans(is.na(data)) * 100

# Count the number of rows with more than 50% missing
rows_with_more_than_50_percent_missing <- sum(missing_percentage_per_row > 50)

# Calculate the percentage of rows with more than 50% missing
percentage_of_rows_with_more_than_50_percent_missing <- (rows_with_more_than_50_percent_missing / nrow(data)) * 100

# Display the results
cat("Number of rows with more than 50% missing:", rows_with_more_than_50_percent_missing, "\n")
cat("Percentage of rows with more than 50% missing:", percentage_of_rows_with_more_than_50_percent_missing, "%\n")

# By col
missing_percentage_per_column <- colMeans(is.na(data)) * 100

# Display the percentage of missing values for each column
print(missing_percentage_per_column) # 41.6% of almost all outsourcing q's are missing

# Subset to only complete outsourcing q's
ColumnName <- "Instructed_By_Other"
data_subset <- data[complete.cases(data[[ColumnName]]), ]

# Recheck cols
missing_percentage_per_column <- colMeans(is.na(data_subset)) * 100
print(missing_percentage_per_column)

#### save csv ####
write_sav(data, "./data/cleaned_full_data.sav")
write_sav(data_subset, "./data/cleaned_complete_case_data.sav")

# Remove everything apart from data_subset (ready for next)
rm(list = setdiff(ls(), "data_subset"))
