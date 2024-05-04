library(haven)
library(poLCA)
library(dplyr)
library(ggplot2)
library(tidyr)
library(skimr)

rm(list = ls())
# data <- read_sav("./Data/uncleaned_full_data.sav")
data <- read_sav("./Data/uncleaned_full_data.sav")



# invert response function for summing
invert_response <- function(x){
  x <- 2 + (-1*x)
}

# Now for just indicators
indicator_data <- data %>% 
  select(ID, starts_with("Q1")) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x)), .by = ID) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6)
  ) %>% 
  select(-starts_with("Q1"))

# Check ID the same
sum(data$ID == indicator_data$ID) == nrow(data)

# Merge
data <- left_join(data, indicator_data, by = "ID")

##################################
### Assigning people to groups ###
##################################

## These groups are mutually exclusive
data <- data %>%
  mutate(
    # SURE outsourced or MIGHT BE outsourced + LONGTERM
    outsourced = ifelse(Q3v3a == 1 | (Q3v3a == 2 & Q2 == 1), 1, 0),
    # NOT outsourced, SURE agency, and LONG-TERM
    likely_agency = ifelse(outsourced == 0 & Q2 == 1 & (Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1), 1, 0),
    likely_agency = ifelse(is.na(likely_agency), 0, likely_agency),
    # NOT outsourced, NOT likely agency, & 5 or more indicators
    high_indicators = ifelse(outsourced == 0 & likely_agency == 0 & sum_true >= 5, 1, 0),
    # NOT outsourced, NOT likely agency, 5 or more indicators, & LONGTERM
    high_indicators_LT = ifelse(outsourced == 0 & likely_agency == 0 & (Q2 == 1 & sum_true >= 5), 1, 0)
  )

# count the groupings
lapply(list(data$outsourced,
             data$likely_agency,
             data$high_indicators,
             data$high_indicators_LT), sum)

# Flatten these groupings into a single variable

data <- data %>%
  mutate(
    outsourcing_group = factor(case_when(outsourced == 1 ~ 'outsourced',
                                  likely_agency == 1 ~ 'likely_agency',
                                  high_indicators == 1 ~ 'high_indicators',
                                  TRUE ~ 'not_outsourced'), 
                               levels = c("not_outsourced",
                                          "outsourced",
                                          "likely_agency",
                                          "high_indicators")
    )
  )



######################################################
#### Agreement to each indicator based on grouping ###
######################################################

data %>% 
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(outsourcing_group, Q1, Q1_True_False) %>%
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
  ggplot(., aes(Q1, perc_within, fill = outsourcing_group)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9))



##################################
#### Sector breakdown by group ###
##################################

# add new industry data
# for now just combine the relevant variables
data_2 <- read_sav("./Data/JRF Outsourced Workers - Occupations and Sectors.sav")
# skim(data_2)

data_check <- data_2[,c("MIProRspId","unit_code","UnitOccupation","smg_code","MajorsubgroupOccupation",
                        "major_code","Majorgroupcode","SectorCode","SectorName")]

# data_check <- as_factor(data_check)

# lapply(data_check[,-1], unique)
# 
# apply(data_check$SectorName, as_factor)

data_combined <- left_join(data, data_check, by = c("ID" = "MIProRspId"))

data_combined %>%
  group_by(SectorName,outsourcing_group) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(SectorName, perc, fill = outsourcing_group)) +
  geom_col(position = "dodge") +
  theme(text = element_text(angle = 60)) +
  scale_x_continuous(breaks = seq(1,23, 1)) +
  scale_y_continuous(breaks = seq(0,100,10))
  
#geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc, 2),"%")), position = position_dodge(width = 0.9))

data_combined$SectorName <- as_factor(data_combined$SectorName)

levels(data_combined$SectorName)


### 2 Feb ###

# swap the axes around for sector breakdown

data_combined %>%
  group_by(outsourcing_group,SectorName) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(SectorName, perc, fill = outsourcing_group)) +
  geom_col(position = "dodge") +
  theme(text = element_text(angle = 60)) +
  scale_x_continuous(breaks = seq(1,23, 1)) +
  scale_y_continuous(breaks = seq(0,100,5))


levels(data_combined$SectorName)

###############################################################
## different definitions - do all again with new defs 2 Feb ###
###############################################################

rm(list = ls())
data <- read_sav("./Data/uncleaned_full_data.sav")

# invert response function for summing
invert_response <- function(x){
  x <- 2 + (-1*x)
}

# Now for just indicators
indicator_data <- data %>% 
  select(ID, starts_with("Q1")) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x)), .by = ID) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6)
  ) %>% 
  select(-starts_with("Q1"))

# Check ID the same
sum(data$ID == indicator_data$ID) == nrow(data)

# Merge
data <- left_join(data, indicator_data, by = "ID")

## These groups are mutually exclusive
data <- data %>%
  mutate(
    # SURE outsourced + LONGTERM or MIGHT BE outsourced + LONGTERM
    outsourced_LT = ifelse((Q3v3a == 1 & Q2 == 1) | (Q3v3a == 2 & Q2 == 1), 1, 0),
    # NOT outsourced, SURE agency, and LONG-TERM
    likely_agency = ifelse(outsourced_LT == 0 & Q2 == 1 & (Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1), 1, 0),
    likely_agency = ifelse(is.na(likely_agency), 0, likely_agency),
    # NOT outsourced, NOT likely agency, & 5 or more indicators
    high_indicators = ifelse(outsourced_LT == 0 & likely_agency == 0 & sum_true == 6, 1, 0),
    # NOT outsourced, NOT likely agency, 6 or more indicators, & LONGTERM
    high_indicators_LT = ifelse(outsourced_LT == 0 & likely_agency == 0 & (Q2 == 1 & sum_true == 6), 1, 0)
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


# add new industry data
# for now just combine the relevant variables
data_2 <- read_sav("./Data/JRF Outsourced Workers - Occupations and Sectors.sav")
# skim(data_2)

data_check <- data_2[,c("MIProRspId","unit_code","UnitOccupation","smg_code","MajorsubgroupOccupation",
                        "major_code","Majorgroupcode","SectorCode","SectorName")]

# data_check <- as_factor(data_check)

# lapply(data_check[,-1], unique)
# 
# apply(data_check$SectorName, as_factor)

data_combined <- left_join(data, data_check, by = c("ID" = "MIProRspId"))


### 2 Feb ###

# swap the axes around for sector breakdown

data_combined %>%
  group_by(outsourcing_group,SectorName) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(SectorName, perc, fill = outsourcing_group)) +
  geom_col(position = "dodge") +
  theme(text = element_text(angle = 60)) +
  scale_x_continuous(breaks = seq(1,23, 1)) +
  scale_y_continuous(breaks = seq(0,100,5))


levels(data_combined$SectorName)


### difference between outsrouced LT and non-LT
# first just hte ggroups

rm(list = ls())
data <- read_sav("./Data/uncleaned_full_data.sav")

## These groups are mutually exclusive
data <- data %>%
  mutate(
    outsourced = ifelse(Q3v3a == 1 , 1, 0),
    # SURE outsourced + LONGTERM or MIGHT BE outsourced + LONGTERM
    outsourced_LT = ifelse(Q3v3a == 1 & Q2 == 1, 1, 0)
  ) 

data <- data %>%
  mutate(
    outsourcing_group = factor(case_when(outsourced_LT == 1 ~ 'outsourced_LT',
                                         outsourced == 1 ~ 'outsourced',
                                         TRUE ~ 'other')
    )
  )

data %>%
  filter(outsourcing_group != "other") %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(outsourcing_group, Q1, Q1_True_False) %>%
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
  ggplot(., aes(Q1, perc_within, fill = outsourcing_group)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9))


# second by defintion

rm(list = ls())
data <- read_sav("./Data/uncleaned_full_data.sav")

## These groups are mutually exclusive
data <- data %>%
  mutate(
    # YES to outsourced plus LONGTERM
    outsourced = ifelse(Q3v3a == 1 & Q2 == 1, 1, 0),
    # SURE outsourced + LONGTERM or MIGHT BE outsourced + LONGTERM
    outsourced_LT = ifelse((Q3v3a == 1 & Q2 == 1) | (Q3v3a == 2 & Q2 == 1), 1, 0)
  ) 

data <- data %>%
  mutate(
    outsourcing_group = factor(case_when(outsourced_LT == 1 ~ 'outsourced_LT',
                                         outsourced == 1 ~ 'outsourced',
                                         TRUE ~ 'other')
    )
  )

# invert response function for summing
invert_response <- function(x){
  x <- 2 + (-1*x)
}

# Now for just indicators
indicator_data <- data %>% 
  select(ID, starts_with("Q1")) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x)), .by = ID) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6)
  ) %>% 
  select(-starts_with("Q1"))

# Check ID the same
sum(data$ID == indicator_data$ID) == nrow(data)

# Merge
data <- left_join(data, indicator_data, by = "ID")


data %>%
  filter(outsourcing_group != "other") %>%
  pivot_longer(starts_with("Q1"), names_to = "Q1", values_to = "Q1_True_False") %>%
  group_by(outsourcing_group, Q1, Q1_True_False) %>%
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
  ggplot(., aes(Q1, perc_within, fill = outsourcing_group)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9))

data %>% 
  filter(outsourcing_group == "outsourced_LT") %>%
  group_by(sum_true) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(sum_true, perc_within)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9)) +
  scale_x_continuous(breaks = seq(0,6,1))

data %>% 
  filter(outsourcing_group == "outsourced") %>%
  group_by(sum_true) %>%
  # select(starts_with("Q1")) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sum = sum(n),
    perc_overall = 100 * (n / nrow(data)),
    perc_within = 100 * (n / sum(n))
  ) %>%
  ggplot(., aes(sum_true, perc_within)) +
  geom_col(position = "dodge") +
  geom_label(inherit.aes = T, aes(label = paste0(n, "; ",round(perc_within, 2),"%")), position = position_dodge(width = 0.9)) +
  scale_x_continuous(breaks = seq(0,6,1))


#### 13 Feb groupings ####

# high indicators = 5 or 6 + LT

rm(list = ls())
data <- read_sav("./Data/uncleaned_full_data.sav")

# invert response function for summing
invert_response <- function(x){
  x <- 2 + (-1*x)
}

# Now for just indicators
indicator_data <- data %>% 
  select(ID, starts_with("Q1")) %>%
  reframe(across(starts_with("Q1"), ~invert_response(.x)), .by = ID) %>%
  rowwise() %>%
  mutate(
    sum_true = sum(Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6)
  ) %>% 
  select(-starts_with("Q1"))

# Check ID the same
sum(data$ID == indicator_data$ID) == nrow(data)

# Merge
data <- left_join(data, indicator_data, by = "ID")

## These groups are mutually exclusive
data <- data %>%
  mutate(
    # SURE outsourced + LONGTERM or MIGHT BE outsourced + LONGTERM
    outsourced_LT = ifelse((Q3v3a == 1 & Q2 == 1) | (Q3v3a == 2 & Q2 == 1), 1, 0),
    # NOT outsourced, SURE agency, and LONG-TERM
    likely_agency = ifelse(outsourced_LT == 0 & Q2 == 1 & (Q3v3b == 1 | Q3v3c == 1 | Q3v3d == 1), 1, 0),
    likely_agency = ifelse(is.na(likely_agency), 0, likely_agency),
    # NOT outsourced, NOT likely agency, 6 or more indicators, & LONGTERM
    high_indicators_LT = ifelse(outsourced_LT == 0 & likely_agency == 0 & (Q2 == 1 & sum_true == 5), 1, 0)
  )

# count the groupings
lapply(list(data$outsourced_LT,
            data$likely_agency,
            data$high_indicators_LT), sum)

percentages <- lapply(list(data$outsourced_LT,
            data$likely_agency,
            data$high_indicators_LT), function(x) 100 * sum(x) / nrow(data))


sum(percentages[[1]],percentages[[2]],percentages[[3]])

percentages
