# Outsourced workers in the UK: Joseph Rowntree Foundation
# Nationally representative analysis


[Click here for the data cleaning script](https://justknowledge-uk.github.io/jrf_nat_rep/R/data_cleaning)

[Click here for the report template](https://justknowledge-uk.github.io/jrf_nat_rep/R/report_template)

[Click here for the most recent analysis](https://justknowledge-uk.github.io/jrf_nat_rep/R/key_findings_reduced)

The focus of this project is to co-develop a piece of research on the low-paid outsourced workforce in the UK, with a particular focus on the experiences of Black workers.

The quantitative aspect of this work involves three surveys:

1. A short ‘test’ survey of 2,000 respondents, the aim of which is to help refine questions on outsourcing
2. A short survey of 10,000 workers, the aim of which is to estimate the number of outsourced workers and the demographics of the outsourced workforce
3. A longer survey of 2,000 outsourced workers, the aim of which is to understand more about their experiences of work

This project is an analysis of survey #2 conducted on 10000 respondents in December 2023.

## Pre-processing variable translation

The table below shows how old variable names have been mapped onto new variable names

| New variable                   | Old variable   |
|--------------------------------|----------------|
| ID                             | MIProRspId     |
| Sex                            | D1_Gender_C    |
| Age                            | D2_Age         |
| Region                         | D4_Region_C    |
| Employment_Status              | D3_employment  |
| Is_an_employee                 | E2             |
| Consent_1_Ethnicity            | SCD_Opt_In_1   |
| Consent_2_TU                   | SCD_Opt_In_2   |
| Consent_3_Health               | SCD_Opt_In_3   |
| Has_Degree                     | D6_educ1       |
| Has_Second_Job                 | E1             |
| Who_Pays_You                   | E3             |
| Job_Security                   | E5             |
| Work_Circumstance_Agency       | E6_1           |
| Work_Circumstance_Contract     | E6_2           |
| Work_Circumstance_Seasonal     | E6_3           |
| Work_Circumstance_Trainee      | E6_4           |
| Work_Circumstance_Other        | E6_5           |
| Work_Circumstance_Other_TEXT   | E6_5_other     |
| Org_Size                       | E7A            |
| Is_Supervisor                  | E7B            |
| Job_Title_TEXT                 | E8A            |
| Main_Activity_TEXT             | E8B            |
| Employer_Activity_TEXT         | E9A            |
| Employer_Activity_SIC          | E9B            |
| Biz_Type                       | E10A           |
| Non_Private_Biz_Type           | E10B           |
| Non_Private_Biz_Type_TEXT      | E10B_9_other   |
| Paid_One_Work_Other            | Q1_1           |
| Third_Party                    | Q1_2           |
| Employer_Is_Agency             | Q1_3           |
| Instructed_By_Other            | Q1_4           |
| Work_In_Other                  | Q1_5           |
| Diff_Uniform                   | Q1_6           |
| Short_Long_Employ              | Q2             |
| Short_Long_Employ_TEXT         | Q2_3_other     |
| I_Am_Outsourced                | Q3v3a          |
| Outsourced_And_Agency          | Q3v3b          |
| Might_Be_Outsourced_And_Agency | Q3v3c          |
| Not_Outsourced_And_Agency      | Q3v3d          |
| Disability                     | D7_Disability1 |
| Disability_Impact              | D8_Disability2 |
