# Outsourced workers in the UK: Joseph Rowntree Foundation [UPDATE WITH REPORT NAME]


[Click here for the data cleaning script](https://justknowledge-uk.github.io/jrf_nat_rep/R/data_cleaning)

[Click here for the most recent analysis](https://justknowledge-uk.github.io/jrf_nat_rep/R/key_findings_report_structure)

[Click here for the methodology document](https://justknowledge-uk.github.io/jrf_nat_rep/R/methodology)

## Study 1 - Nationally Representative Survey


## Study 2 - Experiential Survey of Outsourced Workers

This repository contains the data and analysis scripts for analysis of outsourced workers data. The analysis explores various metrics related to outsourced work experiences, compensation, and other relevant factors as detailed in the research report.

### Getting Started

To access the experiential data and analysis:

1. Clone this repository
2. Navigate to the 'experiential' folder

### Repository Structure

The 'experiential' folder contains:
- `data/`: Raw and processed datasets
- `img/`: Generated charts, graphs, and visualizations
- `scripts/`: Analysis scripts written in R and Quarto documents (.qmd)

### Usage Instructions

#### Data Cleaning

Start by running the data cleaning script:
```R
Rscript scripts/data_cleaning.R
```

This script:
- Cleans the raw data
- Calculates pay metrics
- Removes outliers
- Prepares the data for further analysis

#### Analysis Scripts

After cleaning the data, you can run any of the following analysis scripts to reproduce the results reported in the report [NAME OF REPORT TBC]:

```R
Rscript scripts/Linear Modelling.R
```

Or for Quarto documents:
```
quarto render scripts/Crosstabulation.qmd
```

Available analysis scripts include various examinations of the outsourced workers dataset.

## Requirements

- R 4.0.0+
- Required R packages: tidyverse, ggplot2, dplyr, knitr (for Quarto documents)
- Quarto (for .qmd files)

## Citation

If you use this data or analysis in your research, please cite:
[Citation information to be added]

## License

[License information]

## Contact

For questions or issues regarding this repository, please [open an issue](link) or contact Just Knowledge at hello@justknowledge.org.uk
