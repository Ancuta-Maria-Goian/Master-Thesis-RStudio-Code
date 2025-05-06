### Thesis Log File Analysis (MAE4191)

### Thesis Title 

*Solution Patterns in a Problem-Solving Task: Consistency Across Educational Systems*

This repository contains R code used for the log-file based analysis of student problem-solving behavior in the context of a digital assessment task (CP007Q02 from the CBA environment). The script performs data cleaning, transformation, feature extraction, and clustering for three countries: Colombia (COL), Norway (NOR), and Singapore (SGP).

### Project Summary

This work supports a thesis project aiming to understand how students solve a traffic network task by analyzing log file interactions. Students' actions are translated into network features, which are then used to investigate behavioral patterns and performance groups using Gaussian Mixture Models (GMMs).

### Data Sources

A specified working directory is needed for the following datasets that were first extracted with IBM SPSS Statistics (v. 30.0.0.0, 2024):

-Log Data: `CBA_cp007q02_logs12_SPSS.sav`

-Scored Items: `Scored_item.sav`

-Plausible Values: `P_values.sav`

All data files are assumed to be located in the specified working directory.

### Required R Packages

The script uses several packages for data manipulation, statistical modeling, and network analysis:

```r
haven, dplyr, tidyr, stringr, igraph, moments,
knitr, factoextra, mclust, mirt, purrr, scales
```

Install them using:

```r
install.packages(c("haven", "dplyr", "tidyr", "stringr", "igraph",
                   "moments", "knitr", "factoextra", "mclust",
                   "mirt", "purrr", "scales"))
```

### Code Structure

1. **Data Loading and Preparation**

   -Load .sav data files

   -Filter countries of interest

   -Clean and validate log events (START\_ITEM, END\_ITEM, resets, clicks)

2. **Event Transformation**

   -Map raw event values to simplified codes

   -Derive operations (e.g., SP1, CP2) from clicks

   -Compute transitions and durations between actions

3. **Feature Engineering**

   -Create student-level features:

4. **Descriptive Analysis**

   -Summarize behavior by country and performance level (success/failure)

   -Visualize gender and score distributions

5. **Clustering**

   -Apply Hopkins Statistic to assess clustering tendency

   -Use Gaussian Mixture Models (GMM) to identify behavior patterns

   -Output descriptive summaries per cluster

6. **Plausible Value Integration**

   -Compute average domain scores per cluster using averageMI()

7. **Network Visualization**

   -Generate simplified cluster-level network graphs

   -Filter low-frequency transitions for clarity

### Reproducibility Notes

-Working directory must be set correctly

-Random seeds are defined for clustering and Hopkins computations

### Citation / Attribution

If using this code or methodology, please cite the corresponding thesis or contact the author.

### Contact: 

Ancuta-Maria Goian  email: [ancutamg@uio.no](mailto:ancutamg@uio.no)

