# Harvest-Choice Code Details
This file provides information on the code used in the following paper: Zhao, J., Daigneault, A., & Weiskittel, A. (2020). Forest landowner harvest decisions in a new era of conservation stewardship and changing markets in Maine, USA. Forest Policy and Economics, 118, 102251.doi.org/10.1016/j.forpol.2020.102251
## Overview
The GitHub repository contains R code and data for the harvest choice model. Data were cleaned, combined, and analyzed in R using the code provided and described below. 
### Model Data
Original data (Data_V0.csv)
Timber growing stock and harvests are estimated on a green ton per hectare (t ha−1) level using data from the U.S. Forest Service FIA program USDA Forest Service (2019),  approximately 20% of FIA plots are randomly re-measured in a given year such that the entire sample is measured within a 5-year cycle. As a result, we cluster our analysis into three periods: 2002–2006, 2007–2011, and 2012–2016.

### Growth (Growth.csv)
Growing stock volume functions were calculated by regression analysis of no-harvest activity plot records.

### Harvest Intensity 
The harvest intensity of each plot by calculating the net removal of a given timber type relative to the total growing stock. In this analysis, we define a “full” harvest as the removal of 70% or more of merchantable timber on the site and a “partial” harvest as between 1% and 69%, which would include commercial thinning and multistage
shelterwood harvests. Removal ratios for "partial" harvest were estimated by Tobit regression analysis.

### Harvest Choice
We estimated both harvest choices separately for sawlogs and pulp logs. Data were compiled on removals by timber type, location, elevation, and other site characteristics for matched plots for each period.

### Codes Steps
The steps to run this model include: 
1. Data setup: This step should be run via the 1_Data_Clean.R with the original data file Data_V0.csv, generating results output Data_Clean_V0.csv.
2. Growth Volume: This step should be run via the 2_Growth. R with the Growth.csv and Data_Clean_V0.csv, generating parameters output Data_Clean_V1.csv.
3. Harvest Intensity: This step should be run via the 3_Intensity. R with the Data_Clean_V1.csv, generating results output Data_Clean_V2.csv.
4. Harvest Choice: This step should be run via the 4_Choice. R with the Data_Clean_V2.csv, generating model estimates.






