# US Youth Smoking Analysis

> Which group of youth should be the target of smoking prevention?

An statistical analysis has been conducted to analyze national youth tobacco usage using NYTS survey prodiced by CDC. The use of tobacco is known to be addictive and harmful to human health, thus preventing tobacco consumption during the youth is crucial, as youth is likely the time people first start to consume tobacco. It's important to know who are more likely to smoke in order to target this group of people and apply prevention means. It is found that Males high school students whose race is Hispanic, White and Black (in decreasing order) are more likely to smoke. Curiosity is also an important factor. Keeping students in a healthy environment is important.

The report is written in Rmarkdown and can be found in [./outputs/paper/paper.rmd](./outputs/paper/paper.rmd).

## Data

The dataset used comes from CDC and is called [NYTS (National Youth Tobacco Survey)](https://www.cdc.gov/tobacco/data_statistics/surveys/nyts/index.htm).

Historical data is available from 1999 to 2021. Survey results from 2015 to 2020 are used in this analysis.

All the data are downloaded and saved in `.xlsx` format.

The survey results need to be cleaned and pre-processed before using it. 
The cleaning script can be found in [./scripts/preprocess.R](./scripts/preprocess.R).

The pre-processed data are saved in `.csv` format to save time. Kniting the Rmd file will read the pre-saved intermediate csv files.

So if the pre-processed data files are missing, run [./scripts/preprocess.R](./scripts/preprocess.R) to generate them again.

All the raw `xlsx` files and intermediate `.csv` files are saved in [./inputs/data](./inputs/data) folder.