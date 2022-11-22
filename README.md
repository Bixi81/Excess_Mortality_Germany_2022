# Analysis of Excess Mortality in Germany 2016-2022 using R

## Introduction
Mortality varies within each year and also across years. The Covid-19 pandemic has changed seasonal patterns of mortality (at least this is the presumption here). I investigate seasonal excess mortality using a simple linear regression model.

## Data
Mortality data for Germany are published by the [German Statistical Office (Destatis)](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.html). Destatis provides a (not well formated) Excel file containing mortality figures in several stratas (by time, by age etc). I will use aggregated mortality figures (all age groups) on a weekly basis to investigate seasonal excess mortality.

## Data Preparation
Before I start, I need to prepare the data. I first read the Excel file into R, select the respective sheet, and drop a few lines which are headers. The next steps can be followed based on the comments in the code below. Essentially, I end up with a dataframe in long format, which contains a values of deaths per week from 2016 to 2022.

```
library(readxl)
roh <- read_excel("C:/Users/User/Downloads/sonderauswertung-sterbefaelle.xlsx", 
                    sheet = "D_2016_2022_KW_AG_Ins", col_names = FALSE, skip = 8)

# Get header with ISO weeks
header = roh[1,]
# select values of sums over age groups named "Insgesamt"
sums = roh[grep("Insgesamt", roh$...3),]

# Bind data
tmp = rbind(header,sums)
tmp = tmp[,2:ncol(tmp)]
tmp = tmp[,-2]
tmp = data.frame(tmp)
tmp[1,1]<-"nn"

# Set row names (years)
row.names(tmp)<-tmp[,1]
tmp = tmp[,-1]

# Set column names (ISO week per year)
colnames(tmp) <- tmp[1,]
tmp = tmp[-1,]

# Drop week 53 
tmp <- tmp[,-53]

# Set up empty dataframe
# y = year, w = week, v = value
df = data.frame(y=NA,w=NA,v=NA)

# Fill dataframe (aka "wide" to "long")
counter = 1
for (y in row.names(tmp)){
  for (w in colnames(tmp)){
    df[counter,"y"] <- y
    df[counter,"w"] <- w
    df[counter,"v"] <- tmp[[y,w]]
    counter = counter + 1
  }
}

# Drop missing values in 2022
df = df[!is.na(df$v),]
``` 

## Model
In order to assess excess mortality, I estimate a simple OLS model using data from 2016 to 2019 (time before Covid-19) in order to get a reasonable baseline for what would be the expected (viz. average) mortality for a given ISO week.

```
# Split pre/post Covid
df_pre = df[df$y<2020,]
df_post = df[df$y>=2020,]

# Estimate OLS model
reg = lm(v ~ as.factor(w),data=df_pre)
summary(reg)
```

The OLS model essentially 
```math
y_i = \beta_0 + \beta_1 w_i + u_i,
```
where w is the ISO week. Thus, the average mortality for each week from 2016 to 2019 serves as the baseline here.
