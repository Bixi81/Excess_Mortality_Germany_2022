# Analysis of Excess Mortality in Germany 2016-2022 using R

## Introduction
Mortality varies within each year and also across years. The Covid-19 pandemic has changed seasonal patterns of mortality (at least this is the presumption here). I investigate seasonal excess mortality using a simple linear regression model.

## Data
Mortality data for Germany are published by the [German Statistical Office (Destatis)](https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.html). Destatis provides a (not well formated) Excel file containing mortality figures in several stratas (by time, by age etc). I will use aggregated mortality figures (all age groups) on a weekly basis to investigate seasonal excess mortality.

## Data Preparation
Before I start, I need to prepare the data. I first read the Excel file into R and 

```
library(readxl)
roh <- read_excel("C:/Users/User/Downloads/sonderauswertung-sterbefaelle.xlsx", 
                                            sheet = "D_2016_2022_KW_AG_Ins", col_names = FALSE, 
                                            skip = 8)

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


