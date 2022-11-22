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


# Split pre/post Covid
df_pre = df[df$y<2020,]
df_post = df[df$y>=2020,]

# Estimate OLS model
reg = lm(v ~ as.factor(w) ,data=df_pre)
summary(reg)

# Prediction baseline
pred = predict(reg,newdata = df_pre)
pred_pre = data.frame(cbind(pred,df_pre))
pred_pre$date = paste0(pred_pre$y, "-", sprintf("%02d", as.numeric(pred_pre$w)))
pred_pre = pred_pre[order(pred_pre$date),]
plot(pred_pre$v)
lines(pred_pre$pred, col="blue")

# Prediction "post" on baseline
pred = predict(reg,newdata = df_post)
pred_post = data.frame(cbind(pred,df_post))
pred_post$date = paste0(pred_post$y, "-", sprintf("%02d", as.numeric(pred_post$w)))
pred_post = pred_post[order(pred_post$date),]
plot(pred_post$v)
lines(pred_post$pred, col="blue")

# Excess mortality based on baseline
all = rbind(pred_pre, pred_post)
all$excess = all$v - all$pred
all$n = seq(1,nrow(all))
all$date = as.Date(paste(all$y, all$w, 1, sep="-"), "%Y-%U-%u")

plot(all$date, all$excess, type = "l", lty = 1, xaxt='n', xlab="", ylab="Excess Mortality (rel. to 2016-2019)")
abline(h=0, col="gray", lwd=1.8, lty=2)
abline(v=as.numeric(as.Date("2020-03-15")), lwd=1.2, col='red', lty=2)
axis.Date(1,at=all$date,labels=format(all$date,"%Y-%m"),las=2, cex.axis = 0.75)

# Compare means 
mean(all$excess[all$y<2020])
mean(all$excess[all$y>=2020])


