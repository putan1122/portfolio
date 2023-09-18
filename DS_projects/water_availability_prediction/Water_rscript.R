arno_fp = file.path('E:', 'UC San Diego', 'Courses', 'MATH189', 'Final Project', 'data', 'River_Arno_Mean.csv')
#madonna_fp = file.path('E:', 'UC San Diego', 'Courses', 'MATH189', 'Final Project', 'data', 'Water_Spring_Madonna_di_Canneto.csv')

arno_raw = read.csv(arno_fp, header=T)

# change column name, no need to do so for the updated dataframe from python
## names(arno_raw)[names(arno_raw) == 'ï..Date'] = 'Date'

# take mean of rainfalls of different regions
rainfall_cols = c('Rainfall_Le_Croci', 'Rainfall_Cavallina', 'Rainfall_S_Agata', 'Rainfall_Mangona', 'Rainfall_S_Piero', 
                 'Rainfall_Vernio', 'Rainfall_Stia', 'Rainfall_Consuma', 'Rainfall_Incisa', 'Rainfall_Montevarchi', 
                 'Rainfall_S_Savino', 'Rainfall_Laterina', 'Rainfall_Bibbiena', 'Rainfall_Camaldoli')
## due to the characteristic of r rowMeans, this step is accomplished via python

### code is copied as below
### fp2 = os.path.join('data', 'River_Arno.csv')
### arno = pd.read_csv(fp2)
### rainfall_cols = ['Rainfall_Le_Croci', 'Rainfall_Cavallina', 'Rainfall_S_Agata', 'Rainfall_Mangona', 'Rainfall_S_Piero', 
###                  'Rainfall_Vernio', 'Rainfall_Stia', 'Rainfall_Consuma', 'Rainfall_Incisa', 'Rainfall_Montevarchi', 
###                  'Rainfall_S_Savino', 'Rainfall_Laterina', 'Rainfall_Bibbiena', 'Rainfall_Camaldoli']
### arno['Rainfall'] = arno[rainfall_cols].mean(axis = 1)
### arno = arno.drop(rainfall_cols, axis = 1)

# mean squared error function -- for later use
mse = function(fit)
  mean(fit$residuals ^ 2)

# Dates manipulation
arno = arno_raw
arno$Date = as.Date(arno_raw$Date, '%d/%m/%Y')

# remove na values
arno = na.omit(arno)
# rename the columns to make code more concise
names(arno)[names(arno) == 'Hydrometry_Nave_di_Rosano'] = 'Hydrometry'
names(arno)[names(arno) == 'Temperature_Firenze'] = 'Temperature'

# graphical display of the hydrometry distribution over time
plot(Hydrometry ~ Date, arno, cex = 0.5, col = 26, main = 'Hydrometry of Arno River Over Time')
hist(arno$Hydrometry, breaks = 30, main = 'Histogram of Arno River Hydrometry')

plot(Rainfall ~ Date, arno, type = 'l', col = 228, main = 'Average Rainfall around Arno River')
plot(Temperature ~ Date, arno, type = 'l', col = 228, main = 'Temperature around Arno River')


# removing outliers which hydrometry is zero
old_arno = arno
arno = arno[which(arno['Hydrometry'] > 0),]

# linear regression on only the rainfall to predict Hydrometry
lr_rainfall = lm(formula = Hydrometry ~ Rainfall, data = arno)
summary(lr_rainfall)
plot(Hydrometry ~ Rainfall, arno, cex = 0.5, col = 28, main = 'Linear Regression on Rainfall')
abline(lr_rainfall, col = 'red')
## residual plot
plot(lr_rainfall$residuals, cex = 0.5, col = 28, main = 'Residuals of Regression on Rainfall')
abline(0, 0, col='red', lwd = 2)
qqnorm(lr_rainfall$residuals)
qqline(lr_rainfall$residuals, col="red")
mse(lr_rainfall)

# linear regression on only the temperature to predict Hydrometry
lr_temp = lm(formula = Hydrometry ~ Temperature, data = arno)
summary(lr_temp)
plot(Hydrometry ~ Temperature, arno, cex = 0.5, col = 28, main = 'Linear Regression on Temperature')
abline(lr_temp, col = 'red')
## residual plot
plot(lr_temp$residuals, cex = 0.5, col = 28, main = 'Residuals of Regression on Temperature')
abline(0, 0, col='red', lwd = 2)
qqnorm(lr_temp$residuals)
qqline(lr_temp$residuals, col="red")
mse(lr_temp)


# multivariate linear regression model on both temperature and rainfall
lr_temp_rainfall = lm(formula = Hydrometry ~ Rainfall + Temperature, data = arno)
summary(lr_temp_rainfall)
plot(lr_temp_rainfall$residuals, cex = 0.5, col = 28, main = 'Residuals of Regression on Temperature and Rainfall')
abline(0, 0, col='red', lwd = 2)
qqnorm(lr_temp_rainfall$residuals)
qqline(lr_temp_rainfall$residuals, col="red")
mse(lr_temp_rainfall)
hist(lr_temp_rainfall$residuals, breaks = 20)

# Advanced Analysis: one hot encoding
arno$Month = format(arno$Date, '%m')

oneHot_arno = arno
for (unique_val in unique(arno$Month)) {
  oneHot_arno[paste("month", unique_val, sep = '_')] = ifelse(arno$Month == unique_val, 1, 0)
}

lr_onehot = lm(formula = Log_hydrometry ~ 
                 Rainfall + Temperature + month_01 + month_02 
               + month_03 + month_04 + month_05 + month_06 + month_07 
               + month_08 + month_09 + month_10 + month_11 + month_12, data = oneHot_arno)
summary(lr_onehot)
plot(lr_onehot$residuals, cex = 0.5, col = 28, main = 'Residuals of Regression with One-Hot Encoding')
abline(0, 0, col='red', lwd = 2)
qqnorm(lr_onehot$residuals)
qqline(lr_onehot$residuals, col="red")

mse(lr_onehot)
mse(lr_temp_rainfall)

# just a test on temp
poly_temp = lm(formula = Hydrometry ~ poly(Temperature, degree = 2), data = arno)
summary(poly_temp)

poly_pred = data.frame(Temperature = seq(0, 40, length.out = 400))
poly_pred$pred = predict(poly_temp, newdata = poly_pred)


plot(Hydrometry ~ Temperature, arno, cex = 0.5, col = 28, main = 'Linear Regression on Temperature')
with(poly_pred, lines(x = Temperature, y = pred))

## residual plot
plot(poly_temp$residuals, cex = 0.5, col = 28, main = 'Residuals of Regression on Temperature')
abline(0, 0, col='red', lwd = 2)
qqnorm(poly_temp$residuals)
qqline(poly_temp$residuals, col="red")

# log transformation for hydrometry
arno$Log_hydrometry = log(arno$Hydrometry)
lr_log = lm(formula = Log_hydrometry ~ Rainfall + Temperature, data = arno)
summary(lr_log)
plot(lr_log$residuals, cex = 0.5, col = 28, main = 'Residuals of Regression after Log Transformation')
abline(0, 0, col='red', lwd = 2)
qqnorm(lr_log$residuals)
qqline(lr_log$residuals, col="red")
mse(lr_log)


