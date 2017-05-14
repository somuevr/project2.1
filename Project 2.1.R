# Single Linear Regression for DVD sales Data 
dvd_data <- read.csv(file.choose())
dvd_data
head(dvd_data)
str(dvd_data)
summary(dvd_data)

index <- sample(1:nrow(dvd_data),0.8*nrow(dvd_data),replace = F)
index
train_data <- dvd_data[index,]
test_data <- dvd_data[-index,]
dvd_data_new <-train_data
attach(train_data)
attach(dvd_data_new)
library("e1071")
#Checking the assumptions
#1. Chekcing the Normality 
boxplot.default(advertise)
?shapiro.test (advertise)
skewness(advertise)
sort(boxplot(advertise)$out)
t <- quantile (advertise,probs = c(0.98,0.985,0.99))
t
advertise_new <- ifelse(advertise > t[2],t[2],advertise)
boxplot(advertise_new)
shapiro.test(advertise_new)
skewness(advertise_new)
dvd_data_new$advertise_new <- advertise_new
dvd_data_new$advertise<-NULL
boxplot(sales)
shapiro.test(sales)
skewness(sales)
kurtosis(sales)
boxplot(plays)
shapiro.test(plays)
skewness(plays)
boxplot(attractiveness)
sort(boxplot(attractiveness)$out)
t <- quantile(attractiveness,probs = c(0.01,0.025))
t
attractiveness_new <- ifelse(attractiveness<t[2],t[2],attractiveness)
boxplot(attractiveness_new)
shapiro.test(attractiveness_new)
skewness(attractiveness_new)
dvd_data_new$attractiveness_new <- attractiveness_new
dvd_data_new$attractiveness <- NULL
head(dvd_data_new)

#formula to predict sales
sales_new <- -33.0996+3.3059*plays+0.0899*advertise_new+12.2331*attractiveness_new
dvd_data_new$sales_new <- sales_new
plot(dvd_data_new$sales, type = "b")

obs_v_exp <- data.frame(observed=dvd_data_new$sales,
                        expected=dvd_data_new$sales_new)
matplot(obs_v_exp, type = "b")

test_data$sales_new <- -33.0996+3.3059*test_data$plays+0.0899*test_data$advertise+12.2331*test_data$attractiveness

obs_v_exp <- data.frame(observed=test_data$sales,
                        expected=test_data$sales_new)
matplot(obs_v_exp, type="b")

