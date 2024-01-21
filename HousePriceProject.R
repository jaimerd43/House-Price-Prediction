library(ggplot2) 
library(readxl)
library(tidyverse)
library(caret) 
library(corrplot) 
library(lmtest) 
library(car)


ames <- read_excel('/Users/jaimerd/Desktop/master/Statistics Business/assigment 1/housingdataset.xlsx')

colnames(ames)

#house age and gr living sf are calculated 
ames$house_age <- ames$year_sold - ames$year_remod 
ames$total_sf <- ames$floor1_sf + ames$floor2_sf 

#create a new table with the selected variables
data <- ames %>% select(d_type, lot_area, `exter l_qual`,
                        neighbourhood, prox_1, garage_cars, house_quality, house_condition, bsmt_area,heat_qual, total_sf, bedroom, kitchen, rooms_tot, full_bath, fireplace, garage_area,house_age, sale_price)
summary (data)

##data cleaning -----
#Convert the categorical variables into factors
data <- data %>% mutate_if(is.character,as.factor) 

#LOT_AREA
hist(data$lot_area)
table(data$lot_area)
boxplot(data$lot_area)
data %>% count(lot_area>50000) 
data$lot_area[data$lot_area > 50000] <- NA 
data$lot_area[data$lot_area < 20] <- NA 
summary(data$lot_area)

#BSMT_AREA
hist(data$bsmt_area)
boxplot(data$bsmt_area)
data %>% count(bsmt_area>2470) 
data$bsmt_area[data$bsmt_area > 2470] <- NA 
summary(data$bsmt_area)
table(data$bsmt_area)

#total_sf
boxplot(data$total_sf)
data %>% count(total_sf>3200) 
data$total_sf[data$total_sf > 3200] <- NA

#garage area
boxplot(data$garage_area)
data %>% count(garage_area>1100) 
data$garage_area[data$garage_area > 1100] <- NA 

#house age
boxplot(data$house_age) 
data$house_age[data$house_age < 0] <- NA 
summary(data$house_age)

#sale_price
boxplot(data$sale_price)
data %>% count(sale_price>601000)
data$sale_price[data$sale_price > 601000] <- NA 
summary(data$sale_price)
hist(data$sale_price)

#bedrooms
boxplot(data$bedroom) 
table(data$bedroom) 
data$bedroom[data$bedroom > 6] <- NA 
data$bedroom[data$bedroom < 1] <- NA 

#kitchen
boxplot(data$kitchen) 
table(data$kitchen) 
data$kitchen[data$kitchen > 2] <- NA 
data$kitchen[data$kitchen < 1] <- NA 

#full_bath
boxplot(data$full_bath) 
table(data$full_bath) 
data$full_bath[data$full_bath < 1] <- NA 
data$full_bath[data$full_bath > 3] <- NA 

#fireplace
boxplot(data$fireplace)
table(data$fireplace)
data$fireplace[data$fireplace > 3] <- NA

#neighborhood
table(data$neighbourhood)
data$neighbourhood <- droplevels(data$neighbourhood, exclude = 'Landmrk') 

#garage_cars
boxplot(data$garage_cars) 
table(data$garage_cars) 
data$garage_cars[data$garage_cars > 3] <- NA 

#extenal quality
data$'exter l_qual' <- droplevels(data$'exter l_qual', exclude = "Good")
data %>%
  rename(external_qual = 'exter l_qual') -> data

#house quality
table(data$house_quality)
data$house_quality[data$house_quality == '11'] <- NA

#house_condition
table(data$house_condition)

#rooms_totals
table(data$rooms_tot)
data$rooms_tot[data$rooms_tot == '0'] <- NA
data$rooms_tot[data$rooms_tot > 11] <- NA

#fireplace
table(data$fireplace)
data$fireplace[data$fireplace == '3'] <- NA

#convert the numerical variables which levels affect the price such as rooms total and the ordinal variables in factors
data$d_type <- as.factor(data$d_type) 
data$house_quality <- as.factor(data$house_quality) 
data$house_condition <- as.factor(data$house_condition) 
data$fireplace <- as.factor(data$fireplace) 
data$garage_cars <- as.factor(data$garage_cars) 
data$rooms_tot <- as.factor(data$rooms_tot) 
summary(data)
data <- na.omit(data)

##graphs ------
#garage area
ggplot(data, aes(x = garage_area, y = sale_price)) +
  geom_point(color = "blue", alpha = 0.7) + geom_smooth(method = 'lm', color = "red") +
  
  labs(title = "Garage Area and Sale Price", x = "Garage Area",
       y = "Sale Price") +
  theme_minimal()

#house_quality
ggplot(data, aes(x = house_quality, y = sale_price, fill = house_quality)) +
  geom_boxplot() +
  labs(title = "House Quality and Sale Price",
       x = "House Quality",
       y = "Sale Price") + theme_minimal() + guides(fill = FALSE)

#house age, sale price and house condition
ggplot(data, aes(x = house_age, y = sale_price, color = house_quality)) +
  geom_point() +
  labs(title = "House age, house quality and house age",
       x = "House Age",
       y = "Sale Price") + theme_minimal()

#Neighborhood and sale price
ggplot(data, aes(x = neighbourhood, y = sale_price, fill = neighbourhood)) +
  geom_boxplot() +
  labs(title = "Neighbourhood and Sale Price",
       x = "Neighbourhood",
       y = "Sale Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + guides(fill = FALSE)

#total_sf and sale price
ggplot(data, aes(x = total_sf, y = sale_price)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "Ground living area and sale price",
       x = "Ground living area",
       y = "Sale Price") + theme_minimal()
#lot area, sale price and house age
ggplot(data, aes(x = lot_area, y = sale_price, color = house_age)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ house_quality, scales = "free") + labs(title = "Lot area, sale price and house age",
                                                      x = "Lot area",
                                                      y = "Sale Price") + theme_minimal()
#Total rooms and sale price
ggplot(data, aes(x = as.factor(rooms_tot), y = sale_price)) +
  geom_point(color = "blue", alpha = 0.5) + labs(title = "Total rooms and Sale Price",
                                                 x = "Total rooms",
                                                 y = "Sale Price") + theme_minimal()
#bedroom and sale price
ggplot(data, aes(x = bedroom, y = sale_price)) +
  geom_point(color = "blue", alpha = 0.5) + labs(title = "Bedrooms and Sale Price",
                                                 
                                                 x = "Bedrooms",
                                                 y = "Sale Price") + theme_minimal()
##Correlation---------

#Pearson correlation numeric variables
data_numeric <- data[sapply(data, is.numeric)] 
correlation_matrix <- cor(data_numeric, use = "complete.obs") 
corrplot(correlation_matrix, method = "circle")

#numeric hypothesis correlation
cor.test(data$total_sf, data$sale_price)
cor.test(data$house_age, data$sale_price) 
cor.test(data$garage_area, data$sale_price)
pairs(data[, c("sale_price", "house_age", "garage_area", "total_sf")])

#categorical hypothesis
anova_result <- aov(sale_price ~ house_quality + neighbourhood, data = data) 
cor.test(data$sale_price, as.numeric(data$house_quality), method = "spearman", exact = FALSE) 
cor.test(data$sale_price, as.numeric(data$neighbourhood), method = "spearman", exact = FALSE)

###Linear Regresion Model --------
#split the data 80 TRAIN Y 20 TEST
set.seed(40425150)
index <- createDataPartition(data$sale_price, p = 0.8, list = FALSE) 
train <- data[index,]
test <- data[-index, ]

#build the models
model0 <- lm(sale_price ~ neighbourhood, data = train) 
model01 <- lm(sale_price ~ house_quality, data= train)

model1 <- lm(sale_price ~ total_sf + neighbourhood + house_quality + garage_area + house_age, data = train)
summary(model1)
test$predictions1 <- predict(model1, test)
print(test$predictions1)
postResample(test$predictions1, test$sale_price)

model2 <- lm(sale_price ~ total_sf + neighbourhood + house_quality + garage_area + house_age + d_type + bedroom + bsmt_area, data = train)
summary(model2)
test$predictions2 <- predict(model2, test)
print(test$predictions2)
postResample(test$predictions2, test$sale_price)

model3 <- lm(sale_price ~ total_sf + neighbourhood + house_quality + garage_area + house_age + kitchen + bsmt_area + fireplace + external_qual, data = train)
summary(model3)
test$predictions3 <- predict(model3, test)
print(test$predictions3)
postResample(test$predictions3, test$sale_price)

model4 <- lm(sale_price ~ total_sf + neighbourhood + house_quality + garage_area + house_age + d_type + bsmt_area + bedroom + fireplace + heat_qual + external_qual, data=train)
summary(model4)
test$predictions4 <- predict(model4, test) 
print(test$predictions4) 
postResample(test$predictions4, test$sale_price) 
plot(model4)

#line chart actual values vs predicted values with half of the test dataset for increased clarity 
ggplot(test, aes(x = seq_along(sale_price)/2)) +
  geom_line(aes(y = sale_price, color = "Actual Price"), alpha = 0.5, size = 0.5) + 
  geom_line(aes(y = predictions4, color = "Predicted Price"), alpha = 0.5, size = 0.5) + 
  labs(title = "Actual Prices vs Predicted Prices",
  x = "Observations",
  y = "Sale Price") +
  scale_color_manual(values = c("Actual Price" = "red", "Predicted Price" = "blue")) + 
  theme_minimal()

#residuals examination. 105 standardised residuals
diag <- train
diag$residuals <- resid(model4)
diag$standar_residuals <- rstandard(model4)
diag$large_residuals <- diag$standar_residuals > 2 | diag$standar_residuals < -2 
sum(diag$large_residuals)
diag_investigate <- diag[diag$large_residuals, c("sale_price", "total_sf", "neighbourhood", "house_quality", "garage_area", "house_age", "d_type", "bsmt_area", "bedroom", "fireplace", "heat_qual", "external_qual")]
diag_investigate 
hist(diag$residuals)

#assumption independent erros dwtest(model4)
#assumption of no multicollinearity vif(model4)
mean(vif(model4))



