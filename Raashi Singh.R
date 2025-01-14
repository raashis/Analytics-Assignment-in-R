install.packages("ggplot2")
install.packages("MASS")
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("car")
install.packages("caTools")

#my libraries
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(car)
library(ggplot2)
library(dplyr)
library(MASS)

setwd("/Users/raashisingh/Downloads/AY24 CBA")

hospital.data <- read.csv("INF002v4.csv")

#question 1:
#to properly explore my data, i should clean it up first

#ill be first checking for any missing or na values. by checking the different categories of the dataset (which u can do by taking a look at quick filer for each column), there were 33 missing values that i found in 'Hospital Service Area', 'APR Severity of Illness Description' and 'APR Risk of Mortality'. i decided to remove the complete rows with the missing values because 33 out of the 28000+ rows is insignificant.

hospital.data[!is.na(hospital.data$Hospital.Service.Area) & !is.na(hospital.data$APR.Severity.of.Illness.Description) & !is.na(hospital.data$APR.Risk.of.Mortality),] #removing the rows with NA values in these selected 3 columns

#ill now see if there any duplicates, and if there are ill remove them 

hospital.data[!duplicated(hospital.data),] 

#i should make sure that all my numeric columns (like 'Length Of Stay', 'Total Charges' and 'Total Costs') are numbers, and if theyre not, convert them to numbers

str(hospital.data)

#here i notice that the 'Length Of Stay' is indeed numeric, but 'Total Charges' and 'Total Costs' are  characters, which i have to convert into numeric
#however when i just tried to do as.numeric directly, the data type for 'Total Charges' and 'Total Costs' had  become numeric, but they were coming as NA values. after that i understood that this was due to the commas present in the chr, so i removed them first, then applied as.numeric

hospital.data$Total.Charges <- gsub(",", "", hospital.data$Total.Charges)
hospital.data$Total.Charges <- as.numeric(hospital.data$Total.Charges)
hospital.data$Total.Costs <- gsub(",", "", hospital.data$Total.Costs)
hospital.data$Total.Costs <- as.numeric(hospital.data$Total.Costs)

#some columns such as APR DRG Description and Payment Typology 2, had some values such as IMPLANTABLE HEART ASSIST SYSTEMS in APR DRG Description and Department of Corrections in Payment Typology 2, which just appeared once in the dataset. So they could not be in both training and test sets, so I am coming back here to clean such values in these columns. I will put such values in the APR DRG Description column (since there are multiple) as 'Others', but since theres only just 1 value like this in Payment Typology 2, I'll drop it (cause 1 value out of 28000+ is very insignificant. 
#Note: FOr APR DRG Description, LIVER TRANSPLANT AND/OR INTESTINAL TRANSPLANT appears 4 times in the whole 28000+ values, so I also included that to be put in other

put_in_other <- c("LIVER TRANSPLANT AND/OR INTESTINAL TRANSPLANT", "PANCREAS TRANSPLANT", "ALLOGENEIC BONE MARROW TRANSPLANT", "AUTOLOGOUS BONE MARROW TRANSPLANT OR T-CELL IMMUNOTHERAPY", "UNGROUPABLE", "HEART AND/OR LUNG TRANSPLANT", "IMPLANTABLE HEART ASSIST SYSTEMS")

#for apr drg description
hospital.data <- hospital.data %>%
mutate(APR.DRG.Description = case_when(APR.DRG.Description %in% put_in_others ~ "Other", TRUE ~ APR.DRG.Description))

#for payment 2
hospital.data <- hospital.data %>%
filter(Payment.Typology.2 != "Department of Corrections")

#i also wanted to check for length of stay's normality, so i plotted it on a qq plot

qqnorm(hospital.data$Length.of.Stay, main = "QQ Plot of Length of Stay")
qqline(hospital.data$Length.of.Stay, col = "red")

#since los appears to be very very right skewed, i will try to log it  

hospital.data$Length.of.Stay.logged <- log(hospital.data$Length.of.Stay + 1)

#lastly, i just wanted to standardise the Ethnicity Column

hospital.data$Ethnicity <- gsub("Not Span/Hispanic", "Not Spanish/Hispanic", hospital.data$Ethnicity)

View(hospital.data)

#for my first notable finding, i wanted to find the correlation between APR.Severity.of.Illness.Code (a numerical value) and logged LOS

cor(hospital.data$Length.of.Stay.logged, hospital.data$APR.Severity.of.Illness.Code)

#got an answer of 0.383, showing a weak positive relationship between the severity of a patient's illness and how long they stay in the hospital, which is an interesting and unexpected finding

#second notable finding; i want to see how length of stay is correlated with a categorical variable, so i did a boxplot of LOS with type of admission

boxplot(Length.of.Stay.logged ~ Type.of.Admission, data = hospital.data, main = "Logged LOS by Type of Admission", xlab = "Type of Admission", ylab = "Logged LOS")

#trauma is seen to have the highest median of Logged LOS, as trauma might include life-threatening injuries like head trauma etc, so patients might take a longer time to recover (thus higher LOS)

#now for my third finding, i wanted to see the patients who are outliers in total charges (i.e. are paying the higher end of the total charges, so essentially theyre paying a lot of money) and how long theyre staying in the hospital

#so first im calculating my outlier for total charges
Q1_TC <- quantile(hospital.data$Total.Charges, 0.25)
Q3_TC <- quantile(hospital.data$Total.Charges, 0.75)
IQR_TC <- Q3_TC - Q1_TC
lower_TC <- Q1_TC - 1.5 * IQR_TC
upper_TC <- Q3_TC + 1.5 * IQR_TC
outlier_rows <- hospital.data$Total.Charges < lower_TC | hospital.data$Total.Charges > upper_TC

# then i take a look at what the los is for those outliers
outlier_los <- hospital.data$Length.of.Stay.logged[outlier_rows]
outlier_los

#however, if i try to print outlier_los, i get a lot of rows, and im not sure how to visually process this information. so i segregate los into days of 20 (as in 1-20, 21-40, 41-60 etc...)
bins <- seq(0, 120, by = 20)
outlier_los_binned <- cut(outlier_los, breaks = bins, right = TRUE, include.lowest = TRUE)
outlier_count <- table(outlier_los_binned)
outlier_count

#since the distribution of my results were a little confusing (with 2522 outlier patients having a los from 0-20, and 0 outlier patients having a los over 20 days), i printed out the histogram of total charges, which i saw was incredibly right skewed, and hence i also log total charges and repeat my above process

hospital.data$Total.Charges.logged <- log(hospital.data$Total.Charges + 1)

#and then repeat everything i did above, first ill find the outlier of my logged TC

Q1_TC_logged <- quantile(hospital.data$Total.Charges.logged, 0.25)
Q3_TC_logged <- quantile(hospital.data$Total.Charges.logged, 0.75)
IQR_TC_logged <- Q3_TC_logged - Q1_TC_logged
lower_TC_logged <- Q1_TC_logged - 1.5 * IQR_TC_logged
upper_TC_logged <- Q3_TC_logged + 1.5 * IQR_TC_logged
outlier_rows_logged <- hospital.data$Total.Charges.logged < lower_TC_logged | hospital.data$Total.Charges.logged > upper_TC_logged

outlier_los_logged <- hospital.data$Length.of.Stay.logged[outlier_rows_logged]
bins <- seq(0, 120, by = 20)
outlier_los_logged_binned <- cut(outlier_los_logged, breaks = bins, right = TRUE, include.lowest = TRUE)
outlier_count_logged <- table(outlier_los_logged_binned)
outlier_count_logged

#now when i see my outlier_count_logged, i see that 241 patients who are considered to be outliers in (log of) total charges, all have a LOS of 0-20 days, which is interesting as one might think that if a patient is paying a high amount of charge, it may be due to them having a high length of stay in the hospital (which would also include higher costs of doctor's and nurse's care for that longer LOS). so this is an interesting and notable finding



#question 2
#trying to create a new dataset with my chosen potential x variables against logged length of stay

new.data <- c("Age.Group", "Gender", "Type.of.Admission", "APR.Severity.of.Illness.Code", "Length.of.Stay", "Emergency.Department.Indicator", "Race", "APR.DRG.Description", "Payment.Typology.1", "Payment.Typology.2", "Payment.Typology.3", "Length.of.Stay.logged")

new_hospital.data <- hospital.data %>%
select(all_of(new.data))

str(new_hospital.data)



#question 3

#let me first split up the new hospital data (from qn 2) in my train and test sets

set.seed(4260)  
split <- sample.split(new_hospital.data$Length.of.Stay.logged, SplitRatio = 0.7)
train_data <- subset(new_hospital.data, split == TRUE)
train_data
test_data <- subset(new_hospital.data, split == FALSE)
test_data

#my lin reg model with all the variables i had chosen in qn 2

linear_model <- lm(Length.of.Stay.logged ~ Age.Group + Gender + Type.of.Admission + APR.Severity.of.Illness.Code + Emergency.Department.Indicator + Race + APR.DRG.Description + Payment.Typology.1 + Payment.Typology.2 + Payment.Typology.3, data = train_data)

#to optimise the model, let us now check for multicollinearity between the variables 

vif_values <- vif(linear_model)
vif_values
#no multicollinearity here, all gvif values are below 10

# second way to optimise my lm model, ill be doing stepwise regression using AIC here

optimised_lm <- step(linear_model, direction = "both")

#seeing the AIC of -16175.25, i see that here it is telling me that i should remove gender and emergency department indicator

#so updated model is as follows (without gender and emergency department indicator)

new_optimised_lm <- lm(Length.of.Stay.logged ~ Age.Group + APR.Severity.of.Illness.Code + Type.of.Admission + Race + APR.DRG.Description + Payment.Typology.1 + Payment.Typology.2 + Payment.Typology.3, data = train_data)

#can finally predict on test set, with new optimised lm

test_predictions <- predict(new_optimised_lm, test_data)

#rmse value for logged LOS

rmse_logged <- sqrt(mean((test_predictions - test_data$Length.of.Stay.logged)^2, na.rm = TRUE))
rmse_logged #value is 0.6553759

#rmse for un-logged values (what my RMSE would have been if i had not logged LOS - it would have been 9.815472, which is incredibly higher than what my RMSE is with logged LOS)
#un log it
#test_predictions_original <- exp(test_predictions)
#test_actuals_original <- exp(test_data$Length.of.Stay.logged)
#rmse_test <- sqrt(mean((test_predictions_original - test_actuals_original)^2, na.rm = TRUE))
#rmse_test

#lin reg over!



#for cart: LOS is also kept logged here as if it is not logged and the model is run with the right-skewed LOS, the RMSE value would be around 9.28 (a LOT higher than what my RMSE value is, when LOS is logged)

cart_model <- rpart(Length.of.Stay.logged ~ Age.Group + Gender + Type.of.Admission + APR.Severity.of.Illness.Code + Emergency.Department.Indicator + Race + APR.DRG.Description + Payment.Typology.1 + Payment.Typology.2 + Payment.Typology.3, data = train_data, method = "anova")

#pruning seq and 10 fold cv errors in table form
printcp(cart_model)  

#looking at what the best cp can be 

best_cp <- cart_model$cptable[which.min(cart_model$cptable[, "xerror"]), "CP"]

#prune the tree according to the best cp found before

pruned_cart_model <- prune(cart_model, cp = best_cp)

#plotting my tree

rpart.plot(pruned_cart_model, cex = 0.7)

#finding rmse

predictions_cart <- predict(pruned_cart_model, newdata = test_data)
rmse_cart_logged <- sqrt(mean((predictions_cart - test_data$Length.of.Stay.logged)^2, na.rm = TRUE))
rmse_cart_logged #it is 0.6632084

sum(pruned_cart_model$frame$var == "<leaf>") #6 leaf nodes

#cart over!!
#qn 3 over ;D