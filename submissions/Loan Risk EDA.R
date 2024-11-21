library(readr)
library(dplyr)

loans <- Loan_prediction_mini_dataset

#checking for the number of missing values per column
colSums(is.na(loans)) 

# Remove rows with missing values in specific columns
loans <- loans [complete.cases(loans$Emp_length, loans$Rate), ]

library(ggplot2)

#plotting the distribution of loan amounts
viz1 <- ggplot(loans, aes(x=Amount)) +
  geom_histogram(binwidth = 50, fill = 'pink', color = 'black') +
  labs(title = 'Loan Amounts Distribution', x = 'Loan Amount', y = 'Frequency')
print(viz1)

#plotting loan status trends
viz2 <- ggplot(loans, aes(x = factor(Status), fill = factor(Status))) +
  geom_bar() +
  scale_fill_manual(values =c('0' = 'red', '1' = 'green')) +
  labs(title = 'Loan Approval Status', x = 'Approval Status', y = 'Count')+
  theme_minimal()
print(viz2)

viz3 <- ggplot(loans, aes(x = factor(Status), y = Amount, fill = factor(Status)))+
  geom_violin()+
  scale_fill_manual(values =c('0' = 'red', '1' = 'green')) +
  labs(title = 'Loan Amounts by Approval Status', x = 'Approval Status', y = 'Loan Amount')
print(viz3)

str(loans$Default)
# Convert 'Y' to 1 (default) and 'N' to 0 (no default)
loans$Default <- ifelse(loans$Default == "Y", 1, ifelse(loans$Default == "N", 0, NA))
str(loans$Default)

# Calculating overall default rate
default_rate <- mean(loans$Default, na.rm = TRUE)  
default_rate * 100  # To get the percentage

#Bar Plot for Default Rate by Approval Status:
viz4 <- ggplot(loans, aes(x = factor(Status), fill = factor(Default))) +
  geom_bar(position = "fill") +  
  scale_fill_manual(values = c("0" = "green", "1" = "red")) +
  labs(title = "Default Rate by Loan Approval Status", 
       x = "Loan Approval Status", 
       y = "Proportion of Defaults") +
  scale_x_discrete(labels = c("Not Approved", "Approved")) +
  theme_minimal()

write_csv(loans, "loans_cleaned.csv")

