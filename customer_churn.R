##### GENERAL INFORMATION ########################################################################
# MODEL: BANK CUSTOMER CHURN PREDICTION 
# DATA SOURCE: https://www.kaggle.com/code/kmalit/bank-customer-churn-prediction/data
# PREPARED BY: OLAWALE FRANCIS ONAOLAPO (D3598469)

##################### NOT NEEDED AGAIN ###########################################################
#if (!require("ROSE")) { install.packages("ROSE", dependencies = TRUE); library(ROSE) }
#if (!require("smotefamily")) { install.packages("smotefamily", dependencies = TRUE); library(smotefamily) }

##### SETTING THE WORKING DIRECTORY ##############################################################
setwd("C:/Users/OMEN 16/Desktop/dsfica")
getwd()

##### LOADING THE REQUIRED PACKAGES ##############################################################
if (!require("tidyverse")) { install.packages("tidyverse", dependencies = TRUE); library(tidyverse) } else { library(tidyverse) }
if (!require("corrplot")) { install.packages("corrplot", dependencies = TRUE); library(corrplot) } else { library(corrplot) }
if (!require("car")) { install.packages("car", dependencies = TRUE); library(car) } else { library(car) }
if (!require("caret")) { install.packages("caret", dependencies = TRUE); library(caret) } else { library(caret) }
if (!require("pROC")) { install.packages("pROC", dependencies = TRUE); library(pROC) } else { library(pROC) }
if (!require("reshape2")) { install.packages("reshape2", dependencies = TRUE); library(reshape2) } else { library(reshape2) }
if (!require("gridExtra")) { install.packages("gridExtra", dependencies = TRUE); library(gridExtra) } else { library(gridExtra) }
if (!require("MASS")) { install.packages("MASS", dependencies = TRUE); library(MASS) } else { library(MASS) }
if (!require("grid")) { install.packages("grid", dependencies = TRUE); library(grid) } else { library(grid) }
if (!require("DMwR")) { install.packages("DMwR", dependencies = TRUE); library(DMwR) } else { library(DMwR) }

##### IMPORTING THE CUSTOMER CHURN DATASET ########################################################
customer_churn <- read_csv("churn_modelling.csv")

##### DATA PREPARATION - DATA CLEANING / DATA PREPROCESSING (1) ###################################
# TO VIEW THE IMPORTED DATASET 
View(customer_churn) 

# TO CHECK THE NUMBER OF ROWS AND COLUMNNS IN THE IMPORTED DATASET
dim(customer_churn)

# TO CHECK FOR DUPLICATES
sum(duplicated(customer_churn)) # No duplicate

# TO CHECK FOR DUPLICATES WITH THE CUSTOMER ID
length(unique(customer_churn$CustomerId)) # No duplicate - 10000 unique IDs

# TO CHECK FOR MISSING VALUES
colSums(is.na(customer_churn)) # No missing value

# TO CHECK THE STRUCTURE SUMMARY
str(customer_churn) 

# TO CHECK THE DESCRIPTIVE SUMMARY
summary(customer_churn) 

# TO CHECK THE NUMBER OF UNIQUE VALUES IN THE DEPENDENT VARIABLE
length(unique(customer_churn$Exited)) # TO VERIFY THE NUMBERS OF UNIQUE VALUES IN THE EXITED COLUMN

# TO REMOVE SOME COLUMNS NOT NEEDED
customer_churn$RowNumber <- NULL      
customer_churn$CustomerId <- NULL     
customer_churn$Surname <- NULL  

# TO CONVERT CATEGORICAL VARIABLES IN NUMERIC TO FACTOR
customer_churn$HasCrCard <- as.factor(customer_churn$HasCrCard) 
customer_churn$IsActiveMember <- as.factor(customer_churn$IsActiveMember) 
customer_churn$Exited <- as.factor(customer_churn$Exited) 

str(customer_churn)

##### EXPLORATORY DATA ANALYSIS (1) ###########################################################################
# DETECTING OUTLIERS USING Z-SCORES WITH 3 STANDARD DEVIATIONS FROM THE MEAN
customer_churn_numeric <- customer_churn %>%                    # Selecting the numeric columns
  select_if(is.numeric)                                          

customer_churn_z_scores <- scale(customer_churn_numeric)         # Calculate z-scores

customer_churn_outliers <- lapply(1:ncol(customer_churn_z_scores), function(x) {
  
  outlier_indices <- which(abs(customer_churn_z_scores[, x]) > 3) # Get the indices of the outliers
  
  outlier_values <- customer_churn_numeric[outlier_indices, x]    # Get the values of the outliers
  
  data.frame(Row = outlier_indices, Value = outlier_values)       # Return both the row numbers and values
})

customer_churn_outliers      # To show the outliers and their row number

# TO CHECK FOR MULTICOLLINEARITY
# Correlation between numerical variables 
customer_churn_num_corr <- cor(customer_churn_numeric)
corrplot(customer_churn_num_corr, main = "\nNumerical Features Correlation Plot", type = "lower", 
         method = "number", cl.cex = 0.5)

# Using Variance Inflation Factor (VIF) to detect multicollinearity on a generalized linear model of the dataset
set.seed(42)
customer_churn_vif_model <- glm(Exited ~ ., data = customer_churn, family = binomial)
vif(customer_churn_vif_model)           # To calculate the GVIF^(1/(2*Df))

# TO PLOT THE VIF
plot_customer_churn_vif <- vif(customer_churn_vif_model)

plot_customer_churn_vif <- data.frame(plot_customer_churn_vif)

colnames(plot_customer_churn_vif)[3]<- "customer_churn_independent_features_vif"

ggplot(plot_customer_churn_vif, aes(x=rownames(plot_customer_churn_vif), y=(customer_churn_independent_features_vif)))+ 
  geom_bar(stat = "identity", fill = "tan") +
  geom_text(aes(label = round(customer_churn_independent_features_vif, 2)), , vjust = -0.5) +
  labs(title = "Variance Inflation Factor (VIF)", x = "Independent Features", y = "GVIF^(1/(2*Df))") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1.2))  

##### EXPLORATORY DATA ANALYSIS (2) ##### UNIVARIATE ANALYSIS #########################################################
# TO CHECK THE DATA DISTRIBUTION FOR THE CATEGORICAL VARIABLES / VARIABLES WITH QUALITATIVE DATA

# To check the gender data distribution 
customer_churn %>%
  ggplot(aes(x = Gender)) +
  geom_bar(position = "dodge", alpha = 0.5, fill = "#52beda") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Gender Distribution", x = "Gender", y = "Frequency")

# To check the geography data distribution 
customer_churn %>%
  ggplot(aes(x = Geography)) +
  geom_bar(position = "dodge", alpha = 0.5, fill = "#52beda") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Distribution across the Geography", x = "Geography", y = "Frequency")

# To check the data distribution for credit card ownership
customer_churn %>%
  ggplot(aes(x = HasCrCard)) +
  geom_bar(position = "dodge", alpha = 0.5, fill = "#52beda") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Credit Card Ownership Distribution", x = "Credit Card Ownership", y = "Frequency")+
  scale_x_discrete(labels = c("0" = "No credit card", "1" = "Has credit card"))

# To check the data distribution for the customer activeness
customer_churn %>%
  ggplot(aes(x = IsActiveMember)) +
  geom_bar(position = "dodge", alpha = 0.5, fill = "#52beda") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Customer Activeness Distribution", x = "Customer Activeness", y = "Frequency")+
  scale_x_discrete(labels = c("0" = "Not Active", "1" = "Active"))

# To check the customer exit distribution 
customer_churn %>%
  ggplot(aes(x = Exited)) +
  geom_bar(position = "dodge", alpha = 0.5, fill = "#52beda") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Customer Exit Status Distribution", x = "Customer Exit Status", y = "Frequency")+
  scale_x_discrete(labels = c("0" = "Not Exit", "1" = "Exit"))

##### EXPLORATORY DATA ANALYSIS (3) ##### UNIVARIATE ANALYSIS ################################################
# TO CHECK THE DATA DISTRIBUTION FOR THE QUANTITATIVE VARIABLES USING HISTOGRAM AND BOX PLOT

##### USING HISTOGRAM #####
# Histogram illustrating the number of products distribution
customer_churn %>%
  ggplot(aes(x = NumOfProducts)) +
  geom_histogram(binwidth = 1, color = "white", fill = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of Products Distribution", x = "Number of Products", y = "Frequency")

# Histogram illustrating the tenure distribution
customer_churn %>%
  ggplot(aes(x = Tenure)) +
  geom_histogram(binwidth = 2, color = "white", fill = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Tenure Distribution", x = "Tenure", y = "Frequency")

# Histogram illustrating the age distribution
customer_churn %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 10, color = "white", fill = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Histogram illustrating the credit score distribution
customer_churn %>%
  ggplot(aes(x = CreditScore)) +
  geom_histogram(binwidth = 75, color = "white", fill = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Credit Score Distribution", x = "Credit score", y = "Frequency")

# Histogram illustrating the estimated salary distribution
customer_churn %>%
  ggplot(aes(x = EstimatedSalary)) +
  geom_histogram(binwidth = 10000, color = "white", fill = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Estimated Salary Distribution", x = "Estimated salary", y = "Frequency")

# Histogram illustrating the account balance distribution
customer_churn %>%
  ggplot(aes(x = Balance)) +
  geom_histogram(binwidth = 20000, color = "white", fill = "#023972") +
  scale_x_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Account Balance Distribution", x = "Balance", y = "Frequency")

######## USING BOXPLOT #####
# THE OUTLIERS DETECTED BY THE BOXPLOT CHECKED AND WERE NOT OUTLIERS IN THIS CONTEXT

# Boxplot illustrating the age distribution
customer_churn %>%
  ggplot(aes(x = Age)) +
  geom_boxplot(color = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Age Distribution", x = "Age", y = "Density")

# Boxplot illustrating the tenure distribution
customer_churn %>%
  ggplot(aes(x = Tenure)) +
  geom_boxplot(color = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Tenure Distribution", x = "Tenure", y = "Density")

# Boxplot illustrating the account balance distribution
customer_churn %>%
  ggplot(aes(x = Balance)) +
  geom_boxplot(color = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Account Balance Distribution", x = "Balance", y = "Density")

# Boxplot illustrating the distribution for the number of products by the customer
customer_churn %>%
  ggplot(aes(x = NumOfProducts)) +
  geom_boxplot(color = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Distribution of the Number of Products", x = "Number of products", y = "Density")

# Boxplot illustrating the distribution for the estimated salary by the customer
customer_churn %>%
  ggplot(aes(x = EstimatedSalary)) +
  geom_boxplot(color = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Estimated Salary Distribution", x = "Estimated salary", y = "Density")

# Boxplot illustrating the credit score distribution
customer_churn %>%
  ggplot(aes(x = CreditScore)) +
  geom_boxplot(color = "#023972") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Credit Score Distribution", x = "Credit score", y = "Density")

##### EXPLORATORY DATA ANALYSIS (4) ##### BIVARIATE ANALYSIS ######################################################
# TO CHECK THE EXIT STATUS DISTRIBUTION WITH REFERENCE TO BOTH THE CATEGORICAL FEATURES AND THE NUMERICAL FEATURES

##### BIVARIATE ANALYSIS ### EXIT STATUS DISTRIBUTION WITH REFERENCE TO THE CATEGORICAL FEATURES ####

# To check the the Exit Distribution with regards to the gender
customer_churn %>%
  filter(Gender == "Female" | Gender == "Male") %>%
  ggplot(aes(x = Gender, fill = (Exited))) +
  geom_bar(position = "dodge", alpha = 0.5) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Gender Exit Distribution",
       x = "Gender", y = "Frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited"))      

# To check the the normalized Exit Distribution with regards to the gender
customer_churn %>%
  filter(Gender == "Female" | Gender == "Male") %>%
  ggplot(aes(x = Gender, fill = (Exited))) +
  geom_bar(position = "fill", alpha = 0.5) +    
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Gender Exit Distribution (Normalized)", fill = "Exit Status",
       x = "Gender", y = "Normalized frequency")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited"))     

# To check the the Exit Distribution with regards to geography
customer_churn %>%
  filter(Geography == "France" | Geography == "Spain" | Geography == "Germany") %>%
  ggplot(aes(x = Geography, fill = (Exited))) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Geography Exit Distribution",
       x = "Geography", y = "Frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited"))  

# To check the the normalized Exit Distribution with regards to the geography
customer_churn %>%
  filter(Geography == "France" | Geography == "Spain" | Geography == "Germany") %>%
  ggplot(aes(x = Geography, fill = (Exited))) +
  geom_bar(position = "fill", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11)) +
  labs(title = "Geography Exit Distribution (Normalized)",
       x = "Geography", y = "Normalized frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited"))  

# To check the the Exit Distribution of the customer activeness status
customer_churn %>%
  filter(IsActiveMember == 1 | IsActiveMember == 0) %>%
  ggplot(aes(x = IsActiveMember, fill = (Exited))) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Customer Activeness Exit Distribution",
       x = "Customer Activeness Status", y = "Frequency", fill = "Exit Status")+
  scale_x_discrete(labels = c("0" = "Not Active", "1" = "Active"))+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited"))  

# To check the the normalized Exit Distribution of the customer activeness status
customer_churn %>%
  filter(IsActiveMember == 1 | IsActiveMember == 0) %>%
  ggplot(aes(x = IsActiveMember, fill = (Exited))) +
  geom_bar(position = "fill", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10.5)) +
  labs(title = "Customer Activeness Exit Distribution (Normalized)",
       x = "Customer Activeness", y = "Normalized frequency", fill = "Exit Status")+
  scale_x_discrete(labels = c("0" = "Not Active", "1" = "Active"))+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the the Exit Distribution of the customer who has credit card
customer_churn %>%
  filter(HasCrCard == 1 | HasCrCard == 0) %>%
  ggplot(aes(x = HasCrCard, fill = (Exited))) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12)) +
  labs(title = "Credit Card Possession Exit Distribution",
       x = "Credit card possession status", y = "Frequency", fill = "Exit Status")+
  scale_x_discrete(labels = c("0" = "No credit card", "1" = "Has credit card"))+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the the normalized Exit Distribution of the customer who has credit card
customer_churn %>%
  filter(HasCrCard == 1 | HasCrCard == 0) %>%
  ggplot(aes(x = HasCrCard, fill = (Exited))) +
  geom_bar(position = "fill", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10)) +
  labs(title = "Credit Card Possession Exit Distribution(Normalized)",
       x = "Credit card possession status", y = "Normalized frequency", fill = "Exit Status")+
  scale_x_discrete(labels = c("0" = "No credit card", "1" = "Has credit card"))+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

##### BIVARIATE ANALYSIS ### EXIT STATUS DISTRIBUTION WITH REFERENCE TO THE QUANTITATIVE FEATURES ### USING BOXPLOT #####
# To check the exit status distribution across age using a boxplot
customer_churn %>%
  ggplot(aes(x = (Exited), y = Age, fill = (Exited))) +  
  geom_boxplot() +                                        
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) + 
  scale_x_discrete(labels = c("0" = "Not Exited", "1" = "Exited"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Age Exit Status Distribution",
       x = "Exit Status", y = "Age", fill = "Exit Status")

# To check the exit status distribution across credit score using boxplot
customer_churn %>%
  ggplot(aes(x = (Exited), y = CreditScore, fill = (Exited))) +  
  geom_boxplot() +                                        
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) + 
  scale_x_discrete(labels = c("0" = "Not Exited", "1" = "Exited"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Credit Score Exit Status Distribution",
       x = "Exit Status", y = "Credit Score", fill = "Exit Status")

# To check the exit status distribution for the account balance using boxplot
customer_churn %>%
  ggplot(aes(x = (Exited), y = Balance, fill = (Exited))) +  
  geom_boxplot() +                                        
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) + 
  scale_x_discrete(labels = c("0" = "Not Exited", "1" = "Exited"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Balance Exit Status Distribution",
       x = "Exit Status", y = "Balance", fill = "Exit Status")

# To check the exit status distribution for the estimated salary using boxplot
customer_churn %>%
  ggplot(aes(x = (Exited), y = EstimatedSalary, fill = (Exited))) +  
  geom_boxplot() +                                        
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) + 
  scale_x_discrete(labels = c("0" = "Not Exited", "1" = "Exited"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Estimated Salary Exit Status Distribution",
       x = "Exit Status", y = "Estimated Salary", fill = "Exit Status")

# To check the exit status distribution for the number of products using boxplot
customer_churn %>%
  ggplot(aes(x = (Exited), y = NumOfProducts, fill = (Exited))) +  
  geom_boxplot() +                                        
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) + 
  scale_x_discrete(labels = c("0" = "Not Exited", "1" = "Exited"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number Of Products Exit Status Distribution",
       x = "Exit Status", y = "Number Of Products", fill = "Exit Status")

# To check the exit status distribution for the tenure using boxplot
customer_churn %>%
  ggplot(aes(x = (Exited), y = Tenure, fill = (Exited))) +  
  geom_boxplot() +                                        
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) + 
  scale_x_discrete(labels = c("0" = "Not Exited", "1" = "Exited"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Tenure Exit Status Distribution",
       x = "Exit Status", y = "Tenure", fill = "Exit Status")

##### BIVARIATE ANALYSIS ### EXIT STATUS DISTRIBUTION WITH REFERENCE TO THE QUANTITATIVE FEATURES ##### USING HISTOGRAM #####
# To check the exit status distribution for the credit score using histogram
customer_churn %>%
  ggplot(aes(x = CreditScore, fill = (Exited))) +
  geom_histogram(binwidth = 75, position = 'dodge', color = "white") +
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12)) +
  labs(title = "Credit Score Exit Status Distribution",
       x = "Credit score", y = "Frequency", fill = "Exit Status")

# To check the normalized exit status distribution for the credit score using histogram
customer_churn %>%
  ggplot(aes(x = CreditScore, fill = (Exited))) +
  geom_histogram(binwidth = 75, position = 'fill', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11)) +
  labs(title = "Credit Score Exit Status Distribution (Normalized)",
       x = "Credit score", y = "Normalized freuency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the exit status distribution for the age using histogram
customer_churn %>%
  ggplot(aes(x = Age, fill = (Exited))) +
  geom_histogram(binwidth = 10, position = 'dodge', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Age Exit Status Distribution",
       x = "Age", y = "Frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the normalized exit status distribution for the age using histogram
customer_churn %>%
  ggplot(aes(x = Age, fill = (Exited))) +
  geom_histogram(binwidth = 10, position = 'fill', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Age Exit Status Distribution (Normalized)",
       x = "Age", y = "Normalized frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the exit status distribution for the tenure using histogram
customer_churn %>%
  ggplot(aes(x = Tenure, fill = (Exited))) +
  geom_histogram(binwidth = 2, position = 'dodge', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Tenure Exit Status Distribution",
       x = "Tenure", y = "Frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the normalized exit status distribution for the tenure using histogram
customer_churn %>%
  ggplot(aes(x = Tenure, fill = (Exited))) +
  geom_histogram(binwidth = 2, position = 'fill', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12)) +
  labs(title = "Tenure Exit Status Distribution (Normalized)",
       x = "Tenure", y = "Normalized frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the exit status distribution for the account balance using histogram  
customer_churn %>%
  ggplot(aes(x = Balance, fill = (Exited))) +
  geom_histogram(binwidth = 20000, position = 'dodge', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Account Balance Exit Status Distribution",
       x = "Account balance", y = "Frequency", fill = "Exit Status")+
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the normalized exit status distribution for the account balance using histogram  
customer_churn %>%
  ggplot(aes(x = Balance, fill = (Exited))) +
  geom_histogram(binwidth = 20000, position = 'fill', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10)) +
  labs(title = "Account Balance Exit Status Distribution (Normalized)",
       x = "Account balance", y = "Normalized frequency", fill = "Exit Status")+
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the exit status distribution for the estimated salary using histogram
customer_churn %>%
  ggplot(aes(x = EstimatedSalary, fill = (Exited))) +
  geom_histogram(binwidth = 10000, position = 'dodge', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Estimated Salary Exit Status Distribution",
       x = "Customer Estimated Salary", y = "Frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the normalized exit status distribution for the estimated salary using histogram
customer_churn %>%
  ggplot(aes(x = EstimatedSalary, fill = (Exited))) +
  geom_histogram(binwidth = 10000, position = 'fill', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10)) +
  labs(title = "Estimated Salary Exit Status Distribution (Normalized)",
       x = "Customer Estimated Salary", y = "Normalized frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the exit status distribution for the number of products using histogram
customer_churn %>%
  ggplot(aes(x = NumOfProducts, fill = (Exited))) +
  geom_histogram(binwidth = 1, position = 'dodge', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11)) +
  labs(title = "Number of Products Exit Status Distribution",
       x = "Number of Products", y = "Frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

# To check the normalized exit status distribution for the number of products using histogram
customer_churn %>%
  ggplot(aes(x = NumOfProducts, fill = (Exited))) +
  geom_histogram(binwidth = 1, position = 'fill', color = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.2, size = 10)) +
  labs(title = "Number of Products Exit Status Distribution (Normalized)",
       x = "Number of Products", y = "Normalized frequency", fill = "Exit Status")+
  scale_fill_manual(values = c("0" = "green", "1" = "red"), labels = c("Not Exited", "Exited")) 

##### TO CHECK STATISTICALLY SIGNIFICANT VARIABLES USING THE GLM FUNCTION ##################################### 
# To change the variable data type as required
customer_churn$Gender <- as.factor(customer_churn$Gender)
customer_churn$Geography <- as.factor(customer_churn$Geography)

str(customer_churn)

# Fitting the generalized linear model
set.seed(42)
customer_churn_feat_sel <- glm(Exited ~ ., data = customer_churn, family = binomial)

# Stepwise selection using AIC (backward selection)
customer_churn_feat_sel_step_model <- stepAIC(customer_churn_feat_sel, direction = "backward")

summary(customer_churn_feat_sel_step_model)   # Summary of the final model

# Extracting the summary of the final model
customer_churn_feat_sel_step_model_summary <- summary(customer_churn_feat_sel_step_model)

# Extracting coefficients and related statistics
customer_churn_feat_selection_coefs <- as.data.frame(customer_churn_feat_sel_step_model_summary$coefficients)
customer_churn_feat_selection_coefs <- customer_churn_feat_selection_coefs %>%
  rownames_to_column(var = "Variable") %>% 
  rename(Estimate = Estimate,
    Std_Error = `Std. Error`,
    Z_Value = `z value`,
    P_Value = `Pr(>|z|)`
  )

customer_churn_custom_theme <- ttheme_default(core = list(fg_params = list(cex = 0.5)),  
  colhead = list(fg_params = list(cex = 0.6)))

customer_churn_table_grob <- tableGrob(customer_churn_feat_selection_coefs, 
  rows = NULL, theme = customer_churn_custom_theme)

customer_churn_title_grob <- textGrob("Feature Selection Model P-Values", 
  gp = gpar(fontsize = 14, fontface = "bold"))

customer_churn_combined_grob <- grid.arrange(customer_churn_title_grob, 
  customer_churn_table_grob, nrow = 2, heights = c(0.2, 1))

#grid.newpage()  
#grid.draw(customer_churn_combined_grob)  

####################################################################
customer_churn$Exited <- factor(customer_churn$Exited, levels = c(0, 1), labels = c("Not.Exited", "Exited"))

customer_churn_features <- customer_churn[, !(names(customer_churn) %in% "Exited")] # Separate features and target variable
customer_churn_target <- customer_churn$Exited


customer_churn_numerical_columns <- sapply(customer_churn_features, is.numeric) # Numerical and categorical columns
customer_churn_categorical_columns <- !customer_churn_numerical_columns

#View(customer_churn_numerical_columns)
#View(customer_churn_categorical_columns)

# Define normalization function for numerical features using the min-max scaling method
customer_churn_normalize_function <- function(x) {return((x - min(x)) / (max(x) - min(x)))}

customer_churn_normalized_features <- as.data.frame(lapply(customer_churn_features[, customer_churn_numerical_columns], 
                                                           customer_churn_normalize_function)) # Normalize numerical features

# Apply one-hot encoding to categorical features
customer_churn_categorical_features <- customer_churn_features[, customer_churn_categorical_columns]
customer_churn_encoded_features <- as.data.frame(model.matrix(~ . - 1, data = customer_churn_categorical_features))

# Combine normalized numerical features and one-hot encoded categorical features
customer_churn_scaled_features <- cbind(customer_churn_normalized_features, customer_churn_encoded_features)

# Combine scaled features and target variable into a single data frame
customer_churn_scaled <- data.frame(customer_churn_scaled_features, Exited = customer_churn_target)

# Create the stacked bar chart with normalized frequencies before oversampling
ggplot(customer_churn_scaled, aes(x = "", fill = factor(Exited))) +  
  geom_bar(position = "fill", alpha = 0.8) +  
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +  
  labs(title = "Customer Exit Status Distribution Before Oversampling", 
       x = "Exit Status", y = "Normalized Frequency", fill = "Exit Status") +
  scale_fill_manual(values = c("green", "red"), labels = c("Not Exited", "Exited")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.3, size = 10))

# Apply SMOTE oversampling (200%)
set.seed(42)
customer_churn_smote <- SMOTE(Exited ~ ., data = customer_churn_scaled, K = 5, perc.over = 200)

# Create the stacked bar chart with normalized frequencies after oversampling
ggplot(customer_churn_smote, aes(x = "", fill = factor(Exited))) +  
  geom_bar(position = "fill", alpha = 0.8) +  
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +  
  labs(title = "Customer Exit Status Distribution After Oversampling", 
       x = "Exit Status", y = "Normalized Frequency", fill = "Exit Status") +
  scale_fill_manual(values = c("green", "red"), labels = c("Not Exited", "Exited")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.3, size = 10))

# Split data into training and testing sets (70:30 split)
set.seed(42)
customer_churn_intrain <- sample(1:nrow(customer_churn_smote), size = floor(0.7 * nrow(customer_churn_smote)), replace = FALSE)
customer_churn_train_data <- customer_churn_smote[customer_churn_intrain, ]
customer_churn_test_data <- customer_churn_smote[-customer_churn_intrain, ]
#View(customer_churn_train_data)

# Create the stacked bar chart with normalized frequencies for the training data after random sampling split
ggplot(customer_churn_train_data, aes(x = "", fill = factor(Exited))) +  
  geom_bar(position = "fill", alpha = 0.8) +  
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +  
  labs(title = "Customer Exit Status Distribution for the Training Data", 
       x = "Exit Status", y = "Normalized Frequency", fill = "Exit Status") +
  scale_fill_manual(values = c("green", "red"), labels = c("Not Exited", "Exited")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.3, vjust = 0.2, size = 10))

# Define train control for KNN
customer_churn_train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# To define the range of k values for grid search
customer_churn_k_values <- data.frame(k = seq(1, 10, by = 2))

# Train the KNN model
set.seed(42)
customer_churn_knn_model <- train(Exited ~ ., data = customer_churn_train_data, method = "knn",
  trControl = customer_churn_train_control, tuneGrid = customer_churn_k_values)

customer_churn_best_k <- customer_churn_knn_model$bestTune$k
cat("Optimal k:", customer_churn_best_k, "\n") # Best k-value

# Predictions on the test dataset
customer_churn_predicted_probs <- predict(customer_churn_knn_model, customer_churn_test_data, type = "prob")
customer_churn_predictions <- predict(customer_churn_knn_model, customer_churn_test_data)

# Evaluate the model
customer_churn_conf_matrix <- confusionMatrix(customer_churn_predictions, customer_churn_test_data$Exited, positive = "Exited")
print(customer_churn_conf_matrix)

# To output customer_churn_coef_matrix as image
customer_churn_stats_text <- capture.output(print(customer_churn_conf_matrix))
customer_churn_stats_grob <- textGrob(paste(customer_churn_stats_text, 
                                            collapse = "\n"), x = 0.5, y = 0.5, just = "center", gp = gpar(fontsize = 7)) 
grid.newpage()
grid.draw(customer_churn_stats_grob)

# Extract metrics
customer_churn_accuracy <- customer_churn_conf_matrix$overall["Accuracy"]
customer_churn_precision <- customer_churn_conf_matrix$byClass["Precision"]
customer_churn_recall <- customer_churn_conf_matrix$byClass["Recall"]
customer_churn_f1_score <- customer_churn_conf_matrix$byClass["F1"]
customer_churn_specificity <- customer_churn_conf_matrix$byClass["Specificity"]
cat("Accuracy:", customer_churn_accuracy, "\nPrecision:", customer_churn_precision, 
    "\nRecall:", customer_churn_recall, "\nF1-Score:", customer_churn_f1_score, 
    "\nSpecificity:", customer_churn_specificity, "\n")

# To Plot the Confusion Matrix
customer_churn_conf_matrix_melt <- melt(as.table(customer_churn_conf_matrix$table))
customer_churn_conf_matrix_melt
colnames(customer_churn_conf_matrix_melt) <- c("Predicted", "Actual", "Count")
ggplot(customer_churn_conf_matrix_melt, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = paste("Customer Churn Confusion Matrix for KNN (k =", customer_churn_best_k, ")"),
       x = "Predicted Class", y = "Actual Class") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.3, size = 11))

# Plot AUROC curve
customer_churn_roc_curve <- roc(customer_churn_test_data$Exited, customer_churn_predicted_probs[, "Exited"], 
  levels = rev(levels(customer_churn_test_data$Exited)), direction = ">")
customer_churn_auc_value <- auc(customer_churn_roc_curve)
customer_churn_roc_plot <- ggplot(data.frame(FPR = 1 - customer_churn_roc_curve$specificities, 
             TPR = customer_churn_roc_curve$sensitivities)) +
  geom_line(aes(x = FPR, y = TPR), color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  annotate("text", x = 0.80, y = 0.60, label = paste("AUC =", round(customer_churn_auc_value, 2)), 
           color = "black", size = 3, fontface = "bold") +
  annotate("text", x = 0.80, y = 0.50, label = paste("Accuracy =", round(customer_churn_accuracy, 2)), 
           color = "black", size = 3, fontface = "bold") +
  annotate("text", x = 0.80, y = 0.40, label = paste("Precision =", round(customer_churn_precision, 2)), 
           color = "black", size = 3, fontface = "bold") +
  annotate("text", x = 0.80, y = 0.30, label = paste("Recall =", round(customer_churn_recall, 2)), 
           color = "black", size = 3, fontface = "bold") +
  annotate("text", x = 0.80, y = 0.20, label = paste("F1-Score =", round(customer_churn_f1_score, 2)), 
           color = "black", size = 3, fontface = "bold") +
  annotate("text", x = 0.80, y = 0.10, label = paste("Specificity =", round(customer_churn_specificity, 2)), 
           color = "black", size = 3, fontface = "bold") +
  labs(title = paste("Customer Churn AUROC Curve for KNN Model (k = ", customer_churn_best_k, ")"), 
    x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1, size = 11))

print(customer_churn_roc_plot)