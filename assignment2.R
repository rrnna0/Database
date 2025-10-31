install.packages("randomForest")
install.packages("readr")
install.packages("ggplot2")
install.packages("imputeTS")
install.packages("gridExtra")
install.packages("randomForest")
install.packages("moments")
install.packages("dplyr")
install.packages("scales")
install.packages("ggridges")
install.packages("writexl")
install.packages("tidyverse")
install.packages("fastDummies")
install.packages("moments")
install.packages("dplyr")
install.packages("caret")
install.packages("recipes")
install.packages("themis")
install.packages("janitor")
install.packages("corrplot")
install.packages("smotefamily")

library(smotefamily)
library(corrplot)
library(janitor)
library(themis) 
library(recipes)
library(caret)
library(writexl)
library(dplyr)
library(moments)
library(randomForest)
library(gridExtra)
library(imputeTS)
library(readr)
library(randomForest)
library(ggplot2)
library(scales)
library(ggridges)
library(tidyverse)       
library(fastDummies)     
library(moments)         
library(dplyr)

# -----------------------------------------------------------------------------------------------------
#   IMPORT AND MERGE DATA
# -----------------------------------------------------------------------------------------------------

# Define file paths
file1_path <- "C:/Users/user/Downloads/bank+marketing (1)/bank/bank-full.csv"
file2_path <- "C:/Users/user/Downloads/bank+marketing (1)/bank/bank.csv"

# Read the datasets (semicolon-delimited format)
bankfull1 <- read.csv2(file1_path)
bankfull2 <- read.csv2(file2_path)

# Align column names (for consistent merge)
colnames(bankfull2) <- colnames(bankfull1)

# Merge both datasets and remove duplicate rows
bank_data <- distinct(rbind(bankfull1, bankfull2))

View(bank_data)
str(bank_data)
summary(bank_data)

# -----------------------------------------------------------------------------------------------------
#  Section 1: Data Exploration and Analysis
# ----------------------------------------------------------------------------------------------------

bank_data <- bank_data %>%
  mutate(y_subscription = factor(y, levels = c(1, 2), labels = c("Yes", "No")))


ggplot(bank_data, aes(x = y_subscription, fill = y_subscription)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(title = "Distribution of Term Deposit Subscriptions",
       x = "Did the Customer Subscribe?",
       y = "Count of Customers",
       fill = "Subscribed?") + 
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

job_labels <- c("1" = "Admin", "2" = "Blue-Collar", "3" = "Entrepreneur",
                "4" = "Housemaid", "5" = "Management", "6" = "Retired",
                "7" = "Self-Employed", "8" = "Services", "9" = "Student",
                "10" = "Technician", "11" = "Unemployed")

bank_data <- bank_data %>%
  mutate(job_factor = factor(job, levels = 1:11, labels = job_labels))

ggplot(bank_data, aes(x = job_factor, fill = y_subscription)) +
  geom_bar(position = "fill") +
  labs(title = "Subscription Rate by Job Type",
       x = "Job Type",
       y = "Proportion of Customers",
       fill = "Subscription") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() # Flip coordinates to make job labels easier to read

#Age Distribution graphs
ggplot(bank_data, aes(x = age, fill = y_subscription)) +
  geom_density(alpha = 0.6) + # alpha makes the plots semi-transparent
  labs(title = "Age Distribution by Subscription Outcome",
       x = "Age of Customer",
       y = "Density",
       fill = "Subscription") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggplot(bank_data, aes(x = y_subscription, y = duration_log, fill = y_subscription)) +
  geom_boxplot() +
  labs(title = "Call Duration vs. Subscription Outcome",
       x = "Did the Customer Subscribe?",
       y = "Log-Transformed Call Duration (seconds)") +
  theme_minimal() +
  guides(fill = "none") # Hide the legend as the x-axis is clear enough



marital_labels <- c("1" = "Divorced", "2" = "Married", "3" = "Single")
bank_data <- bank_data %>%
  mutate(marital_factor = factor(marital, levels = 1:3, labels = marital_labels))

# Faceted histogram
ggplot(bank_data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ marital_factor) +
  labs(title = "Age Distribution by Marital Status",
       x = "Age of Customer",
       y = "Count of Customers") +
  theme_bw()


bank_data <- bank_data %>%
  mutate(y_subscription = factor(y, levels = c(1, 2), labels = c("Yes", "No")))

ggplot(bank_data, aes(x = y_subscription, y = campaign_log, fill = y_subscription)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Number of Campaign Contacts vs. Subscription Outcome",
       x = "Did the Customer Subscribe?",
       y = "Log-Transformed Number of Contacts",
       fill = "Subscribed?") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")



economic_cols <- bank_data %>%
  select(emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed) %>%
  na.omit()


cor_matrix_econ <- round(cor(economic_cols), 2)
melted_cor_econ <- melt(cor_matrix_econ)

ggplot(melted_cor_econ, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "#377EB8", high = "#E41A1C", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(title = "Correlation Matrix of Economic Indicators", x = "", y = "")

# --- Previous Campaign Outcome ---

plot_data_poutcome <- bank_data %>%
  mutate(poutcome_plot_category = case_when(
    poutcome == 1 ~ "Success",
    poutcome == 2 ~ "Failure",
    poutcome == 3 ~ "Other",
    is.na(poutcome) ~ "No Previous Contact",
    TRUE ~ "Unknown" # A fallback just in case
  ))

ggplot(plot_data_poutcome, aes(x = poutcome_plot_category, fill = y_subscription)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Subscription Rate by Previous Campaign Outcome",
       x = "Outcome of Previous Campaign",
       y = "Proportion of Customers",
       fill = "Subscribed?") +
  theme_minimal() +
  coord_flip()


# --- Housing vs. Personal Loans ---
bank_data <- bank_data %>%
  mutate(
    housing_factor = factor(housing,
                            levels = c(1, 2),
                            labels = c("Has Housing Loan", "No Housing Loan")),
    loan_factor = factor(loan,
                         levels = c(1, 2),
                         labels = c("Has Personal Loan", "No Personal Loan"))
  )

# Faceted bar plot, filtering out any NA values that may exist
# (These would be the original 'unknown' values that became NA)
ggplot(bank_data %>% filter(!is.na(housing_factor) & !is.na(loan_factor)),
       aes(x = y_subscription, fill = y_subscription)) +
  geom_bar() +
  facet_grid(housing_factor ~ loan_factor) +
  labs(title = "Subscription Count by Housing and Personal Loan Status",
       x = "Did the Customer Subscribe?",
       y = "Count of Customers",
       fill = "Subscribed?") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---Day of the Week ---

# Order the days of the week logically
day_levels <- c("mon", "tue", "wed", "thu", "fri")

# Create the proportional bar chart, filtering out the NAs from the first dataset
ggplot(bank_data %>% filter(!is.na(day_of_week)),
       aes(x = factor(day_of_week, levels = day_levels), fill = y_subscription)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Subscription Rate by Day of the Week",
       x = "Day of Contact",
       y = "Proportion of Subscriptions",
       fill = "Subscribed?") +
  theme_minimal()



# --- Balance by Education Level ---
# All other original categories (like 'high.school', 'basic.4y') became NA. - Needs to be fixed
bank_data <- bank_data %>%
  mutate(education_factor = case_when(
    education == 1    ~ "Primary",
    education == 2    ~ "Secondary",
    education == 3    ~ "Tertiary",
    is.na(education)  ~ "Other/Unknown" # This captures all other categories
  ))

# Filtered data to make the plot readable (remove NAs, outliers, and the 'Other' group)
plot_data_balance <- bank_data %>%
  filter(!is.na(balance) & balance > 0 & balance < 10000 & education_factor != "Other/Unknown")

#Violin plot
ggplot(plot_data_balance, aes(x = education_factor, y = balance, fill = education_factor)) +
  geom_violin() +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Distribution of Bank Balance by Education Level",
       subtitle = "For customers with balances between $0 and $10,000",
       x = "Education Level",
       y = "Bank Balance") +
  theme_light() +
  guides(fill = "none") + # Hide legend as x-axis is clear
  coord_flip()

# ---Age vs. Bank Balance ---(Requires scales)
# Removed NAs, people with a negative balance, and cap the balance at a reasonable level to avoid extreme outliers skewing the view
plot_data_scatter <- bank_data %>%
  filter(!is.na(balance) & balance > 0 & balance < 50000)

ggplot(plot_data_scatter, aes(x = age, y = balance, color = y_subscription)) +
  geom_point(alpha = 0.4, size = 1.5) + # Use alpha for transparency to see dense areas
  geom_smooth(method = "loess", se = FALSE, color = "black") + # Add a smoothed trendline
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Bank Balance Across Customer Age",
       subtitle = "Showing trendline for customers with balances under $50,000",
       x = "Customer Age",
       y = "Bank Balance",
       color = "Subscribed?") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# ---Default Rate by Marital Status ---
bank_data <- bank_data %>%
  mutate(default_factor = factor(default,
                                 levels = c(1, 2),
                                 labels = c("Has Defaulted", "No Default")))

ggplot(bank_data %>% filter(!is.na(marital_factor)),
       aes(x = marital_factor, fill = default_factor)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Credit Default Rate by Marital Status",
       x = "Marital Status",
       y = "Proportion of Customers",
       fill = "Credit Status") +
  theme_light() +
  scale_fill_manual(values = c("Has Defaulted" = "#E41A1C", "No Default" = "#377EB8"))

# ---Age Distribution by Job ---
#Ridge plot
ggplot(bank_data %>% filter(!is.na(job_factor)),
       aes(x = age, y = job_factor, fill = job_factor)) +
  geom_density_ridges() +
  labs(title = "Age Distribution Across Different Professions",
       x = "Customer Age",
       y = "Job Type") +
  theme_ridges() +
  theme(legend.position = "none") # Hide legend as y-axis is clear

# Subscription by Education -- EDUCATION CLEANING NEEDS TO BE REWORKED
education_summary <- bank_data %>%
  filter(!is.na(education_factor) & education_factor != "Other/Unknown") %>%
  group_by(education_factor) %>%
  summarise(
    subscription_rate = mean(y_subscription == "Yes", na.rm = TRUE),
    count = n()
  ) %>%
  arrange(subscription_rate) # Arrange by rate for a cleaner look

#Lollipop chart
ggplot(education_summary, aes(x = subscription_rate, y = reorder(education_factor, subscription_rate))) +
  geom_segment(aes(x = 0, yend = education_factor, xend = subscription_rate), color = "grey") +
  geom_point(color = "dodgerblue", size = 4) +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Term Deposit Subscription Rate by Education Level",
       x = "Subscription Rate",
       y = "Education Level") +
  theme_minimal()


# ---Balance vs. Duration ---
# Filter data for a clearer plot (positive balance, under $50k)
plot_data_bal_dur <- bank_data %>%
  filter(!is.na(balance) & balance > 0 & balance < 50000)

ggplot(plot_data_bal_dur, aes(x = duration_log, y = balance, color = y_subscription)) +
  geom_point(alpha = 0.3) + # Use transparency to see dense areas
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Subscription by Call Duration and Bank Balance",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Call Duration (seconds)",
       y = "Bank Balance",
       color = "Subscribed?") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

#Balance vs. Campaign Contacts
ggplot(plot_data_bal_dur, aes(x = campaign_log, y = balance, color = y_subscription)) +
  geom_point(alpha = 0.3) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Subscription by Campaign Contacts and Bank Balance",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Number of Campaign Contacts",
       y = "Bank Balance",
       color = "Subscribed?") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

#Campaign Contacts by Job Type ---

#Faceted box plots, filtering out NA job values
ggplot(bank_data %>% filter(!is.na(job_factor)),
       aes(x = job_factor, y = campaign_log, fill = job_factor)) +
  geom_boxplot() +
  facet_wrap(~ y_subscription, ncol = 1) + # Create separate panels for "Yes" and "No"
  coord_flip() + # Flip axes to make job titles readable
  labs(title = "Campaign Contact Distribution by Job and Subscription Outcome",
       x = "Job Type",
       y = "Log-Transformed Number of Contacts") +
  theme_bw() +
  theme(legend.position = "none")

#Balance vs. Duration for Subscribers ---
#Filtered data for SUBSCRIBED customers with a clear balance range
plot_data_subscribers <- bank_data %>%
  filter(y_subscription == "Yes", # <-- This is the key change
         !is.na(balance),
         balance > 0,
         balance < 50000)

ggplot(plot_data_subscribers, aes(x = duration_log, y = balance)) +
  geom_point(alpha = 0.5, color = "darkgreen") + # Color is now static
  geom_smooth(method = "loess", se = FALSE, color = "black") + # Optional trendline
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Balance vs. Call Duration for Subscribed Customers",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Call Duration (seconds)",
       y = "Bank Balance") +
  theme_minimal()


# --- Balance vs. Campaign Contacts for Subscribers ---

# We can use the same 'plot_data_subscribers' data frame created above
ggplot(plot_data_subscribers, aes(x = campaign_log, y = balance)) +
  geom_point(alpha = 0.5, color = "darkcyan") + # Color is now static
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Balance vs. Campaign Contacts for Subscribed Customers",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Number of Campaign Contacts",
       y = "Bank Balance") +
  theme_minimal()




# -----------------------------------------------------------------------------------------------------
#  Section 2: Data Preparation and Feature Engineering
# ----------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
#  STEP 1: INITIAL DATA CLEANING
# -----------------------------------------------------------------------------------------------------

# Replace "unknown" with NA to mark missing entries
bank_data[bank_data == "unknown"] <- NA

# Check missing values
missing_values <- colSums(is.na(bank_data))
cat("\n🔍 Missing values per variable:\n")
print(missing_values)

get_mode <- function(x) {
  ux <- na.omit(unique(x))                # unique non-NA values
  ux[which.max(tabulate(match(x, ux)))]   # returns the most frequent value
}

# Loop through each column and replace NA with mode (if any)
for (col in names(bank_data)) {
  if (any(is.na(bank_data[[col]]))) {
    mode_value <- get_mode(bank_data[[col]])
    bank_data[[col]][is.na(bank_data[[col]])] <- mode_value
    cat("✅ Missing values in", col, "replaced with mode:", mode_value, "\n")
  }
}

# Verify that there are no missing values left
cat("\n🔍 Checking for remaining missing values:\n")
print(colSums(is.na(bank_data)))

# -----------------------------------------------------------------------------------------------------
#  STEP 2: HANDLE CATEGORICAL VARIABLES
# -----------------------------------------------------------------------------------------------------

#  2.1 Education (Ordinal Variable)
# Order matters: primary < secondary < tertiary
bank_data$education <- factor(bank_data$education,
                              levels = c("primary", "secondary", "tertiary"),
                              ordered = TRUE)

# 2.2 Binary Variables (yes/no) → 1/0 encoding
binary_to_num <- function(x) ifelse(x == "yes", 1, 0)
bank_data$default <- binary_to_num(bank_data$default)
bank_data$housing <- binary_to_num(bank_data$housing)
bank_data$loan <- binary_to_num(bank_data$loan)
bank_data$y <- binary_to_num(bank_data$y)

# 2.3 Nominal Variables (job, marital, contact, poutcome)
# One-hot encoding (create dummy columns for each category)
bank_data <- dummy_cols(bank_data,
                        select_columns = c("job", "marital", "contact", "poutcome"),
                        remove_first_dummy = TRUE,    # drop one dummy to avoid multicollinearity
                        remove_selected_columns = TRUE)

# 2.4 Convert ordinal factor to numeric (preserves order)
bank_data$education_num <- as.numeric(bank_data$education)
bank_data$default_num <- as.numeric(bank_data$default)
bank_data$housing_num <- as.numeric(bank_data$housing)
bank_data$loan_num <- as.numeric(bank_data$loan)
bank_data$y_num <- as.numeric(bank_data$y)


# -----------------------------------------------------------------------------------------------------
#  STEP 3: HANDLE TEMPORAL AND SEASONAL VARIABLES
# -----------------------------------------------------------------------------------------------------

# 3.1 Month: Convert to numeric and assign seasons
month_levels <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
bank_data$month <- factor(bank_data$month, levels = month_levels)

# Convert month to numeric order (1–12)
bank_data$month_num <- as.numeric(bank_data$month)

# Assign season based on marketing patterns
bank_data$season <- case_when(
  bank_data$month %in% c("mar","apr","may") ~ "Spring",
  bank_data$month %in% c("jun","jul","aug") ~ "Summer",
  bank_data$month %in% c("sep","oct","nov") ~ "Autumn",
  TRUE ~ "Winter"
)

# Drop 'day' variable as it doesn’t carry useful predictive info
bank_data <- subset(bank_data, select = -c(day))



# -----------------------------------------------------------------------------------------------------
#  STEP 4: FEATURE ENGINEERING — Deriving New Insights
# -----------------------------------------------------------------------------------------------------

# 4.1 Age Group
bank_data$age_group <- cut(bank_data$age,
                           breaks = c(0, 25, 35, 50, 65, 100),
                           labels = c("Youth", "YoungAdult", "MiddleAge", "Senior", "Elder"))

# 4.2 Balance Category
bank_data$balance_category <- cut(bank_data$balance,
                                  breaks = c(-Inf, 0, 1000, 5000, Inf),
                                  labels = c("Debt", "Low", "Medium", "High"))

# 4.3 Customer Risk Score
# Clients with either default or loan = high risk
bank_data$risk_score <- ifelse(bank_data$default == 1 | bank_data$loan == 1, "High", "Low")

# 4.4 Campaign Intensity
# Ratio of contacts in this campaign vs previous attempts
bank_data$campaign_intensity <- bank_data$campaign / (bank_data$previous + 1)

# 4.5 Recently Contacted Flag (pdays < 30)
bank_data$recently_contacted <- ifelse(bank_data$pdays < 30, 1, 0)

# -----------------------------------------------------------------------------------------------------
#  STEP 5: TRANSFORMATION & NORMALIZATION
# -----------------------------------------------------------------------------------------------------

# Identify continuous numeric variables
numeric_vars <- c("age", "balance", "duration", "pdays", "campaign", "previous")

# 5.1 Safe Log Transformation
for (v in numeric_vars) {
  # Replace negative or NA values with 0 before log
  safe_values <- pmax(bank_data[[v]], 0)
  safe_values[is.na(safe_values)] <- 0
  bank_data[[paste0(v, "_log")]] <- log10(safe_values + 1)
}

# 5.2 Min-Max Normalization
normalize <- function(x) {
  x <- ifelse(is.na(x), 0, x)  # Replace NA with 0
  if (max(x) != min(x)) {
    (x - min(x)) / (max(x) - min(x))
  } else {
    rep(0, length(x))  # Avoid division by zero
  }
}

for (v in numeric_vars) {
  log_col <- paste0(v, "_log")
  norm_col <- paste0(v, "_norm")
  bank_data[[norm_col]] <- normalize(bank_data[[log_col]])
}

}

# -----------------------------------------------------------------------------------------------------
#  STEP 6: FINAL INSPECTION 
# -----------------------------------------------------------------------------------------------------

# Check structure and summary statistics
str(bank_data)
summary(bank_data)


# -----------------------------------------------------------------------------------------------------
#  Section 3: Data Sampling and Validation Strategy 
# -----------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------
#  STEP 1: STRATIFIED SAMPLING
# -----------------------------------------------------------------------------------------------------



set.seed(123)
train_proportion <- 0.7
train_index <- sample(1:nrow(bank_data), 
                      size = floor(train_proportion * nrow(bank_data)))
train_data <- bank_data[train_index,]
test_data <- bank_data[-train_index,]

# Ensure target is a factor
train_data$y <- as.factor(train_data$y)
test_data$y  <- as.factor(test_data$y)


# -----------------------------------------------------------------------------------------------------
#  STEP 2: BASE RECIPE
# -----------------------------------------------------------------------------------------------------

base_recipe <- function(bank_data) {
  recipe(y ~ ., data = bank_data ) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
}

#----------------------------------------------------------------------------------------------------
# STEP 3: CREATE MULTIPLE SAMPLING
# -----------------------------------------------------------------------------------------------------

# 3.1 SMOTE 
table(train_data$y)

# Calculate percentage of each class
cat("\nPercentages:\n")
prop.table(table(train_data$y)) * 100

# Simple bar chart
barplot(table(train_data$y),
        main = "BEFORE SMOTE: Highly Imbalanced!",
        col = c("skyblue", "orange"),
        ylab = "Number of Customers",
        names.arg = c("No Subscription", "Yes Subscription"))

train_data_nominal <- train_data %>% select(-sample_weight)

# Separate features (X) and target (y)
X <- train_data_nominal[, !(names(train_data_nominal) %in% c("y"))]
y <- train_data_nominal$y
X_numeric <- X %>% select(where(is.numeric))

# Apply SMOTE (Synthetic Minority Oversampling Technique)
# K = number of nearest neighbors (can tune)
smote_result <- SMOTE(X_numeric, y, K = 5)

balanced_data <- smote_result$data
names(bank_data$y) <- "Subscription of Customers"

cat("BALANCED DATA:\n")
cat("Total transactions:", nrow(balanced_data), "\n\n")

# Count each type
table(balanced_data$y_num)

# Calculate percentages
cat("\nPercentages:\n")
prop.table(table(balanced_data$fraud)) * 100

# Simple bar chart
barplot(table(balanced_data$y),
        main = "AFTER SMOTE: Balanced!",
        col = c("green", "red"),
        ylab = "Number of Transactions")



# 3.2 oversampling 
rec_scaled_balanced <- base_recipe(train_data) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_upsample(y) %>%
  prep(training = train_data, retain = TRUE)


train_scaled_balanced <- juice(rec_scaled_balanced)
test_scaled_balanced  <- bake(rec_scaled_balanced, new_data = test_data)

# 3.3 undersampling 
rec_scaled_downsample <- base_recipe(train_data) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_downsample(y) %>%
  prep(training = train_data, retain = TRUE)

train_scaled_downsample <- juice(rec_scaled_downsample)
test_scaled_downsample  <- bake(rec_scaled_downsample, new_data = test_data)

#3.4 Cost-sensitive sampling

class_counts <- table(train_data$y)
cat("Class distribution in training data:\n")
print(class_counts)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)  # normalize so weights sum to 1
cat("\nCalculated class weights:\n")
print(class_weights)

train_data$sample_weight <- ifelse(train_data$y == 1,
                                   class_weights["1"],
                                   class_weights["0"])

cost_factor <- 5  # can adjust this value if needed
train_data$sample_weight <- ifelse(train_data$y == 1,
                                   train_data$sample_weight * cost_factor,
                                   train_data$sample_weight)

train_cost_sensitive <- train_data
test_cost_sensitive  <- test_data

# Display the dimensions of the resulting data frames to verify the split
cat("Dimensions of training data:\n")
print(dim(train_cost_sensitive))
cat("\nDimensions of testing data:\n")
print(dim(test_cost_sensitive))

# Check class distribution in training and testing sets
cat("\nClass distribution in training set:\n")
print(table(train_cost_sensitive$y))
print(prop.table(table(train_cost_sensitive$y)))

cat("\nClass distribution in testing set:\n")
print(table(test_cost_sensitive$y))
print(prop.table(table(test_cost_sensitive$y)))




# -----------------------------------------------------------------------------------------------------
#  Section 4: Model Development and Training
# -----------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------
#  STEP 1: 
# -----------------------------------------------------------------------------------------------------

# Load additional libraries
library(xgboost)
library(pROC)

set.seed(104644994)

# -----------------------------------------------------------------------------------------------------
#  STEP 1: Data Preparation for XGBoost (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Preparing Data for Gradient Boosting Model \n")

# Use SMOTE balanced data
train_final <- balanced_data
test_final <- test_data

# Ensure y column exists and is properly formatted
if(!"y" %in% names(train_final)) {
  if("class" %in% names(train_final)) {
    colnames(train_final)[colnames(train_final) == "class"] <- "y"
  }
}

# Convert y to numeric 0/1
if(!is.numeric(train_final$y)) {
  train_final$y <- as.numeric(as.character(train_final$y))
}
if(max(train_final$y, na.rm=TRUE) > 1) {
  train_final$y <- train_final$y - 1
}

# Same for test data
if(!is.numeric(test_final$y)) {
  test_final$y <- as.numeric(as.character(test_final$y))
}
if(max(test_final$y, na.rm=TRUE) > 1) {
  test_final$y <- test_final$y - 1
}

cat("Training samples:", nrow(train_final), "\n")
cat("Test samples:", nrow(test_final), "\n")

# Class distribution
cat("\nTraining class distribution (after SMOTE):\n")
print(table(train_final$y))
cat("\nTest class distribution:\n")
print(table(test_final$y))

# -----------------------------------------------------------------------------------------------------
#  STEP 2: Feature Selection (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Selecting Features \n")

# Columns to exclude
exclude_cols <- c("y", "y_subscription", "y_num", "sample_weight", 
                  "education", "month", "age_group", "balance_category", "risk_score")

# Get all column names
all_cols <- names(train_final)
feature_cols <- setdiff(all_cols, exclude_cols)

# Keep only numeric columns
feature_cols_clean <- c()
for(col in feature_cols) {
  if(is.numeric(train_final[[col]]) && !is.factor(train_final[[col]])) {
    feature_cols_clean <- c(feature_cols_clean, col)
  }
}

cat("Total features selected:", length(feature_cols_clean), "\n")

# Extract features and target
X_train <- train_final[, feature_cols_clean, drop = FALSE]
y_train <- train_final$y

X_test <- test_final[, feature_cols_clean, drop = FALSE]
y_test <- test_final$y

# Handle missing values
if(any(is.na(X_train))) {
  for(col in names(X_train)) {
    if(any(is.na(X_train[[col]]))) {
      median_val <- median(X_train[[col]], na.rm = TRUE)
      X_train[[col]][is.na(X_train[[col]])] <- median_val
      X_test[[col]][is.na(X_test[[col]])] <- median_val
    }
  }
  cat("Missing values imputed\n")
}

# Create XGBoost matrices
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# -----------------------------------------------------------------------------------------------------
#  STEP 3: Hyperparameter Tuning (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Hyperparameter Tuning (this may take 5-10 minutes) \n")

# Parameter grid
param_grid <- expand.grid(
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.05, 0.1),
  subsample = c(0.8, 0.9),
  colsample_bytree = c(0.8, 0.9)
)

# Test first 10 combinations
n_combinations <- min(10, nrow(param_grid))
cv_results <- data.frame()

for(i in 1:n_combinations) {
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )
  
  cv_model <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0,
    stratified = TRUE
  )
  
  best_iter <- cv_model$best_iteration
  best_auc <- max(cv_model$evaluation_log$test_auc_mean)
  
  cv_results <- rbind(cv_results, data.frame(
    iteration = i,
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i],
    best_rounds = best_iter,
    cv_auc = best_auc
  ))
  
  if(i %% 3 == 0) {
    cat("  Tested", i, "/", n_combinations, "combinations\n")
  }
}

# Find best parameters
best_idx <- which.max(cv_results$cv_auc)
best_params <- cv_results[best_idx, ]

cat("\nOptimal hyperparameters found:\n")
cat("  max_depth:", best_params$max_depth, "\n")
cat("  eta:", best_params$eta, "\n")
cat("  subsample:", best_params$subsample, "\n")
cat("  colsample_bytree:", best_params$colsample_bytree, "\n")
cat("  optimal_rounds:", best_params$best_rounds, "\n")
cat("  CV AUC:", round(best_params$cv_auc, 4), "\n")

# -----------------------------------------------------------------------------------------------------
#  STEP 4: Train Final Model (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Training Final Gradient Boosting Model \n")

final_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = 1
)

watchlist <- list(train = dtrain, test = dtest)

final_model <- xgb.train(
  params = final_params,
  data = dtrain,
  nrounds = best_params$best_rounds,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 20
)

cat("\nModel training complete\n")

# -----------------------------------------------------------------------------------------------------
#  STEP 5: Feature Importance (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Feature Importance \n")

importance_matrix <- xgb.importance(
  feature_names = colnames(X_train),
  model = final_model
)

cat("\nTop 15 most important features:\n")
print(head(importance_matrix, 15))

# -----------------------------------------------------------------------------------------------------
#  Section 5: Model Evaluation and Comparison (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Model Evaluation \n")

# -----------------------------------------------------------------------------------------------------
#  STEP 1: Generate Predictions (Rachana)
# -----------------------------------------------------------------------------------------------------

y_pred_prob <- predict(final_model, dtest)
y_pred_class <- ifelse(y_pred_prob > 0.5, 1, 0)
y_pred_class <- factor(y_pred_class, levels = c(0, 1))
y_test_factor <- factor(y_test, levels = c(0, 1))

cat("\nPredictions generated for", length(y_pred_prob), "test samples\n")

# -----------------------------------------------------------------------------------------------------
#  STEP 2: Confusion Matrix and Metrics (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Confusion Matrix \n")

conf_matrix <- confusionMatrix(y_pred_class, y_test_factor, positive = "1")
print(conf_matrix)

# Extract components
TP <- conf_matrix$table[2,2]
TN <- conf_matrix$table[1,1]
FP <- conf_matrix$table[2,1]
FN <- conf_matrix$table[1,2]

cat("\nConfusion Matrix Components:\n")
cat("  True Positives (TP):", TP, "\n")
cat("  True Negatives (TN):", TN, "\n")
cat("  False Positives (FP):", FP, "\n")
cat("  False Negatives (FN):", FN, "\n")

# Calculate metrics
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]
specificity <- conf_matrix$byClass["Specificity"]

# AUC-ROC
roc_obj <- roc(y_test, y_pred_prob, quiet = TRUE)
auc_score <- auc(roc_obj)

cat("\n Performance Metrics \n")
cat(sprintf("Accuracy:    %.4f (%.2f%%)\n", accuracy, accuracy * 100))
cat(sprintf("Precision:   %.4f (%.2f%%)\n", precision, precision * 100))
cat(sprintf("Recall:      %.4f (%.2f%%)\n", recall, recall * 100))
cat(sprintf("F1-Score:    %.4f\n", f1_score))
cat(sprintf("Specificity: %.4f (%.2f%%)\n", specificity, specificity * 100))
cat(sprintf("AUC-ROC:     %.4f\n", auc_score))

# -----------------------------------------------------------------------------------------------------
#  STEP 3: Baseline Comparison (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Baseline Comparison (No SMOTE) \n")

# Train baseline model
X_train_baseline <- train_data[, feature_cols_clean, drop = FALSE]
y_train_baseline <- train_data$y

# Handle missing values in baseline
for(col in names(X_train_baseline)) {
  if(any(is.na(X_train_baseline[[col]]))) {
    X_train_baseline[[col]][is.na(X_train_baseline[[col]])] <- 
      median(X_train_baseline[[col]], na.rm = TRUE)
  }
}

dtrain_baseline <- xgb.DMatrix(
  data = as.matrix(X_train_baseline), 
  label = y_train_baseline
)

baseline_model <- xgb.train(
  params = final_params,
  data = dtrain_baseline,
  nrounds = best_params$best_rounds,
  verbose = 0
)

# Baseline predictions
y_pred_baseline_prob <- predict(baseline_model, dtest)
y_pred_baseline <- ifelse(y_pred_baseline_prob > 0.5, 1, 0)
y_pred_baseline <- factor(y_pred_baseline, levels = c(0, 1))

conf_baseline <- confusionMatrix(y_pred_baseline, y_test_factor, positive = "1")
roc_baseline <- roc(y_test, y_pred_baseline_prob, quiet = TRUE)

cat("\nBaseline Model (No SMOTE):\n")
cat(sprintf("  Accuracy:  %.4f\n", conf_baseline$overall["Accuracy"]))
cat(sprintf("  Precision: %.4f\n", conf_baseline$byClass["Precision"]))
cat(sprintf("  Recall:    %.4f\n", conf_baseline$byClass["Recall"]))
cat(sprintf("  F1-Score:  %.4f\n", conf_baseline$byClass["F1"]))
cat(sprintf("  AUC-ROC:   %.4f\n", auc(roc_baseline)))

cat("\nSMOTE Model:\n")
cat(sprintf("  Accuracy:  %.4f\n", accuracy))
cat(sprintf("  Precision: %.4f\n", precision))
cat(sprintf("  Recall:    %.4f\n", recall))
cat(sprintf("  F1-Score:  %.4f\n", f1_score))
cat(sprintf("  AUC-ROC:   %.4f\n", auc_score))

cat("\nImprovement from SMOTE:\n")
cat(sprintf("  Recall:    %+.2f%%\n", (recall - conf_baseline$byClass["Recall"]) * 100))
cat(sprintf("  F1-Score:  %+.4f\n", f1_score - conf_baseline$byClass["F1"]))
cat(sprintf("  AUC-ROC:   %+.4f\n", auc_score - auc(roc_baseline)))

# -----------------------------------------------------------------------------------------------------
#  STEP 4: Save Results (Rachana)
# -----------------------------------------------------------------------------------------------------

cat("\n Saving Results \n")

# Save model
xgb.save(final_model, "gradient_boosting_smote_model.model")
cat("Model saved: gradient_boosting_smote_model.model\n")

# Save evaluation summary
evaluation_summary <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC-ROC"),
  Baseline = c(
    conf_baseline$overall["Accuracy"],
    conf_baseline$byClass["Precision"],
    conf_baseline$byClass["Recall"],
    conf_baseline$byClass["F1"],
    auc(roc_baseline)
  ),
  SMOTE_Model = c(accuracy, precision, recall, f1_score, auc_score)
)

write.csv(evaluation_summary, "model_evaluation_summary.csv", row.names = FALSE)
cat("Evaluation summary saved: model_evaluation_summary.csv\n")

# Save predictions
predictions_df <- data.frame(
  Actual = y_test,
  Predicted_Probability = y_pred_prob,
  Predicted_Class = y_pred_class
)
write.csv(predictions_df, "test_predictions.csv", row.names = FALSE)
cat("Predictions saved: test_predictions.csv\n")

cat("\n Analysis Complete \n")
