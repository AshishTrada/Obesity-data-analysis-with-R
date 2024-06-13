library(dplyr)
library(ggplot2)
data <- read.csv("C:\\Users\\ashis\\OneDrive\\Desktop\\sem4\\bigdata\\obesity_dataset.csv")

str(data)

#data creation
#vector
age_vector <- c(data$Age)

height_weight_matrix <- as.matrix(data[, c("Height", "Weight")])

df <- data.frame(Age = age_vector, Height = data$Height, Weight = data$Weight)


gender_factor <- factor(data$Gender)
obs_level_factors <- factor(data$Obesity_Level)
levels(obs_level_factors)
levels(gender_factor)

data_list <- list(
  age_vector = age_vector,
  height_weight_matrix = height_weight_matrix,
  gender_factor = gender_factor
)

# data manipulation with dplyr
selected_data <- data %>%
  select(Age, Gender, Height, Weight, Obesity_Level)

arranged_data <- data %>%
  arrange(Age)

mutated_data <- data %>%
  mutate(BMI = Weight / (Height)^2)

obese_individuals <- data %>%
  filter(Obesity_Level 
  %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

# Calculating average Age and BMI for obese individuals
avg_age_bmi_obese <- obese_individuals %>%
  summarise(Avg_Age = mean(Age, na.rm = TRUE),
  Avg_BMI = mean(Weight / (Height)^2, na.rm = TRUE))

# Calculate the average number of main meals per day
avg_main_meals <- data %>%
  summarise(Avg_Main_Meals = mean(Number_of_Main_Meals, na.rm = TRUE))

high_caloric_food_percentage <- data %>%
  summarise(Percentage = mean(Frequent_High_Caloric_Food == "yes", na.rm = TRUE))

avg_technology_usage <- data %>%
  group_by(Gender) %>%
  summarise(Avg_Technology_Usage = mean(Time_Using_Technology, na.rm = TRUE))

avg_water_intake_age <- data %>%
  group_by(Gender) %>%
  summarise(Avg_Water_Intake = mean(Daily_Water_Intake, na.rm = TRUE))

#charts

#age distribution
age_histogram <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

#BMI distribution
data$BMI <- data$Weight / (data$Height/100)^2

# Create a histogram for BMI distribution
bmi_histogram <- ggplot(mutated_data, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "BMI Distribution", x = "BMI", y = "Frequency") +
  theme_minimal()

print(bmi_histogram)

#Gender-wise Obesity Level distribution
gender_obesity_bar <- ggplot(data, aes(x = Obesity_Level, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender-wise Obesity Level Distribution", x = "Obesity Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(gender_obesity_bar)

# Water Intake by Gender
water_intake_boxplot <- ggplot(data, aes(x = Gender, y = Daily_Water_Intake, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Water Intake by Gender", x = "Gender", y = "Daily Water Intake(L)") +
  theme_minimal()

print(water_intake_boxplot)

# Create a bar plot for Alcohol Consumption Frequency by Obesity Level
alcohol_obesity_bar <- ggplot(data, aes(x = Obesity_Level, fill = Alcohol_Consumption_Frequency)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Alcohol Consumption Frequency by Obesity Level", x = "Obesity Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(alcohol_obesity_bar)

#height vs weight
height_weight_scatter <- ggplot(data, aes(x = Height, y = Weight)) +
  geom_point() +
  labs(title = "Height vs Weight", x = "Height", y = "Weight") +
  theme_minimal()

print(height_weight_scatter)


#bar plot for Alcohol Consumption Frequency by Gender
alcohol_gender_bar <- ggplot(data, aes(x = Alcohol_Consumption_Frequency, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Alcohol Consumption Frequency between Genders", x = "Alcohol Consumption Frequency", y = "Count") +
  theme_minimal()

print(alcohol_gender_bar)

#scatter plot of Physical Activity Frequency vs BMI
physical_activity_bmi_scatter <- ggplot(mutated_data, aes(x = Physical_Activity_Frequency, y = BMI)) +
  geom_point() +
  labs(title = "Relationship between Physical Activity Frequency and BMI", x = "Physical Activity Frequency", y = "BMI") +
  theme_minimal()

print(physical_activity_bmi_scatter)

#Physical Activity Frequency by Gender
physical_activity_gender_boxplot <- ggplot(data, aes(x = Gender, y = Physical_Activity_Frequency, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Distribution of Physical Activity Frequency by Gender", x = "Gender", y = "Physical Activity Frequency") +
  theme_minimal()

print(physical_activity_gender_boxplot)

#Family History of Overweight by Obesity Level
family_obesity_bar <- ggplot(data, aes(x = Obesity_Level, fill = Family_History_Overweight)) +
  geom_bar() +
  labs(title = "Distribution of Family History of Overweight by Obesity Level", x = "Obesity Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(family_obesity_bar)

#Calorie Monitoring by Physical Activity Frequency
calorie_activity_bar <- ggplot(data, aes(x = Physical_Activity_Frequency, fill = Calorie_Monitoring)) +
  geom_bar() +
  labs(title = "Distribution of Calorie Monitoring by Physical Activity Frequency", x = "Physical Activity Frequency", y = "Count") +
  theme_minimal()

print(calorie_activity_bar)

#scatter plot of Weight vs BMI colored by Obesity Level
weight_bmi_obesity_scatter <- ggplot(mutated_data, aes(x = Weight, y = BMI, color = Obesity_Level)) +
  geom_point() +
  labs(title = "Relationship between Weight and BMI by Obesity Level", x = "Weight", y = "BMI") +
  theme_minimal()

print(weight_bmi_obesity_scatter)

# box plot for Physical Activity Frequency by Alcohol Consumption Frequency
activity_alcohol_boxplot <- ggplot(data, aes(x = Alcohol_Consumption_Frequency, y = Physical_Activity_Frequency, fill = Alcohol_Consumption_Frequency)) +
  geom_boxplot() +
  labs(title = "Distribution of Physical Activity Frequency by Alcohol Consumption Frequency", x = "Alcohol Consumption Frequency", y = "Physical Activity Frequency") +
  theme_minimal()

print(activity_alcohol_boxplot)

#box plot for Number of Main Meals by Gender and Obesity Level
meals_gender_obesity_boxplot <- ggplot(data, aes(x = Gender, y = Number_of_Main_Meals, fill = Obesity_Level)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Distribution of Main Meals by Gender and Obesity Level", x = "Gender", y = "Number of Main Meals") +
  theme_minimal()

print(meals_gender_obesity_boxplot)

# Calculate the counts of each smoking habit
smoking_counts <- table(data$Smoking_Habit)

# Create a pie chart for smoking habits
smoking_pie <- ggplot(data.frame(smoking_counts), aes(x = "", y = smoking_counts, fill = factor(Var1))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Smoking Habits", fill = "Smoking Habit", x = NULL, y = NULL) +
  theme_void()

print(smoking_pie)

#box plot for Physical Activity Frequency by Frequent High Caloric Food Consumption
activity_caloric_boxplot <- ggplot(data, aes(x = Frequent_High_Caloric_Food, y = Physical_Activity_Frequency, fill = Frequent_High_Caloric_Food)) +
  geom_boxplot() +
  labs(title = "Comparison of Physical Activity Frequency by Frequent High Caloric Food Consumption", x = "Frequent High Caloric Food Consumption", y = "Physical Activity Frequency") +
  theme_minimal()

print(activity_caloric_boxplot)

#bar plot for Physical Activity Frequency by Frequent High Caloric Food Consumption on obesity levels
activity_caloric_obesity <- ggplot(data, aes(x = Frequent_High_Caloric_Food, y = Physical_Activity_Frequency, fill = Obesity_Level)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Comparison of Physical Activity Frequency by Frequent High Caloric Food Consumption on Obesity Levels",
       x = "Frequent High Caloric Food Consumption", y = "Mean Physical Activity Frequency") +
  scale_fill_brewer(palette = "Paired") +  # Change the color palette as needed
  theme_minimal()

print(activity_caloric_obesity)


#grouped box plot for Height by Obesity Level and Gender
height_obesity_gender_boxplot <- ggplot(data, aes(x = Obesity_Level, y = Height, fill = Gender)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Distribution of Height by Obesity Level and Gender",
       x = "Obesity Level", y = "Height") +
  theme_minimal()

print(height_obesity_gender_boxplot)


#box plot of Weight by Food Between Meals
weight_food_between_meals_boxplot <- ggplot(data, aes(x = Food_Between_Meals, y = Weight, fill = Food_Between_Meals)) +
  geom_boxplot() +
  labs(title = "Box Plot of Weight by Food Between Meals",
       x = "Food Between Meals", y = "Weight") +
  theme_minimal()

print(weight_food_between_meals_boxplot)