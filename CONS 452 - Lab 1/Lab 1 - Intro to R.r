# -----------------------------
# Titanic and Rainforest Analysis
# -----------------------------

# Set working directory
setwd("C:/Users/student/L1_intro_to_R")

# Install packages
install.packages("ggplot2")      
library(ggplot2)

# -----------------------------
# Import datasets
# -----------------------------
titanic <- read.csv("titanic.csv")
rainforest <- read.csv("rainforest.csv")

# View datasets
View(titanic)
View(rainforest)

# Preview Titanic dataset
head(titanic)
str(titanic)
summary(titanic)

# -----------------------------
# Titanic: Exploratory Analysis
# -----------------------------

# Frequency tables
table(titanic$passenger_class)
table(titanic$sex)

# Proportions
prop.table(table(titanic$sex))
round(prop.table(table(titanic$sex)), digits = 2)

# Sex distribution across passenger classes
table(titanic$sex, titanic$passenger_class)

# Proportion by row (sex distribution within classes)
prop.table(table(titanic$sex, titanic$passenger_class), 1)

# Proportion by column (proportion of class that is male/female)
prop.table(table(titanic$sex, titanic$passenger_class), 2)

# Survival by sex
prop.table(table(titanic$sex, titanic$survive), 2)

# -----------------------------
# Titanic: Subsets
# -----------------------------
# 1st class passengers
titanic_1st <- subset(titanic, passenger_class == "1st")
View(titanic_1st)

# Females in 1st class
titanic_1st_fem <- subset(titanic_1st, sex == "female")

# -----------------------------
# Titanic: New columns
# -----------------------------
# Purpose column
titanic$purpose <- "leisure"

# Concatenate age and sex
titanic$Age_Sex <- paste(titanic$age, titanic$sex, sep = " ")

# Concatenate name and age
titanic$Name_Age <- paste(titanic$name, titanic$age)

# -----------------------------
# Titanic: Visualizations
# -----------------------------
# Histogram of age
hist(titanic$age, xlab = "Age", main = "Age Distribution")

# Boxplot of age by passenger class
boxplot(titanic$age ~ titanic$passenger_class,
        xlab = "Passenger Class",
        ylab = "Age (years)",
        main = "Titanic Age by Class")

# -----------------------------
# Rainforest: Visualizations
# -----------------------------
# Scatter plots
plot(rainforest$dbh, rainforest$wood,
     xlab = "DBH (cm)",
     ylab = "Wood (kg)",
     main = "Wood as a Function of DBH")

# Saving a scatter plot as JPEG
jpeg("scatter.jpeg", width = 400, height = 350)
plot(rainforest$dbh, rainforest$wood,
     xlab = "DBH (cm)",
     ylab = "Wood (kg)",
     main = "Wood as a Function of DBH")
dev.off()

# -----------------------------
# ggplot2 Visualizations
# -----------------------------
# Boxplot of DBH by species
ggplot(data = rainforest, aes(x = species, y = dbh)) +
  geom_boxplot()

# Violin plot alternative
ggplot(data = rainforest, aes(x = species, y = dbh)) +
  geom_violin()

# Bar chart: count of trees by species
ggplot(data = rainforest, aes(x = species)) +
  geom_bar(stat = "count", fill = "blue", width = 0.6) +
  labs(x = "Species", y = "No. of trees")

# Scatter plot with trendline
ggplot(data = rainforest, aes(x = dbh, y = wood)) +
  geom_point() +
  geom_smooth(method = "lm")

# Save ggplot
ggsave("ggplot.jpeg", width = 5, height = 4)

# -----------------------------
# End of Script
# -----------------------------
