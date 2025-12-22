# -----------------------------
# Household Malawi DDS Analysis
# -----------------------------

# Set working directory
setwd("S:/UG_Store/L4 R Part/Section 2 data")

# Load required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("wesanderson")

library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)

# -----------------------------
# Import dataset
# -----------------------------
HH_Malawi <- read.csv("S:/UG_Store/L4 R Part/Section 2 data/HH_Malawi.csv",
                      fileEncoding = "UTF-8-BOM")

View(HH_Malawi)

# -----------------------------
# Calculate Dietary Diversity Scores (DDS)
# -----------------------------

# Healthy DDS (10 food groups)
HH_Malawi <- HH_Malawi %>%
  mutate(Healthy_DDS = CerealsTubers + Pulses + NutsSeeds + Dairy +
           MeatPoultryFish + Eggs + DarkGreenLeafyVeg + VitaminA +
           OtherVeg + OtherFruit)

# Combined DDS (12 food groups)
HH_Malawi <- HH_Malawi %>%
  mutate(Combined_DDS = Cereal + Tubers + Legumes + Veggies + Meat_PoultryFAO5 +
           FruitsFAO6 + Milk + Sweets + OilFat + SpiceCondiments +
           EggsFao11 + FishFAO12)

View(HH_Malawi)

# -----------------------------
# Exploratory analysis
# -----------------------------
# Proportion of fruit consumption by wealth group
prop.table(table(HH_Malawi$FruitsFAO6, HH_Malawi$wealth.index), 2)

# -----------------------------
# Visualization: Fruit consumption by wealth
# -----------------------------
ggplot(HH_Malawi, aes(x = wealth.index, fill = FruitsFAO6 == 1)) +
  geom_bar(position = "fill", width = 0.7) +
  labs(title = "Fruit Consumption by Household Wealth Group",
       x = "Wealth Group",
       y = "% of Households",
       fill = "") +
  scale_fill_discrete(labels = c("No Fruits Consumed", "Fruits Consumed")) +
  scale_x_discrete(limits = c("Poor", "Middle", "Rich")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

# -----------------------------
# DDS Scores by wealth groups
# -----------------------------
percent_HH_DDS <- HH_Malawi %>%
  count(wealth.index, Healthy_DDS) %>%
  group_by(wealth.index) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

ggplot(percent_HH_DDS, aes(x = wealth.index, y = percent, fill = factor(Healthy_DDS))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  scale_fill_manual(values = wes_palette(10, name = "Zissou1", type = "continuous")) +
  labs(x = "Wealth Index", y = "Percent of Households", fill = "Number of Food Groups") +
  scale_x_discrete(limits = c("Poor", "Middle", "Rich")) +
  theme_light()

# -----------------------------
# Boxplots of DDS by wealth
# -----------------------------
boxplot(Healthy_DDS ~ wealth.index, data = HH_Malawi, main = "Healthy DDS by Wealth Group")
boxplot(Combined_DDS ~ wealth.index, data = HH_Malawi, main = "Combined DDS by Wealth Group")

# -----------------------------
# Means of DDS by wealth group
# -----------------------------
HH_Malawi %>% filter(wealth.index == "Poor") %>% summarise(mean(Healthy_DDS), mean(Combined_DDS))
HH_Malawi %>% filter(wealth.index == "Middle") %>% summarise(mean(Healthy_DDS), mean(Combined_DDS))
HH_Malawi %>% filter(wealth.index == "Rich") %>% summarise(mean(Healthy_DDS), mean(Combined_DDS))

# -----------------------------
# ANOVA for DDS scores by wealth
# -----------------------------
ES_ANOVA3 <- lm(Healthy_DDS ~ wealth.index, data = HH_Malawi)
ES_ANOVA4 <- lm(Combined_DDS ~ wealth.index, data = HH_Malawi)
anova(ES_ANOVA3)
anova(ES_ANOVA4)

# -----------------------------
# t-tests between wealth groups
# -----------------------------
T_test1 <- subset(HH_Malawi, wealth.index %in% c("Poor", "Middle"))
t.test(Healthy_DDS ~ wealth.index, data = T_test1)

T_test2 <- subset(HH_Malawi, wealth.index %in% c("Middle", "Rich"))
t.test(Healthy_DDS ~ wealth.index, data = T_test2)

T_test3 <- subset(HH_Malawi, wealth.index %in% c("Poor", "Rich"))
t.test(Healthy_DDS ~ wealth.index, data = T_test3)

# -----------------------------
# Cluster-level DDS analysis
# -----------------------------
ClustersHealthy <- HH_Malawi %>%
  group_by(ClusterID) %>%
  summarise(X..forest = mean(X..forest),
            Number.of.patches = mean(Number.of.patches),
            dist_road = mean(dist_road),
            HealthyDDS = mean(Healthy_DDS))

ClustersCombined <- HH_Malawi %>%
  group_by(ClusterID) %>%
  summarise(X..forest = mean(X..forest),
            Number.of.patches = mean(Number.of.patches),
            dist_road = mean(dist_road),
            CombinedDDS = mean(Combined_DDS))

# Scatter plots
ggplot(ClustersCombined, aes(x = X..forest, y = CombinedDDS)) +
  geom_point() +
  theme_minimal() +
  xlab("Percent Forest Cover") +
  ylab("Combined DDS")

ggplot(ClustersHealthy, aes(x = X..forest, y = HealthyD
