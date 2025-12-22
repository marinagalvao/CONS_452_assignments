# Load packages
library(tidyverse)

# Break down forest product usage by household, showing who collected the forest product
ggplot(data = cameroon) + geom_bar(mapping = aes(x = factor(fup_pdt), fill = factor(fup_collby))) + scale_fill_discrete(name = "Code for Whom the Product was Collected by") + labs(x = "Forest Product by Code", y = "Number of Households", title = "Forest Product Usage by Household", subtitle = "Breakdown of Different Forest Products' Usage in Households in Cameroon")

# Create a Welch Two Sample t-test for comparing means of net income from wild vegetables and game meat – mammals in Cameroon. 
T_Test3 = subset(cameroon, cameroon$fup_pdt == "25" | cameroon$fup_pdt == "51")
t.test(fup_nety~fup_pdt, data = T_Test3)

# Split households into income deciles (by country so that we don't have currency conversion problems)
# First,  add wage income to the cameroon data 
wage_income <- read_csv(file = "/Users/juliana/Sync/CONS452_2021/Labs/L5 - PENdataset (formerly A9)/L5_2021/qtr_f1_wage.csv")
wage_income <- as_tibble(wage_income)
wage_income <- select(wage_income, ghousecode, wage_toty)
wage_income

# Aggregate all the reported wages into the household level (at the moment the data is wage by person, not household)
# This will also sum all the reports of each qtr, thus this is now showing annual wage for each household
wage_income <- aggregate(wage_toty ~ ghousecode, data = wage_income, sum)
wage_income <- as_tibble(wage_income)
wage_income

# Aggregate all the net incomes from forest products to the household level 
cameroon_aggregated <- aggregate(fup_nety ~ ghousecode, data = cameroon, sum)

# Join the wage income to the cameroon_wage data by household
cameroon_wage <- left_join(cameroon_aggregated, wage_income, by = "ghousecode")
# Make it tibble again for consistency
cameroon_wage <- as_tibble(cameroon_wage)
# View this new tibble
cameroon_wage

# Assign each household to a decile based on their wage income
cameroon_decile <- mutate(cameroon_wage, income_ranking = ntile(wage_toty,10))
# View the tibble 
cameroon_decile

# Add a new variable called forest_dependence 
cameroon_decile<- mutate(cameroon_decile, forest_dependence = fup_nety/(fup_nety + wage_toty)) 

# View the dataset to confirm this worked: 
cameroon_decile

# Use box plots to compare forest dependence by income decile 
ggplot(data = cameroon_decile) +
  geom_boxplot(mapping = aes(factor(income_ranking), forest_dependence), colour = "blue", notch = FALSE) +
  labs( title = "Forest Dependence by Income Decile in Cameroon", subtitle = "Created using the PEN Dataset (CIFOR)", x = "income decile ranking", y = "forest dependence")

# Now, the goal is to calculate average forest dependence per household for 10 subsaharan African countries
# Use the forest_use_simpler_country tibble
forest_use_simpler_country

# Aggregate it by household to get total net income from forest products by HH
forest_use_household <- aggregate(fup_nety ~ ghousecode, data = forest_use_simpler_country, sum) 
forest_use_household <- as_tibble(forest_use_household)

# Use the wage_income by household data which we've already created, and join it to the forest_use_household
forest_use_wage <- left_join(forest_use_household, wage_income, by = "ghousecode")
forest_use_wage

# Create the forest dependence variable using mutate
forest_use_wage <- mutate(forest_use_wage, forest_dependence = fup_nety / (fup_nety + wage_toty))
forest_use_wage

# Add country_code again
forest_use_wage <- mutate(forest_use_wage, country_code = floor(ghousecode / 10000))
forest_use_wage

# Obtain the average forest dependence by country
forestdep_bycountry <- aggregate(forest_dependence ~ country_code, data = forest_use_wage, mean)
view(forestdep_bycountry)
