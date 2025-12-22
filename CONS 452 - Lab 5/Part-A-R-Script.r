# Install required packages 
install.packages("tidyverse")
library(tidyverse)

# Import your qtr_b_fup data and call it forest_use 
forest_use <- read_csv(file = "/Users/juliana/Sync/CONS452_2021/Labs/L5 - PENdataset (formerly A9)/L5_2021/qtr_b_fup.csv")

# The following code can be used to see your data entirely in a new window in table format:
view(forest_use)

# Running the code below will turn forest_use into a tibble 
forest_use_tibble <- as_tibble(forest_use)
# See what a tibble looks like in your console:
forest_use_tibble

# Let's use select() to isolate variables of interest:
forest_use_simpler <- select(forest_use_tibble, ghousecode, qtr, fup_pdt, fup_collby, fup_nety)
forest_use_simpler

# Arrange this simpler tibble by fup_nety (net income from the forest product)
arrange(forest_use_simpler, desc(fup_nety))

# Filter for the wild coffee code within the fup_pdt category
filter(forest_use_simpler, fup_pdt == 42) %>%
  arrange(desc(fup_nety))

# Create a new variable called country_code to help query countries 
forest_use_simpler_country <- mutate(forest_use_simpler, country_code = floor(ghousecode / 10000)) 

# Now, create a new tibble for cameroon only (country code 301)
cameroon <- filter(forest_use_simpler_country, country_code == 301)
cameroon

# Visualize the data using ggplot
# Create a bar graph of the number of households that collect each forest product
ggplot(data = cameroon) +
  geom_bar(mapping = aes(x = fup_pdt))

# Colour in the bars to show which quarter the collection occurred in 
ggplot(data = cameroon) +
  geom_bar(mapping = aes(x = factor(fup_pdt), fill = factor(qtr))) +
  labs(x = "Forest Product by Code", y = "Number of Households", title = "Forest Product Usage by Household", subtitle = "Breakdown of Different Forest Products' Usage in Households in Cameroon") 

# Create a Uganda dataset 
uganda <- filter(forest_use_simpler_country, country_code == 307)

# Plot Uganda forest product use
ggplot(data = uganda) + geom_bar(mapping = aes(x = factor(fup_pdt), fill = factor(qtr))) + scale_fill_discrete(name = "Quarter") + labs(x = "Forest Product by Code", y = "Number of Households", title = "Forest Product Usage by Household", subtitle = "Breakdown of Different Forest Products' Usage in Households in Uganda")

# Create another Cameroon plot with an added legend
ggplot(data = cameroon) + geom_bar(mapping = aes(x = factor(fup_pdt), fill = factor(qtr))) + scale_fill_discrete(name = "Quarter") + labs(x = "Forest Product by Code", y = "Number of Households", title = "Forest Product Usage by Household", subtitle = "Breakdown of Different Forest Products' Usage in Households in Cameroon")

# Option to use the code below in labelling your ggplot:  
# scale_fill_discrete(name = "Quarter")

# Create a subset for two countries
T_Test1 = subset(forest_use_simpler_country, forest_use_simpler_country$country_code == "301" | forest_use_simpler_country$country_code == "307")

Perform a t-test
t.test(fup_nety~country_code, data = T_Test1)
