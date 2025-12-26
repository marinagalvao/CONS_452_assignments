# Install packages

install.packages("tidyr")
install.packages("dplyr")

# Load the packages 

library(tidyr)
library(dplyr)
library(ggplot2)

#### TASK 1: Importing & describing your data ####

# Import the first dataset, assign it a name

Costs = read.csv("Iowa_AnnualProductionCosts.csv", fileEncoding="UTF-8-BOM")

#  Visualize the spread of each variable:

summary(Costs)

#   Find the mean, median, and standard deviation of Annual Nitrogen

   mean(Costs$Nitrogen)

  # Notice how when you run this code the console returns NA. There are
  # missing values in the data
  # Tell R to ignore the NA values when it runs the descriptive statistics:

  mean(Costs$Nitrogen,na.rm = TRUE)
  
  # this can be added to many functions in R, including median()
  
  median(Costs$Nitrogen, na.rm = TRUE)

  # Now, do the same for variance and standard deviation.
  
var(Costs$Nitrogen, na.rm = TRUE)
sd(Costs$Nitrogen, na.rm = TRUE)
  
  # Make a note of the mean, median, and sd of the Nitrogen
  # costs with NAs removed

mean(Costs$Nitrogen,na.rm = TRUE)
97.156
median(Costs$Nitrogen, na.rm = TRUE)
115.25
var(Costs$Nitrogen, na.rm = TRUE)
3101.64
sd(Costs$Nitrogen, na.rm = TRUE)
55.69243

# Redo the analysis by replacing the missing values instead of ignoring
# them. Replace missing values from the Nitrogen variable with 0
  
  Costs$Nitrogen[is.na(Costs$Nitrogen)] = 0
  
# Ensure it worked by using the summary() function for the column

  summary(Costs$Nitrogen)
  
# Replace the NA values with the mean price in the Price.Mg column
  
  mean(Costs$Price.Mg, na.rm = TRUE)
  185.247
  Costs$Price.Mg[is.na(Costs$Price.Mg)] =185.247
  
# Make a column to calculate the Total Cost of all the agricultural inputs:  
  
  Costs = mutate(Costs,
         Total_Cost = Costs$Machinery + Costs$Seed + Costs$Nitrogen
         + Costs$Phosphate + Costs$Potassium + Costs$Herbicide + 
           Costs$Crop.Insurance + Costs$Miscellaneous + Costs$X_Combine +
           Costs$Grain.cart + Costs$Hauling + Costs$Drying + Costs$Handling
         + Costs$Labor + Costs$Property.tax.on.owned.land)
  
  # The code above simply creates a new column, Total_Cost, and fills it with a value
  # calculated by adding up the costs of all the other variables. 
  
  # It looks as if one of the variables being added still contains NA values
  # Change those NA values to 0s, and then add up the Total Cost again
  
Costs$Drying[is.na(Costs$Drying)] = 0
  
  # Create a new column called Annual_Revenue by multiplying the Average Yield by the Price.

Costs = mutate(Costs,
               +Annual_Revenue = Costs$Average.yield.Mg * Costs$Price.Mg)
  

#### TASK 2: Using a T-test to examine differences between two groups ####

# The two-sample T-test is used to compare the means of two groups
# Before running a T-test let's create a multiple histogram plot. 
# Let's look at the differences between Seed costs across the two crop types:
  
  ggplot(Costs, aes(x=Seed)) + geom_histogram() + facet_wrap(~Crops, ncol=1)

# Notice the difference in spread in seed costs between Corn and Soybean
# Let's run a T-test to determine if the means of the two groups are 
# significantly different:
  
    # First we want to subset the data into the two groups we are testing.
    # We want to see the difference in seed costs for Soybean and Corn:
  
    T_test1 = subset(Costs, Costs$Crops == "Soybean" | Costs$Crops == "Corn")
    
    # Now for the T-test:
    
    t.test(Seed~Crops, data = T_test1, var.equal = TRUE)
    
    # The code runs a t-test to examine the effect of Crops on Seed costs, then gives the name of the dataset, 
    # and confirms that the variance in the datasets is equal.
    
#### TASK 3: Examining the relationship between 2 continuous variables with linear regression ####
    
# Create a scatter plot to view the relationship between the cost of labour and commodity prices
    
    ggplot(Costs, aes(x=Price.Mg, y=Labor)) + geom_point() + 
      labs(x= "Commodity Prices", y= "Cost of Labor") + geom_smooth(method = lm)
    + theme_minimal()
    
# In R, regression is a two-step process. We start by using lm() to fit a linear model
# to the data. We then give the output of the lm() to the function summary() to see
# the results of the analysis
      
  # Let's look at the relationship between Labor costs, and commodity prices
    Regression1 = lm(Price.Mg ~ Labor, data = Costs)
    
  # Let's see the results using the summary() of the lm() results
    summary(Regression1)
  
    Call:
      lm(formula = Price.Mg ~ Labor, data = Costs)
    
# Create two subset datasets: one for soybean and one for corn. 
# Then conduct linear regressions for each dataset to examine the effect of herbicide costs on average yield. 
# Is there a linear relationship for either crop? Why might this be? 
# Is the relationship statistically significant?
    
    Soybean = subset(Costs,Costs$Crops == "Soybean")
    Corn = subset(Costs, Costs$Crops == "Corn")
    Soybean_lm = lm(Herbicide ~ Average.yield.Mg, data = Soybean)
    summary(Soybean_lm)
    Corn_lm = lm(Herbicide ~ Average.yield.Mg, data = Corn)
    summary(Corn_lm)
    
    ggplot(Soybean, aes(x=Herbicide, y=Average.yield.Mg)) + geom_point() + 
    labs(x= "Herbicide costs", y= "Average yield of soybean (Mg)", 
         title = "Effects of herbicide costs on soybean yield") 
    + geom_smooth(method = lm) + 
      theme(axis.title.x = element_text(face = "bold", size = 12),
            axis.text.x  = element_text(size=10), 
            axis.title.y = element_text(face = "bold", size = 12),
            axis.text.y  = element_text(size=10))
    
    ggplot(Corn, aes(x=Herbicide, y=Average.yield.Mg)) + geom_point() + 
      labs(x= "Herbicide costs", y= "Average yield of corn (Mg)", 
           title = "Effects of herbicide costs on corn yield") 
    + geom_smooth(method = lm) + theme(axis.title.x = element_text(face = "bold", size = 12),
                                       axis.text.x  = element_text(size=10), 
                                       axis.title.y = element_text(face = "bold", size = 12),
                                       axis.text.y  = element_text(size=10))
    
#### TASK 4: Using ANOVA to examine differences between more than two groups ####

# Load the PrairieStrips_ES.csv dataset
    
    PrarieStrips = read.csv("PrairieStrips_ES.csv") 
    
# Let's look at whether there are differences in Pollinator Abundance between the three treatments. 
# My hypothesis is that there will be a difference in pollinator abundance amoungst the three treatments. 
# Create a quick boxplot to visualize the data:
    
  ggplot(PrarieStrips, aes(x=CropType, y=PollinatorAbundance)) + geom_boxplot() + 
  +     labs(x= "Treatment area", y= "Pollinator abundance") + 
  geom_smooth(method = lm) + theme(axis.title.x = element_text(face = "bold", size = 12),
                                   axis.text.x  = element_text(size=10), 
                                   axis.title.y = element_text(face = "bold", size = 12),
                                   axis.text.y  = element_text(size=10))

# The hypothesis was correct, there are differeces in pollinator abundance in each of the treatment areas

  # Perform an ANOVA to compare the means of all groups
  # Fit the ANOVA model to the data
  
  ES_ANOVA1 = aov(PollinatorAbundance ~ CropType, data = MyDataset)
  summary(ES_ANOVA1)

  # The p-value is less than 0.05 (7.03e-10), therefore pollinator abundance is 
  # statistically different in the different areas 
  
# Create a boxplot showing how the variable differs across CropType (treatment areas)
# Overlay a linear trend line 
# Run a one-way ANOVA to test whether the mean values differ by CropType

  ggplot(PrarieStrips, aes(x=CropType, y=Crop_Yield)) + geom_boxplot() + 
    labs(x= "Treatment area", y= "Crop yield") + geom_smooth(method = lm) +
  theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.text.x  = element_text(size=10), 
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.y  = element_text(size=10))
  
  ES_ANOVA2 = aov(Crop_Yield ~ CropType, data = PrarieStrips)
  
  ggplot(PrarieStrips, aes(x=CropType, y=Surface_Runoff)) + geom_boxplot() + 
    labs(x= "Treatment area", y= "Surface Runoff") + 
    geom_smooth(method = lm) +
    theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.text.x  = element_text(size=10), 
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.y  = element_text(size=10))
  
  ES_ANOVA3 = aov(Surface_Runoff ~ CropType, data = PrarieStrips)
  
  ggplot(PrarieStrips, aes(x=CropType, y=BirdSpecies_Richness)) + geom_boxplot() + 
  labs(x= "Treatment area", y= "Bird Species Richness") + 
  geom_smooth(method = lm) + 
    theme(axis.title.x = element_text(face = "bold", size = 12),
          axis.text.x  = element_text(size=10), 
          axis.title.y = element_text(face = "bold", size = 12),
          axis.text.y  = element_text(size=10))
  
  ES_ANOVA4 = aov(BirdSpecies_Richness ~ CropType, data = PrarieStrips)
  
  ggplot(PrarieStrips, aes(x=CropType, y=InsectTaxa_Richness)) + geom_boxplot() + 
    labs(x= "Treatment area", y= "Insect Richness") + 
    geom_smooth(method = lm) + 
    theme(axis.title.x = element_text(face = "bold", size = 12),
          axis.text.x  = element_text(size=10), 
          axis.title.y = element_text(face = "bold", size = 12),
          axis.text.y  = element_text(size=10))
  
  ES_ANOVA5 = aov(InsectTaxa_Richness ~ CropType, data = PrarieStrips)
  
#### TASK 5: Using a T-test to further discriminate between the groups ####

# The ANOVA results tell us if there are significant differences among crop types, 
# but they don't tell us where those differences lie. 
# Run the Tukey HSD test, a suitable "post-hoc" test

  TukeyHSD(ES_ANOVA1)

  
# Welch's t-test, on the other hand, does not make an assumption of equal variance. 
# The code to run Welch's t-test is exactly like that for the 2-sample t-test,
# but with the option var.equal set to FALSE.
  
  # Welch's t-test 
  t.test(PollinatorAbundance ~ CropType, data = SUBSETDATA, var.equal = FALSE)

  # Now perform a Levene's test to compare the variance between groups.
  # This way, we'll know which t-test is most appropriate.
  
  # Start by installing and loading the car package
  install.packages("car")
  library(car)
  
  # Now we can run Levene's test:
  leveneTest(data = SUBSETDATA, PollinatorAbundance ~ CropType, center = mean)
  
  # Based on the P-value returned by Levene's test, we can reject the null hypothesis (if the P-value were low enough)
  # that the different Crop Types had the same variance.
  
  