#######################################################################
############################  CONS 452 ################################
##############         L2: Data Analysis with R        ################
############################ 20 Points ################################
#######################################################################

# Before we begin follow these steps:

# 1. Create a new folder on your computer's desktop and name it something like
#   Cons452_YourINITALS_L2

# 2. Download the Iowa_AnnualProductionCosts.csv,
#    and PrairieStrips_ES.csv

# 3. Set your working directory in R to the folder with the datasets
#    If you have forgotten how to do this, check the R script from the last lab

# 4. Install two new packages: tidyr will be for cleaning data and dplyr for
#    data manipulation

install.packages("tidyr")
install.packages("dplyr")

# 5. Load the packages you just installed (as well as ggplot2) using the 
#    library () function:

library(tidyr)
library(dplyr)
library(ggplot2)


#### TASK 1: Importing & describing your data ####

# 1. Let's start by importing the first dataset.
#    Assign it a name.
#    When naming files, be mindful that capitalization matters
#    in Rstudio. Annual_cost and annual_cost will be read
#    differently by the console.

Costs = read.csv("Iowa_AnnualProductionCosts.csv", fileEncoding="UTF-8-BOM")

#    We have named the dataset "Costs". You can name it whatever you want,
#    but be aware you would need to update the code below to reflect the name.  
#    Don't worry about the file encoding bit, it simply solves an occasional 
#    problem when reading files generated in Excel.

# 2.Let's explore the data set to get an idea of what's there! This is a really
#   important step whenever you start doing analysis with new data.
#   Click on the dataset in 'Global Environment' (top right box in RStudio)
#   to view the entire dataset (it will open in the top left box in RStudio)
#   or use the View(datasetname) function to do the same thing.
#   How many variables are there? Can you guess what they might represent?

#   Try the summary(datasetname) function to start visualizing the spread 
#   of each variable:

summary(Costs)

#   the summary() function returns the mean, median, max/min, and quartiles for each variable
#   this is an easy way to quickly scan your data

#   What if we wanted to find the mean, median, and standard deviation of
#   for a single variable?
#   Let's take Annual Nitrogen Costs as an example (Costs is the name of my
#   dataset):

   mean(Costs$Nitrogen)

  # Notice how when you run this code the console returns NA. This is because there are
  # missing values in the data (look at the dataset again to see if you can find them),
  # and we need to tell R what to do about them. The easiest thing is to tell R
  # to ignore the NA values when it runs the descriptive statistics:

  mean(Costs$Nitrogen,na.rm = TRUE)
  
  # the na.rm = TRUE tells R to remove ("rm") the NAs before taking the mean
  # this can be added to many functions in R, including median()
  
  median(Costs$Nitrogen, na.rm = TRUE)

  # Now, try the same with variance and standard deviation.
  # For variance use var() and for standard deviation use sd():
  
var(Costs$Nitrogen, na.rm = TRUE)
sd(Costs$Nitrogen, na.rm = TRUE)
  
  # Before you go further, make a note of the mean, median, and sd of the Nitrogen
  # costs with NAs removed. You will need these as a comparison for the next step.
  
mean(Costs$Nitrogen,na.rm = TRUE)
97.156
median(Costs$Nitrogen, na.rm = TRUE)
115.25
var(Costs$Nitrogen, na.rm = TRUE)
3101.64
sd(Costs$Nitrogen, na.rm = TRUE)
55.69243
# 3. Now, let's redo the analysis by replacing the missing values instead of ignoring
#    them. It's important when creating (and analyzing) data to be careful with
#    missing values, and to be sure we understand what NA values represent.
#    They could mean that the data are truly missing (i.e., weren't collected), 
#    in which case removing the NAs makes sense. But those "missing" values sometimes
#    are actually '0' values (that are incorrectly entered or changed on import).  
#    With the data for Nitrogen cost,  it's plausible that they
#    could actually be 0 values.   
#    To replace the missing values from the Nitrogen variable do this:
  
  Costs$Nitrogen[is.na(Costs$Nitrogen)] = 0
  
# Make sure it worked by using the summary() function for the column,
# or by viewing the dataset again.You can also just type in
# the name of the column and run that as a command
# and it will show you the data values in the variable

  summary(Costs$Nitrogen)
  
## NOW ANSWER QUESTIONs 1 and 2 ON THE ASSIGNMENT SHEET ##
  
  mean(Costs$Price.Mg, na.rm = TRUE)
  185.247
  Costs$Price.Mg[is.na(Costs$Price.Mg)] =185.247
  
# 4. Last week, we reviewed how to create columns using basic R functions.
#    Now we are going to learn how to do it using the mutate() function from
#    Dplyr. We'll also add some basic calculations for good fun.
#    We'll start by making a column to calculate the Total Cost of all
#    the agricultural inputs:  
  
  Costs = mutate(Costs,
         Total_Cost = Costs$Machinery + Costs$Seed + Costs$Nitrogen
         + Costs$Phosphate + Costs$Potassium + Costs$Herbicide + 
           Costs$Crop.Insurance + Costs$Miscellaneous + Costs$X_Combine +
           Costs$Grain.cart + Costs$Hauling + Costs$Drying + Costs$Handling
         + Costs$Labor + Costs$Property.tax.on.owned.land)
  
  # Note: If you get an error "could not find function "mutate", make sure
  # you have installed and loaded the required packages (dplyr in this case)
  
  # This code is long but overall quite simple to understand.
  # It simply creates a new column, Total_Cost, and fills it with a value
  # calculated by adding up the costs of all the other variables. 
  
  # Take a look at the Total_Cost data. Do you notice anything funny? (It's always smart
  # to look at new columns to make sure they are working as you expect them to). 
 
   #### take a look at the data before continuing to read ###
  
  # There are NA values in some of the rows. Can you figure out why?
  
#there are still NA values for drying that correspond with NA values for Total_Cost
  
  #### take a look at the data before continuing to read ###
  
  # It looks as if one of the variables that you are adding contains
  # NA values. Can you change those NA values to 0s, and then add up 
  # the Total Cost again?
  
Costs$Drying[is.na(Costs$Drying)] = 0
  
  ## TIP: Running the Total Cost command again will simply replace the values in the
  ## existing column. But if you want to remove a column you can use the following code
  ## Dataset$Variable = NULL   (for example: Costs$Total_Cost = NULL)
  
  # Now create a new column called Annual_Revenue by multiplying 
  # the Average Yield by the Price.
  # We recommend using mutate() but if you want to experiment with other
  # methods go for it:

Costs = mutate(Costs,
               +Annual_Revenue = Costs$Average.yield.Mg * Costs$Price.Mg)
  
## NOW ANSWER QUESTION 3 ON YOUR ANSWER SHEET ##


#### TASK 2: Using a T-test to examine differences between two groups ####

# 1. The two-sample T-test is used to compare the means of two groups. The
#    test requires two variables:
#               - explanatory variable, which for a t-test is always categorical 
#                 (ie the groups you are comparing)
#               - response variable, which for a t-test is always continuous
#    Your dataset will need to have one column for the explanatory variable (ie
#    which group an individual belongs to), and another column for the 
#    response variable (measurements for the continuous variable of interest).
#    Let's explore the differences among crop types in Iowa.

# 2. Before running a T-test let's create a multiple histogram plot (it is
#    always good practice to visualize your data before you analyze it). A
#    multiple histogram plot is used to visualize the frequency distribution
#    of a numerical variable separately for each of two or more groups.
#    It allows easy comparison of the location and spread of the variable
#    in the different groups. It also helps assess whether the assumptions
#    of statistical tests are met. Let's look at the differences between Seed
#    costs across the two crop types:
  
  ggplot(Costs, aes(x=Seed)) + geom_histogram() + facet_wrap(~Crops, ncol=1)

  # If you get an error message saying 'could not find function "ggplot",
  # make sure you loaded the package using the library() command

# 3. Notice the difference in spread in seed costs between Corn and Soybean
#    Let's run a T-test to determine if the means of the two groups are 
#    significantly different:
  
    # First we want to subset the data into the two groups we are testing.
    # We want to see the difference in seed costs for Soybean and Corn:
  
    T_test1 = subset(Costs, Costs$Crops == "Soybean" | Costs$Crops == "Corn")
    
    # This command creates a new dataset with just soybean and corn crops in it. 
    # Strictly speaking, this step isn't necessary, as the original dataset 
    # only had those two crops. It's still a good skill to learn, as if you were
    # working with a dataset containing more than two groups, you'd need to do this.
    # Now for the T-test:
    
    t.test(Seed~Crops, data = T_test1, var.equal = TRUE)
    
    # Take a look at the command, and see how it is structured. It runs a t-test to
    # examine the effect of Crops on Seed costs, then gives the name of the dataset, and
    # confirms that the variance in the datasets is equal (more on this later).
    # The output gives us the test statistic t, the degrees of freedom for the test
    # (df), and the P-value for the test of equal population means. Finally,
    # it gives us the sample means for each group in the last line.
    # Make sure you understand what the p-value means and how to interpret it.
    
    # You'll be performing another round of T-tests on the next dataset. Take note
    # of these steps so you can perform them again!
    
    #Two Sample t-test
    
    #data:  Seed by Crops
    #t = 9.6273, df = 27, p-value = 3.187e-10
   # alternative hypothesis: true difference in means between group Corn and group Soybean is not equal to 0
    #95 percent confidence interval:
      #87.58632 135.03234
    #sample estimates:
     # mean in group Corn mean in group Soybean 
    #249.8200              138.5107 

## NOW ANSWER QUESTION 4 ON YOUR ANSWER SHEET ##
    
#### TASK 3: Examining the relationship between 2 continuous variables with linear regression ####
    
# In the last section we used a T-test to examine the differences between two crops.
# When the farmer assesses the total cost of their crops, however, there may be other
# variables influencing the bottom line. Here, we will examine the relationship
# between the cost of labour ("Labor" a continuous variable) and commodity prices
# ("Price.Mg" another continuous variable).
# We use linear regression to assess if there is a relationship between two continuous
# variables.
    
# Let's start by creating a scatter plot to view the relationship between our two
# variables. Use what you learned about ggplot to generate this plot.
# Think carefully about which variable goes on the x-axis, and which on the y-axis.    
# Hint: use geom_point(),theme_minimal(), and add labels with the commands xlab("yourlabel")
# and ylab("yourlabel") for a quick and easy plot!
    
    ggplot(Costs, aes(x=Price.Mg, y=Labor)) + geom_point() + 
      labs(x= "Commodity Prices", y= "Cost of Labor") + geom_smooth(method = lm)
    + theme_minimal()
    

# In R, regression is a two-step process. We start by using lm() to fit a linear model
# to the data. We then give the output of the lm() to the function summary() to see
# the results of the analysis. The lm() function has two arguments (parts). The first
# is a formula, in the form (response variable ~ explanatory variable). The second
# is the name of the data frame with the data. We will want to assign the results
# to a new object with a name (Regression 1), so that we can use the results with 
# the summary() function.
   
  # Here's an example:   
  # Let's look at the relationship between Labor costs, and commodity prices
    Regression1 = lm(Price.Mg ~ Labor, data = Costs)
    
  # Now let's see the results using the summary() of the lm() results
    summary(Regression1)
  
  # The table we get as an output provides the estimates of the slope
  # and the intercept of the linear model, in the "Coefficients" table,
  # under "Estimate". It also provides the P-value for each of these
  # coefficients, as well as the r2 value of the relationship.
    Call:
      lm(formula = Price.Mg ~ Labor, data = Costs)
    
    #Residuals:
    #  Min      1Q  Median      3Q     Max 
    #-15.301  -6.870  -2.681   6.159  20.001 
    
    Coefficients:
    #  Estimate Std. Error t value Pr(>|t|)    
   # (Intercept)  51.2136    15.3898   3.328  0.00254 ** 
   #   Labor         2.1406     0.2438   8.779 2.15e-09 ***
      ---
    #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    #Residual standard error: 10.44 on 27 degrees of freedom
   # Multiple R-squared:  0.7406,	Adjusted R-squared:  0.731 
    #F-statistic: 77.07 on 1 and 27 DF,  p-value: 2.146e-09
    
  # Make sure you understand what these P-values mean. They test the null hypothesis,
  # which is that the coefficients are equal to 0 (i.e. that there is no relationship
  # between x and y and that the intercept is 0). A low P-value (generally <0.05)
  # indicates that you can reject the null hypothesis (i.e reject the notion that 
  # there is no relationship between the x and y variables, and/or the intercept 
  # is 0. We generally care more about the slope than the intercept, but it really 
  # just depends on your question).
  
  # TIP: if you want to add the regression line to your scatter plot use:
  #      + geom_smooth(method = lm)
  #      This layer will add both the best-fitting regression line as well as the
  #      95% confidence interval for the line (thats the grey shading).
    
    
## NOW ANSWER QUESTIONS 5 AND 6 ON YOUR ANSWER SHEET ##
    
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

# 1. Load the PrairieStrips_ES.csv dataset
    
    PrarieStrips = read.csv("PrairieStrips_ES.csv") 
    
  # This dataset shows data collected from the Prairie Strips experiment. The
  # scientist laid out three different treatments:
    # 1. Row C: The conventional full row cropping system.
    # 2. 0.10PS: row crops with 10% prairie strips added in
    # 3. 0.20PS: row crops with 20% prairie strip coverage
  
  # In each system, they measured five Ecosystem Services/Characteristics:
  # Pollinator Abundance, Insect Taxa Richness, Bird Species Richness,
  # Surface Runoff, and Crop Yield.
  # Note: we will refer to all of these as ecosystem services (ES), although some 
  #       of the biodiversity measures may not strictly be ES.
    
  # The goal of this task is to determine whether there are meaningful differences
  # in ES between the different treatments.

# 2. Let's look at whether there are differences in Pollinator Abundance between
#    the three treatments. First make a hypothesis. What do you expect to see and why?
#    Next, create a quick boxplot to visualize the data:
    
 #An increase in pollinator abundance will lead to an increase in crop yeild.
 #There will be a difference in pollinator abundance amoungst the three treatments. 

  ggplot(PrarieStrips, aes(x=CropType, y=PollinatorAbundance)) + geom_boxplot() + 
  +     labs(x= "Treatment area", y= "Pollinator abundance") + 
  geom_smooth(method = lm) + theme(axis.title.x = element_text(face = "bold", size = 12),
                                   axis.text.x  = element_text(size=10), 
                                   axis.title.y = element_text(face = "bold", size = 12),
                                   axis.text.y  = element_text(size=10))

  # Was your hypothesis right?
  
#Yes, there are differeces in pollinator abundance in each of the treatment areas

  # Next we will perform an ANOVA to compare the means of all groups. Remember ANOVA
  # is similar to a t-test, but it allows us to compare more than 2 groups. Like the
  # Regression we performed this will be a two-step process:
    
  # 1. First, we fit the ANOVA model to the data using the function aov()
  ES_ANOVA1 = aov(PollinatorAbundance ~ CropType, data = MyDataset)
  # Remember to replace "MyDataset" with the name you have given to the dataset
    
  # 2. The function summary() takes the results of aov() as input and returns an ANOVA
  #    table as output:
  summary(ES_ANOVA1)

  # The output table shows the result of your ANOVA test. The null hypothesis is that
  # the mean Pollinator Abundance is the same among the three groups.
  # What do the results tell you? Is pollinator abundance statistically different?
  # Pr(>F) tells you your p-value.
  
  # The p-value is less than 0.05 (7.03e-10), therefore pollinator abundance is 
  # statistically different in the different areas 
  
## NOW ANSWER QUESTION 7 ON YOUR ANSWER SHEET ##

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

# So far you've examined whether there are significant differences for each ES
# across the three different Crop Types. The ANOVA results tell us if there are
# significant differences among crop types, but they don't tell us where those
# differences lie. Are all three crop types different from one another?
# Are two of the types similar but the third is different? It is important to 
# understand the significance of these differences across each crop type.
# Considering that a farmer could be jeopardizing their potential yields,
# the benefits between the transition from Row Crops to 10% Prairie Strips, and from
# 10% Prairie Strips to 20% Prairie Strips needs to be carefully assessed.

# In theory, we could run a series of t-tests for each possible comparison (ie
# three t-tests: Control vs 10%, Control vs 20%, and 10% vs 20%). Every time we
# run a test, however, there is a possibility that we get a p-value less than our
# alpha (0.05) purely due to chance, even if the null hypothesis is true. This 
# issue of multiple testing is particularly a concern when we are comparing many
# groups and therefore conducting many t-tests. It is therefore preferable to 
# run a "post-hoc" test, which accounts for the issues created by multiple testing.
  
# A suitable post-hoc test for us to use is the Tukey HSD test.
  
  TukeyHSD(ES_ANOVA1)
  
# Notice that we get a p-value ("p adj", which is the p-value adjusted for
# multiple comparisons) for each of our comparisons. 
  
  
## NOW ANSWER QUESTION 8 ON YOUR ANSWER SHEET ##

  
  
#### Appendix on Variance ####
# How do we check variance?
  
  # The 2-sample t-test assumes that both populations have the same variance
  # for the continuous variable. The 2-sample t-test can have very high Type 1 error
  # rates when the populations in fact have unequal variances.
  
  # Fortunately, Welch's t-test does not make this assumption of equal variance. 
  # (Remember the functions assume that the two samples are random samples from their
  # respective populations, and that the numerical variable has a normal distribution
  # in both populations).
  # The code to run Welch's t-test is exactly like that for the 2-sample t-test,
  # but with the option var.equal set to FALSE.
  
  # Welch's t-test Example (remember this will only work after you've subset your
  # data into the groups we want to test):
  t.test(PollinatorAbundance ~ CropType, data = SUBSETDATA, var.equal = FALSE)

  # Now we'll perform a Levene's test to compare the variance between groups.
  # This way, we'll know which t-test is most appropriate.
  
  # Start by installing and loading the car package
  install.packages("car")
  library(car)
  
  # Now we can run Levene's test:
  leveneTest(data = SUBSETDATA, PollinatorAbundance ~ CropType, center = mean)
  
  # Check the P-value returned by Levene's test. if the P-value were low enough we
  # would reject the null hypothesis that the different Crop Types had the same
  # variance.
  
  