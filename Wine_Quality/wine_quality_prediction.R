# Load required libraries
library("ggplot2")
library("dplyr")
library("gridExtra")
library(GGally)
library(memisc)
library(pander)
library(corrplot)

# Loading the data/ csv file
wine <- read.csv('D:/MyPortfolio/Github/R Studio/Wine_Quality/wineQualityReds.csv')
str(wine)

# Transform Quality from an integer to a Factor
wine$quality <- factor(wine$quality, ordered = T)
wine$quality

# Create a new Factored variable called 'Rating"
wine$rating <- ifelse(wine$quality <5, 'Bad', ifelse(wine$quality <7, 'Average', 'Good'))
wine$rating

wine$rating <- ordered(wine$rating, 
                       levels = c('Bad', 'Average', 'Good'))

# Structure of the DataFrame
str(wine)
# Summary of the DataFrame
summary(wine)

# Univariate Plots - plot the distribution of each of the variables
## Based on the shape:- Normal/+ve Skew/-ve Skew
## Removal of extreme outliers present -  for a more robust analysis

# Dependent Variable : Wine Quality and Rating
ggplot(data = wine, aes(x = quality)) +
  geom_bar(width = 1, color = 'black', fill = I('orange'))

ggplot(data = wine, aes(x = rating)) +
  geom_bar(width = 0.8, color = 'black', fill = I('blue'))

# Output: Most of the wines in the dataset are average quality wines:
# Thoughts;was data collected accurate, complete or incomplete, was the data from same geographical location - Was it from a big area
# Good quality and poor quality wines are almost like outliers = difficult to develop an accurate model for wine quality prediction

# Independent Variable 1: Fixed Acidity
grid.arrange(ggplot(wine, aes(x = 1, y = fixed.acidity)) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.1, color = 'red' ) +
               scale_y_continuous(lim = c(4,12)),
ggplot(data = wine, aes(x = fixed.acidity)) +
               geom_histogram(binwidth = 1, color = 'black', fill = I('orange')) +
               scale_x_continuous(lim = c(4,12)), ncol = 2)

# Output: The distribution of fixed acidity is postively skewed.

# Independent Variable 2: Volatile Acidity
grid.arrange(ggplot(wine, aes(x = 1, y = volatile.acidity)) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,1)),
             
ggplot(data = wine, aes(x = volatile.acidity)) +
               geom_histogram(binwidth = 0.05, color = 'black', fill = I('orange')) +
               scale_x_continuous(lim = c(0,1)), ncol = 2)

# Output: The distribution of volatile acidity looks like bimodal with two peaks around 0.4 and 0.6

# Independent Variable 3: Citric Acid
grid.arrange(ggplot(wine, aes(x = 1, y = citric.acid)) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             
             ggplot(data = wine, aes(x = citric.acid)) +
               geom_histogram(binwidth = 0.08, color = 'black', fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)

# Output: Apart from some outliers, the distribution of citric acid looks strange, some higher values have no data at all and apart from them, 
# Distribution looks almost rectangular; Possible there is error in the data, possibility of incomplete data

# Independent Variable 4: Residual Sugar
grid.arrange(ggplot(wine, aes(x = 1, y = residual.sugar)) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(1,8)),
             
             ggplot(data = wine, aes(x =  residual.sugar)) +
               geom_histogram(binwidth = 0.1, color = 'black', fill = I('orange')) +
               scale_x_continuous(lim = c(1,8)), ncol = 2)

# Output: Residual sugar is postively skewed with highest peak at 2 with many outliers present at higher ranges.

# Independent Variable 5: Chlorides
grid.arrange(ggplot(wine, aes(x = 1, y = chlorides )) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0, 0.25)),
             
             ggplot(data = wine, aes(x =  chlorides )) +
               geom_histogram(binwidth = 0.01, color = 'black', fill = I('orange')) +
               scale_x_continuous(lim = c(0, 0.25)), ncol = 2)

# Output: Similar distribution to residual sugar, - extreme outliers 

# Independent Variable 6: Free Sulfur Dioxide
grid.arrange(ggplot(wine, aes(x = 1, y = free.sulfur.dioxide )) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0, 45)),
             
ggplot(data = wine, aes(x =  free.sulfur.dioxide )) +
               geom_histogram(binwidth = 1, color = 'black', fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,80,5), lim = c(0, 45)), ncol = 2)

# Output: Free sulphur dioxide, high peak at 6 but then it again follows the same postively skewed long tailed patterns with some outliers in the high range.

# Independent Variable 7: Total Sulfur Dioxide
grid.arrange(ggplot(wine, aes(x = 1, y = total.sulfur.dioxide )) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0, 180)),
             
ggplot(data = wine, aes(x =  total.sulfur.dioxide )) +
               geom_histogram(binwidth = 5, color = 'black', fill = I('orange')) +
               scale_x_continuous(lim = c(0, 180)), ncol = 2)

# Output: Superset of the previous variable (Total Sulfur Dioxide), Total sulphur dioxide also follows a similar pattern

# Independent Variable 8: Density
grid.arrange(ggplot(wine, aes(x = 1, y = density )) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             
ggplot(data = wine, aes(x =  density )) +
               geom_histogram(binwidth = 0.001, color = 'black', fill = I('orange')), ncol = 2)

# Output: The Density variable, has almost a perfect Normal Distribution. Outliers present.

# Independent Variable 9: pH
grid.arrange(ggplot(wine, aes(x = 1, y = pH )) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ), 
             
ggplot(data = wine, aes(x =  pH )) +
               geom_histogram(binwidth = 0.1, color = 'black', fill = I('orange')), ncol = 2)

# Output: pH also has a very Normal distribution shape, with few outliers

# Independent Variable 10: Sulphates
grid.arrange(ggplot(wine, aes(x = 1, y = sulphates )) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0.3, 1.6)),
             
             ggplot(data = wine, aes(x =  sulphates )) +
               geom_histogram(binwidth = 0.1, color = 'black', fill = I('orange')) +
               scale_x_continuous(lim = c(0.3, 1.6)), ncol = 2)

# Output: Sulphates exhibits similar long tailed distribution like chlorides or total sulphur dioxide.
# It has relatively less outliers compared to Chlorides

# Independent Variable 11: Alcohol
grid.arrange(ggplot(wine, aes(x = 1, y = alcohol )) +
               geom_jitter(alpha =0.1) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(8, 14)),
             
ggplot(data = wine, aes(x =  alcohol )) +
               geom_histogram(binwidth = 0.1, color = 'black', fill = I('orange')) +
               scale_x_continuous(lim = c(8, 14)), ncol = 2)

# Output: Alcohol has a skewed distribution but the skewness is less than that of Residual sugars

############# CONCLUSIONS
# Univariate plots
## Analysis of the Univariate plots
# Dataset structure, 1599 rows and 13 columns originally, after the additional new column called 'rating', number of column became 14. 
# Categorical variable is 'quality' and the rest of the variables are numerical variables reflecting the physical and chemical properties of the wine.
# Quality is the point of interest in the dataset [dependent variable] - determine which factors determine the quality of a wine.
# Unique features of the dataset -  citric acid has a unique distribution compared to the other numerical variables

# Distribution and Outliers
##1. Density and pH seems normally distributed with few outliers.
##2. Residual sugar and Chloride seems to  have extreme outliers.
##3. Fixed and volatile acidity , total and free sulfur dioxides, alcohol and sulphates seem to be long-tailed for the outliers present.
##4. Citric acid has large number of zero values. Is it due to incomplete data entry??

#### Bivariate Plots
  
# Correlation table between the variables in the dataset - to find which ones may be correlated to each other.
## Bivariate Plots
# Correlation table between the variables in the dataset - find which ones may be correlated to each other.

c <- cor(
  wine %>%
    # first remove unwanted columns
    dplyr :: select(-X) %>%
    dplyr :: select(-rating) %>%
    mutate(
      # now we translate quality to a number 
      quality = as.numeric(quality)
    )
)
emphasize.strong.cells(which(abs(c) > .5 & c != 1, arr.ind = TRUE))
pandoc.table(c)

## Interpretation:
#1. Volatile acidity has +ve correlation with pH (is it possible??) - we know a decrease in pH , acidity increases = it is possible that a Simpson's Paradox is at play - further investigation is recommended.
#2. Density has a very strong correlation with fixed acidity (+ve).
#3. The variables most strongly correlated to quality are volatile acidity (-ve) and alcohol (+ve).
#4. Alcohol has -ve correlation with density, [density of water is greater than the density of alcohol.

# Strong +ve Correlations
  # Fixed acidity with Citric acid
  # Fixed acidity with Density
  # Free sulphur dioxide with Total sulphur dioxide
  # Quality with Alcohol
# Strong -ve Correlations
  # Fixed acidity with pH
  # Citric acid with Volatile acidity
  # Citric acid with pH
  # Density with Alcohol
  # Alcohol with pH
  
# Create boxplots between these variables - see if anything was missed from the correlation table

#1. Quality Vs Fixed acidity
ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_jitter (alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: Fixed acidity has almost no effect on the quality, the mean and median values of fixed acidity remains almost unchanged with increase in quality.


#2. Quality Vs Volatile acidity
ggplot(data = wine, aes(x = quality, y = volatile.acidity)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: Volatile acid seems to have -ve impact on the quality of the wine. 
# As volatile acid level goes up, the quality of the wine degrades.

#3. Quality Vs Citric acid
ggplot(data = wine, aes(x = quality, y = citric.acid)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: Citric acid seems to have +ve correlation with wine quality, better wines have higher citric acid.

#4. Quality Vs Residual sugar
ggplot(data = wine, aes(x = quality, y = residual.sugar)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  scale_y_continuous(lim =c(0,5)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: Previously I thought the residual sugar may have an effect on the wine quality, but this plot contradicts that assumption, 
# It shows it has no effect on the quality of the wine, mean value for the residual sugar is almost the same as every quality of wine.

#5. Quality Vs Chlorides
ggplot(data = wine, aes(x = quality, y = chlorides )) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  scale_y_continuous(lim =c(0,0.2)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output:  Even though weakly correlated,from the decrease in median values of the chlorides with increase in quality, 
# Seems that lower % of chlorides seems to produce better wines.

#6. Quality Vs Free sulfur dioxide
ggplot(data = wine, aes(x = quality, y = free.sulfur.dioxide )) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  scale_y_continuous(lim =c(0,40)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

#  Output: Interesting observation, Too low concentration of Free Sulphur Dioxide produces poor wine and too high concentration results in average wine.

#7. Quality Vs Total sulphur dioxide
ggplot(data = wine, aes(x = quality, y = total.sulfur.dioxide )) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  scale_y_continuous(lim =c(0,150)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: As a subset of Free sulphur dioxide, we see a very similar pattern here

#8. Quality Vs pH
ggplot(data = wine, aes(x = quality, y = pH )) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: Better wines seem to have less pH, i.e. they are more acidic, 
# There are a few outliers, RECCOMENDATION: maybe see how individual acids affect the pH.

## Individual acids Vs pH
#(a) Fixed Acidity
ggplot(data = wine, aes(x = fixed.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(5,15,1)) +
  xlab("Fixed Acidity in Log Scale") +
  geom_smooth(method="lm")

#(b) Volatile Acidity
ggplot(data = wine, aes(x = volatile.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("Volatile Acidity in Log Scale") +
  geom_smooth(method="lm")

#(c) Citric Acid
ggplot(data = subset(wine, citric.acid > 0), aes(x = citric.acid, y =pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  xlab("Citric Acid in Log Scale") +
  geom_smooth(method="lm")

# Output: These three plots brings back the old question, we saw for volatile acid, pH has +ve correlation. 
# We know acidity has -ve correlation with pH. Is it possible, its a Simpson's Paradox ??Next step is to investigate it.

# library(Simpsons)
# simpsons <- Simpsons(volatile.acidity, pH, data = wine)
# plot(simpsons)

#9. Quality Vs Sulphates
ggplot(data = wine, aes(x = quality, y = sulphates )) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  scale_y_continuous(lim =c(0.25,1)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: Even though we see many outliers in the 'Average' quality wine, seems that better wines have a stronger concentration of Sulphates.

#10. Quality Vs Alcohol
ggplot(data = wine, aes(x = quality, y = alcohol )) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# Output: Correlation is really distinct here. It is pretty evident that better wines have higher Alcohol content in it. 

# Linear model between Quality and Alcohol
alcoholQualityLinearModel <- lm(as.numeric(quality) ~ alcohol,
                                data = wine)
summary(alcoholQualityLinearModel)

# Output: Based on the value of R squared, we see that Alcohol alone contributes only 22% of the Wine Quality. 
# In-order to build a better regression model the other variables responsible need to be figured out.
# Put a correlation test against each variable to the quality of the wine.

simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric (y))$estimate)
}

correlations <- c(
  simple_cor_test(wine$fixed.acidity, wine$quality),
  simple_cor_test(wine$volatile.acidity, wine$quality),
  simple_cor_test(wine$citric.acid, wine$quality),
  simple_cor_test(log10(wine$residual.sugar), wine$quality),
  simple_cor_test(log10(wine$chlorides), wine$quality),
  simple_cor_test(wine$free.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$total.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$density, wine$quality),
  simple_cor_test(wine$pH, wine$quality),
  simple_cor_test(log10(wine$sulphates), wine$quality),
  simple_cor_test(wine$alcohol, wine$quality))
names(correlations) <- c('fixed.acidity','volatile.acidity','citric.acid',
                         'log10.residual.sugar',
                         'log10.chlorides','free.sulfur.dioxide',
                         'total.sulfur.dioxide','density','pH',
                         'log10.sulphates','alcohol')

correlations

# Output: From the correlation test, seems that the following variables have a higher correlation to Wine Quality.
# 1. Alcohol		2. Sulphates(log10)		3. Volatile Acidity		4. Citric Acid

## Analysis of Bivariate Plots
# Observations
#1. Fixed Acidity seems to have almost no effect on quality.
#2. Volatile Acidity seems to have a -ve correlation with the quality.
#3. Better wines seem to have higher concentration of Citric Acid.
#4. Better wines seem to have higher alcohol percentages. But when I created a linear model around it, I saw from the R squared 
###value that alcohol by itself only contributes like 22% on the variance of the quality, so there may be other factors in play here.
#5. Even though it's a weak correlation, lower % of Chlorides seems to produce better quality wines.
#6. Better wines see to have lower densities, then again this maybe due to the higher alcohol content in them.
#7. Better wines seem to be more acidic.
#8. Residual sugar almost has no effect on the wine quality.

## Multivariate Plots

#Alcohol plays a strong part in the quality of the wine even though it actually contributes only 22% of the total quality , 
#I will first make alcohol constant and try to insert a few more variables to see if they contribute to the overall quality in any other way.

ggplot(data = wine,
       aes(y = density, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title = 'Quality'))

# Output: With constant Alcohol, Density does not seem to play a prominent role in changing the quality of the alcohol, 
# Previous suspicion must be True that the correlation we were seeing of density with quality was due to alcohol percent.

ggplot(data = wine, 
       aes(y = sulphates, x =alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  scale_y_continuous(limits=c(0.3,1.5)) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: Looks like Wines with higher alcohol content produce better wine if they have higher level of Sulphates.

ggplot(data = wine, 
       aes(y = volatile.acidity, x =alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: Looks like Volatile acid has just the opposite effect, 
# With less concentration of volatile acid and higher concentration of alcohol seems to produce better wines.

ggplot(data = wine, 
       aes(y = pH, x =alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: Low pH and high Alcohol percentage seems to produce better wines.

ggplot(data = wine, 
       aes(y = residual.sugar, x =alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: No such correlation between residual sugar and quality.

ggplot(data = wine, 
       aes(y = total.sulfur.dioxide, x =alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: Lower total sulphur dioxide seems to produce better wine even though some high outliers for better wine with sulphur dioxide.

###### NEXT; Try to investigate the effect of Acids on the Quality of Wines.

ggplot(data = wine, 
       aes(y = citric.acid, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: Higher Citric acid and low volatile acid seems to produce better wines.

ggplot(data = wine, 
       aes(y = citric.acid, x = fixed.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: Not much correlations 

ggplot(data = wine, 
       aes(y = fixed.acidity, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size=1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq',
                     guide=guide_legend(title='Quality'))

# Output: Not much correlation with the quality here


########### Linear Modelling 
# Using the variables most strongly correlated with the quality of wine from the analysis in generating a linear model

set.seed(1221)
training_data <- sample_frac(wine, .6)
test_data <- wine[ !wine$X %in% training_data$X, ]
m1 <- lm(as.numeric(quality) ~ alcohol, data = training_data)
m2 <- update(m1, ~ . + sulphates)
m3 <- update(m2, ~ . + volatile.acidity)
m4 <- update(m3, ~ . + citric.acid)
m5 <- update(m4, ~ . + fixed.acidity)
m6 <- update(m2, ~ . + pH)
mtable(m1,m2,m3,m4,m5,m6)

wine_predict <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(wine_predict) <- c("quality", "error")
ggplot(data = wine_predict, aes(x=quality, y=error)) +
  geom_jitter(alpha = 0.3)


### Analysis of the multivariate plots
# Observations
#1. High Alcohol and sulphate content seem to produce better wines.
#2. Citric Acid, even thought weakly correlated plays a part in improving the wine quality

## Linear Models created
# I created a couple of linear models, main problem was there was not enough statistict to have a significant confidence level in the equations produced.
# Because of the low R2 value, I saw that alcohol contibutes 22% of wine quality and most factors converged on the average quality wine. 
## - This can be due to the fact that the dataset comprised mainly of 'Average' quality wines and there are very few data on 'Good' and 'Bad' quality wines in the training dataset.
# That is why its difficult to predict statistics for the edge case, Maybe a more complete dataset would have helped in better predictions of the higher range values.


































































