

library(RCurl)
# Import the HANES data set from GitHub; break the string into two for readability
# (Please note this readability aspect very carefully)
URL_text_1 <- "https://raw.githubusercontent.com/kannan-kasthuri/kannan-kasthuri.github.io"
URL_text_2 <- "/master/Datasets/HANES/NYC_HANES_DIAB.csv"
# Paste it to constitute a single URL 
URL <- paste(URL_text_1,URL_text_2, sep="")
HANES <- read.csv(text=getURL(URL))
# Rename the GENDER factor for identification
HANES$GENDER <- factor(HANES$GENDER, labels=c("M","F"))
# Rename the AGEGROUP factor for identification
HANES$AGEGROUP <- factor(HANES$AGEGROUP, labels=c("20-39","40-59","60+"))
# Rename the HSQ_1 factor for identification
HANES$HSQ_1 <- factor(HANES$HSQ_1, labels=c("Excellent","Very Good","Good", "Fair", "Poor"))
# Rename the DX_DBTS as a factor
HANES$DX_DBTS <- factor(HANES$DX_DBTS, labels=c("DIAB","DIAB NO_DX","NO DIAB"))
# Omit all NA from the data frame
HANES <- na.omit(HANES)
# Observe the structure
str(HANES)
## 'data.frame':    1112 obs. of  23 variables:
##  $ KEY              : Factor w/ 1527 levels "133370A","133370B",..: 28 43 44 53 55 70 84 90 100 107 ...
##  $ GENDER           : Factor w/ 2 levels "M","F": 1 1 1 1 1 1 1 1 1 1 ...
##  $ SPAGE            : int  29 28 27 24 30 26 31 32 34 32 ...
##  $ AGEGROUP         : Factor w/ 3 levels "20-39","40-59",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ HSQ_1            : Factor w/ 5 levels "Excellent","Very Good",..: 2 2 2 1 1 3 1 2 1 3 ...
##  $ UCREATININE      : int  105 53 314 105 163 150 46 36 177 156 ...
##  $ UALBUMIN         : num  0.707 1 8 4 3 2 2 0.707 4 3 ...
##  $ UACR             : num  0.00673 2 3 4 2 ...
##  $ MERCURYU         : num  0.37 0.106 0.487 2.205 0.979 ...
##  $ DX_DBTS          : Factor w/ 3 levels "DIAB","DIAB NO_DX",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ A1C              : num  5 5.2 4.8 5.1 4.3 5.2 4.8 5.2 4.8 5.2 ...
##  $ CADMIUM          : num  0.2412 0.1732 0.0644 0.0929 0.1202 ...
##  $ LEAD             : num  1.454 1.019 0.863 1.243 0.612 ...
##  $ MERCURYTOTALBLOOD: num  2.34 2.57 1.32 14.66 2.13 ...
##  $ HDL              : int  42 51 42 61 52 50 57 56 42 44 ...
##  $ CHOLESTEROLTOTAL : int  184 157 145 206 120 155 156 235 156 120 ...
##  $ GLUCOSESI        : num  4.61 4.77 5.16 5 5.11 ...
##  $ CREATININESI     : num  74.3 73 80 84.9 66 ...
##  $ CREATININE       : num  0.84 0.83 0.91 0.96 0.75 0.99 0.9 0.84 0.93 1.09 ...
##  $ TRIGLYCERIDE     : int  156 43 108 65 51 29 31 220 82 35 ...
##  $ GLUCOSE          : int  83 86 93 90 92 85 72 87 96 92 ...
##  $ COTININE         : num  31.5918 0.0635 0.035 0.0514 0.035 ...
##  $ LDLESTIMATE      : int  111 97 81 132 58 99 93 135 98 69 ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:415] 2 15 16 24 26 28 33 34 35 39 ...
##   .. ..- attr(*, "names")= chr [1:415] "2" "15" "16" "24" ...








################# DATA SCIENCE FUNDAMENTALS 01: VISUAL AND EXPLORE ####################
#######################################################################################


# HOMEWORK NOTES: I USE EXAMPLES FROM CLASS AS A GUIDE FOR MY OWN EXAMPLES #

library(tidyverse)
ggplot(data=HANES)+
  geom_point(mapping = aes(x=A1C, y=DX_DBTS))

nrow=(HANES)

# There are 1112 observations and 23 variables.

ncol=(HANES)

# Here we can see that most of the people that have diabetes have a A1C level of atleast 5.0 and there is a lot of 
# density around the 7.5 test level area. This makes sense because the consensus is that people get diabetes when 
# their test levels start eaching near 6%. 

# This plot shows a positive relationship between A1C levels and having diabetes. In other words, the higher someone's 
# A1C levels, the more likely they are to have been diagnosed with diabetes. This confirms my hypothesis. 


# Below - MAke a ggplot with asthetic color for he variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR), color=DX_DBTS))

# This color visualization/graph reveals many things. Here it can be seen that most of 
# the people in the clusters (each dot represens a person) does not have diabetes. 
# Thus, it appears that UACR and A1C may not be risk factors for diabetes. 
# Just to recap, A1C is blood test level. Research shows that A1C test results of 6.5% or above
# indicate diabetes and prediabetes is from a A1C test of 5.7% to 6.4% so these results are
# consistent with mainstream data. Source: https://www.virginiamason.org/whatarenormalbloodglucoselevels


# Below - MAke a ggplot with asthetic color for he variable DX_DBTS and x log Glucose with y log Trigyceride

ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(GLUCOSE), y = log(TRIGLYCERIDE), color=DX_DBTS))

# This color visualization/graph reveals many things. Here it can be seen that most of 
# the people have no diabetes. The people that do have diabetes when considering the log(triglyceride) vs log(Glucose)
# is seldom and sparse. This may indicate that triglycerie and glucose levels are not sole risk factors for diabetes. 

ggplot(data = HANES) +
  geom_point(mapping = aes(x = log(GLUCOSESI), y = log(CREATININESI), color=DX_DBTS))

# Make a ggplot with asthetic size for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR), size=AGEGROUP))

ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(GLUCOSESI), y = log(CREATININESI), size=AGEGROUP))

# Make a ggplot with asthetic size for the variable DX_DBTS using x log glucose and y log trigyceride
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(GLUCOSE), y = log(TRIGLYCERIDE), size=AGEGROUP))
# For the above graph we see that the dots are congragated in one general areal between values of 3 and 5.5 on the 
# log(Triglyceride) scale and from 4.25 and 4.75 on the log(glucose) scale. It is difficult to see the shape of the 
# dots to indicate which dot is large or small so another type of graph would be best to look at indicating agegroup.
# I need to retry this graph adjusting for transparency and shape of the dots so I am better able to view what
# exactly is going on and analyze sufficiently what is going on. 


# Make a ggplot with asthetic alpha for the variable DX_DBTS using x log glucose and y log trigyceride
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(GLUCOSE), y = log(TRIGLYCERIDE), alpha=DX_DBTS))

# Now, after mapping DX_DBTS to the alpha aesthetic, the control over transparency of the points is significantly improved.
# Here we see that most of the people have no diabetes. 

# Make a ggplot with asthetic shape for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(GLUCOSE), y = log(TRIGLYCERIDE), shape=DX_DBTS))

#Nowthe shapes of the dots have altered and produces the same information as above, this is just a different way of viewing the visualization with shapes.

# Make a ggplot with asthetic shape for the variable DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR)), color="blue")

# Make a ggplot with facets
ggplot(data=HANES) +
  geom_point(mapping = aes (x = log (A1C), y = log(UACR))) +
  facet_wrap(~ DX_DBTS, nrow = 2)

ggplot(data=HANES) +
  geom_point(mapping = aes (x = log (GLUCOSE), y = log(TRIGLYCERIDE))) +
  facet_wrap(~ DX_DBTS, nrow = 2)

# This visualization is great in enabling us to view DIAB, DIAB No_DX and NO DIAB
# all based on log triglyceride (x axis) and log glucose (y axis)

# Make a ggplot with facet grid - GENDER vs DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR))) + 
  facet_grid(GENDER ~ DX_DBTS)


# Make a ggplot with facet grid - GENDER vs DX_DBTS - GLUCOSE ND TRIGLYCERIDE INSTEAD OF A1C AND UACR
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(GLUCOSE), y = log(TRIGLYCERIDE))) + 
  facet_grid(GENDER ~ DX_DBTS)

# Make a ggplot with facet grid - AGEGROUP vs DX_DBTS
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(GLUCOSE), y = log(TRIGLYCERIDE))) + 
  facet_grid(AGEGROUP ~ DX_DBTS)

ggplot(data = HANES) +
  geom_point(mapping = aes(x = log(GLUCOSESI), y = log(CREATININESI))) +
  facet_grid(AGEGROUP ~ DX_DBTS)

# Plot smooth and point geom in the same plot 
ggplot(data = HANES) + 
  geom_smooth(mapping = aes(x = GLUCOSE, y = TRIGLYCERIDE, color=GENDER)) + 
  xlim(c(0,400)) + ylim(c(0,425)) +
  geom_point(mapping = aes(x = GLUCOSE, y = TRIGLYCERIDE, color=GENDER))

# Plot smooth and point geom in the same plot 
ggplot(data = HANES, mapping = aes(x = GLUCOSE, y = TRIGLYCERIDE, color=GENDER)) + 
  xlim(c(0,400)) + ylim(c(0,425)) +
  geom_smooth() + 
  geom_point()

# Plot smooth and point geom in the same plot 
ggplot(data = HANES, mapping = aes(x = UCREATININE, y = UALBUMIN)) + 
  xlim(c(0,100)) + ylim(c(0,10)) +
  geom_smooth(mapping = aes(color = AGEGROUP)) + 
  geom_point() 

################# DATA SCIENCE FUNDAMENTALS 02: TRANSFORM AND EXPLORE ####################
#######################################################################################


# Convert HANES data frame into a tibble and observe it
HANES_TIB <- as.tibble(HANES)
HANES_TIB

filter(HANES, SPAGE == 55, TRIGLYCERIDE==500)

# I wrote the above code because I wanted to pick all records of patients that were
# age == 55 and had Triglyceride levels above 500 (which is considered dangerous triglyceride levels.
# However, this came to be out to 0 rows. So now I will explore a different code/variable to explore.

filter(HANES, SPAGE == 50, HDL == 50, GENDER == "M")

# Here I looked for age 50, HDL level of 50, and gender male but results came out to be 0.

filter(HANES, SPAGE == 40, HDL == 50)

# Above I looked for those age of 40 with HDL levels of 50. 

filter(HANES, SPAGE >= 40 & SPAGE <= 46, HDL == 50)

# Pick all the records of patients who are between 45 and 46 and HDL value of 50
filter(HANES, SPAGE >= 45 & SPAGE <= 46, HDL == 50)

# Using the example above I want to pick patients that are between 40 and 50 and have HDL value of 50 and are female
filter(HANES, SPAGE >=40 & SPAGE <=50, HDL ==50, GENDER == "F")

# Arrange the data frame arranging GENDER and AGE with descending order of the variable UCREATININE - Class Lecture example
a <- arrange(HANES, GENDER, SPAGE, desc(UCREATININE))
atib <- as.tibble(a)
atib

tail(atib)

# Arrange the data frame arranging GENDER and AGE with descending order of the variable HDL - Non class lecture example using HDL instead of UCREATININE
a <- arrange(HANES, GENDER, SPAGE, desc(HDL))
atib <- as.tibble(a)
atib

tail(atib)

# Arrange the data frame arranging GENDER and AGE GROUP WITH DESCENDING ORDER OF THE VARIABLE HSQ_1
# HERE I HAVE ARRANGED - SORTED HANES TO FIND PATIENTS WITH POOR TO EXCELLENT HSQ_1 
a <- arrange(HANES, GENDER, AGEGROUP, desc(HSQ_1))
atib <- as.tibble(a)
atib

tail(atib)

# SELECT ONLY THE HSQ_1 AND GLUCOSE VARIABLES FROM THE HANES DATA
s <- select(HANES, HSQ_1, GLUCOSE)
as.tibble(s)

# Now I am going to push the HSQ_1 and HDL variables to the front keeping everything
s <- select(HANES, HSQ_1, HDL, everything())
as.tibble(s)












################ DATA SCIENCE FUNDAMENTALS 03 AND 04: EXPLORATORY DATA ANALYSIS ####################
#######################################################################################


# Find the distribution of people with diabetes diagnostic status 

ggplot(data=HANES)+
  geom_point(mapping = aes(x=HDL, y=A1C))

ggplot(data=HANES)+
  geom_point(mapping = aes(x=HDL, y=A1C, color=DX_DBTS))

ggplot(data=HANES)+
  geom_point(mapping = aes(x=HDL, y=A1C, color=GENDER))

# Based on this visualization we can see that Females appear to have higher levels of HDL and men have lower levels of 
# HDL. There are not manycounts for A1C levels above 7.5 which is in line with consensus. The consensus is that A1C 
# levels above 7.5 is rare. My hypothesis was that men would have higher levels of HDL so this graph goes against my 
# hypothesis that men would have higher levels of HDL. 

ggplot(data = HANES) +
  geom_bar(mapping = aes(x = DX_DBTS))

# The vast majority of people are not diagnosed with daibetes. Almost 1000 people are not diagnosed with diabetes
# compared to roughly 125 people that are diagnosed with diabetes.

ggplot(data = HANES) +
  geom_bar(mapping = aes(x = HSQ_1))

#/ HSQ_1 - In this visualization it is evident that most of the people have either Good, Very Good, or Excellent
# HSQ_1 levels, in that order. The next highest ranking groups are fair and then poor. 

ggplot(data= HANES) + 
  geom_bar(mapping = aes(x=CADMIUM))

# The count for those with CADMIUM is so low that this does not warrant further analysis. 
# It is good to know that CADMIUM has a low count and hence, can not produce significant
# and meaningful data along this journey of data exploration.

ggplot(data=HANES) + 
  geom_bar(mapping = aes(x=MERCURYTOTALBLOOD))

# This graph is empty. This is good to know as there is either something
# wrong or there is not enough data. Either way this is not a variable to 
# use right now and something I may want to check on later.

ggplot(data=HANES) + 
  geom_bar(mapping = aes(x =A1C))

#/EXPLORATORY DATA ANALYSIS 

# EXAMPLE - FIND THE DISTRIBUTION OF PEOPLE WITH CHOLESTEROLTOTAL 
# DIAGNOSTIC STATUS
ggplot(data=HANES) + 
  geom_bar(mapping=aes(x = CHOLESTEROLTOTAL))

# THERE APPEARS TO BE SOME SPIKES IN THE DISTRIBUTION. IT IS 
# INTERSTING AND THUS I QUESTION. ARE THERE OUTLIERS? HOW MANY AND WHY?
# ADDITIONALLY... WHICH VALUES ARE COMMON? ARE THERE RARE VALUES?
# ARE THERE TYPICAL VALUES? ARE THERE ANY ODD PATTERNS? OR ARE THE
# PATTERNS DISPLAYING/VISUALIZING AS EXPECTED?

# THERE DOESN'T APPEAR TO BE ANY IRREGULARITY IN THE CHOLESTEROLTOTAL
# GRAPH BECAUSE IT APPEARS THAT THE GRAPH SEEMS TO BE NORMALLY DISTRIBUTED
# EVEN THOUGH IT IS SLIGHLY SKEWED. IMMEDIATELY UPON LOOKING AT THE GRAPH
# I NOTICE OUTLIERS, BUT NOT ANYTHING TO OBSCENE. 

# Manually find the counts
HANES %>% count(DX_DBTS)

# EDA - MANUALLY FIND THE COUNTS FOR CHOLESTEROLTOTAL
HANES %>% count(CHOLESTEROLTOTAL)

# Find the distribution of the GLUCOSE levels in patients
ggplot(data = HANES) +
  geom_histogram(mapping = aes(x = GLUCOSE))

# EDA 
ggplot(data=HANES) +
  geom_histogram(mapping = aes(x = GLUCOSESI))

ggplot(data=HANES) + 
  geom_bar(mapping = aes(x=CREATININESI))

#THE GRAPH OF THE CREATININESI DOES NOT TELL USE MUCH, THERE IS JUST ONE SPIKE. THIS IS HELPFUL TO KNOW
# FOR EXAMPLE IF A PI WANTED TO KNOW ABOUT WHETHER OR NOT CREATINENESI WOULD BE A GOOD VARIABLE 
# TO INCLUDE IN ANALYSIS, I COULD CONCLUDE THAT NO, THERE IS NOT ENOUGH SIGNIFICANT COUNT/INFORMATION
# AVAILABLE ABOUT THE CREATININESI VARIABLE TO INCLUDE IN OUR ANALYSIS AND RECOMMEND INSTEAD TO USE GLUCOSE, HDL, OR DX_DBST SINCE
# THERE IS SIGNFICANT/QUANTIFIABLE INFORMATION AVAILABLE AND READY FOR SUFFICIENT LEVELS OF BASIC ANALYSIS.

ggplot(data=HANES) + 
  geom_bar(mapping = aes(x=HDL))

# Find the distribution of the GLUCOSE levels in different age groups
ggplot(data = HANES, mapping = aes(x = GLUCOSE, colour = AGEGROUP)) +
  geom_freqpoly()

#EDA
ggplot(data = HANES, mapping = aes (x=GLUCOSE, colour = GENDER)) +
  geom_freqpoly()

# Find the distribution of the HDL levels in different health status
ggplot(data = HANES, mapping = aes(x = HDL, colour = HSQ_1)) +
  geom_freqpoly()

#Explorative Data Analysis Questions Below:

# Which values are most common? Least common? 
# Are there any patterns going on? What about unsual patterns?
# What is the data telling us? 
# Do we see any rarities in these patterns? 
# Does anything seem out of the norm or unusual? 


# Find the distribution of log(UACR) for different age groups
ggplot(data = HANES, mapping = aes(x = log(UACR), colour = AGEGROUP)) +
  geom_freqpoly()

# EDA Example

ggplot(data = HANES, mapping = aes (x = log(UACR), colour= GENDER)) + 
  geom_freqpoly()

# This time I am looking at GENDER instead of AGEGROUP to explore my
# data even further

# Find the distribution of the CREATININE levels in patients
ggplot(data = HANES) +
  geom_histogram(mapping = aes(x = COTININE))

# DISTRIBUTION OF CHOLESTEROLTOTAL IN PATIENTS 

ggplot(data = HANES) +
  geom_histogram(mapping = aes(x = CHOLESTEROLTOTAL))
# Are there any outliers? If so, is there a significant amount of 
# outliers? Here, I can see that there are some outliers. I have
# found enough information to see that this warrants looking into 
# this further. 

# Find the distribution of the CREATININE levels in patients
# restricted to the limits 0 to 0.5
ggplot(data = HANES) +
  geom_histogram(mapping = aes(x = COTININE)) + xlim(c(0,0.5)) 

ggplot(data = HANES) +
  geom_histogram(mapping = aes(x = CHOLESTORALTOTAL)) + xlim(c(0,0.5)) 

ggplot(data = HANES) +
  geom_histogram(mapping = aes(x = HDL)) + xlim(c(0,0.5)) 

# In all of the above graphs, is looking at count on the y axis the best way to analyze the data? I think not. 
# In order to go in dephth with his analysis we need to break down the variables by more than just count, 
# such as agegroup, race, and gender. Furthermore, we need to see why certain groups have certains counts and 
# how these variables are distributed based on certain factors. Will certain relaionships change based on 
# individual subgroups of the data? 

# EDA NOTE - MUCH EASIER TO LOOK AT OVERLAPPING LINES VS. BARS

# From the HANES data set
unusual <- HANES %>% 
  # filter people with CREATININE levels between 0.42 and 0.37
  filter(CREATININE <= 0.42 & CREATININE > 0.375) %>% 
  # select only the people id (key) and gender
  select(CREATININE, KEY, GENDER) %>%
  # and order CREATININE as a first variable
  arrange(CREATININE)
# And display it as a tibble
as.tibble(unusual) 


############### PART 2 OF EXPLORATORY DATA ANALYSIS  ####################


# Input HANSES
HANES_repl_unusual <- HANES %>% 
  # Replace using ifelse
  mutate(CREATININE = ifelse(CREATININE <= 0.42 & CREATININE > 0.375, NA, CREATININE))

# Load the package RCurl
library(RCurl)
# Import the HANES data set from GitHub; break the string into two for readability
# (Please note this readability aspect very carefully)
URL_text_1 <- "https://raw.githubusercontent.com/kannan-kasthuri/kannan-kasthuri.github.io"
URL_text_2 <- "/master/Datasets/HANES/HANES.original.csv"
# Paste it to constitute a single URL 
URL <- paste(URL_text_1,URL_text_2, sep="")
HANES_with_NA <- read.csv(text=getURL(URL))

# From the HANES_with_NA dataset
HANES_with_NA %>% 
  # Find the glucose levels of people with no A1C through the
  # logical operator/function is.na()
  mutate(no_A1C = is.na(A1C),
         glucose_level = GLUCOSE) %>% 
  # Now plot the result for the logical categories
  ggplot(mapping = aes(glucose_level)) + 
  geom_freqpoly(mapping = aes(colour = no_A1C), binwidth = 1/4)

### COVARIATION

# Plot the frequency distribution of mercury for different age groups
ggplot(data = HANES, mapping = aes(x = MERCURYU)) + 
  geom_freqpoly(mapping = aes(colour = AGEGROUP), binwidth = 1)

#   TRY OTHER VARIABLES

ggplot(data = HANES, mapping = aes(x = LEAD)) + 
  geom_freqpoly(mapping = aes(colour = AGEGROUP), binwidth = 1)

# THIS PLOT THAT SAYS PEOPLE IN AGE GROUPS 20-39 IS THE LARGEST
# GROUP WITH LEAD MIGHT BE MISLEADING BECAUSE THE NUMBER
# OF PEOPLE IN EACH GROUP MIGHT ACTUALLY DIFFER. EXPLORATORY
# DATA ANALYSIS WILL HELP UNCOVER MORE OF WHAT IS GOING ON.

# Find the distribution of people in each age group
ggplot(HANES) + 
  geom_bar(mapping = aes(x = AGEGROUP))

# WE CAN SEE FROM THIS GRAPH THAT THE NUMBER OF 
# PEOPLE IN THE GROUP 20-39 IS ACTUALLY ESS THAN 
# THE NUMBER OF PEOPLE IN THE GROUP 40-59. 

# FURTHERMORE, THE NUMBER OF PEOPLE IN GROUP 40-59
# IS LESS THAN THE NUMBER OF PEOPLE IN THE GROUP OF 
# PEOPLE OVER 60. HENCE, TO EXPLORE THIS DATA 
# FURTHER, WE NEED TO CONDUCT MORE EXPLORATORY DATA ANALYSIS

# Plot density instead of count for LEAD variable
ggplot(data = HANES, mapping = aes(x = LEAD, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = AGEGROUP), binwidth = 1)

# I CAN SEE THAT ALL OF THE GROUPS DO NOT HAVE EQUAL NUMBERS OF 
# PEOPLE WITHIN LEAD LEVELS...HOWEVER IF WE DID A PLOT DENSITY 
# GRAPH OF MERCURYU WE WOULD SEE RIGHT AWAY THAT ALL THE GROUPS
# HAVE EQUAL NUMBER OF PEOPLE

#We can look at the variation between A1C and AGEGROUP through a box plot.

# Make a box plot for A1C and AGEGROUP
ggplot(data = HANES, mapping = aes(x = AGEGROUP, y = A1C)) +
  geom_boxplot()

# To explore this data further using an example of my own
# I am going to make a box plot for LEAD and AGEGROUP and then
# TRIGLYCERIDE and AGEGROUP

ggplot(data = HANES, mapping = aes(x = AGEGROUP, y = LEAD)) +
  geom_boxplot()

# THE AGE GROUP 20-39 SHOWS THE SMALLEST VARIATION. THE AGE
# GROUP 40-59 SHOWS THE SECOND LARGEST VARIATION AND THEN 
# AGE GROUP 60+ SHOWS THE LARGEST VARIABTION IN AGE GROUP 
# ACCORDING TO THE BOX PLOT LOOKING AT AGE GROUP AND LEAD.

ggplot(data = HANES, mapping = aes(x = GENDER, y = LEAD)) +
  geom_boxplot()


# What vaiales might be affecting this relationship? 
# It appears that men have a wider distribution range of lead amount compared with females.
# However, what might be accounting for this wider distribution in men? Perhaps it can be environmental hazards or exposure
# to more hazardous working conditions or it can be just more expsore working jobs such as construction where men dominate
# the field and are more likely to be around lead. 

ggplot(data = HANES, mapping = aes(x = AGEGROUP, y = CHOLESTEROLTOTAL)) +
  geom_boxplot()

# The greatest variation is in the 50+ group. 
# Questions to further explore is what factors contribute to 
# the whide spread in age group 50+ and cholesteroltotal
# It does make sense that the largest spread for cholesteroltotal would be in the 50+ age group given 
# historic risk factors greatly associated with increased risk for higher and dangerous cholesterol
# levels increasing with age. 

ggplot(data = HANES, mapping = aes(x = GENDER, y = CHOLESTEROLTOTAL)) +
  geom_boxplot()

# Here we can see the differences by gender. It might be worth exploring why there are differences and what factors/variables are contributing to these differences

# Make a box plot for HDL and HSQ_1
ggplot(data = HANES, mapping = aes(x = HSQ_1, y = HDL)) +
  geom_boxplot()

# Now I will make a box plot for other variables to further explore this data set
ggplot(data = HANES, mapping = aes(x = AGEGROUP, y = CREATININESI)) +
  geom_boxplot()

# THIS GRAPH CONFIRMS THAT THE AGE GROUP 40-59 HAS THE LOWEST COUNT FOR CREATININESI
# WHICH ALIGNS WITH OUR OBSERVATIONS

ggplot(data = HANES, mapping = aes(x = AGEGROUP, y = GLUCOSE)) +
  geom_boxplot()

# We can see that the smallest amount of variation is in the age group 20-39, followed by age group 40-59, and then 
# followed by age group 60+. This makes sense since this is a box plot of age group against glucose levels and 
# research shows glucose levels tend to spread more/grow more as one's age increases. This would be an interesting
# study to look into - especially if glucose levels could be deterimined by comordities and other factors such as 
# race and socioeconomic level.

# Make a box plot for HDL and HSQ_1 based on reordering by median values
ggplot(data = HANES, mapping = aes(x = reorder(HSQ_1, HDL, FUN = median), y = HDL)) +
  geom_boxplot()

# EXPLORE USING OTHER VARIABLES

ggplot(data = HANES, mapping = aes(x = reorder(AGEGROUP, GLUCOSE, FUN = median), y = HDL)) +
  geom_boxplot()

# Make a box plot for HDL and HSQ_1 based on reordering by median values
# and flip coordinates
ggplot(data = HANES, mapping = aes(x = reorder(HSQ_1, HDL, FUN = median), y = HDL)) +
  geom_boxplot() + coord_flip()

#EXPLORE USING OTHER VARIABLES

ggplot(data = HANES, mapping = aes(x = reorder(HSQ_1, GLUCOSE, FUN = median), y = HDL)) +
  geom_boxplot() + coord_flip()

ggplot(data = HANES, mapping = aes(x = reorder(AGEGROUP, HDL, FUN = median), y = HDL)) +
  geom_boxplot() + coord_flip()

#### - VARIATION BETWEEN TWO (2) CATEGORICAL VARIABLES #######

# Plot two categorical variables AGEGROUP and diabetes status DX_DBTS
# using geom_count
ggplot(data = HANES) +
  geom_count(mapping = aes(x = AGEGROUP, y = DX_DBTS))

# EXPLORE SAME TECHNIQUE ABOVE USING DIFFERENT VARIABLES

ggplot(data = HANES) +
  geom_count(mapping = aes(x = AGEGROUP, y = HDL))

# THERE APPEARS TO BE A VERY STRONG CORRELATION BETWEEN HDL AND AGEGROUP. THIS IS FOUND FROM REMEMBERING THAT
# THE SIZE OF EACH AND EVERY CIRCLE DISPLAYS THE AMOUNT OF OBSERVATIONS AND THAT COVARIATION WILL LOOK AS THOUGH
# THERE IS A STRONG CORRELATION BETWEEN SPECIFIC Y AND X VARIABLES/VALUES. 

# Count the people in age group and health status
HANES %>% 
  count(AGEGROUP, HSQ_1) %>%  
  ggplot(mapping = aes(x = AGEGROUP, y = HSQ_1)) +
  # and use the geom_tile to fill in the aesthetics
  geom_tile(mapping = aes(fill = n))

# Plot the variation between LDLESTIMATE and CHOLESTEROLTOTAL
ggplot(data = HANES) +
  geom_point(mapping = aes(x = LDLESTIMATE, y = CHOLESTEROLTOTAL))

# EXPLORE SAME TECHNIQUE ABOVE USING DIFFERENT VARIABLES

ggplot(data = HANES) +
  geom_point(mapping = aes(x = LDLESTIMATE, y = HDL))

# THIS GRAPH LOOKS VERY INTERESTING, THERE IS NOT SLOPE IN THIS GRAPH, INSTEAD THE DOTS ARE AT FIRST IN A CIRCLE
# AND THEN BECOME SCATTERED AND SPREAD OUT AND THE LDLESTIMATE AND HDL FIGURES INCREASE

# Plot the variation between LDLESTIMATE and CHOLESTEROLTOTAL 
# adding alpha aesthetics
ggplot(data = HANES) +
  geom_point(mapping = aes(x = LDLESTIMATE, y = CHOLESTEROLTOTAL), alpha = 1 / 5)

# Plot the variation between LDLESTIMATE and CHOLESTEROLTOTAL 
# using geom_bin2d
ggplot(data = HANES) +
  geom_bin2d(mapping = aes(x = LDLESTIMATE, y = CHOLESTEROLTOTAL))

install.packages("hexbin",repos = "http://cran.r-project.org/")

# install.packages("hexbin")
# Plot the variation between LDLESTIMATE and CHOLESTEROLTOTAL 
# using geom_hex
ggplot(data = HANES) +
  geom_hex(mapping = aes(x = LDLESTIMATE, y = CHOLESTEROLTOTAL))

# checking other variables 

ggplot(data = HANES) +
  geom_hex(mapping = aes(x = LDLESTIMATE, y = GLUCOSE))

# This graph shows a strong upward correlation. we can still 
# explore his data further.

# Bin LDLESTIMATE using cut_width argument and do a box plot like we 
# did between categorical and continuous variable allowing the size 
# of the box plot represent the number of points - varwidth argument
ggplot(data = HANES, mapping = aes(x = LDLESTIMATE, y = CHOLESTEROLTOTAL)) + 
  geom_boxplot(mapping = aes(group = cut_width(LDLESTIMATE, 20)), varwidth = TRUE)

# This graph looks quite odd. Is this rare? There appears to be outliers. 
# Further significant (past the scope of this assignement) exploratory data analysis 
# could be performed to ascertain the scope of this issue.


##- using other variables 

ggplot(data = HANES, mapping = aes(x = LDLESTIMATE, y = CHOLESTEROLTOTAL)) + 
  geom_boxplot(mapping = aes(group = cut_width(LDLESTIMATE, 20)), varwidth = TRUE)

# Use the same number of points for LDLESTIMATE using cut_number argument 
# and do a box plot like we did between categorical and continuous variable 

ggplot(data = HANES, mapping = aes(x = LDLESTIMATE, y = CHOLESTEROLTOTAL)) + 
  geom_boxplot(mapping = aes(group = cut_number(LDLESTIMATE, 20)))

ggplot(data = HANES, mapping = aes(x = LDLESTIMATE, y = GLUCOSE)) + 
  geom_boxplot(mapping = aes(group = cut_number(LDLESTIMATE, 20)))

ggplot(data = HANES, mapping = aes(x = LDLESTIMATE, y = DX_DBTS)) + 
  geom_boxplot(mapping = aes(group = cut_number(LDLESTIMATE, 20)))



############### PATTERNS AND MODELS ################

# Make a ggplot out of log(A1C) and log(UACR) variables
ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(UACR)))

#### 

ggplot(data = HANES) + 
  geom_point(mapping = aes(x = log(A1C), y = log(GLUCOSE)))

# There appears to be a lot of outliers. Why? What factors are contributing to these outliers? Past the scope of this
# classroom assignment would be to further explore the extent and complexity and risk factors for these outliers.


# Load the modelr library
library(modelr)
# Compute the model using the `lm` - linear regression function
mod <- lm(CHOLESTEROLTOTAL ~ LDLESTIMATE , data = HANES)

# Patterns and models

mod <- lm(GLUCOSE ~ LDLESTIMATE , data = HANES)
# add the residuals to the data and name it as resid
HANES_mod <- HANES %>% 
  add_residuals(mod) %>% 
  mutate(resid = resid)
# Plot the total cholesterol and residuals
ggplot(data = HANES_mod) + 
  geom_point(mapping = aes(x = GLUCOSE, y = resid))

# Patterns show us that there might be certain variables that affect the relationship and if so what are those variables? 
# In this case the veriable, since we are looking at glucose and ldestimate can be age and gender, but especially age.
# We saw earlier in our exploratory analysis that age group had significant impacts on the variation in glucose and ldestimate
# with older groups haveing higher levels of glucose and ldestimates

# Could this pattern be due to a coincidence? (random change)? 
# It is highly unlikely that this pattern change is due to a coincidence. 

# Boxplot the health status and residuals that gives the 
# effect of total cholesterol removing the effects of LDL
# and reorder by the median
ggplot(data = HANES_mod) + 
  geom_boxplot(mapping = aes(x = reorder(HSQ_1, resid, FUN = median), y = resid))


mod <- lm(CHOLESTEROLTOTAL ~ LDLESTIMATE , data = HANES)
# add the residuals to the data and name it as resid
HANES_mod <- HANES %>% 
  add_residuals(mod) %>% 
  mutate(resid = resid)
# Plot the total cholesterol and residuals
ggplot(data = HANES_mod) + 
  geom_point(mapping = aes(x = CHOLESTEROLTOTAL, y = resid))
# Boxplot the health status and residuals that gives the 
# effect of total cholesterol removing the effects of LDL
# and reorder by the median
ggplot(data = HANES_mod) + 
  geom_boxplot(mapping = aes(x = reorder(HSQ_1, resid, FUN = median), y = resid))

#Overall from my exploratory data analysis I learned a lot (I also learned that this could literally go on forever)

# My exploratory data analysis revealed that age is a huge mitagating factor when looking at a lot of the 
# health variables such as glucose levels, HDL, Diabetes, and cholesterol levels, which is to be expected given 
# that research shows as a person gets older their health tends to worsen, especially their cardiovascular health, diabetic health as 
# well as glucose levels. It would be very interesting to see if there are any patterns between these variables, age, and race.
# For example, does a 60 year old white male have the same chance of having high cholesterol levels as an Asian American male at age 60? 
# What factors such as neighbhorhood walkability and access to affordable food affect one's cholesterol level? 
# I am interested in taking this research even further by analyzing the patterns and relationships between socioeconomic factors and 
# health determinants as well as risk factors and health variables. 

# WRANGLE DATA PART 5 - WRANGLE DATA


################# DATA SCIENCE FUNDAMENTALS 05: VISUAL AND EXPLORE ####################
#######################################################################################


# Load the package RCurl
library(RCurl)
# Import the HANES data set from GitHub; break the string into two for readability
# (Please note this readability aspect very carefully)
URL_text_1 <- "https://raw.githubusercontent.com/kannan-kasthuri/kannan-kasthuri.github.io"
URL_text_2 <- "/master/Datasets/HANES/NYC_HANES_DIAB.csv"
# Paste it to constitute a single URL 
URL <- paste(URL_text_1,URL_text_2, sep="")
HANES <- read.csv(text=getURL(URL))
# Rename the GENDER factor for identification
HANES$GENDER <- factor(HANES$GENDER, labels=c("M","F"))
# Rename the AGEGROUP factor for identification
HANES$AGEGROUP <- factor(HANES$AGEGROUP, labels=c("20-39","40-59","60+"))
# Rename the HSQ_1 factor for identification
HANES$HSQ_1 <- factor(HANES$HSQ_1, labels=c("Excellent","Very Good","Good", "Fair", "Poor"))
# Rename the DX_DBTS as a factor
HANES$DX_DBTS <- factor(HANES$DX_DBTS, labels=c("DIAB","DIAB NO_DX","NO DIAB"))
# Omit all NA from the data frame
HANES <- na.omit(HANES)
# Observe the structure
str(HANES)

# Load the tidyverse library
library(tidyverse)

# Convert HANES to tibble
HANES_tibble <- as.tibble(HANES) 
# Subset A1C variable from the tibble HANES
HANES_A1C <- HANES_tibble %>% .$A1C 
# You can set the print options for the 
# number of lines to be printed
options(tibble.print_min = 7)
as.tibble(HANES_A1C)

# Read the HANES orginal csv file 
URL_text_1 <- "https://raw.githubusercontent.com/kannan-kasthuri/kannan-kasthuri.github.io"
URL_text_2 <- "/master/Datasets/HANES/HANES.original.csv"
# Paste it to constitute a single URL 
URL <- paste(URL_text_1,URL_text_2, sep="")
HANES_original <- read.csv(URL)
# and convert into tibble
as.tibble(HANES_original)

# Parse a logical vector
str(parse_logical(c("TRUE", "FALSE", "NA")))
# Parse an integer vector 
# Actually EDUCATION is a factor variable 
str(parse_integer(HANES_original$EDUCATION))
# Parse a date vector
str(parse_date(c("2010-01-01", "1970-12-21")))

# Parse a logical vector
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(HANES_original$MARITAL))
str(parse_date(c("2010-01-01", "1970-12-21")))

#Replace Marital variable #2 as NA
as.tibble(parse_integer(HANES_original$MARITAL, na="2"))



################# DATA Validation Plans/ Further Analysis / Collection of additional data ####################################
##############################################################################################################################

# I have plans and an outline for the data validation plan. 
# First, I will decide what data integrity checks should occur to ensure that the data is valid.
# I will write up a code list and procedures to carry out based on research and best practices.

library(magrittr)
install.packages("validate")

# Now I have installed the validate package in order to be able to conduct data integrity checks

library(magrittr)
library(RCurl)
library(validate)

iris%>% check_that(
  HDL>0
) %>% summary()

# The above summary provides an overview of the number of items checked. For the purposes of this
# assignment a total check will not be carried out, but the above is an example of the package validate
# that will be downloaded and some sample code that will be utilized in my data validation plan. 
# I will spot check the entire data set using validate code and also the most important variables
# based on my research. 

# I would also consult the NHANES database to gain additional consultation and advice on best practices 
# for data validation as apart of my data validation plan. 

# After planning and conducting implementation and testing, I will then do data entry and validation.
# In this process I will run checks on th data entry at predesignated intervals. The final data checks
# will consist of data cleaning. From there I will impart a database lock in which no more updates or 
# changes to the data will be allowed so that the data can be easily checked by others for the exact analysis athand.
# After the database lock I may also run additional data checks or include instructions for others to run checks.
# Given that this is a national nhanes database, there are no plans for additional data gathering.
# As I learn more about concatenation in R I may concatenate NHANES survey with other surveys such as the
# National Survey on Drug Abuse and Health in order to gain further analysis across national surveys.

