# Read in data
SurveyResponses <- read_csv("SurveyResponses.csv", 
                            col_types = cols(`Question 1` = col_integer(), 
                            `Question 2` = col_integer(), 
                            `Question 3` = col_integer(), 
                            `Question 4` = col_integer()))

# Convert to a data frame.
SurveyResponses.df<- as.data.frame(SurveyResponses)

# Get the number questions. This is the 'N' term in Cronbach's Alpha
N <- ncol(SurveyResponses.df)

# R calculates sample variance/covariance. This term is multiplied by the sample variance/ covariance to get the population variance/ covariance
r<-(nrow(SurveyResponses.df)-1)/nrow(SurveyResponses.df)

# Get all the covariances
covariances <- c(cov(x = SurveyResponses.df[,1], y= SurveyResponses.df[,2])*r,
        cov(x = SurveyResponses.df[,1], y= SurveyResponses.df[,3])*r,
        cov(x = SurveyResponses.df[,1], y= SurveyResponses.df[,4])*r,
        cov(x = SurveyResponses.df[,2], y= SurveyResponses.df[,3])*r,
        cov(x = SurveyResponses.df[,2], y= SurveyResponses.df[,4])*r,
        cov(x = SurveyResponses.df[,3], y= SurveyResponses.df[,4])*r)

# Get the variances
variances <- c(var(SurveyResponses.df[,1])*r,
               var(SurveyResponses.df[,2])*r,
               var(SurveyResponses.df[,3])*r,
               var(SurveyResponses.df[,4])*r)

# calculate averages of the covariances and varainces
c <- mean(covariances)
v <- mean(variances)

# calculate Cronbach's Alpha
alpha = (N*c) / (v+(N-1)*c)

# Using the DescTools package
install.packages("DescTools")
library(DescTools)
CronbachAlpha(SurveyResponses.df) # This was a lot easier.

# N number of items (questions)
# c average covariance
# v average variance
