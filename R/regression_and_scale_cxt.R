library(psych)
# In this dataset I discover two topics: 1. scale/factor construction,2.column correlation
# and 3.logistic regression (using different factors to predic satisfaction in HRI)


# read the data
setwd("/Users/Daywatch/Desktop/robotics/final")
m<-read.table("finaltable.csv", sep=",", header=TRUE)
m <- as.data.frame(m)

# data:
#         name What.is.your.gender. mentally.demanding physically.demanding
# 1          Sidharth Tyagi                 Male                  4                    1
# 2          Michael Walker                 Male                  5                    1
# 3          Omar Abughalib                 Male                  3                    1
# 4               Catherine               Female                  3                    1
# 5                    none                 Male                  1                    1
# 6        Michael Reinisch                 Male                  5                    1
# 7                 Mahshab               Female                  5                    4
# 8           Jackson Meyer                 Male                  5                    1
# 9            Haizheng Zhu                 Male                  2                    2
# 10                    Kim                 Male                  2                    2
# 11 George Matthew Helmick                 Male                  4                    1
# hurried.or.rushed subjective.success hard discouraged like.working.with.robot satisfaction time
# 1                  1                  5    3           2                       5            5  553
# 2                  3                  1    5           6                       1            1  706
# 3                  3                  7    5           3                       6            6  577
# 4                  2                  7    3           2                       4            6  176
# 5                  3                  1    1           7                       1            1  720
# 6                  3                  2    3           3                       4            4  720
# 7                  2                  5    5           5                       5            6  720
# 8                  3                  4    5           3                       3            3  720
# 9                  1                  5    2           2                       3            4  410
# 10                 5                  6    4           4                       6            3  145
# 11                 2                  6    6           3                       5            4  344
# result green stuck comm
# 1       1     2     1    1
# 2       0     1     2    1
# 3       1     0     1    1
# 4       1     1     0    1
# 5       0     3     3    1
# 6       0     1     3    1
# 7       0     1     1    1
# 8       1     3     1    0
# 9       1     1     0    0
# 10      1     1     0    0
# 11      1     0     0    0

# get basic stats such as median and max
summary(m)


# check the data types
sapply(m, class)

# convert gender into numeric
library(car)
#Recode(x, "1:2='A'; 3='B'")
m$What.is.your.gender. <- recode(m$What.is.your.gender.,"'Male'=1;'Female'=0")
m1 <- m

# calculate a correlation matrix
library(dplyr)
# delete the name column
m <- select(m,-name, -time, -satisfaction, -result, -green)
corMat <- cor(m)
#corMat # return a pairwise correlation table (partial below)
# What.is.your.gender. mentally.demanding physically.demanding
# What.is.your.gender.              1.00000000        -0.15609764          -0.55329401
# mentally.demanding               -0.15609764         1.00000000           0.09462916
# physically.demanding             -0.55329401         0.09462916           1.00000000
# hurried.or.rushed                 0.23904572        -0.13992927          -0.06900656
# subjective.success               -0.34614676        -0.14874882           0.18074481
# hard                             -0.05847053         0.63661608           0.13292204
# discouraged                       0.03990434        -0.11585891           0.17855058
# like.working.with.robot          -0.16617575         0.06105792           0.27121956
# satisfaction                     -0.56985885         0.17403969           0.32185581
# time                              0.17338065         0.45802973           0.03958238
# result                            0.13363062        -0.25031309          -0.25074294
# green                             0.13363062        -0.11264089          -0.14465939
# stuck                             0.25713603         0.15005559          -0.23125119
# comm                             -0.35634832         0.16270351          -0.03857584


# check the y class bias
table(m$satisfaction)
# 1 3 4 5 6 
# 2 2 3 1 3 

# exploratory factor analysis
# ref: http://rtutorialseries.blogspot.com/2011/10/r-tutorial-series-exploratory-factor.html
library(nFactors)
ev <- eigen(cor(char2numeric(m)))
#new.data <- char2numeric(data) 
ap <- parallel(subject=nrow(m),var=ncol(m),rep=100,cent=.05)
nS<-nScree(ev$values, ap$eigen$qevpea)
# the plot shows that there are 5 factors in the survey
plotnScree(nS)

# plug the 5 factors into the factorial analysis
library(psych)
solution <- fa(r = corMat, nfactors = 5, rotate = "oblimin", fm = "pa")
solution
# 
# Factor Analysis using method =  pa
# Call: fa(r = corMat, nfactors = 5, rotate = "oblimin", fm = "pa")
# Standardized loadings (pattern matrix) based upon correlation matrix
# PA1   PA2   PA4   PA3   PA5   h2     u2 com
# What.is.your.gender.    -0.05 -0.05 -0.01  0.86 -0.18 0.82  0.181 1.1
# mentally.demanding      -0.05  1.19 -0.06 -0.02  0.04 1.42 -0.424 1.0
# physically.demanding     0.06  0.05  0.29 -0.66 -0.15 0.48  0.521 1.5
# hurried.or.rushed        0.37 -0.07  0.62  0.31 -0.03 0.49  0.512 2.2
# subjective.success       0.73 -0.15 -0.30 -0.22 -0.11 0.99  0.011 1.7
# hard                     0.46  0.55  0.25  0.02 -0.22 0.63  0.371 2.8
# discouraged             -0.24 -0.08  0.92 -0.14  0.08 1.05 -0.051 1.2
# like.working.with.robot  0.89  0.02 -0.07 -0.02  0.07 0.79  0.207 1.0
# stuck                   -0.30  0.08  0.26  0.34  0.57 0.88  0.116 2.8
# comm                     0.09  0.02 -0.01 -0.11  1.00 0.96  0.036 1.0

# Logistic regression
# make y a variable
m1$satisfaction <- factor(m1$satisfaction)
# run model
mylogit <- glm(satisfaction ~ mentally.demanding + physically.demanding + hurried.or.rushed + subjective.success + hard + discouraged + like.working.with.robot + stuck + comm, data = m1, family = "binomial")
summary(mylogit)


# Call:
#         glm(formula = satisfaction ~ mentally.demanding + physically.demanding + 
#                     hurried.or.rushed + subjective.success + hard + discouraged + 
#                     like.working.with.robot + stuck + comm, family = "binomial", 
#             data = m1)
# 
# Deviance Residuals: 
#         1           2           3           4           5           6           7           8           9  
# 2.710e-06  -6.878e-06   4.661e-06   8.433e-06  -6.498e-06   8.691e-06   6.698e-06   3.236e-06   6.402e-06  
# 10          11  
# 6.228e-06   9.104e-06  
# 
# Coefficients:
#                       Estimate Std. Error     z value Pr(>|z|)
# (Intercept)             -3.276e+01  8.411e+05       0        1
# mentally.demanding       7.653e+00  1.664e+05       0        1
# physically.demanding     3.861e+00  1.191e+05       0        1
# hurried.or.rushed        5.572e-01  6.945e+04       0        1
# subjective.success       8.990e+00  1.516e+05       0        1
# hard                    -5.765e+00  1.535e+05       0        1
# discouraged             -3.357e+00  1.408e+05       0        1
# like.working.with.robot  2.360e+00  1.114e+05       0        1
# stuck                    1.079e+01  1.994e+05       0        1
# comm                    -1.948e+01  2.744e+05       0        1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1.0431e+01  on 10  degrees of freedom
# Residual deviance: 4.8323e-10  on  1  degrees of freedom
# AIC: 20
# 
# Number of Fisher Scoring iterations: 23

