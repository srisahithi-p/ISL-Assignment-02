library(MASS)
library(ISLR)
install.packages("ISLR")

##chapter2 - problem 8
#a
college=read.csv("C:/Users/SriS/OneDrive/Documents/R/College.csv", header=TRUE)
#b
rownames (college) <- college[, 1]
View(college)
college <- college[, -1]
View(college)
#c(i)
summary(college)
#c(ii)
college[,1] = factor(college[,1])
View(college)
pairs(college[, 1:10])
#c(iii)
boxplot(Outstate ~ Private,data = college,col=c("green","blue"))
#c(iv)
Elite = rep ("No", nrow (college))
Elite[college$Top10perc > 50] = " Yes "
Elite = as.factor (Elite)
college = data.frame (college , Elite)
View(college)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out-of-state tuition (dollars)",col=c("green","blue"))
#c(v)
par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "Yes"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "No"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "No"], xlab = "Number of applicants", main = "Histogram for non elite schools")

par(mfrow = c(2, 2))
hist(college$Expend, xlab = "Instructional expenditure per student (dollars)", main = "Histogram for all colleges")
hist(college$Expend[college$Private == "Yes"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for private schools")
hist(college$Expend[college$Private == "No"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for public schools")
hist(college$Expend[college$Elite == "No"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for non elite schools")

#c(vi)
NonTuitionCosts = college$Room.Board + college$Books + college$Personal
college = data.frame(college, NonTuitionCosts)
par(mfrow = c(1, 2))
plot(college$Private, college$NonTuitionCosts, xlab = "Private", ylab = "Total non-tuition costs per year (dollars)")
plot(college$Elite, college$NonTuitionCosts, xlab = "Elite", ylab = "Total non-tuition costs per year (dollars)")

##Based on the above box plots, it looks like that, aside from some outlier schools with very high costs, there isn't a wide gap for the median non-tution costs between private schools and public schools. The box plots do show, though, that there is a distinct difference in median non-tuition costs between elite and non-elite schools, with elite schools having higher costs.

##chapter2 - problem 9
Auto = read.csv("C:/Users/SriS/OneDrive/Documents/R/Auto.csv", header=TRUE)
Auto = na.omit(Auto)
dim(Auto)
##a
str(Auto)
##b
?range
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
range(Auto$origin)
##c
sapply(Auto[, 1:7], mean)
sapply(Auto[, 1:7], sd)
##d
subset = Auto[-c(10:85), -c(4,9)]
sapply(subset, range)
sapply(subset, mean)
sapply(subset, sd)
##e
par(mfrow = c(2, 1))
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "mpg")
plot(Auto$weight, Auto$displacement, xlab = "Car weight (pounds)", ylab = "Engine displacement (cubic inches)")
##f
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")
#Looking at the above box plot, we can also see that there is a relationship between a car's country of origin and fuel efficiency, where on average Japanese cars are the most efficient, followed by European cars and then by American cars.

##chapter2 - problem 10
##a
Boston$chas = as.factor(Boston$chas)
nrow(Boston)
ncol(Boston)
##b
par(mfrow = c(2, 2))
plot(Boston$age, Boston$cmedv, xlab = "Percent of units built prior to 1940", ylab = "Median home value in $1000s")
plot(Boston$lstat, Boston$cmedv, xlab = "Percent of lower status residents", ylab = "Median home value in $1000s")
plot(Boston$lstat, Boston$cmedv, xlab = "Percent of lower status residents", ylab = "Median home value in $1000s")
plot(as.factor(Boston$chas), Boston$cmedv, xlab = "Borders Charles River", ylab = "Median home value in $1000s")
##c
par(mfrow = c(2, 2))
plot(Boston$b, Boston$crim, xlab = "1000(Proportion of black residents - 0.63)^2", ylab = "Per capita crime rate")
plot(Boston$lstat, Boston$crim, xlab = "Percent of lower status residents", ylab = "Per capita crime rate")
plot(Boston$age, Boston$crim, xlab = "Percent of units built prior to 1940", ylab = "Per capita crime rate")
plot(Boston$dis, Boston$crim, xlab = "Weighted distance to Boston employment centers", ylab = "Per capita crime rate")
##d
par(mfrow = c(2, 2))
hist(Boston$crim, xlab = "Per capita crime rate", main = "Histogram of Boston crime rates")
hist(Boston$tax, xlab = "Tax rate per 10000 USD", main = "Histogram of Boston tax rates")
hist(Boston$ptratio, xlab = "Pupil-teacher ratio", main = "Histogram of Boston pupil-teacher ratios")
##e
nrow(Boston[Boston$chas==1, ])
##f
median(Boston$ptratio)
##g
row.names(Boston[min(Boston$medv), ])
range(Boston$tax)
Boston[min(Boston$medv), ]$tax
##h
nrow(Boston[Boston$rm >7, ])
nrow(Boston[Boston$rm >8, ])

##chapter3 - problem 8
#a
Auto = read.csv("C:/Users/SriS/OneDrive/Documents/R/Auto.csv", header=TRUE)
Auto = na.omit(Auto)
fit = lm(mpg ~ horsepower, data = Auto)
summary(fit)
#Simple linear regression gives a model  Y^=39.935861???0.157845X1  between the predictor horsepower and the response mpg. A p-value of essentially zero for  ??^1=???0.157845  gives very strong evidence that there is a relationship between mpg and horsepowerSince  R2=0.6059 , approximately 60.6% of the variability in mpg is explained by a linear regression onto horsepower. This is a modest relationship between the predictor and the response, since as discussed in the chapter we can improve our  R2  value to 0.688 by including a quadratic term. The value of  ??^1  itself indicates that in the model each increase of 1 horsepower results on average in a decrease of 0.157845 miles per gallon. In other words, in this model there is a negative relationship between the predictor and the response.

predict(fit, data.frame(horsepower = "98"), interval = "confidence")
predict(fit, data.frame(horsepower = "98"), interval = "prediction")
#Plugging in a horsepower value of 98 gives a predicted mpg of 20.25. The 95% confidence interval for this prediction is (14.60675, 25.89325) and the 95% prediction interval is (10.4756, 30.0244)

#b
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
abline(fit,lwd = 3, col = "red")
#c
par(mfrow = c(2, 2))
plot(fit)
##Looking at the Residuals vs. Fitted plot, there is a clear U-shape to the residuals, which is a strong indicator of non-linearity in the data. This, when combined with an inspection of the plot in Part 2, tells us that the simple linear regression model is not a good fit. In addition, when looking at the Residuals vs. Leverage plot, there are some high leverage points (remember that after dropping the rows with null values, there are 392 observations in the data set, giving an average leverage value of  2/392???0.0051 ) which also have high standardized residual values (greater than 2), which is also of concern for the simple linear regression model. There are also a number of observations with a standardized residual value of 3 or more, which is evidence to suggest that they would be possibile outliers if we didn't already have the suspicion that the data is non-linear

##chapter3 - problem 9
Auto = read.csv("C:/Users/SriS/OneDrive/Documents/R/Auto.csv", header=TRUE,na.strings = "?")
Auto = na.omit(Auto)
head(Auto)
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
head(Auto)
#a
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)
#b
cor(Auto[,-c(8, 9)])
#c
mpg.fit = lm(mpg ~ . - name, data = Auto)
summary(mpg.fit)
contrasts(Auto$origin)
#Since the F-statistic is 224.5, giving a p-value of essentially zero for the null hypothesis, there is strong evidence to believe that there is a relationship between the predictors and the response. The predictors that appear to have a statistically significant relationship to the response mpg are displacement with a p-value of 0.001863, and weight, year, originEuropean, and originJapanese with p-values of essentially zero. The coefficients for cylinders, horsepower, and acceleration have p-values which are not small enough to provide evidence of a statistically significant relationship to the response mpg. The coefficient of 0.777 for the year variable suggests that when we fix the number of engine cylinders, engine displacement, horsepower, weight, acceleration, and country of origin, fuel efficiency increases on average by about 0.777 miles per gallon each year. In other words, the model suggests that we would expect cars from 1971 to be more fuel efficient by 0.777 miles per gallon on average compared to equivalent cars from 1970. Also of interest are the coefficients for originEuropean and originJapanese, which suggest that compared to equivalent cars from the United States, we would expect European cars to be more fuel efficient by 2.630 miles per gallon on average, and Japanese cars to be more fuel efficient by 2.853 miles per gallon on average. Lastly, the  R2  value of 0.8242 indicates that about 82% of the variation in mpg is explained by this least squares regression model.
##d
par(mfrow = c(2, 2))
plot(mpg.fit)
#e
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)
mpg.fit.reduced.interactions = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
summary(mpg.fit.reduced.interactions)
#f
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)
summary(lm(mpg ~ acceleration, data = Auto))
par(mfrow = c(2, 2))
plot(lm(mpg ~ acceleration, data = Auto))
displacement.linear = lm(mpg ~ displacement, data = Auto)
summary(displacement.linear)
displacement.quadratic = lm(mpg ~ poly(displacement, 2), data = Auto)
summary(displacement.quadratic)
anova(displacement.linear, displacement.quadratic)
par(mfrow = c(2, 2))
plot(log(Auto$horsepower),Auto$mpg)
plot(sqrt(Auto$horsepower),Auto$mpg)
plot((Auto$horsepower)^2,Auto$mpg)

##chapter3 - problem 10
data("Carseats")
fit3<-lm(Sales ~ Price+US,data=Carseats)
summary(fit3)
fit2 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit2)
contrasts(Carseats$Urban)
contrasts(Carseats$US)
#b
#The coefficient of the "Price" variable may be translated by saying that the normal impact of a cost increment of 1 dollar may be diminish of 54.4588492 units in deals all other indicators remaining settled.The coefficient of the "Urban" variable may be translated by saying that on normal the unit deals in urban area are 21.9161508 units less than in rustic area all other indicators remaining settled. The coefficient of the "US" variable may be deciphered by saying that on normal the unit deals in a US store are 1200.5726978 units more than in a non US store all other indicators remaining settled.#
##c

#The model has the following equation.

#sales =13.043???0.054X1???0.022X2+1.200X3
#x1j  is the price of the carseat at the jth store, in dollars; and  x2j  and  x3j  are dummy variables to represent whether or not the  j th store at is located in an urban area and in the United States, respectively. More concretely,  x2j  and  x3j  use the following coding scheme.

#d

#The p-values for the intercept, Price, and USYes are all essentially zero, which provides strong evidence to reject the null hypothesis  H0:??j=0  for those predictors. The p-value for UrbanYes, however, is 0.936, so there is no evidence to reject the null hypothesis that it has a non-zero coefficient in the true relationship between the predictors and Sales.

#e

fit4 = lm(Sales ~ Price + US, data = Carseats)
summary(fit4)

#f
par(mfrow = c(2, 2))
plot(fit4)


#g
confint(fit4)

#h
#When we look at the residuals vs. leverage plot for the model from "e" that I generated in "f", we see that there are a number of observations with standardized residuals close to 3 in absolute value. Those observations are possible outliers. We can also see in the same plot that there are number of high leverage points with leverage values greatly exceeding the average leverage of  3/400=0.0075 , though those high leverage observations are not likely outliers, as they have studentized residual values with absolute value less than 2.