rm(list=ls())
## ----Simple addition prints to screen------------------------------------
1+2

## ----Creating an object--------------------------------------------------
oneplustwo <- 1 + 2

## ----What kind of object is it?------------------------------------------
# Every object has a class, a type, and a structure, which will affect what you are able to do with the object.
class(oneplustwo) #The class() function returns the class attribute of an R object.
str(oneplustwo) #The str() function displays the internal structure of an object such as an array, list, matrix, factor, or data frame.
typeof(oneplustwo) #The typeof() determines the (R internal) type or storage mode of any object

## The class of the object can be changed ---------------------------------
str(as.character(oneplustwo))

## ----Making a vector-----------------------------------------------------
oneANDtwo <- c(1,2)
class(oneANDtwo)
str(oneANDtwo)
typeof(oneANDtwo)

# Create a matrix
matix1 <- cbind(oneANDtwo,oneplustwo)
matrix2 <- rbind(oneANDtwo,oneplustwo)

class(matix1)
str(matix1)
typeof(matix1)

## Create a dataframe------------------------------------------------------------------------
data.frame(oneANDtwo, oneplustwo)
df <- data.frame(oneANDtwo, oneplustwo)

df$sum <- (df$oneANDtwo + df$oneplustwo) ^ 4
class(df)
str(df)
typeof(df)

## ----results='hide'------------------------------------------------------
oneANDtwo - 1

## ----Remove some objects-------------------------------------------------
rm(oneplustwo)
rm(oneANDtwo)

# Remove all objectives from the environment
rm(list=ls())

# Clear Console, making error checking easier.
cat("\014")


## ----Load packages, warning=FALSE, message=FALSE-------------------------
pacman::p_load(pdfetch, xts, stargazer, zoo, ggplot2, knitr)

# Find help documentation for packages
??pdfetch
??stargazer
library(help = stargazer)

## ----Loading built in datasets-------------------------------------------
data("iris")
head(iris)
tail(iris)
precip
?data

species <- iris$Species
head(species)
plot(iris$Species, iris$Sepal.Length, col = c('red','white','dodgerblue'), pch = 19, cex = 1.5, main = "Iris Data", xlab = "Species", ylab = "Sepal Length")
plot(iris$Sepal.Length, iris$Sepal.Width, col = species, pch = 19, cex = 1.5, main = "Iris Data", xlab = "Sepal Length", ylab = "Sepal Width")
legend("topleft", legend = c("Setosa", "Versicolor", "Virginica"), col = c("black", "red", "green"), pch = 19, cex = 0.8)

## Import data
#ATLHDD <- read.csv("C:/Users/Tim_SYL/Documents/TA work/Spring_2024/Dr Kyle Lee/ATLHDD.csv")

# SET wroking directory
#setwd("C:\\Users\\Tim_SYL\\Documents\\TA work\\Spring_2024\\Dr Kyle Lee")

ATLHDD <- read.csv("Ch 1 Introduction/1.2 Heating Degree Days/ATLHDD.csv")


# Get current working directory
getwd()

## ----Get Fred Data, results="hide"---------------------------------------
pdfetch_FRED("THREEFY10")

??pdfetch

treasury <- pdfetch_FRED(c("DGS2","DGS10"))
treasury <- as.data.frame(treasury)
index <-as.data.frame(index(treasury))
dat <- as.data.frame(cbind(treasury,index))
dat$index <- dat[,3]
# remove the second column
dat <- dat[,c(1,2,4)]
dat$date <- as.Date(dat$index, format = "%Y-%m-%d")
as.Date(dat$date, format = "%Y-%m-%d")
dat$year <- format(dat$date, "%Y")
?mdy
# Plot the data
plot(dat$DGS2, dat$DGS10, col = dat$year, lwd = .5, lty = .5, main = "2 vs 10 year", xlab = "2 year", ylab = "10 year")
# create line with slope of 1
abline(a = 0, b = 1, col = "black", lwd = 2, lty = 2)

## ----Get Fred Data Into an Object----------------------------------------
# THREEFY10: Fitted Yield on a 10 Year Zero Coupon Bond
# THREEFYTP10: Term Premium on a 10 Year Zero Coupon Bond 
treasury <- pdfetch_FRED(c("DGS2","DGS10","T10Y2Y"))
last10 <- treasury["2014/2024"]
plot(last10, col = c("blue", "green",'red'),lwd=c(1,1,2), lty = 1, main = "Ten Year Treasury Yield and Term Premium, 1990-2017", xlab = "Year", ylab = "Percent")
# create line of best fit
#legend("topleft", legend = c("Fitted Yield", "Term Premium"), col = c("black", "red"), lty = c(1,4), cex = 0.8)

# Clear old plots
while (!is.null(dev.list()))  dev.off()

## ----Write data to CSV---------------------------------------------------
write.csv(as.data.frame(treasury),"treasury.csv")

## ----Information about xts (Extensible Time Series) objects---------------------------------------
class(treasury)
dim(treasury)
names(treasury)
start(treasury)
end(treasury)
first(treasury)
last(treasury)
periodicity(treasury)

## ----Subsetting xts objects, results='hide'------------------------------
head(treasury$THREEFY10)
treasury["2000"]
treasury["2000-07"]
treasury_subset <- treasury["2008/2011"]


## ----Using autoplot.zoo--------------------------------------------------
p <- autoplot.zoo(treasury, facets=NULL)
p

## ----Customizing the plot------------------------------------------------
p1 <- p +  labs(title = "Ten Year Treasury Yield and Term Premium, 1990-2017",
                caption = "Sources: Federal Reserve Bank of New York, Federal Reserve", x = "Year") + scale_colour_grey()  +
  theme(legend.title = element_blank()) + theme(legend.justification=c(1,1), legend.position=c(.95,.95))
p1 + theme_bw()

## ----Making a regression model-------------------------------------------
data("mtcars")
model <- lm(mpg ~ wt + cyl, data = mtcars)
str(model)
class(model)
summary(model)

#plot 
wt <- mtcars$wt
mpg <- mtcars$mpg

plot(wt, mpg)
# Create line of best fit
abline(model)


## ----Making a pretty table, results='asis'-------------------------------
??stargazer
output <- stargazer(model,type="text",align=TRUE)
output
write(output, file = "output.txt", append = TRUE, sep = "\t")

## Function in R
# For example, to write a present value function, we need just one line R codes.
pv_f<-function(fv,r,n) {
  fv/(1+r)^n
}

# Calling such a function
pv_f(100,0.1,1)

## For loop and if else
# Suppose we have a list of matches 
matches <- list(c(2,1),c(5,2),c(6,3),c(2,3))

for (match in matches){
  if (match[1] > match[2]){
    print("Win")
  } else {
    print ("Lose")
  }
}



