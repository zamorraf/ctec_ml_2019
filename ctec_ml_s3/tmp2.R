library(lessR)
# Generate random data, place in dataframe mydata
X1 <- rnorm(20)
X2 <- rnorm(20)
Y <- .7*X1 + .2*X2 + .6*rnorm(20)
mydata <- data.frame(X1, X2, Y)

# One-predictor regression
# Provide all default analyses including scatterplot etc.
reg(Y ~ X1)

# Multiple regression model
# Provide the full range of default analyses
reg(Y ~ X1 + X2)
# Provide only the brief analysis
reg(Y ~ X1 + X2, results="brief")

# Modify the default settings as specified
reg(Y ~ X1 + X2, res.row=8, res.sort="rstudent", sig.digits=8, pred=FALSE)

# Specify values of the predictor variables for calculating forecasted
#  values and the corresponding prediction interval
# Note in this analysis it is just a coincidence that the variables are
#  named X1 and X2, which always begin the names X1.new and X2.new
reg(Y ~ X1 + X2, X1.new=seq(.4,.8,.2), X2.new=c(.5,.7))

# Scatterplot matrix with correlation coefficients in upper triangle
# Specify an input dataframe other than mydata
# help(mtcars) to see description of the data, included with R
reg(mpg ~ cyl + disp + hp + drat + wt + gear, scatter.cor=TRUE, dframe=mtcars)
