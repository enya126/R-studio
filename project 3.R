# find mean
x_bar = mean(MustangUsed$Miles)
y_bar = mean(MustangUsed$Price)
# find slope
# initialize
sum_num = 0
sum_deno = 0
for (i in 1:25) {
  num = (MustangUsed$Miles[i] - x_bar)*(MustangUsed$Price[i] - y_bar)
  deno = (MustangUsed$Miles[i] - x_bar)**2
  sum_num = sum_num + num
  sum_deno = sum_deno + deno
}

beta1_hat = sum_num/sum_deno
print(beta1_hat)

# intercept
beta0_hat = y_bar - beta1_hat * x_bar
print(beta0_hat)

# variance
variance = 0
for (i in 1:25) {
  var = (MustangUsed$Price[i]-(beta0_hat+beta1_hat*MustangUsed$Miles[i]))**2
  variance = variance + var
}
variance = variance/25
print(variance)

# RMSE
RMSE = 0
for (i in 1:25) {
  # sum of (yi hat - yi)^2
  rmse = ((MustangUsed$Miles[i]*(-218.8)+30495.3)-MustangUsed$Price[i])**2
  RMSE = RMSE + rmse
}
RMSE = sqrt(RMSE/25)
print(RMSE)

# model
model <- lm(Price ~ Miles, data = MustangUsed)
model

# lm model for x y
x = 12
y_hat = (-218.8)*x + 30495.3
print(y_hat)

# check linearity
plot(MustangUsed$Miles, MustangUsed$Price, main="Check Linearity",
     xlab="Miles", ylab="Price ",pch=19)
# regression line
abline(model, col="red")

# The data are mostly linear
# But there is also one outlier on the upper left part of the scatterplot

# 1/26 lab1
data <- 1:20
data <- data*2-1
data <- rev(data)
data[1]

(5>4 | 2==3) & (3!=4 & 7<10)

x <- c("dog", "cat", "lizard")

y <- c("mouse", "lizard", "bird")

x %in% y

x <- c("dog", "cat", "lizard")

y <- c("mouse", "lizard", "bird")

z <- c(y, x)

# create a function
# function1
zScore <- function(number = 1){
  mean_num <- mean(number)
  sd_num <- sd(number)
  (number - mean_num)/sd_num
}

zScore(1:10)
scale(1:10)

# function2
cubeSquare <- function(number=1){
  temp<-number^3
  sqrt(temp)
}
cubeSquare(4)






