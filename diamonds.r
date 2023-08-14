data <- read.csv("C:/Users/akimp/Desktop/anreg prak 5/diamonds.csv")

price <- data$price
carat <- data$carat
depth <- data$depth
table <- data$table
x <- data$x
y <- data$y
z <- data$z

#correlation
cor(carat, depth)
cor(carat, table)
cor(table, depth)

#regression model
model <- lm(price ~ carat + depth + table)
summary(model)

#variable
B <- cbind(7858.771, -151.236, -104.473)
dim(B) <- c(3,1)

X <- cbind(1, carat, depth, table)

Y <- cbind(price)
C <- solve(t(X) %*% X)



Bb <- solve(t(X) %*% X) %*% (t(X) %*% Y)

models <- function(x1, x2, x3){
        13003.441 + 7858.771*x1 - 151.236*x2 - 104.473*x3
}

models(2, 64, 72)

Yh <- models(carat, depth, table)

N <- nrow(X)

KTS <- (t(Y)%*%Y)-t(Bb)%*%t(X)%*%Y
var <- KTS/(N-3)
var
sqrt(var)
se <- sqrt(2328920*diag(C))

#interval
t_crit <- qt(1-0.025, N-3)

inter_p <- Bb + se*t_crit
inter_m <- Bb - se*t_crit

inter_m

#anova
JKT <- sum(Y^2)-sum(Y)^2/N #SSt
JKR <- sum(t(Bb) %*% t(X) %*% Y) -sum(Y)^2/N
JKS <- JKT - JKR

JKT
JKR
JKS

Yh_mean <- mean(Yh)
MSR <- sum((Yh-Y)^2)/N
MSR

qf(1-0.01, 3, 53936)

#uji t
t0 <- Bb/se
qt(0.05, N-3-1)
t0

anova(model)

R2 <- cor(price, Yh)^2
R2
cor(price, Yh)

Ra2 <- 1-(JKS/(N-3-1))/(JKT/N-1)
Ra2

summary(model)

cor(cbind(price, carat, depth, table, x, y, z))
summary(data)

plot(carat, price)
plot(price, depth)
plot(price, table)

diag(C)
N

mean(carat)
summary(data)
qf(1-.05, 3, 53936)
qf(1-.1, 4, 10)

plot(Yh,Y,  xlab="Price Prediction", ylab="Price")

x0 <- cbind(2, 63, 72)
x0

(x0) %*% C %*% x0
C

X1 <- cbind(carat, depth, table)
C1 <- solve(t(X1) %*% X1)
C1
sey <- sqrt((x0) %*% C1 %*% t(x0))*1526.08
sey
11519.82 + sey*qt(0.05, N-3-1)
11519.82 - sey*qt(0.05, N-3-1)
qt(0.05, N-3-1)
qt(0.05, 17-1)

1/(1-cor(cbind(price, carat, depth, table, x, y, z))^2)

1/(1-cor(carat, depth, table)^2)

X <- c(5, 5.6, 6.1, 6.8, 7.4, 8.6)
X2 <- X^2
Y <- c(16.5, 22.4, 24.9, 28.8, 31.5, 35.8)

model <- lm(Y~X+X2)
summary(model)

-44.1925 + 16.3339*7.7 + -0.8198*7.7^2
