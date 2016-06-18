#Introduction to R by Michael Clark

##importing & working w/ data

###creation of data
x = sample(c("jane", "elizabeth", "darcy", "bingley"), 5, replace = T, prob = c(.3, .2, .2, .3))
x2 = rbinom(5, 1, .5)
x3 = rnorm(50, mean = 50, sd = 10)

####from correlation matrix
cormat = matrix(c(1, .5, .5, 1), nrow = 2)
library(MASS)
xydata = mvrnorm(40, mu = c(0, 0), Sigma = cormat, empirical = T)
head(xydata)
cor(xydata)

####w/ factor variables
mydata <- expand.grid(time = factor(1:4), subject = factor(1:10))
mydata$gender <- factor(rep(0:1, each = 4), labels = c("Male", "Female"))
mydata$y <- as.numeric(mydata$time) +
  rep(rnorm(10, 50, 20), e = 4) +
  rnorm(mydata$time, 0, 1)
library(lattice)
dotplot(y~time, data = mydata, xlab = "Time", ylab = "DV")
dotplot(y~time, data = mydata, groups = gender, xlab = "Time", ylab = "DV",
        key = simpleKey(levels(mydata$gender)))
dotplot(y~time|gender, data = mydata, groups = subject, xlab = "Time", ylab = "DV",
        pch = 19, cex = 1)

###working w/ data
state2 <- data.frame(state.x77)
str(state2)
head(state2, 10)

####merging & reshaping
mydat <- data.frame(id = factor(1:12), group = factor(rep(1:2, e = 3)))
x = rnorm(12)
y = sample(70:100, 12)
x2 = rnorm(12)

mydat$grade = y
df <- data.frame(id = mydat$id, y)
mydat2 <- merge(mydat, df, by = 'id', sort = F)
mydat3 <- cbind(mydat, x)

df <- data.frame(id = factor(13:24), group = factor(rep(1:2, e = 3)), grade = sample(y))
mydat2 <- rbind(myudat, df)

mydat <- data.frame(id = factor(rep(1:6, e = 2)), time = factor(rep(1:2, 6)), y)
mydat2 <- reshape(mydat, v.name = "y", direction = "wide")
mydat2 <- reshape(mydat2, direction = "long")

##initial data analysis & graphics

###initial data analysis: numeric
table(state.region)
cor(state2)

library(psych)
describe(state2)
describeBy(state2$Frost, state.region)

###initial data analysis: graphics
hist(state2$Illiteracy)
barplot(table(state.region), col = c("lightblue", "mistyrose", "papayawhip", "lavender"))
plot(state2$Illiteracy ~ state2$Population)
stripchart(state2$Illiteracy ~ state.region, data = state2, col = rainbow(4), method = "jitter")

##the basic modeling approach

mod1 = lm(Life.Exp ~ Income, data = state2)
summary(mod1)

mod2 = update(mod1, ~. + Frost + HS.Grad)
summary(mod2)

mod3 = update(mod2, ~. - Frost)
summary(mod3)

anova(mod1, mod2)
anova(mod3, mod2)

mod1$coef
coef(mod1)

confint(mod1)

plot(mod1)

##visualization of information

x <- rnorm(100)
hist(x)
boxplot(x)

hist(Cars93$MPG.highway, col = "lightblue1", main = "Distance per Ballon 1993", xlab = "Highway MPG")

par(mfrow = c(1, 2))
hist(Cars93$MPG.highway, col = "lightblue1", prob = T)
lines(density(Cars93$MPG.highway), col = "lightblue4")
boxplot(MPG.highway ~ Origin, col = "burlywood3", data = Cars93)
par(mfrow = c(1, 1))

library(car)
scatterplot(prestige ~ income | type, data = Prestige)
scatterplot(vocabulary ~ education, jitter = list(x = 1, y = 1), data = Vocab,
            col = c("green", "red", "gray80"), pch = 19)
