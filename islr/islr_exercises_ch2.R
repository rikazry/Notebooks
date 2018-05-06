#personal study notes
#reference
#http://www-bcf.usc.edu/~gareth/ISL/
#http://yahwes.github.io/ISLR/

#ch2
##Q8
###a
require(ISLR)
data("College")
str(College)
###b
#fix(College)
rownames(College)
head(College)
###c
####i
summary(College)
####ii
pairs(College[,2:3])
####iii
plot(Outstate ~ Private, data=College, type="box")
####iv
Elite=rep("No", nrow(College))
Elite[College$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college_extended=data.frame(College, Elite)
summary(Elite)
plot(Outstate~Elite, data=college_extended, type="box")
####v
num.flag=sapply(college_extended, is.numeric)
num.count=sum(num.flag)
set.seed(1)
num.plot=sample(num.count,4)
num.frame=college_extended[, num.flag]
par(mfrow=c(2,2))
for (i in num.plot){
  hist(num.frame[,i], main=colnames(num.frame)[i])
}
####vi
##Q9
###a
require(ISLR)
data("Auto")
str(Auto)
num.flag=sapply(Auto, is.numeric)
num.flag
###b
sapply(Auto[, num.flag], range)
###c
sapply(Auto[, num.flag], mean)
sapply(Auto[, num.flag], sd)
###d
sapply(Auto[-(10:85), num.flag], mean)
sapply(Auto[-(10:85), num.flag], sd)
###e
colnames(Auto)
pairs(Auto)
###f
##Q10
###a
library(MASS)
?Boston
dim(Boston)
###b
colnames(Boston)
pairs(Boston)
###c
require(ggplot2)
require(reshape2)
ggplot(melt(Boston, id="crim"),
       aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") +
  geom_point()
cor(Boston, use="complete.obs")["crim",]
####d
ggplot(Boston,
       aes(x=1:nrow(Boston), y=crim)) +
  geom_point()
ggplot(Boston,
       aes(x=1:nrow(Boston), y=tax)) +
  geom_point()
ggplot(Boston,
       aes(x=1:nrow(Boston), y=ptratio)) +
  geom_point()
####e
table(Boston$chas)
####f
median(Boston$ptratio)
###g
Boston[Boston$medv==min(Boston$medv),]
sapply(Boston, quantile)
####h
sum(Boston$rm>7)
sum(Boston$rm>8)
rbind(sapply(Boston[Boston$rm>8,], mean), sapply(Boston[Boston$rm>8,], median))
