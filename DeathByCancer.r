cnr <- read.csv("C:\\Users\\HEENA\\Desktop\\UOP\\Analytical Computing (F-21)\\cancer.csv")
ss1 <- data.frame(cnr)
ss1$totalfdr <- cnr$Rates.Age.and.Sex...18.Female + cnr$Rates.Age.and.Sex.18...45.Female + cnr$Rates.Age.and.Sex.45...64.Female
names(ss1)

df = subset(ss1, select = -c(38:47,49:61,63:75) )
names(df)

genral <- data.frame(STATES = cnr$State, population = cnr$Total.Population, deathno = cnr$Total.Number, rate = cnr$Total.Rate)
genral$sno[1:51] = c(1:51)
head(genral,5)

usa <- data.frame(snum = genral$sno, statename = cnr$State, population = cnr$Total.Population, numofdeaths = cnr$Total.Number, rate = cnr$Total.Rate)
head(usa,10)
barplot(t(as.matrix(usa$numofdeaths)), names.arg = usa$snum, xlab = "States", ylab = "rate", col = "green",main = "USA", border = "black")

male <- data.frame(snum = genral$sno, statename = cnr$State,rate = cnr$Total.Rate)
male$tmr <- cnr$Rates.Age.and.Sex...18.Male + cnr$Rates.Age.and.Sex.18...45.Male + cnr$Rates.Age.and.Sex.45...64.Male
head(male,10)
barplot(t(as.matrix(male$tmr)), names.arg = male$snum, xlab = "Males in each state", ylab = "rate", col = "blue",main = "total male death rate", border = "black")

female <- data.frame(snum = genral$sno, statename = cnr$State,rate = cnr$Total.Rate)
female$tfr <- cnr$Rates.Age.and.Sex...18.Female + cnr$Rates.Age.and.Sex.18...45.Female + cnr$Rates.Age.and.Sex.45...64.Female
head(female,10)
barplot(t(as.matrix(female$tfr)), names.arg = female$snum, xlab = "Females in each state", ylab = "Rate", col = "yellow",main = "total male death rate", border = "black")

breastcancer <- data.frame(snum = genral$sno, statename = cnr$State,rate = cnr$Total.Rate, bcr = cnr$Types.Breast.Total)
head(breastcancer,10)
barplot(t(as.matrix(breastcancer$bcr)), names.arg = breastcancer$snum, xlab = "breastcancer in each state", ylab = "Rate", col = "red",main = "total male death rate", border = "black")

drw <- data.frame(snum = genral$sno, statename = ss1$State,usrate = ss1$Total.Rate, 
                  young = ss1$Rates.Age.and.Sex...18.Female, midle = ss1$Rates.Age.and.Sex.18...45.Female ,
                  senior = ss1$Rates.Age.and.Sex.45...64.Female, FDR = ss1$totalfdr )
head(drw,10)

x1 <- as.matrix(ss1$Total.Rate)
y1 <- as.matrix(ss1$totalfdr)
cor(x1,y1)
plot(x1, y1, pch = 19, col = "lightblue")
abline(lm(y1 ~ x1), col = "red", lwd = 3)

x2 <- as.matrix(ss1$Types.Breast.Total)
y2 <- as.matrix(ss1$totalfdr)
cor(x2,y2)
plot(x2, y2, pch = 19, col = "blue")
abline(lm(y2 ~ x2), col = "red", lwd = 3)

x3 <- as.matrix(ss1$Types.Colorectal.Total)
y3 <- as.matrix(ss1$totalfdr)
cor(x3,y3)
plot(x3, y3, pch = 19, col = "blue")
abline(lm(y3 ~ x3), col = "red", lwd = 3)

m1 <- lm(ss1$totalfdr ~ ss1$Types.Breast.Total, data = ss1)                                                                   
m1
summary(m1)

m2 <- lm(ss1$totalfdr ~ ss1$Types.Colorectal.Total, data = ss1)   # lm is used to create model                                                                     
m2
summary(m2)

m3 <- lm(ss1$totalfdr ~ ss1$Types.Lung.Total, data = ss1)    # lm is used to create model                                                                     
m3
summary(m3)

mlr1 <- lm(ss1$totalfdr ~ ss1$Types.Lung.Total  + ss1$Rates.Race.and.Sex.Female.Black + ss1$Rates.Race.and.Sex.Female.Black.non.Hispanic , data = ss1)
mlr1
summary(mlr1)

mlr2 <- lm(ss1$totalfdr ~ ss1$Types.Lung.Total + ss1$Rates.Race.and.Sex.Female.White + ss1$Rates.Race.and.Sex.Female.White.non.Hispanic  , data = ss1)
mlr2
summary(mlr2)

mlr3 <- lm(ss1$totalfdr ~  ss1$Types.Lung.Total + ss1$Rates.Race.and.Sex.Female.Asian, data = ss1)
mlr3
summary(mlr3)

mlr4 <- lm(ss1$totalfdr ~  ss1$Types.Lung.Total + ss1$Rates.Race.and.Sex.Female.Hispanic, data = ss1)
mlr4
summary(mlr4)

mlr5 <- lm(ss1$totalfdr ~  ss1$Types.Lung.Total + ss1$Rates.Race.and.Sex.Female.Indigenous, data = ss1)
mlr5
summary(mlr5)


