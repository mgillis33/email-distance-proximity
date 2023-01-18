dataE = read.csv("/Users/mgillis/Downloads/EmailStats.csv")
head(dataE)


#Email Count
summary(dataE$Count)
sd(dataE$Count)
hist(dataE$Count, main = "Histogram of Email Count By College", xlab = "Email Count", col = "darkgray")
boxplot(dataE$Count, main = "Boxplot of Email Count By College", ylab = "Email Count", col = "darkgray")

#Distance from Hometown (mi)
summary(dataE$Distance)
sd(dataE$Distance)
hist(dataE$Distance, main = "Histogram of Distance", xlab = "Distance (mi)", col = "darkgray")
boxplot(dataE$Distance, main = "Boxplot of Distance", ylab = "Distance (mi)", col = "darkgray")

#Average GPA
summary(dataE$Avg.GPA)
sd(dataE$Avg.GPA)
hist(dataE$Avg.GPA, main = "Histogram of GPA", xlab = "GPA", col = "darkgray")
boxplot(dataE$Avg.GPA, main = "Boxplot of GPA", ylab = "GPA", col = "darkgray")

#Acceptance Rate (%)
summary(dataE$Acceptance.Rate)
sd(dataE$Acceptance.Rate)
hist(dataE$Acceptance.Rate, main = "Histogram of Acceptance Rate", xlab = "Acceptance Rate (%)", col = "darkgray")
boxplot(dataE$Acceptance.Rate, main = "Boxplot of Acceptance Rate", ylab = "Acceptance Rate (%)", col = "darkgray")

#Enrollment Size
summary(dataE$Enrollment.Size)
sd(dataE$Enrollment.Size)
hist(dataE$Enrollment.Size, main = "Histogram of Enrollment Size", xlab = "Enrollment Size", col = "darkgray")
boxplot(dataE$Enrollment.Size, main = "Boxplot of Enrollment Size", ylab = "Enrollment Size", col = "darkgray")

#Endowment (millions of $)
summary(dataE$Endowment, na.rm=TRUE)
sd(dataE$Endowment, na.rm=TRUE)
hist(dataE$Endowment, main = "Histogram of Endowment", xlab = "Endowment (Millions of $)", col = "darkgray")
boxplot(dataE$Endowment, main = "Boxplot of Endowment", ylab = "Endowment (Millions of $)", col = "darkgray")

#Distance Vs. Count
plot(dataE$Distance,dataE$Count, main = "Distance vs. Count", xlab = "Distance", ylab = "Count")
cor(dataE$Distance,dataE$Count)
regression = lm(dataE$Count ~ dataE$Distance)
abline(regression)
summary(regression)

residual = resid(regression)
plot(fitted(regression),residual, main = "Residuals", xlab = "Distance", ylab = "Residuals")
abline(0,0)

#Multivariable Linear Regression
regression2 = lm(dataE$Count ~ dataE$Distance + dataE$Acceptance.Rate + dataE$Enrollment.Size + dataE$Endowment)
summary(regression2)

