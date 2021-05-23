## Part 0
df <- read.csv(file = "./dataset/UniversityAdmissions.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)
dim(df)
colnames(df)[apply(df, 2, anyNA)]
sum(is.na(df$CGPA))
df <- na.omit(df)
dim(df)

## Part 1
library(ggplot2)
colData = df$GRE.Score

# A.
summary(colData)
ggplot(df, aes(x=GRE.Score)) +
    geom_histogram(binwidth=10, aes(y=..density..), color="black", fill="white") +
    geom_density(alpha=.4, fill="red")

# B.
qqnorm(colData)
qqline(colData, col = "red", lwd = 2)

# C.
library(e1071)
skewness(colData)

# D.
boxplot(colData)

# E.
mean(colData)
median(colData)
var(colData)
sqrt(var(colData))

# F.
ggplot(df, aes(x=GRE.Score)) +
    geom_density(alpha=.2, fill="red") +
    geom_vline(xintercept=mean(colData), size=1.5, color="green") +
    geom_vline(xintercept=median(colData), size=1.5, color="blue")

# G.
intervalNum <- 4
minData <- min(colData)
maxData <- max(colData)
intervalLength <- (maxData - minData) / intervalNum

intervals <- c()
labels <- c()
for (i in 0:intervalNum) {
    newVal <- minData + i * intervalLength
    if (i > 0) {
        lastVal <- tail(intervals, n=1)
        labels <- append(labels, sprintf("%s%s, %s]", if(i == 1) '(' else '[', lastVal, newVal))
    }
    intervals <- append(intervals, newVal)
}
slices <- cut(colData, breaks=intervals, include.lowest=TRUE)

pie(
    as.data.frame(table(slices))$Freq,
    labels=labels,
    col=rainbow(length(intervals) - 1),
    main="GRE Score"
)

# H.
quantile(colData)
boxplot(colData)


## Part2
