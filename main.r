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
ggplot(data=df, aes(x=GRE.Score)) +
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
colData = df$University.Rating

# A.
colDf = as.data.frame(
    table(
        colData,
        dnn=list("University Rating")
    ),
    responseName = "freq"
)

colDf$percentage = colDf$freq * 100 / sum(colDf$freq)

# B.
ggplot(data=df, aes(x=University.Rating, label=scales::percent(prop.table(stat(count))))) +
    geom_bar(fill=rainbow(length(unique(df$University.Rating)))) +
    geom_text(stat = 'count', position = position_dodge(.9), vjust = -0.5, size = 3)

# C.
library(dplyr)
ggplot(data=df %>% group_by(University.Rating) %>% summarize(count=n()),
    aes(x=reorder(as.factor(University.Rating), count), y=count, fill=University.Rating)) +
    geom_bar(stat="identity") +
    coord_flip()

# D.
ggplot(data=df, aes(x=University.Rating, y=Chance.of.Admit, color = University.Rating)) +
    geom_violin() +
    geom_boxplot(width=0.1)
