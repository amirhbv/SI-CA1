## Part 0
df <- read.csv(file="./dataset/UniversityAdmissions.csv", header=TRUE,  sep=",", row.names=NULL,  stringsAsFactors=FALSE)
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
qqline(colData, col="red", lwd=2)

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


## Part 2
colData = df$University.Rating

# A.
colDf = as.data.frame(
    table(
        colData,
        dnn=list("University Rating")
    ),
    responseName="freq"
)

colDf$percentage = colDf$freq * 100 / sum(colDf$freq)

# B.
ggplot(data=df, aes(x=University.Rating, label=scales::percent(prop.table(stat(count))))) +
    geom_bar(fill=rainbow(length(unique(df$University.Rating)))) +
    geom_text(stat='count', position=position_dodge(.9), vjust=-0.5, size=3)

# C.
library(dplyr)
ggplot(data=df %>% group_by(University.Rating) %>% summarize(count=n()),
    aes(x=reorder(as.factor(University.Rating), count), y=count, fill=University.Rating)) +
    geom_bar(stat="identity") +
    coord_flip()

# D.
ggplot(data=df, aes(x=University.Rating, y=Chance.of.Admit, color=University.Rating)) +
    geom_violin() +
    geom_boxplot(width=0.1)


## Part 3

# B.
ggplot(df, aes(x=GRE.Score, y=TOEFL.Score)) +
    geom_point()

# C.
cor(df$GRE.Score, df$TOEFL.Score)

# E.
cor.test(df$GRE.Score, df$TOEFL.Score)

# F.
ggplot(data=df, aes(x=GRE.Score, y=TOEFL.Score, shape=factor(Research))) +
    geom_point(aes(color=factor(Research)))

# G.
library(hexbin)

ggplot(data=df, aes(x=GRE.Score, y=TOEFL.Score)) +
    stat_binhex(bins=10) +
    scale_fill_gradient(low="blue", high="red")

ggplot(data=df, aes(x=GRE.Score, y=TOEFL.Score)) +
    stat_binhex(bins=20) +
    scale_fill_gradient(low="blue", high="red")


## Part 4

# A.
pairs(dplyr::select_if(df, is.numeric))

# C.
library(scatterplot3d)
scatterplot3d(
    df[, c('GRE.Score', 'TOEFL.Score', 'CGPA')],
    main="3D Scatter Plot",
    xlab="GRE Score",
    ylab="TOEFL Score",
    zlab="CGPA",
    color=c("red", "blue")[df$Research + 1],
    pch=16,
    angle=30
)
legend(
    "bottomright",
    legend=c("Without Research", "With Research"),
    col=c("red", "blue"),
    pch=16
)

## Part 5

# A.
frequency_table = table(df$University.Rating, df$internship_abroad)
margined_frequency_table <- addmargins(frequency_table)
colnames(margined_frequency_table) <- c("Without Research", "With Research", "Sum")
print(margined_frequency_table)

# B.
frequency_table = table(df$internship_abroad, df$University.Rating)
colors <- c("blue", "red")
barplot(frequency_table, col=colors, beside=TRUE, xlab="University Rating", ylab="Count")
legend("topleft", legend=c("Without Research", "With Research"), fill=colors)

# C.
frequency_table = table(df$internship_abroad, df$University.Rating)
colors <- c("blue", "red")
barplot(frequency_table, col=colors, xlab="University Rating", ylab="Count")
legend("topleft", legend=c("Without Research", "With Research"), fill=colors)

# D.
manipulated_df <- df
manipulated_df$Research[manipulated_df$Research==0] <- "Without Research"
manipulated_df$Research[manipulated_df$Research==1] <- "With Research"
ggplot(data=manipulated_df) +
    geom_mosaic(aes(x=product(University.Rating), fill=Research)) +
    theme_mosaic()
