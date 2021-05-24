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
library(ggmosaic)
manipulated_df <- df
manipulated_df$Research[manipulated_df$Research==0] <- "Without Research"
manipulated_df$Research[manipulated_df$Research==1] <- "With Research"
ggplot(data=manipulated_df) +
    geom_mosaic(aes(x=product(University.Rating), fill=Research)) +
    theme_mosaic()


## Part 6

# A.
s <- sample(df$GRE.Score, 100)
s_mean <- mean(s)
s_sd <- sd(s)
SE <- qnorm(0.975) * s_sd / sqrt(length(s))
CI <- c(s_mean - SE, s_mean + SE)
cat("95% CI : (", CI, ")\n")

# C.
ggplot(data=df, aes(x=GRE.Score)) +
    geom_histogram(binwidth=10, color="black", fill="white") +
    geom_vline(xintercept=s_mean, size=1.5, color="red") +
    geom_vline(xintercept=CI[1], size=1.5, color="blue") +
    geom_vline(xintercept=CI[2], size=1.5, color="blue")

# D.
h0 <- 320
SE <- s_sd / sqrt(length(s))
z_statistic <- (s_mean - h0) / SE
p_value <- 2 * pnorm(abs(z_statistic), lower.tail=FALSE)
cat("P-value is ", p_value, "\n")

# F.
alpha = .05
sigma = sd(df$GRE.Score)
sem = sigma / sqrt(length(s));
mu0 = h0
I = c(alpha / 2, 1 - alpha / 2)
q = qnorm(I, mean=mu0, sd=sem);
p = pnorm(q, mean=mean(df$GRE.Score), sd=sem)
print(p)
beta <- diff(p)
print(beta)

# G.
power <- 1 - beta
print(power)


## Part 7

# A.
n <- 25
sample_population <- df[sample(nrow(df), n),]
sample_population_var1 <- sample_population$GRE.Score
sample_population_var2 <- sample_population$TOEFL.Score
mean_diff <- abs(mean(sample_population_var1) - mean(sample_population_var2))
t_score <- mean_diff / (sqrt(sd(sample_population_var1) ^ 2 / n) + sqrt(sd(sample_population_var2) ^ 2 / n))
p_value <- 2 * pt(t_score, df=n - 1, lower.tail=FALSE)
print(p_value)

# B.
n <- 100
sample_population_var1 <- sample(df$GRE.Score, n)
sample_population_var2 <- sample(df$TOEFL.Score, n)
mean_diff <- abs(mean(sample_population_var1) - mean(sample_population_var2))
t_score <- mean_diff / (sqrt(sd(sample_population_var1) ^ 2 / n) + sqrt(sd(sample_population_var2) ^ 2 / n))
p_value <- 2 * pt(t_score, df=n - 1, lower.tail=FALSE)
print(p_value)


## Part 8
library(boot)
cgpa_mean_calculator <- function(data, indices) {
    return (mean(data[indices, ]$CGPA))
}

# A.
boot_results <- boot(df, R=1000, statistic=cgpa_mean_calculator)
print(boot.ci(boot_results, type="perc"))

# Part B
boot_results <- boot(df[sample(nrow(df), 20),], R=1000, statistic=cgpa_mean_calculator)
print(boot.ci(boot_results, type="norm"))


## Part 9
print(summary(aov(df$Chance.of.Admit~as.factor(df$University.Rating))))
