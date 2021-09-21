library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(broom)
options(scipen = 10)

rawData <- as.data.frame(read_excel("", sheet = "Data"))
Data <- rawData[-1][rawData$Borough %in% c("Camden", "Hackney", "Haringey", "Islington", "Tower Hamlets", "Westminster"), c(1:3, 34:43, 49:53)]
Data[, 4:18] <- lapply(Data[, 4:18], as.numeric)

### Data preprocessing
p1.data <- melt(Data[, 2:8], id.vars = c("Ward", "Borough"), measure.vars = 3:7)
colnames(p1.data) <- c("Ward", "Borough", "Year", "GCSE")
p1.data <- within(p1.data, {
  GCSE = as.numeric(GCSE) 
  Year = case_when(grepl("2009", Year) ~ "2009",
                   grepl("2010", Year) ~ "2010",
                   grepl("2011", Year) ~ "2011",
                   grepl("2012", Year) ~ "2012",
                   grepl("2013", Year) ~ "2013")
  })

p2.data <- melt(Data[, c(2:3, 9:13)], id.vars = c("Ward", "Borough"), measure.vars = 3:7)
colnames(p2.data) <- c("Ward", "Borough", "Year", "Absence")
p2.data <- within(p2.data, {
  Abscence = as.numeric(Absence)
  Year = case_when(grepl("2009", Year) ~ "2009",
                   grepl("2010", Year) ~ "2010",
                   grepl("2011", Year) ~ "2011",
                   grepl("2012", Year) ~ "2012",
                   grepl("2013", Year) ~ "2013")
  })

p3.data <- melt(Data[, c(2:3, 14:18)], id.vars = c("Ward", "Borough"), measure.vars = 3:7)
colnames(p3.data) <- c("Ward", "Borough", "Year", "Transport")
p3.data <- within(p3.data, {
  Transport = as.numeric(Transport)
  Year = case_when(grepl("2009", Year) ~ "2009",
                   grepl("2010", Year) ~ "2010",
                   grepl("2011", Year) ~ "2011",
                   grepl("2012", Year) ~ "2012",
                   grepl("2013", Year) ~ "2013")
  })

final <- merge(p1.data, merge(p2.data, p3.data, by = c("Ward", "Borough", "Year")), by = c("Ward", "Borough", "Year"))

### Descriptive statistics
summary.table <- rbind(as.numeric(summary(final$GCSE)), as.numeric(summary(final$Abscence)), as.numeric(summary(final$Transport)))
dimnames(summary.table) <- list(c("GCSE", "Absence", "Transport"), c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))
View(summary.table)

shapiro.test(final$GCSE)
shapiro.test(final$Absence)
shapiro.test(final$Transport)

### Visualisation
mycolours <- c(rgb(0.8, 0.2, 0.2), rgb(0.2, 0.8, 0.2), rgb(0.2, 0.2, 0.8), rgb(0.8, 0.8, 0.2), rgb(0.2, 0.8, 0.8), rgb(0.8, 0.2, 0.8))
ggplot(data = final, aes(x = Year, group = Year, y = GCSE, fill = Year)) + geom_boxplot() + 
  scale_fill_manual(values = alpha(mycolours, 0.75))
ggplot(data = final, aes(x = Borough, y = GCSE, fill = Borough)) + geom_boxplot() + theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = alpha(mycolours, 0.75))
ggplot(data = final, aes(x = interaction(Borough, Year), y = GCSE)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title.x = element_blank())

### Average GCSE by year
print(aggregate(data = final, GCSE ~ Year, mean))

### Kruskal-Wallis test to compare multiple means
kruskal.test(data = final, GCSE ~ Borough)

pvals <- c("Westminster - Haringey" = with(final, wilcox.test(GCSE[Borough == "Westminster"], GCSE[Borough == "Haringey"]))$p.value,
           "Westminster - Islington" = with(final, wilcox.test(GCSE[Borough == "Westminster"], GCSE[Borough == "Islington"]))$p.value,
           "Westminster - Camden" = with(final, wilcox.test(GCSE[Borough == "Westminster"], GCSE[Borough == "Camden"]))$p.value,
           "Westminster - Hackney" = with(final, wilcox.test(GCSE[Borough == "Westminster"], GCSE[Borough == "Hackney"]))$p.value,
           "Westminster - T.Hamlets" = with(final, wilcox.test(GCSE[Borough == "Westminster"], GCSE[Borough == "Tower Hamlets"]))$p.value,
           "Haringey - Islington" = with(final, wilcox.test(GCSE[Borough == "Haringey"], GCSE[Borough == "Islington"]))$p.value,
           "Haringey - Camden" = with(final, wilcox.test(GCSE[Borough == "Haringey"], GCSE[Borough == "Camden"]))$p.value,
           "Haringey - Hackney" = with(final, wilcox.test(GCSE[Borough == "Haringey"], GCSE[Borough == "Hackney"]))$p.value,
           "Haringey - T.Hamlets" = with(final, wilcox.test(GCSE[Borough == "Haringey"], GCSE[Borough == "Tower Hamlets"]))$p.value,
           "Islington - Camden" = with(final, wilcox.test(GCSE[Borough == "Islington"], GCSE[Borough == "Camden"]))$p.value,
           "Islington - Hackney" = with(final, wilcox.test(GCSE[Borough == "Islington"], GCSE[Borough == "Hackney"]))$p.value,
           "Islington - T.Hamlets" = with(final, wilcox.test(GCSE[Borough == "Islington"], GCSE[Borough == "Tower Hamlets"]))$p.value,
           "Camden - Hackney" = with(final, wilcox.test(GCSE[Borough == "Camden"], GCSE[Borough == "Hackney"]))$p.value,
           "Camden - T.Hamlets" = with(final, wilcox.test(GCSE[Borough == "Camden"], GCSE[Borough == "Tower Hamlets"]))$p.value,
           "Hackney - T.Hamlets" = with(final, wilcox.test(GCSE[Borough == "Hackney"], GCSE[Borough == "Tower Hamlets"]))$p.value)
pvals.df <- tidy(round(p.adjust(pvals, method = "holm"), 4))
colnames(pvals.df) <- c("Pair", "p-value")

### Linear regression models
summary(lm(data = final, GCSE ~ Absence))
summary(lm(data = final, GCSE ~ Transport))
summary(lm(data = final, GCSE ~ Absence + Transport))

### A linear model with non-numeric factors
summary(lm(data = final, GCSE ~ Absence + factor(Borough)))

ggplot(data = final, aes(x = Absence, y = GCSE, col = Borough)) + geom_point() + scale_colour_manual(values = mycolours)
