# ---------------------------------------------------------
# Import, Set/Get Directory

setwd("/Users/Administrator/OneDrive/Desktop/Project")
getwd()
project.data <- read.csv(file= "Heart Disease Indicators.csv", 
                      header = TRUE, 
                      sep = ",")
project.data
# ---------------------------------------------------------
# Create Aggregate function per Age Group

aggregate.data <- aggregate(project.data$MentHlth_Scale,
                            by=list(Age=project.data$Age),
                            FUN=mean)
colnames(aggregate.data) <- c("Age_Group", 
                              "Average_Mental_Health")
aggregate2.data <-aggregate(project.data$PhysHlth_Scale,
                            by=list(Age=project.data$Age),
                            FUN=mean)
colnames(aggregate2.data) <- c("Age_Group", 
                              "Average_Physical_Health")
aggregate.data$Average_Physical_Health <-aggregate2.data$Average_Physical_Health


aggregate.data

# ---------------------------------------------------------
# Install ggpubr package

# Pearson Correlation
install.packages("ggpubr")
library("ggpubr")

install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")

ggscatter(aggregate.data, x = "Average_Mental_Health", y = "Average_Physical_Health", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Correlation Test Between Physical & Mental Health",
          label = row.names(aggregate.data),
          repel = TRUE,
          color = "#00AFBB",
          font.label = c("bold", "red"),
          ggtheme = theme_dark(),
          xlab = "Mental Health (Avg Effected Days per Month)",
          ylab = "Physical Health (Avg Effected Days per Month)")
          
aggregate.data

# Theoretical plots (Q-Q plots (quantile-quantile plots)
ggqqplot(aggregate.data$Average_Mental_Health,
         main= "Theoretical Q-Q Plot: Mental Health",
         label = row.names(aggregate.data),
         ylab = "Mental Health Scale (1-30 Days)")
ggqqplot(aggregate.data$Average_Physical_Health,
         main = "Theoretical Q-Q Plot: Physical Health",
         ylab = "Physical Health Scale (1-30 Days)")

# Kendall Correlation Coefficient
res <- cor.test(aggregate.data$Average_Mental_Health, 
                aggregate.data$Average_Physical_Health,
                method="kendall")
res

# Spearman Rank Correlation Coefficient
res2 <-cor.test(aggregate.data$Average_Mental_Health, 
                aggregate.data$Average_Physical_Health,  method = "spearman")
res2
