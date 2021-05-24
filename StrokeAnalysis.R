####Loading packages
packages = c('sf', 'tmap', 'tidyverse','plotly','ggthemes','heatmaply','RColorBrewer','DataExplorer')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

## Load the strokes dataset
stroke <- read.csv("stroke.csv")
str(stroke)

## Null values count in the dataset
na_count <-sapply(stroke, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

## Gender-wise count
table(stroke$gender)

## Gender-wise stroke and no-stroke count
withstroke <- stroke %>% filter(stroke==1)
wostroke <-  stroke %>% filter(stroke==0)
stat <- ifelse(stroke$stroke==1,"stroke","no stroke")
sum(stroke$stroke)

## Histogram of Age
hist(stroke$age)

## Stroke by Age in male and female
withstroke %>% ggplot(aes(age, fill=gender)) + geom_density(alpha=0.2) + ggtitle("Stroke by Age in Male and Female")

## Histogram of Glucose level
hist(stroke$avg_glucose_level)

## Stroke and Glucose level by Gender
withstroke %>% ggplot(aes(avg_glucose_level, fill=gender)) + geom_density(alpha=0.2) + ggtitle("Stroke and Glucose Level by Gender")

## Stroke and Glucose level over Age
withstroke %>% ggplot(aes(age, avg_glucose_level, color=gender)) + geom_point() + ggtitle("Stroke and Glucose Level over Time")

## BMI with stroke
bmi_withstroke <- ifelse(withstroke$bmi=="N/A",0,withstroke$bmi)
bmi_withstroke <- as.numeric(bmi_withstroke)
hist(bmi_withstroke)

## Hypothesis-1
ht1<- cor.test(stroke$age, stroke$stroke, method = "spearman")
ht1

## Hypothesis-2
ht2<- cor.test(stroke$hypertension, stroke$stroke, method = "spearman")
ht2

## Hypothesis-3
ht3<- cor.test(stroke$avg_glucose_level, stroke$stroke, method = "spearman")
ht3

## Hypothesis-4
ht4<- cor.test(stroke$avg_glucose_level, stroke$hypertension, method = "spearman")
ht4

## Hypothesis-5
ht5<- cor.test(stroke$heart_disease, stroke$stroke, method = "spearman")
ht5

## Age Vs Stroke
r1<-ggplot(stroke,aes(x=age,y=stroke,color=gender,size=age))+geom_point(alpha=0.6)
fig1 <- ggplotly(r1)
fig1

## Hypertension Vs Stroke
ggplot(stroke, aes(as.factor(stroke), hypertension))+
  geom_boxplot(col = "blue")+
  ggtitle("Distribution of Hypertension by Stroke")+
  xlab("stroke")+
  theme(plot.title = element_text(hjust = .5))

## Average Glucose Level Vs Stroke
ggplot(stroke, aes(as.factor(stroke), avg_glucose_level))+
  geom_boxplot(col = "blue")+
  ggtitle("Distribution of Glucose by Stroke")+
  xlab("stroke")+
  theme(plot.title = element_text(hjust = .5))

## Correlation Plot
plot_correlation(stroke,'continuous')