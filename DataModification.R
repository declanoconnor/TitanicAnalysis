library(ggplot2)
library(dplyr)
library(GGally)
library(rpart)
library(rpart.plot)
library(randomForest)
library(car)
library(psych)
library(mnormt)
library(pastecs)
library(ggpubr)
library(nortest)
library(ggmosaic)

#________Preprocessing________#

#Load data
test <- read.csv('C:/Users/user/Desktop/Titanic/test.csv',stringsAsFactors = FALSE)
train<- read.csv('C:/Users/user/Desktop/Titanic/train.csv', stringsAsFactors = FALSE)

#Bind rows
ful <- bind_rows(train,test)
full <- bind_rows(ful,gendersub)

# Checking the structure
str(full)

#removes all NA values?
colSums(is.na(full))
Pfull1 <-na.omit(full)
colSums(is.na(Pfull1))

#check for empty values 
colSums(Pfull1=="")

#move embarked empty values to C?
Pfull1$Embarked[Pfull1$Embarked==""]="C"

#check unique values
apply(Pfull1,2, function(x) length(unique(x)))

#Drop the cabin collumn as it has many missing values
Pfull1$Cabin<-NULL 

#features to factors
cols<-c("Survived","Pclass","Sex","Embarked")
for (i in cols){
  Pfull1[,i] <- as.factor(Pfull1[,i])
}

#check unique values for new variable
apply(Pfull1,2, function(x) length(unique(x)))


#________H1 - Analysis - Age Part 1 (Age, Sex, Parch, SibSp, Survival) ________#

LT=dim(Pfull1)[1]

ggplot(Pfull1, aes(x=Age)) + 
  ggtitle("Passenger Age") + 
  xlab("Age") + 
  ylab("Density") + 
  geom_histogram(aes(y=..density..), binwidth=1)+
  geom_density(alpha=.5, fill="#FFFFFF")

qqnorm(pfull1$Age)
qqline(pfull1$Age)

qqPlot(Pfull1$Age) 

ggqqplot(Pfull1$Age)

ggdensity(Pfull1$Age, 
          main = "Density plot of Age",
          xlab = "Ages")


plot(Pfull1$Age, Pfull1$Survived, xlab = 'Age', ylab = 'Survived')

hist.Age<-ggplot(Pfull1,aes(Age))+geom_histogram(aes(y=..density..),binwidth=1.5,colour="black",fill="white")
hist.Age+stat_function(fun=dnorm,args=list(mean=mean(Pfull1$Age,na.rm=TRUE),sd=sd(Pfull1$Age,na.rm=TRUE)),colour="blue",size=1)

ggplot(data=Pfull1[!(is.na(Pfull1[1:LT,]$Age)),],aes(x=Age,fill=Survived))+geom_histogram(binwidth =1)

Pfull2<-data.frame(Pfull1)
Pfull2$claint = as.integer(Pfull1$Pclass)
Pfull2$sexint = as.integer(Pfull1$Sex)
Pfull2$ageint = as.integer(Pfull1$Age)
Pfull2$embint = as.integer(Pfull1$Embarked)
Pfull2$surint = as.integer(Pfull1$Survived)
Pfull2$Pclass = NULL
Pfull2$Ticket = NULL
Pfull2$Name = NULL
Pfull2$Fate = NULL
Pfull2$AgeGroup = NULL
Pfull2$Sex = NULL
Pfull2$Embarked = NULL
Pfull2$Survived = NULL
Pfull2$Age = NULL
Pfull2$Fare = NULL
Pfull2$PassengerId = NULL

M<-cor(Pfull2)
head(round(M,2))
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)



#________H1 -Analysis - Age part 2(Age, Sex, Parch, SibSp, Survival)________#

#Create column with new adult or child based on age
Pfull1$AgeGroup <- "Adult"
Pfull1$AgeGroup [Pfull1$Age < 18] <- "Child"
table(Pfull1$AgeGroup)

ggplot(Pfull1, aes(AgeGroup,fill = Survived)) +
    geom_bar() +
   labs(title = "Age-groups Vs Suvival rate") +
   labs(xlab("Age-Group")) +
   labs(ylab("Count"))+
  facet_grid(.~Sex)

ggplot(Pfull1, aes(Sex,fill = Survived)) +
    geom_bar() +
   labs(title = "Sex & Embarked Vs Survival rate") +
   labs(xlab("Sex")) +
   labs(ylab("Count"))+
  facet_grid(.~Embarked)

#add variable lived or died
Pfull1$Fate [Pfull1$Survived == 0] <- "Died"
Pfull1$Fate [Pfull1$Survived == 1] <- "Lived"
fate <- table (Pfull1$Fate)
fate

#Proportion with contingency table
lived_percentage <- round(100*prop.table(fate),2)
lived_percentage

age_group_lived <- table(Pfull1$AgeGroup, Pfull1$Fate)
age_group_lived
age_group_lived_prop <- prop.table(age_group_lived, margin=1) # 1 = rows, 2 = columns
age_group_lived_prop

#________H1 - Other Tests (Age, Sex, Parch, SibSp, Survival)________#

 normtest<-function(x)
{
	library(nortest)
	s<-shapiro.test(x)
	ad<-ad.test(x)
	cvm<-cvm.test(x)
	ll<-lillie.test(x)
	sf<-sf.test(x)
	df<-data.frame(Method=c(s$method, ad$method, cvm$method, ll$method, sf$method),
	P.Value=c(s$p.value, ad$p.value, cvm$p.value, ll$p.value, sf$p.value))
	df
 }
 
normtest(Pfull1$Age)

shapiro.test(Pfull1$Age)
shapiro.test(Pfull1$SibSp)
shapiro.test(Pfull1$Parch)

leveneTest(Pfull1$SibSp,Pfull1$Age)
leveneTest(Pfull1$Parch,Pfull1$Age)
leveneTest(Pfull1$Survived,Pfull1$Age)

chisq.test(Pfull1$Survived, Pfull1$AgeGroup)

wilcox.test(Pfull1$Age) 

cor.test(Pfull1$Age,Pfull1$Parch,method='spearman')
cor.test(Pfull1$Age,Pfull1$SibSp,method='spearman')

#Perform Anderson-Darling normality test
ad.test(Pfull1$Age)

#Perform Cramér-von Mises test for normality
cvm.test(Pfull1$Age)

#Perform Shapiro-Francia test for normality
sf.test(Pfull1$Age)

describe(Pfull1)
stat.desc(Pfull1,basic=FALSE,norm=TRUE)
round(stat.desc(Pfull1[,c("Age","Fare","Sibsp")],basic=FALSE,norm=TRUE),digits=3)



#________H2 - Analysis - CLASS (Pclass, Fare, Embarked, Survival, Name) ________#

t<-table(full[1:LT,]$Embarked,Pfull1[1:LT,]$Survived)
for (i in 1:dim(t)[1]){
    t[i,]<-t[i,]/sum(t[i,])*100
}
t

ggplot(data = Pfull1[1:LT,],aes(x=Fare,fill=Survived))+geom_histogram(binwidth =40, position="fill")

Pfull1$Fare[is.na(Pfull1$Fare)] <- mean(Pfull1$Fare,na.rm=T)
sum(is.na(Pfull1$Age))

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
full$Title[full$Title == 'Mlle']<- 'Miss' 
full$Title[full$Title == 'Ms']<- 'Miss'
full$Title[full$Title == 'Mme']<- 'Mrs' 
full$Title[full$Title == 'Lady']<- 'Miss'
full$Title[full$Title == 'Dona']<- 'Miss'
officer<- c('Capt','Col','Don','Dr','Jonkheer','Major','Rev','Sir','the Countess')
full$Title[full$Title %in% officer]<-'Officer'
full$Title<- as.factor(full$Title)+ylab("Frequency")

LT=dim(Pfull1)[1]

ggplot(Pfull1, aes(x=Fare)) + 
  ggtitle("Passenger Fare") + 
  xlab("Fare") + 
  ylab("Density") + 
  geom_histogram(aes(y=..density..), binwidth=100)+
  geom_density(alpha=.5, fill="#FFFFFF")

#Create column with new wealthclass or ticket fare
Pfull1$Wclass<- "Upper class"
Pfull1$Wclass [Pfull1$Fare < 69] <- "Middle class"
Pfull1$Wclass [Pfull1$Fare < 20] <- "Working Class"
table(Pfull1$Wclass)

qqnorm(Pfull1$Fare)
qqline(Pfull1$Fare)

qqPlot(Pfull1$Fare) 

ggqqplot(Pfull1$Fare)

ggdensity(Pfull1$Fare, 
          main = "Density plot of Fare",
          xlab = "Price")

plot(Pfull1$Fare, Pfull1$Pclass, xlab = 'Fare', ylab = 'Class')
plot(Pfull1$Fare, Pfull1$Survived, xlab = 'Fare', ylab = 'Survived')

hist.Fare<-ggplot(Pfull1,aes(Fare))+geom_histogram(aes(y=..density..),binwidth=1.5,colour="black",fill="white")
hist.Fare+stat_function(fun=dnorm,args=list(mean=mean(Pfull1$Fare,na.rm=TRUE),sd=sd(Pfull1$Fare,na.rm=TRUE)),colour="blue",size=1)

ggplot(data=Pfull1[!(is.na(Pfull1[1:LT,]$Fare)),],aes(x=Fare,fill=Survived))+geom_histogram(binwidth =8)

ggplot(Pfull1, aes(AgeGroup,fill = Survived)) +
    geom_bar() +
   labs(title = "Age-groups Vs Suvival rate(Pclass)") +
   labs(xlab("Pclass")) +
   labs(ylab("Count"))+
  facet_grid(.~Pclass)

ggplot(Pfull1, aes(AgeGroup,fill = Survived)) +
    geom_bar() +
   labs(title = "Age-groups Vs Suvival rate (Embarked)") +
   labs(xlab("Embarked")) +
   labs(ylab("Count"))+
  facet_grid(.~Embarked)

ggplot(Pfull1, aes(Wclass,fill = Survived)) +
    geom_bar() +
   labs(title = "Age-groups Vs Suvival rate (Wealth Class)") +
   labs(xlab("Pclass")) +
   labs(ylab("Count"))+
  facet_grid(.~Pclass)

ggplot(Pfull1, aes(Wclass,fill = Survived)) +
    geom_bar() +
   labs(title = "Age-groups Vs Suvival rate (Wealth Class)") +
   labs(xlab("Embarked")) +
   labs(ylab("Count"))+
  facet_grid(.~Embarked)

ggplot(Pfull1, aes(AgeGroup,fill = Survived)) +
    geom_bar() +
   labs(title = "Age-groups Vs Suvival rate") +
   labs(xlab("Age-Group")) +
   labs(ylab("Count"))+
  facet_grid(.~Pclass)

#NormFare <- (Pfull1$Fare-min(Pfull1$Fare))/(max(Pfull1$Fare)-min(Pfull1$Fare))


#________H2 - Other Tests- CLASS (Pclass, Fare, Embarked, Survival, Name)________#

 normtest<-function(x)
{
	library(nortest)
	s<-shapiro.test(x)
	ad<-ad.test(x)
	cvm<-cvm.test(x)
	ll<-lillie.test(x)
	sf<-sf.test(x)
	df<-data.frame(Method=c(s$method, ad$method, cvm$method, ll$method, sf$method),
	P.Value=c(s$p.value, ad$p.value, cvm$p.value, ll$p.value, sf$p.value))
	df
 }
 
normtest(Pfull1$Fare)

shapiro.test(Pfull1$Fare)

leveneTest(Pfull1$Fare,Pfull1$Embarked)
leveneTest(Pfull1$Fare,Pfull1$Pclass)

chisq.test(Pfull1$Survived, Pfull1$Pclass)
chisq.test(Pfull1$Survived, Pfull1$Wclass)
chisq.test(Pfull1$Survived, Pfull1$Embarked)

wilcox.test(Pfull1$Fare)
 
cor.test(Pfull1$Age,Pfull1$Fare,method='spearman')

#Perform Anderson-Darling normality test
ad.test(Pfull1$Fare)

#Perform Cramér-von Mises test for normality
cvm.test(Pfull1$Fare)

#Perform Shapiro-Francia test for normality
sf.test(Pfull1$Fare)

describe(Pfull1)
stat.desc(Pfull1,basic=FALSE,norm=TRUE)
round(stat.desc(Pfull1[,c("Embarked","Fare","Pclass")],basic=FALSE,norm=TRUE),digits=3)


#________H3 - Analysis - GENDER (Sex, Embarked, Wclass, Survival) ________#

summary(Pfull1$Sex)

table(Pfull1$Survived, Pfull1$Sex)

ggplot(data=Pfull1[1:LT,],aes(x=Sex,fill=Survived))+geom_bar()

ggplot(data=Pfull1[1:LT,],aes(x=Sex,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

ggplot(data=Pfull1[1:LT,],aes(x=product(Sex),fill=survival))+geom_mosaic+labs(x="Sex",y ="Proportion surviving")

ggplot(Pfull1, aes(Sex,fill = Survived)) +
    geom_bar() +
   labs(title = "Sex Vs Suvival rate") +
   labs(xlab("Sex")) +
   labs(ylab("Count"))+
  facet_grid(.~Pclass)

ggplot(Pfull1, aes(Sex,fill = Survived)) +
    geom_bar() +
   labs(title = "Sex Vs Suvival rate") +
   labs(xlab("Sex")) +
   labs(ylab("Count"))+
  facet_grid(.~Embarked)

ggplot(Pfull1, aes(Sex,fill = Survived)) +
    geom_bar() +
   labs(title = "Sex Vs Suvival rate") +
   labs(xlab("Sex")) +
   labs(ylab("Count"))+
  facet_grid(.~Wclass)

ggplot(Pfull1, aes(Sex,fill = Survived)) +
    geom_bar() +
   labs(title = "Sex Vs Suvival rate") +
   labs(xlab("Sex")) +
   labs(ylab("Count"))+
  facet_grid(.~AgeGroup)


#________H3 - Other Tests- GENDER (Sex, Embarked, Survival)________#

normtest(Pfull2$sexint)

shapiro.test(Pfull2$sexint)

leveneTest(Pfull2$Sexint,Pfull2$surint)
leveneTest(Pfull2$Fare,Pfull2$surint)

chisq.test(Pfull1$Survived, Pfull1$Sex)
chisq.test(Pfull1$Survived, Pfull1$Embarked)

wilcox.test(Pfull2$sexint)
 
cor.test(Pfull1$sexint,Pfull1$embint,method='spearman')
