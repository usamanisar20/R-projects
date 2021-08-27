#This project is based on a kaggle project https://www.kaggle.com/uciml/student-alcohol-consumption?select=student-por.csv
#The data were obtained in a survey of students math and portuguese language courses in secondary school. It contains a lot of interesting social, gender and study information about students. You can use it for some EDA or try to predict students final grade.

#Analysis is carried out and attributes are compared to draw conclusions on the academic performance of the students.

#New library added
install.packages("corrplot")

#Libraries
library(dplyr)
library(ggplot2)
library(corrplot)


#import dataset


View(studentdata)



#summary of dataset

summary(studentdata)

#Show or view dataset

head(studentdata)

str(studentdata)

#Table to show male and female performance

stPerformance <- table(studentdata$sex,studentdata$G3)
stPerformance

#Average of G1, G2 and G3 Grades
studentdata$avggrade = (studentdata$G1 + studentdata$G2 + studentdata$G3) / 3

#Box plot on Student performance by gender

ggplot(studentdata, aes(y= sex, x = G3)) +
  geom_boxplot(color="red", fill="yellow", alpha=0.2) +ylab("Gender")+xlab("Final Grade") + 
  ggtitle(" Box Plot on studentdata Perfomance across Genders")

#construct table to show male and female ratio of internet connection
Internet_connection <- table(studentdata$internet, studentdata$sex)
Internet_connection

#Bar Graph to show bar graph of students with internet connection by gender
ggplot(studentdata, aes(x = internet , fill = sex)) + 
geom_bar(position = "dodge") +  xlab("Internet Connection")+ylab("Total") +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle(" Bar Graph on studentdatas with internet connection based on gender")

#construct table number of previous failures against internet connection
previous_failure<- table(studentdata$internet,studentdata$failures)
previous_failure

#Histogram of previous failures against internet connection

ggplot(studentdata, aes(fill =internet, y = failures)) +
  geom_histogram(position = "dodge", bins = 10) +xlab("Internet Conn")+ylab("Previous Failures") +
  ggtitle(" Histogram on previous Failures against Internet connection")

# Set up a 2 x 4 plotting space
par(mfrow = c(2, 4)) 
#draw histogram
family.hist <- unique(studentdata$famrel)
for (famrel.i in family.hist)
{
  data.temp <- subset(studentdata,famrel == famrel.i)
  hist(data.temp$absences,
       main = paste ("Amount of absences
for famrel =", famrel.i),
       xlab = "absences")
}

#draw bar graph on student performance with and without paid classes
ggplot(studentdata, aes(x = paid, y = G3)) +
  geom_boxplot(color="blue", fill="red", alpha=0.2) +xlab("Paid Classes")+ylab("Final Grade") + 
  ggtitle("Box Plot Students Performance with and without Paid Classes")

# Box Plot on Family education support on student performance
ggplot(studentdata, aes(y= famsup, x = G3)) +
  geom_boxplot(color="black", fill="blue", alpha=0.2) +ylab("Family educational support ")+xlab("Final Grade ") + 
  ggtitle(" Box Plot on Family education support on student performance")

#Impact of family relationship on student performance 
  ggplot(studentdata, aes(x=famrel, y=G3)) + 
  geom_point() +xlab("Family Relationship")+ylab("Performance") +
  ggtitle("Dot Plot on impact of Family Relationship on student performance")
  
#Relationship between absence and travel time

    ggplot(studentdata, aes(x = traveltime, y = absences, color = 'blue')) + geom_point() +xlab("Travel Time")+ylab("Absent Count") + ggtitle("Scatter Plot on relationship between Travel Time and Absence")
  
#Female and Male students consumption of Alcohol on workdays. 
  
      ggplot(studentdata, aes(fill = sex , y = Dalc)) +
      geom_bar(position = "dodge")+ylab("Workday Alcohol Consumption") + 
      ggtitle("Horizontal Bar chart of Male and Female Students alcohol consumption on Workdays")
    
#Relationship between student performance and romantic relationship.
  
        ggplot(studentdata, aes(x=G3, fill = romantic)) +
        geom_histogram(position = "dodge") +xlab("Final Grade")+
        ggtitle("Histogram on Student performance and Romantic Relationships")
 
#Dot plot of Student performance avergae with weekday alcohol consumption
        
        ggplot(data = studentdata, aes(x = Dalc, y = avggrade)) +
          geom_point(alpha = 0.1)
        

        #G1: Tile graph comparing the studytime and failure with marks intensity        
 ggplot(studentdata, aes(studytime , failures)) + geom_tile(aes(fill = G1), color="white") + 
          scale_fill_gradient(low ="red", high = "green")   
 
 #G2: Tile graph comparing the studytime and failure with marks intensity  
 ggplot(studentdata, aes(studytime , failures)) + geom_tile(aes(fill = G2), color="white") + 
   scale_fill_gradient(low ="red", high = "green")   
      
 #G3: Tile graph comparing the studytime and failure with marks intensity  
 ggplot(studentdata, aes(studytime , failures)) + geom_tile(aes(fill = G3), color="white") + 
   scale_fill_gradient(low ="red", high = "green")   
 
 #Average grade and failure with marks intensity
 ggplot(studentdata, aes(studytime , failures)) + geom_tile(aes(fill = avggrade), color="white") + 
   scale_fill_gradient(low ="red", high = "green")   
 
 studentdata$avggrade = (studentdata$G1 + studentdata$G2 + studentdata$G3) / 3
 
 t <- mutate(studentdata,
                       Dalc = factor(Dalc, labels=c("very low", "low","medium", "high", "very high"), ordered=T),
                       Walc = factor(Dalc, labels=c("very low", "low","medium", "high", "very high"), ordered=T))

 alcsch <- c("#0ed9f0","#efbaf7", "#79fca9", "#f7c886", "#f2aba7" )
 
 ggplot(studentdata, aes(x=famsup, y=avggrade, fill=factor(Walc))) + geom_boxplot() + 
   labs(title="Average Grades: Family Support by Alcohol Consumption", x="Family Support", y="Average Grades", fill= "Alcohol Consumption During the Week", caption= "Source: studentdata") + scale_fill_manual(values=alcsch)
  
 alccolors <- c("#44cf4d", "#cfcd00","#cf4449", "#444dcf")
 ggplot(studentdata, aes(x=factor(goout), y=avggrade, fill=factor(studytime)))+
   geom_boxplot()+
   labs(x= "Going Out\nScale from 1(Low) to 5(High)", 
        y= "Average Grade", 
        title = "Average Grade based on amount of Going Out",
        color= "Study Time", 
        size= "Weekly Alcohol\n Consumption Scale\n from 1(Low) to 10(High)")+
   scale_fill_manual(values = alccolors)
 
 

 
