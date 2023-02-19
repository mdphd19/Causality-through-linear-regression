library(readxl)
library(dplyr)
library(stringr)
library(readxl)
library(mltools)
library(caret)
library(corrplot)
library(tidytext)
library(stringr)
library(textdata)
library(gmodels)



#(a)Use a t-test to see if there is a statistical difference in the pre-period between schools in the 
#treatment (bal = 1) and control (bal = 0). This will check if randomization has been done 
#correctly. To do this, calculate the average normalized test score(norm) for the pre period (pre = 1) 
#for math (test_type = 0). Is there a statistical difference between students who got the 
#Balsakhi program and did not get the program? Perform the same test for language (test_type = 1)


#filtering data to pre period for math and language 


pre_math_data = filter(data_Q2, data_Q2$pre == 1 & data_Q2$test_type == 0)

pre_lang_data = filter(data_Q2, data_Q2$pre == 1 & data_Q2$test_type == 1)

#running t test on the filtered data sets for Math
t.test(pre_math_data$norm~pre_math_data$bal)

#Average norm value for both treatment and control group are more or less similarly divided given norm 
#value ranges between -1.9 and 3.1 and both control and treatment has average norm value of -6.854839e-09
#and -6.825465e-03 respectively implying randomization has been done correctly. The p-value for this test is 0.7327,
#which is well above our threshold of 0.05.We hence have enough evidence to say that the treatment and control groups have
#similar average normalized maths test scores for pre period.






t.test(pre_lang_data$norm~pre_lang_data$bal)

#Average norm value for both treatment and control group are more or less similarly divided given norm 
#value ranges between -1.614 and 3.962 and both control  and treatment has average norm value of
# -1.313364e-08 and    2.451397e-02  respectively .The p-value for this test is 0.2234, which is above 
#our threshold of 0.05. We hence have enough evidence to say that the treatment and control groups have 
#similar average normalized language test scores for pre period.



#b) Calculate the average test scores for the post period (post = 1) for math for treatment and 
#control. Is there a statistical difference between students in the two groups of schools? 
#Use a t-test model to test the increase.  Perform the same analysis for language test scores. 


#filtering the data into post period for Math and Language seperately 

post_math_data = filter(data_Q2, data_Q2$post == 1 & data_Q2$test_type == 0)

post_lang_data = filter(data_Q2, data_Q2$post == 1 & data_Q2$test_type == 1)



#running t test on the filtered data sets for Math
t.test(post_math_data$test~post_math_data$bal)

#Average test score value for treatment  group after experiment is 21.4639 and for control it is 
#19.7814. the t test returned a p-value of  6.591e-09, which is lower than the threshold of 0.05. 
#We can hence conclude that there us a difference in the mean test scores for maths between treatment and control groups 
#post Balsakhi Experiment


t.test(post_lang_data$test~post_lang_data$bal)

#Average test score value for treatment  group after experiment is 22.11557 and for control it is 
#21.09880. the t test returned a p-value of  0.0001624, which is lower than the threshold of 0.05. 
#We can hence conclude that there is a difference in the mean test scores for language between 
#treatment and control groups post Balsakhi Experiment



#(c)Can you conclude if the Balsakhi program increase test scores in reading and mathematics? 


#Running a linear model for maths to check if we can conclude that Balsakhi program 
#increased the test scores post experiment .



summary(lm(test ~ bal, data = post_math_data))

#The value of Coefficient for bal in the above linear model is 1.6880 indicating an increase in
#the test scores for math for the treatment group compared to control group. the P-value 
#from this test is 6.585e-09, which is lower than our threshold of 0.05. Hence we can conclude that
#the Balsakhi program increases the test scores for Mathematics 


#Running a linear model for language to check if we can conclude that Balsakhi program 
#increased the test scores post experiment 


summary(lm(test ~ bal, data = post_lang_data))

#The value of Coefficient for bal in the above linear model is 1.0168 indicating an increase in
#the test scores for language for the treatment group compared to control group. the P-value 
#from this test is 0.0001637, which is lower than our threshold of 0.05. Hence we can conclude that
#the Balsakhi program increases the test scores for languages 

#(d)Is the SUTVA assumption likely to be violated in the experiment? 


SUTVA assumptions state that:
  1. Treatment can not affect behavior of control group. 
2. External factors do not affect the behaviour of treatment or control group.
In this experiment, it can be violated if in case students in the treatment group interacts with students from 
 the control group, i.e maybe tutoring them  
Additionally, the test score of students in both the treatment and control
groups can be impacted due to external factors, such as intervention from parents and teachers or 
students cheating in the exams 



