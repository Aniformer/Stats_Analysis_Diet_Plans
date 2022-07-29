
  
#setup
library(tidyverse)
library(dplyr)

# Section 1

#Data Ingestion
#Reading dataset
diet_data <- read_csv("adult_diet_data.csv", guess_max = 10000)

#Reviewing dataset
str(diet_data)

#Converting 'sex' and 'diet_cond' field values into factors since they're categorical variables
diet_data$sex <- as.factor(diet_data$sex)
diet_data$diet_cond <- as.factor(diet_data$diet_cond)
str(diet_data)
summary(diet_data)

#Removing Outlier - Data cleansing
#Upon applying the summary function on the dataset an outlier can clearly be noticed since the maximum value for the height column has been identified as - 1636.3cm which is not realistic/possible. 
diet_data_processed <- filter(diet_data, diet_data$height != '1636.3')
#Other columns in the dataset seem to contain appropriate data, hence not plotting the dataset as a histogram to check for more outliers

#For renaming axes in plots. 
Labels <- c('M' = "Males",'F' = "Females", 'no_chocolate'="No Chocolate Diet", 'regular'="Regular Diet")

  
  #Question 1
#Impulsivity & Age Relationship
#R code associated for the first request in the assignment
#Plotting a scatterplot for the overall datase#
( ques1_plot1 <- ggplot(data = diet_data_processed, mapping = aes(x = age, y = impulsivity, color = sex)) + 
    geom_point() + 
    geom_smooth(se=FALSE)+
    labs(x="Age", y="Impulsivity", title = "Relatonship Between Age & Impulsivity For Entire Data")) 

#Plotting scatterplots for each gender 
( ques1_plot2 <- ggplot(data = diet_data_processed, mapping = aes(x = age, y = impulsivity, color = sex)) + 
    geom_point() + 
    geom_smooth(se=FALSE)+
    facet_wrap(~sex, nrow =2, labeller = as_labeller(Labels))+
    labs(x="Age", y="Impulsivity", title = "Relatonship Between Age & Impulsivity By Gender") )


  #Question 2
#Summary Statistics for BMI
#R code associated for the second request in the assignment
#Adding required calculated columns - 'starting_bmi', 'ending_bmi', 'total_weight_loss' to processed dataset
( diet_data_processed <- mutate(diet_data_processed, starting_bmi = diet_data_processed$start_weight/(diet_data_processed$height*0.01)^2, ending_bmi = diet_data_processed$end_weight/(diet_data_processed$height*0.01)^2, total_weight_loss = diet_data_processed$start_weight - diet_data_processed$end_weight) ) 

#Summary statistics for starting BMI
( start_bmi_calc_values <- diet_data_processed %>% group_by(sex, diet_cond) %>% summarise(mean = mean(starting_bmi), sd = sd(starting_bmi)) )

#Summary statistics for ending BMI
( end_bmi_calc_values <- diet_data_processed %>% group_by(sex, diet_cond) %>% summarise(mean = mean(ending_bmi), sd = sd(ending_bmi)) )

#Summary statistics for weight loss
( weight_loss_calc_values <- diet_data_processed %>% group_by(sex, diet_cond) %>% summarise(mean = mean(total_weight_loss), sd = sd(total_weight_loss)) )

#Summary Statistics for BMI_2
#Code for the plots generated in the report for question 2. They're not necessary for the analysis but since I've used these plots for reporting I've included the code here. Strictly for visualization purposes, not included in the knitted file for section 1. 

#Calculated values for male regular diet consumers 
male_reg_diet_values <- diet_data_processed %>% filter(sex=='M',diet_cond=='regular') %>% summarise(mean = mean(starting_bmi), sd = sd(starting_bmi), mean_end = mean(ending_bmi), sd_end = sd(ending_bmi), mean_wt_loss = mean(total_weight_loss), sd_wt_loss = sd(total_weight_loss))
#Calculated values for male no chocolate diet consumers
male_no_choc_diet_values <- diet_data_processed %>% filter(sex=='M',diet_cond=='no_chocolate') %>% summarise(mean = mean(starting_bmi), sd = sd(starting_bmi), mean_end = mean(ending_bmi), sd_end = sd(ending_bmi),mean_wt_loss = mean(total_weight_loss), sd_wt_loss = sd(total_weight_loss))
#Calculated values for female regular diet consumers
female_reg_diet_values <- diet_data_processed %>% filter(sex=='F',diet_cond=='regular') %>% summarise(mean = mean(starting_bmi), sd = sd(starting_bmi), mean_end = mean(ending_bmi), sd_end = sd(ending_bmi),mean_wt_loss = mean(total_weight_loss), sd_wt_loss = sd(total_weight_loss))
#Calculated values for female no chocolate diet consumers
female_no_choc_diet_values <- diet_data_processed %>% filter(sex=='F',diet_cond=='no_chocolate') %>% summarise(mean = mean(starting_bmi), sd = sd(starting_bmi), mean_end = mean(ending_bmi), sd_end = sd(ending_bmi),mean_wt_loss = mean(total_weight_loss), sd_wt_loss = sd(total_weight_loss))

#Starting BMI
#Plotting the starting BMI for all consumers and further plotting the mean and SD for the same
ques2_plot1 <- ggplot(diet_data_processed) + geom_histogram(aes(x=starting_bmi, color = sex), binwidth=1) +
  facet_grid(sex~diet_cond, labeller = as_labeller(Labels))  + 
  labs(x="Starting BMI", y="Frequency", title = "Summary Statistics for Starting BMI") + 
  
  #Plotting for males on the regular diet plan
  geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(xintercept=male_reg_diet_values$mean), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = male_reg_diet_values$mean+3,y = 20, label = paste("Mean =",as.character(round(male_reg_diet_values$mean,1)))), data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(label=paste("SD =", as.character(round(male_reg_diet_values$sd,1))), x=15, y=19), size=2.5)+
  
  #Plotting for males on no chocolate diet plan
  geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(xintercept=male_no_choc_diet_values$mean), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = male_no_choc_diet_values$mean+3,y = 20, label = paste("Mean =",as.character(round(male_no_choc_diet_values$mean,1)))), data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(label=paste("SD =", as.character(round(male_no_choc_diet_values$sd,1))), x=15, y=19), size=2.5) +
  
  #Plotting for females on the regular diet plan
  geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(xintercept=female_reg_diet_values$mean), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = female_reg_diet_values$mean+3,y = 20, label = paste("Mean =",as.character(round(female_reg_diet_values$mean,1)))), data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(label=paste("SD =", as.character(round(female_reg_diet_values$sd,1))), x=15, y=19), size=2.5)+
  
  #Plotting for females on no chocolate diet plan
  geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(xintercept=female_no_choc_diet_values$mean), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = female_no_choc_diet_values$mean+3,y = 20, label = paste("Mean =",as.character(round(female_no_choc_diet_values$mean,1)))), data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(label=paste("SD =", as.character(round(female_no_choc_diet_values$sd,1))), x=15, y=19), size=2.5) 

#Ending BMI
#Plotting the ending BMI for all consumers and further plotting the mean and SD for the same
ques2_plot2 <- ggplot(diet_data_processed) + geom_histogram(aes(x=ending_bmi, color = sex), binwidth=1) +
  facet_grid(sex~diet_cond, labeller = as_labeller(Labels)) + 
  labs(x="Ending BMI", y="Frequency", title = "Summary Statistics for Ending BMI") + 
  
  #Plotting for males on the regular diet plan
  geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(xintercept=male_reg_diet_values$mean_end), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = male_reg_diet_values$mean_end+3,y = 20, label = paste("Mean =",as.character(round(male_reg_diet_values$mean_end,1)))), data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(label=paste("SD =", as.character(round(male_reg_diet_values$sd_end,1))), x=15, y=19), size=2.5)+
  
  #Plotting for males on no chocolate diet plan
  geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(xintercept=male_no_choc_diet_values$mean_end), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = male_no_choc_diet_values$mean_end+3,y = 20, label = paste("Mean =",as.character(round(male_no_choc_diet_values$mean_end,1)))), data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(label=paste("SD =", as.character(round(male_no_choc_diet_values$sd_end,1))), x=15, y=19), size=2.5) +
  
  #Plotting for females on the regular diet plan
  geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(xintercept=female_reg_diet_values$mean_end), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = female_reg_diet_values$mean_end+3,y = 20, label = paste("Mean =",as.character(round(female_reg_diet_values$mean_end,1)))), data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(label=paste("SD =", as.character(round(female_reg_diet_values$sd_end,1))), x=15, y=19), size=2.5)+
  
  #Plotting for females on no chocolate diet plan
  geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(xintercept=female_no_choc_diet_values$mean_end), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = female_no_choc_diet_values$mean_end+3,y = 20, label = paste("Mean =",as.character(round(female_no_choc_diet_values$mean_end,1)))), data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(x=15, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(label=paste("SD =", as.character(round(female_no_choc_diet_values$sd_end,1))), x=15, y=19), size=2.5) 

#WeightLoss
#Plotting the weight loss for all consumers and further plotting the mean and SD for the same
ques2_plot3 <- ggplot(diet_data_processed) + geom_histogram(aes(x=total_weight_loss, color = sex), binwidth=1) +
  facet_grid(sex~diet_cond, labeller = as_labeller(Labels))  + 
  labs(x="Weight Loss", y="Frequency", titles = "Summary Statistics for Weight Loss") + 
  
  #Plotting for males on the regular diet plan
  geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(xintercept=male_reg_diet_values$mean_wt_loss), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = male_reg_diet_values$mean_wt_loss+3,y = 20, label = paste("Mean =",as.character(round(male_reg_diet_values$mean_wt_loss,1)))), data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(x=-11, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(label=paste("SD =", as.character(round(male_reg_diet_values$sd_wt_loss,1))), x=-11, y=19), size=2.5)+
  
  #Plotting for males on no chocolate diet plan
  geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(xintercept=male_no_choc_diet_values$mean_wt_loss), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = male_no_choc_diet_values$mean_wt_loss+3,y = 20, label = paste("Mean =",as.character(round(male_no_choc_diet_values$mean_wt_loss,1)))), data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(x=-11, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(label=paste("SD =", as.character(round(male_no_choc_diet_values$sd_wt_loss,1))), x=-11, y=19), size=2.5) +
  
  #Plotting for females on the regular diet plan
  geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(xintercept=female_reg_diet_values$mean_wt_loss), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = female_reg_diet_values$mean_wt_loss+3,y = 20, label = paste("Mean =",as.character(round(female_reg_diet_values$mean_wt_loss,1)))), data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(x=-11, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(label=paste("SD =", as.character(round(female_reg_diet_values$sd_wt_loss,1))), x=-11, y=19), size=2.5)+
  
  #Plotting for females on no chocolate diet plan
  geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(xintercept=female_no_choc_diet_values$mean_wt_loss), colour="orange", size = 1)+ 
  geom_text(mapping = aes(x = female_no_choc_diet_values$mean_wt_loss+3,y = 20, label = paste("Mean =",as.character(round(female_no_choc_diet_values$mean_wt_loss,1)))), data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), size = 2.5) +
  
  geom_point(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(x=-11, y=20), colour="black") + 
  geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(label=paste("SD =", as.character(round(female_no_choc_diet_values$sd_wt_loss,1))), x=-11, y=19), size=2.5) 
```

  #Question 3
#Visualization of Post Diet BMI
#R code associated for the third request in the assignment
#Visualizing the overall distribution of post diet BMIs
( ques3_plot1 <- ggplot(diet_data_processed, aes(x=ending_bmi, y=..density..)) + geom_histogram(binwidth=1, alpha = 0.5) +
    geom_density()+
    labs(x="End of Trial BMIs", y="Density", title = "Visualization Of Overall Post Diet BMIs")+
    geom_vline(aes(xintercept=mean(ending_bmi)), colour="orange", size = 1)+ 
    geom_text(aes(x = mean(ending_bmi)+1, y = 0.19, label = "Mean"), size = 2.5, color = "black", show.legend = FALSE))

#Visualizing the distributions of post diet BMIs for each gender and diet category by plotting a histogram and density plot
(ques3_plot2 <- ggplot(diet_data_processed, aes(x=ending_bmi, y=..density.., color = sex)) + geom_histogram(binwidth=1) +
    facet_grid(sex~diet_cond, labeller = as_labeller(Labels)) + 
    geom_density()+
    labs(x="End of Trial BMIs", y="Density", title = "Visualization Of Post Diet BMIs By Gender And Type of Diet")+
    
    #plotting of mean post diet BMIs for each gender and diet category
    geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), aes(xintercept=male_reg_diet_values$mean_end), colour="orange", size = 1)+ 
    geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='regular'), mapping = aes(x = male_reg_diet_values$mean_end+3, y = 0.19, label = "Mean"), size = 2.5, color = "black", show.legend = FALSE) +
    
    geom_vline(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), aes(xintercept=male_no_choc_diet_values$mean_end), colour="orange", size = 1)+ 
    geom_text(data=filter(diet_data_processed, sex=="M", diet_cond=='no_chocolate'), mapping = aes(x = male_no_choc_diet_values$mean_end+3, y = 0.19, label = "Mean"), size = 2.5, color = "black", show.legend = FALSE) + 
    
    geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), aes(xintercept=female_reg_diet_values$mean_end), colour="orange", size = 1)+ 
    geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='regular'), mapping = aes(x = female_reg_diet_values$mean_end+3, y = 0.19, label = "Mean"), size = 2.5, color = "black", show.legend = FALSE) +
    
    geom_vline(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), aes(xintercept=female_no_choc_diet_values$mean_end), colour="orange", size = 1)+ 
    geom_text(data=filter(diet_data_processed, sex=="F", diet_cond=='no_chocolate'), mapping = aes(x = female_no_choc_diet_values$mean_end+3, y = 0.19, label = "Mean"), size = 2.5, color = "black", show.legend = FALSE))
  
  #Question 4 
#T-test Comparing Diet Plans
#Reviewing diet data by weight loss and diet plan
( diet_data_by_weightloss <- diet_data_processed %>% group_by(diet_cond) %>% 
    summarise(mean = mean(total_weight_loss), sd = sd(total_weight_loss, na.rm = TRUE), Count = n()) )

#Since there is a difference in the mean of weight loss for each type of diet plan, plotting a histogram to have a look at how different their distributions are 
( ques4_plot1 <- ggplot(diet_data_processed, aes(total_weight_loss,..density.., fill = diet_cond)) + geom_histogram(binwidth = 1, alpha = 0.5, position = "identity")+ labs(x = "Weight Loss", y = "Density", title = "Weight Loss By Diet Plan") )

#Distributions of weight loss for each diet plan is different, hence performing t-test for further analysis
t.test(total_weight_loss ~ diet_cond, data = diet_data_processed)