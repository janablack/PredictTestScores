#Test Score Predictions - R script
#Janalin Black 

#############################################################################
# Install libraries if needed for this project

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(AICcmodavg)) install.packages("AICcmodavg")
if(!require(FactoMineR)) install.packages("FactoMineR")
if(!require(factoextra)) install.packages("factoextra")
if(!require(h2o)) install.packages("h2o")
if(!require(Hmisc))install.packages("Hmisc")
if(!require(Metrics))install.packages("Metrics")
if(!require(ranger))install.packages("ranger")
if(!require(RColorBrewer))install.packages("RColorBrewer")
if(!require(kableExtra))install.packages("kableExtra", dependencies = TRUE)
if(!require(ggridges))install.packages("ggridges")
if(!require(randomForest))install.packages("randomForest")
if(!require(kernlab))install.packages("kernlab")
#for pdf document
#tinytex::install_tinytex()
#install.packages('tinytex') 

library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(ggrepel)
library(ggthemes)
library('scales')
library(readxl)
library(kableExtra)
library(knitr)
library(AICcmodavg)
library(broom)
library("FactoMineR")
library("factoextra")
library(ggridges)
library(h2o) 
library(Hmisc)
library(Metrics)
library(RColorBrewer)
library(randomForest)
library(ranger)
library(reshape2)
library(ggridges)

#############################################################################
# The dataset used for this project comes from Kaggle.com
# Predict Test Scores of students - Predicting the posttest scores of students from 11 features
# "https://www.kaggle.com/kwadwoofosu/predict-test-scores-of-students"
# The datset is loaded into "https://github.com/janablack/PredictTestScores"
#############################################################################

#load dataset
dl <- tempfile()
download.file("https://raw.githubusercontent.com/janablack/PredictTestScores/main/test_scores%20(1).csv", dl)
testscore <- read.csv(dl, stringsAsFactors=TRUE )
file.remove(dl)

#############################################################################

#Wrangling and Exploration

############################################################################

#Initial preview of original dataset (testscore) shows a dataframe with
#11 columns and 2133 rows
class(testscore)
dim(testscore)
ncol(testscore)
nrow(testscore)
describe(testscore)
head(testscore)
str(testscore)

#there are no missing values
anyNA(testscore)

#Check variable classifications prior to dividing testscore dataset
sapply(testscore, class) 

#Change factor to character for student_id
testscore$student_id <- as.character(testscore$student_id)    

#Create validation set from testscore dataset for final prediction
set.seed(2021,sample.kind="Rounding")
test_index <- createDataPartition(y = testscore$posttest, times = 1, p = 0.2, list = FALSE)
scores <- testscore[-test_index,]
temp <- testscore[test_index,]

# Make sure 'school' in validation set is also in scores set
validation <- temp %>% 
  semi_join(scores, by = "school")

# Add rows removed from validation set back into scores set
removed <- anti_join(temp, validation)
scores <- rbind(scores, removed)

#Partition scores_test from scores dataset
set.seed(2021,sample.kind="Rounding")
test_index_test <- createDataPartition(y = scores$posttest, times = 1, p = 0.2, list = FALSE)
scores_train <- scores[-test_index_test,]
temp_1 <- scores[test_index_test,]

# Make sure 'school' in scores_test is also in scores_train
scores_test <- temp_1 %>% 
  semi_join(scores_train, by = "school")

# Add rows removed from scores_test back into scores_train set
removed_1 <- anti_join(temp_1, scores_test)
scores_train <- rbind(scores_train, removed_1)

#delete unnecessary objects
rm(dl, test_index, temp, removed, test_index_test, temp_1, removed_1)

#Rename "scores_train" to "scores" for simplicity
scores <- scores_train

# Unique values for columns: school=23, school_setting=3, school_type=2,
# classroom=97, teaching_method=2, gender=2, lunch=2
str(scores$school)
unique(scores$school_setting)
unique(scores$school_type)
str(scores$classroom)
unique(scores$teaching_method)
unique(scores$gender)
unique(scores$lunch)

#This is to make "scores" dataset look pretty
scores_pretty <- head(scores) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "left",
                full_width = FALSE,
                font_size = 11)
scores_pretty

#############################################################################

#Exploration of the Data

#############################################################################

######Analysis of posttest

#mu and median of posttest
post_mu <- mean(scores$posttest)
post_med <- median(scores$posttest)
mu <- mean(scores$posttest)

#range of posttest scores
min_post <- min(range(scores$posttest))
max_post <- max(range(scores$posttest))

#This plot appears to show normal distribution for posttest scores
post_histo <- scores %>% 
  ggplot(aes(posttest)) +
  geom_histogram(binwidth = 5, color = "black",
                 fill = "darkorange1") +
  theme(plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.position = "none") +
    labs(
    x = "Score",
    y = "Count",
    title = paste(
      "Post Test Scores"
    ) 
  )
post_histo

#QQ-plot shows a normal distribution for posttest
#From this, assume normal distribution for posttest
post_qq <- scores %>%
  ggplot(aes(sample = scale(posttest))) +
  geom_qq() +
  geom_abline() +
  theme(plot.title = element_text(color = "#000066", hjust = .5),
        legend.position = "none") +
  labs(x = "",
       y = "",
    title = paste(
      "QQ Plot for Post Test Scores"
    ) )
post_qq

#A second way to show above plot
qplot(sample = scale(scores$posttest)) + geom_abline()

######Analysis of pretest

# A quick summary shows potential differences between pretest/posttest
summary(scores)

#Mean and standard deviation of pretest and posttest
pp_mu_sd <- scores %>%
  summarise(mu_pre = mean(pretest), sd_pre = sd(pretest),
            mu_post = mean(posttest), sd_post = sd(posttest))
pp_mu_sd

#A quick plot shows a linear relationship between pretest and posttest scores
qplot(scores$posttest,scores$pretest)

#lm model and summary
#Multiple r-squared indicates a large portion of variance in posttest
#scores are explained by pretest scores. P-value is significant
pp_lm <- lm(posttest ~ pretest, data=scores)
summary(pp_lm)

#get just the r squared value
pp_lm_rsq <- summary(pp_lm)$r.squared
pp_lm_rsq_percent <- pp_lm_rsq*100

#Plot of pre/post test with lm regression line further indicates linear regression
pp_regression <- scores %>% 
  ggplot(aes(scale(pretest), scale(posttest))) + 
  geom_point(alpha = 0.5, color = 'darkorange2') +
  geom_smooth(method = "lm")+
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.position = "none") +
  labs(
    x = "Scaled Post Test",
    y = "Scaled Pretest",
    title = paste(
      "Linear Relationship of Scores"
    ) 
  )
pp_regression

#QQ-plot shows a normal distribution for pretest
pre_qq <- scores %>%
  ggplot(aes(sample = scale(pretest))) +
  geom_qq() +
  geom_abline()
pre_qq

#combine columns for pre/posttest scores for ease in creating plots
pre_post <- melt(scores,id.vars='student_id',
                 measure.vars=c('pretest','posttest'))
levels(pre_post$variable) <- c("Pre-test","Post-test")

#A boxplot reveals overlap between scores with posttest higher. Mean scores for
#posttest are also higher. Variablility for both pre and post test is about the same.
pp_box <- pre_post %>% 
  ggplot() +
  geom_boxplot(aes(x=variable, y=value,fill=variable)) +
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.position = "none") +
  scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "",
    y = "Score",
    title = paste(
      "Pre and Post Test Scores"
    ) 
  )
pp_box

#Pre/post density plot indicate similar densities, overlap of pre and post test scores and
#higher scores for posttest.
pp_density <- pre_post %>% 
  ggplot() +
  geom_density(aes(x=value,fill=variable,alpha = .7)) +
  guides(alpha = FALSE)+
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "Score",
    y = "Density",
    title = paste(
      "Pre and Post Test Densities"
    ))
pp_density

#Correlation between pre and post tests indicates a positive correlation between 
#pre and post test scores. P-values for pearson correlation is below alpha level 
#at .05 and reject the null hypothesis. There is significant (positive) correlation between 
#pre and post test scores with .95 correlation.
pre_cor <- cor.test(scores$pretest, scores$posttest,
         method = "pearson",
         exact = FALSE,
         conf.level = 0.95)
#correlation percentage calculated
pre_cor$estimate*100

#calculated slope for pre/post test is .98
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)
pp_slope <- scores %>%
  summarise(slope = get_slope(pretest, posttest))
pp_slope

#t-test-null hypothesis is pre-post test mean scores aren't statistically significant from each other.
#P-value is less than alpha=0.05. Reject the null that pre/post tests mean scores aren't statistically significant.
#Confidence internals don't contain "0". The true difference between mean scores appears to be different and we reject the null.

t.test(scores$pretest, scores$posttest, paired = TRUE, alternative = 'two.sided')

#Top 50 students on pretest and posttest indicate 39 common students
#There appears to be correlation between students' pre and post test top scores.
top_pre <- scores %>% 
  select(student_id, pretest) %>% arrange(desc(pretest)) %>%
  top_n(50, pretest)

top_post <- scores %>% 
  select(student_id, posttest) %>% arrange(desc(posttest)) %>%
  top_n(50, posttest)

#common students in top 50
top_common <- top_pre %>% inner_join(top_post, by = 'student_id')
count(top_common)

#a second way to show this
merge(top_pre,top_post, by = 'student_id')

#Bottom 50 students on pretest and posttest indicate 30 common students.
bottom_pre <- scores %>% 
  select(student_id, pretest) %>% arrange(pretest) %>%
  head(50)

bottom_post <- scores %>% 
  select(student_id, posttest) %>% arrange(posttest) %>%
  head(50)

#common students in bottom 50
bottom_common <- inner_join(bottom_pre,bottom_post)
count(bottom_common)

######Analysis of school

#There are 23 schools represented
n_schools <- nrow(as.data.frame(table(scores$school)))

#Pretest and posttest scores show some schools do better than others
#This is a sample from scores with a combined plot of pre/post scores vs school
set.seed(1, sample.kind="Rounding")
scores_sample_500 <- scores %>% sample_n(500)
pp_schools <- scores_sample_500 %>%
  mutate(school = reorder(school, pretest, FUN = mean)) %>%
  ggplot() +
  geom_point(aes(x=jitter(pretest), y=school), color='darkseagreen3') +
  geom_point(aes(x=jitter(posttest), y=school), color='darkorange1') +
  theme(axis.text.x= element_text(),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Mean Score",
    y = "School",
    title = paste(
      "Pre and Post Test Scores per School"
    ) 
  )
pp_schools

#Mean posttest scores of schools
mu_school <- scores %>% 
  select(school, posttest) %>%
  group_by(school) %>%
  summarise(school_avg = mean(posttest))
range(mu_school$school_avg)

#min and max school averages on posttest
min_school <- round(min(mu_school$school_avg))
max_school <- round(max(mu_school$school_avg))

#Of top 5 schools, 4 are the same for pretest and posttest mean scores 
top_pre_school <- scores %>% 
  select(school, pretest) %>%
  group_by(school) %>%
  summarise(school_avg = mean(pretest)) %>%
  arrange(desc(school_avg)) %>%
  top_n(5, school_avg)

top_post_school <- scores %>% 
  select(school, posttest) %>%
  group_by(school) %>%
  summarise(school_avg = mean(posttest)) %>%
  arrange(desc(school_avg)) %>%
  top_n(5, school_avg)

top_common_school <- top_pre_school %>% inner_join(top_post_school, by = 'school')
top_common_school
count(top_common_school)

#Bonferroni's correction is used on school to see
#if there is statistically significant differences between
#the means of all groups.
#Check for variance using Bartlett's test on school
#groups show that p-value is significant.
#reject null hypothesis that each group has the same variance
#Comparisons show significant p-value. 
#There is significant difference between groups.
bartletts <- bartlett.test(posttest ~ school, data = scores)
bartletts_pvalue <- bartletts$p.value

#Since variance is assumed to be unequal, perform Welch's ANOVA
#P-value is below .05% and is significant
welchs <- oneway.test(posttest ~ school, data = scores, var.equal = FALSE)

#perform pairwise t-tests with Bonferroni's correction to find which groups variance is significant.
#Several groups are significant when compared to each other but others are not.
bonferroni <- pairwise.t.test(scores$posttest, scores$school,
                              p.adjust.method="bonferroni")

######Analysis of classroom

#Classroom has 97 different occurrences.
n_class <- as.data.frame(table(scores$classroom))
class_numbers <- nrow(n_class)

#This table shows mean and median scores per classroom
mu_class <- scores %>%
  group_by(classroom) %>%
  summarise(avg=mean(posttest), median = median(posttest))
class_max <- max(mu_class$avg)
class_min <- min(mu_class$avg)

#This plot shows that the type of classroom impacts pretest and posttest scores. 
#Colors identify classroom
class_plot <- scores %>%
  mutate(classroom = reorder(classroom, posttest, FUN = mean)) %>%
  ggplot(aes(posttest,pretest, col = classroom)) +
  geom_point(size = .1) +
  theme(axis.text.x= element_text(),
        plot.title = element_text(color = "#000066", hjust = .5),
        plot.subtitle = element_text(color = "#CC6633", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.position = "none") +
  labs(
    x = "Posttest",
    y = "Pretest",
    title = paste(
      "Scores by Classroom"),
    subtitle=paste(
      "(colored by classroom)"
    ) 
  )
class_plot

#Create a diverging plot with normalized classroom posttest scores
# Compute normalized posttest and scale above/below
round_posttest <- scores %>%
  group_by(classroom) %>%
  summarise(avg=mean(posttest),
            normalized_posttest = avg-mu,
            posttest_type = ifelse(normalized_posttest < 0, "below", "above"))

#Plot of diverging classroom scores
class_diverging <- round_posttest %>%
  ggplot(aes(x=reorder(classroom,normalized_posttest), y= normalized_posttest)) +
  geom_bar(stat='identity', aes(fill= posttest_type), width=.5)  +
  scale_fill_manual(name="Mean Score", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="darkorange1", "below"="darkseagreen4")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x= element_text(),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(title= paste("Classroom Posttest Scores"),
       x = "Classroom",
       y = "Normalized Score") +
  coord_flip()
class_diverging 

######Analysis of school_setting

#This table shows 3 types of school settings with numbers of students and percentage.
ss_table <- scores %>%
  count(school_setting) %>%
  mutate(percentage =100*(n/sum(n)))
ss_table

#percentages by school setting
rural_total <- round(ss_table$percentage[1])
suburban_total <- round(ss_table$percentage[2])
urban_total <- round(ss_table$percentage[3])

#This plot shows differences in posttest and pretest scores versus school_setting. 
#Many suburban schools score higher than urban schools 
#with only a few suburban schools schools with low scores
ss_pp_plot <- scores %>% ggplot() +
  geom_point(aes(posttest,pretest,col=school_setting))
ss_pp_plot

#This table shows similar results for mean and median posttest scores vs. school_setting 
mu_ss <- scores %>%
  group_by(school_setting) %>%
  summarise(avg=mean(posttest), median = median(posttest))
mu_ss

#Density plot of school_setting with mean posttest scores
#shows suburban schools score higher. Urban schools have lower scores and
#the lowest mean of the three groups
density_ss <- scores %>% 
  ggplot(aes(x = posttest, y = school_setting, fill = school_setting)) + 
  geom_density_ridges(scale = .8,
                      alpha = .5,
                      show.legend = FALSE)+
  geom_vline(data = mu_ss, aes(xintercept = avg, 
                                       color = school_setting), size=1.5)+
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "right") +
  #scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "Posttest Score",
    y = "",
    title = paste(
      "Density Plots with Mean Scores"
    )) 
density_ss

#Bonferroni's correction is used on school_setting to see
#if there is statistically significant differences between
#the means of rural, suburban and urban groups.

#Check for variance using Bartlett's test on school_setting.
#Groups show that p-value is significant.
#Reject null hypothesis that each group has the same variance.
#All three comparisons show significant p-values. 
#There is significant difference between the three groups.
bartlett_ss <- bartlett.test(posttest ~ school_setting, data = scores)
bartlett_pvalue_ss <- bartlett_ss$p.value

#Since variance isn't equal, performed Welch's ANOVA
#P-value is significant.
welch_ss <- oneway.test(posttest ~ school_setting, data = scores, var.equal = FALSE)
welch_pvalue_ss <- welch_ss$p.value

#perform pairwise t-tests with Bonferroni's correction to find which groups variance is significant.
#All three groups are significant from each other.
bonferroni_ss <- pairwise.t.test(scores$posttest, scores$school_setting, p.adjust.method="bonferroni")
bon_pvalue <- bonferroni_ss$p.value[2,1]

######Analysis of school_type

#This table shows that there are nearly three times as many public as non-public schools
school_type_numbers <- scores %>%
  count(school_type) %>%
  mutate(proportion = 100*(n/sum(n)))

#percentages of each
n_non_public <- round(school_type_numbers$proportion[1])
n_public <- round(school_type_numbers$proportion[2])

#Proportion of school_type shown visually
school_type_numbers %>%
  ggplot(aes(school_type,proportion)) +
  geom_bar(stat = "identity")
  
#This plot shows higher scores among non-public than public schools
#for pre and post tests
st_plot <- scores %>%
  ggplot() +
  geom_point(aes(posttest,pretest,col=school_type))+
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "right") +
  #scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "Post-test",
    y = "Pre-test",
    title = paste(
      "Scores by School Type"
    ))
st_plot

#This histogram shows post test scores with school type.
#Non-public schools seem to outperform public schools on post test scores.
st_histo <- scores %>% ggplot(aes(posttest)) +
  geom_histogram(aes(fill=school_type),
                 binwidth = 5,
                 col="black") +
  theme(plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "Post Test Score",
    y = "Count",
    title = paste(
      "School Type and Post Test Scores"
    ) 
  )
st_histo

#A boxplot of school_type vs posttest
scores %>% qplot(school_type, posttest, data=., geom = "boxplot")

#####Analysis of gender

#This table shows that gender is nearly equally split
n_gender <- scores %>% 
  group_by(gender) %>% 
  dplyr::summarize(percent = 100 * n() / nrow(scores),
                   count = n())

#percent gender for female and male
n_female <- n_gender[1,2]
n_male <- n_gender[2,2]

#This boxplot indicates that posttest scores are nearly the same for both genders
gender_box <- scores %>% 
  ggplot() +
  geom_boxplot(aes(x=gender, y=posttest,fill=gender)) +
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.position = "none") +
  scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "",
    y = "Score",
    title = paste(
      "Gender and Post-test Scores"
    ) 
  )
gender_box

#Mean and sd for genders also show that there is little to no difference
#between gender and posttest scores
gender_mu_sd <- scores %>%
  group_by(gender) %>%
  summarise(mu_gender = mean(posttest), sd_pre = sd(posttest))
gender_mu_sd

#Two things must be true for omitted variable bias:
#1. At least one of the included regressors must be correlated with the omitted variable.
#2. The omitted variable must be a determinant of the dependent variable.
#Since gender doesn't appear to have an impact on posttest scores, this variable 
#may be omitted in model creation. 

#####Analysis of number of students in class

#The range of number of students in a classroom is between 14 and 31.
range(scores$n_student)

#min and max numbers of students in class
n_room_min <- min(scores$n_student)
n_room_max <- max(scores$n_student)

#mean number of students in class
room_mu <- round(mean(scores$n_student))

#This shows a count of number of students per classroom
aggregate(data.frame(class_count = scores$n_student),
          list(student_count = scores$n_student),
          length)

#Correlation shows -.508, a negative relationship between
#number of students in class and posttest scores.
room_cor <- cor(scores$n_student,scores$posttest)


#Histogram shows class totals vs number of students in class. 
#Some groups have >25 classes.
class_n_histo <- scores %>% 
  ggplot(aes(n_student)) +
  geom_histogram(binwidth = 1, color = "black",
                 fill = "darkorange1") +
  theme(plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.position = "none") +
  labs(
    x = "Number of Students",
    y = "Number of Classes",
    title = paste(
      "Number of Students in a Class"
    ) 
  )
class_n_histo

#The number of students in a class seems to effect posttest scores.
#As the number of students in class increases, scores decrease.
class_n_post_plot <- scores %>%
  group_by(n_student) %>%
  summarise(n = n(),
            avg = mean(posttest),
            se = sd(posttest)/sqrt(length(posttest))) %>%
  ggplot(aes(n_student, avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point(color = "#CC6633") +
  geom_errorbar(color = "#CC6633")+
  theme(axis.text.x= element_text(hjust = 1),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066")) +
  labs(
    x = "Students Per Class",
    y = "Average Post-test Score",
    title = paste(
      "Number of Students in Class and Scores"
    ) 
  )
class_n_post_plot

#####Analysis of lunch qualification

# This table shows  number of students who qualify for free or reduced lunch
table(scores$lunch)

#Percent who qualify for lunch
lunch_percent <- scores %>% 
  group_by(lunch) %>% 
  dplyr::summarize( percent = 100 * n() / nrow(scores))

#Percent for both groups
not_qualify_lunch <- lunch_percent$percent[1]
qualify_lunch <- lunch_percent$percent[2]

#A boxplot of lunch vs posttest scores show a difference
#between scores depending on lunch qualification
scores %>% qplot(lunch, posttest, data=., geom = "boxplot")

#Ellipse shows overlap between lunch variable for sample of scores dataset with
#"does not qualify" scoring higher.
set.seed(1, sample.kind="Rounding")
scores_sample <- scores %>% sample_n(200)

lunch_sample <- scores_sample %>%ggplot(aes(posttest, pretest, fill = lunch, color=lunch)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm", lwd = 1)+
  theme(plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(
    x = "Post-test",
    y = "Pretest",
    title = paste(
      "Lunch Benefits and Scores"
    ) 
  ) 
lunch_sample

######Analysis of teaching_method

#teaching_method totals
teaching_method_numbers <- scores %>%
  count(teaching_method) %>%
  mutate(percent = 100*(n/sum(n)))

#percents for teaching method totals
tm_experiment <- teaching_method_numbers$percent[1]
tm_standard <- teaching_method_numbers$percent[2]

#Mean and median scores for teaching method
mu_tm <- scores %>%
  group_by(teaching_method) %>%
  summarise(avg=mean(posttest), median = median(posttest))
mu_tm

#percents for posttest mean of teaching method
experimental_avg <- mu_tm$avg[1]
standard_avg <- mu_tm$avg[2]

#Proportion of teaching_method shown visually
teaching_method_numbers %>%
  ggplot(aes(teaching_method,percent)) +
  geom_bar(stat = "identity")

#This plot shows higher scores for experimental group 
tm_plot <- scores %>%
  ggplot() +
  geom_point(aes(posttest,pretest,col=teaching_method))+
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "right") +
  #scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "Post-test",
    y = "Pre-test",
    title = paste(
      ""
    ))
tm_plot

#This density plot shows post test scores vs teaching method with added mean scores.
#This shows higher scores and means for Experimental teaching method over Standard teaching method.
density_tm <- scores %>% 
  ggplot(aes(x = posttest,fill=teaching_method)) + 
  geom_density(alpha = .5,
                      show.legend = FALSE)+
  geom_vline(data = mu_tm, aes(xintercept = avg, 
                               color = teaching_method), size=1.5)+
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "right") +
    labs(
    x = "Score",
    y = "",
    title = paste(
      "Density Plot with Mean Scores"
    )) 
density_tm


#A boxplot of teaching method vs posttest
scores %>% qplot(teaching_method, posttest, data=., geom = "boxplot")

#############################################################################

#Creating a Model

############################################################################

###### MODEL 1

# Because there are multiple promising independent variables to consider, variables 
#that don't seem to have an impact on the regression model may be omitted 
# ANOVA is performed to include categorical variables in determining statistical differences between 
#means of multiple groups. 

#This model includes all possible variables to consider. 
#ANOVA results indicate that 'gender' isn't statistically significant and "school"
#isn't considered.

options(digits = 3)
choice_1_lm <- lm(posttest ~ pretest + school_setting + school_type +
              teaching_method + n_student + gender +
              lunch + classroom + school, data = scores)

choice_1_summary <- summary(choice_1_lm)
choice1_adj_rsq <- 100*(choice_1_summary$adj.r.squared)

#generating p-scores for choice_1
choice1_anova <- anova(choice_1_lm)
gender_pscore <- choice1_anova$'Pr(>F)'[6]

#first model choice using aov for AIC prediction
choice_1 <- aov(posttest ~ pretest + school_setting + school_type +
                 teaching_method + n_student + gender +
                 lunch + classroom + school, data = scores)
summary(choice_1)

#'school' may not be independent from another variable.
#The Chi-squared test is used to measure the relationship between 'school' and 'classroom'.
#Because the p-value is small, school and classroom are not independent.
sc_chisq <- chisq.test(scores$school,scores$classroom)
sc_p.value <- sc_chisq$p.value

#school_type and school_setting are not independent 
chisq.test(scores$school_type,scores$school_setting)

#A second way to determine interactions between variables is with ANOVA
#An asterisk is used instead of a plus-sign in the model to find interactions:
#'school_setting*school_type' variable still
#has a high sum-of-squares value and a low p-value, which means 
#there is variation that can be explained by the interaction between 
#school_setting and school_type.

interaction1 <- aov(posttest ~ school_setting*school_type, data = scores)
summary(interaction1)

#calls p-value for school_setting/school_type interaction
ssss_interaction <- summary(interaction1)[[1]][["Pr(>F)"]][[3]]

#There is an interaction between school_setting and lunch
interaction2 <- aov(posttest ~ school_setting*lunch, data = scores)

#calls p-value for school_setting/lunch interaction
ssl_interaction <- summary(interaction2)[[1]][["Pr(>F)"]][[3]]

#There is an interaction between school_type and lunch
interaction3 <- aov(posttest ~ school_type*lunch, data = scores)
summary(interaction3)

#Are classroom and gender related? P-scores indicate they are independent
chisq.test(scores$classroom,scores$gender)

#classroom and n_student are related
chisq.test(scores$classroom,scores$n_student)

#From here, several more linear models are created using ANOVA and Chi-squared test results:

#The second model excludes school and gender as predictors
choice_2 <- aov(posttest ~ pretest + school_setting + school_type + classroom +
                 teaching_method + n_student + lunch, data = scores)
summary(choice_2)

#The third model removes classroom and gender
choice_3 <- aov(posttest ~ pretest + school + school_setting + school_type +
                 teaching_method + n_student + lunch, data = scores)
summary(choice_3)

#The fourth model excludes lunch, school, and gender
choice_4 <- aov(posttest ~ pretest + school_setting + school_type + classroom +
                 teaching_method + n_student, data = scores)
summary(choice_4)

#The fifth model excludes school_setting, school, and lunch.
choice_5 <- aov(posttest ~ pretest + school_type + classroom +
                 teaching_method + n_student + gender, data = scores)
summary(choice_5)

#Find the best-fit model
#The Akaike information criterion (AIC) is used to test model fit.
#AIC determines value of each model by balancing the variation
#explained against the number of parameters.
#Lowest AIC value is the winner -a lower number indicates more information explained

model.group <- list(choice_1, choice_2, choice_3, choice_4, choice_5)
model.names <- c("choice_1", "choice_2", "choice_3", "choice_4", "choice_5")

#choice_1 has the lowest AICc and is the best fit with 84% of AIC weight.
#84% of total variation in posttest variable can be explained by this model.
#choice_2 can explain additional 16% of AIC weight, but is more than 2 delta-AIC
#worse than choice_1 and shouldn't be included in results.
aic <- aictab(model.group, model.names)

#range of aic numbers
range_aic_min <- min(range(aic$AICc))
range_aic_max <- max(range(aic$AICc))

#call highest weight of AIC
aic_highest <- 100*(aic$Cum.Wt[1])

#choice_1 is trained.
tidy(choice_1, conf.int = TRUE)
train_choice_1 <- train(posttest ~ pretest + school_setting + school_type +
                          teaching_method + n_student + gender +
                          lunch + classroom + school,
                       method = "lm", data = scores)

#Predictions made using scores_test
y_hat_choice_1 <- predict(train_choice_1, scores_test)

#RMSE results using scores_test are 2.740412
model_1_best <- rmse(scores_test$posttest, y_hat_choice_1)
model_1_best

#Table for model 1 test set RMSE
model_1_test <- data_frame("Model 1" = "Test Set",
                           "RMSE Score" = model_1_best )
#Make the table prettier
model_1_test_table <- model_1_test %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)

model_1_test_table

#RMSE results using validation dataset are 2.884267
y_hat_model_1_val <- predict(train_choice_1, validation)
model_1_validation <- rmse(validation$posttest, y_hat_model_1_val)
model_1_validation

#Table for model 1 RMSE results
model_1_table <- data_frame("Model 1" = c("Test Set", "Final Validation Set"),
                               "RMSE Score" = c(model_1_best,model_1_validation))
model_1_table

#Make the table prettier
model_1_results <- model_1_table %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)

model_1_results

###### MODEL 2

#Random Forest model with just pretest as a predictor
set.seed(200, sample.kind="Rounding")
fit <- randomForest(posttest~pretest,
                    data = scores)

#Mean of squared residuals: 19.14314
mean(fit$mse)

#% Var explained: 90.22
mean(fit$rsq)

# RMSE for the model
sqrt(fit$mse[length(fit$mse)]) #4.375287

# predict model on test data
predValues <- predict(fit,scores_test)

# RMSE on test data calculated directly 
sqrt(mean((scores_test$posttest -predValues)^2)) #4.459005

#or use RMSE function in Metrics library
fit_1_rmse <- rmse(scores_test$posttest, predValues)#4.459005

#Random Forest model with predictors from lm best model("classroom" is excluded due
#to too many categories for randomForest to calculate)
set.seed(200, sample.kind="Rounding")
fit2 <- randomForest(posttest ~ pretest + school + 
                       gender + lunch, data = scores)
fit2$importance

#Mean of squared residuals: 30.33015
#% Var explained: 84.51
# take square root to calculate RMSE for the model
sqrt(fit2$mse[length(fit$mse)]) #5.50728

# Prediction on test data
predValues <- predict(fit2,scores_test)

# Calculate RMSE 
sqrt(mean((scores_test$posttest -predValues)^2)) #5.45035

#or use RMSE function:
fit_2_rmse <- rmse(scores_test$posttest, predValues)#5.45035

#Random Forest model without pretest as predictor (also w/o "classroom")
set.seed(200, sample.kind="Rounding")
fit3 <- randomForest(posttest~
                       school_setting + school_type +
                       lunch + school +
                       teaching_method + n_student + gender,
                     data = scores)

#Mean of squared residuals: 20.98
#% Var explained: 89.28
fit3

# Calculate RMSE for the model
sqrt(fit3$mse[length(fit$mse)]) # 4.58079

# Prediction on test data
predValues <- predict(fit3,scores_test)

# RMSE on fit 3 test set 
fit_3_rmse <- rmse(scores_test$posttest, predValues)#4.36

##Random Forest model with all predictors except "classroom"
set.seed(200, sample.kind="Rounding")
fit4 <- randomForest(posttest~ pretest +
                       school_setting + school_type +
                       lunch + school + gender +
                       teaching_method + n_student,
                     data = scores)
#Mean of squared residuals: 10.52424
#% Var explained: 94.62
fit4

# Calculate RMSE for the model
fit4_train_rmse <- sqrt(fit4$mse[length(fit4$mse)]) # 3.24411

# Prediction on test data
predValues <- predict(fit4,scores_test)

#Calculate RMSE fit 4
model_2_best <-rmse(scores_test$posttest, predValues)# 3.02627

#Find best mtry and node size for fit4 - the best model for Model 2

# create grid
params_grid <- expand.grid(
  mtry = c(1,2,4,6,8),
  min.node.size = seq(8,24,2), 
  replace = c(TRUE, FALSE),                               
  rmse = NA                                               
)

# execute grid parameters
set.seed(200, sample.kind="Rounding")
for(i in seq_len(nrow(params_grid))) {
  fit <- ranger(
    formula = posttest ~ pretest +
      school_setting + school_type +
      lunch + school + gender +
      teaching_method + n_student, 
    data            = scores, 
    mtry            = params_grid$mtry[i],
    min.node.size   = params_grid$min.node.size[i],
    respect.unordered.factors = 'order',
  )
  params_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# top 5 results from grid shows best mtry at 4 and node size at 20
top_5_params <- params_grid %>%
  arrange(rmse) %>%
  mutate(percent_gained = (fit4_train_rmse - rmse) / fit4_train_rmse * 100) %>%
  head(5)
top_5_params

#Apply new params to fit4 model
set.seed(200, sample.kind="Rounding")
fit4_best <- randomForest(posttest~ pretest +
                                    school_setting + school_type +
                                    lunch + school + gender +
                                    teaching_method + n_student,
                                  data = scores,
                                  mtry = 4,
                                  importance = TRUE,
                                  nodesize = 20)

# Calculate RMSE for the fit4_best
fit4_best_rmse <- sqrt(fit4_best$mse[length(fit4_best$mse)]) #3.11
fit4_best_rmse

# Prediction on test data
predValues_best <- predict(fit4_best,scores_test)

#Calculate RMSE on fit 4 best
model_2_best_update <-rmse(scores_test$posttest, predValues_best)# 3.02627

#Table for random forest RMSE results
rf_results_table <- data_frame("Random Forest Model(classroom excluded)" =
                                 c("Fit 1 - pretest as only predictor",
                                   "Fit 2 - best-fit predictors from Model 1",
                                   "Fit 3 - doesn't include pretest",
                                   "Fit 4 - all predictors",
                                   "Fit 4 adj. - adding best mtry/node size"),
                               "Test Set RMSE" =
                                 c(fit_1_rmse, fit_2_rmse,fit_3_rmse,
                                   model_2_best, model_2_best_update))
#Make the table prettier
rf_results <- rf_results_table %>% 
  kable() %>%
  kable_styling(bootstrap_options = 
                  c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)%>%
  row_spec(5, bold = T, color = '#CC6633')

rf_results

# Prediction for fit 4 best on validation dataset
predValues_val_update <- predict(fit4_best,validation)

#Calculate RMSE fit 4 best on validation dataset
model_2_validation_update <- rmse(validation$posttest, predValues_val_update)
model_2_validation_update

#Accuracy improves until about 100 trees and then accuracy stabilizes.
plot(fit4_best)

#Variable Importance calculated on fit4_best, the best model 
varimp_fit4_best <- varImp(fit4_best)

#Arrange variable imp desc.
varimp <- varimp_fit4_best  %>% arrange(desc(Overall))

#Make varimp table pretty
varimp_results <- varimp %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover",
                                      "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)

varimp_results


#For  fit 4 best density plot comparison
#combine columns from two dataframes for density plot comparison
density_posttest <- data.frame(posttest = scores$posttest)
density_predicted <- as.data.frame(fit4_best$predicted)
density_posttest$observation <- 1:nrow(density_posttest) 
density_predicted$observation <- 1:nrow(density_predicted) 
density_combined <- merge(density_posttest,density_predicted, by = "observation")
density_pp <- melt(density_combined,id.vars='observation',
                   measure.vars=c('posttest','fit4_best$predicted'))
levels(density_pp$variable) <- c("Actual","Predicted")

#Density plot comparing scores posttest scores to fit4_best(best model)
rf_plot <- density_pp %>%
  ggplot() +
  geom_density(aes(x=value,fill=variable,alpha = .7)) +
  guides(alpha = FALSE)+
  theme(axis.text.x= element_text(angle = 0),
        plot.title = element_text(color = "#000066", hjust = .5),
        plot.caption = element_text(color = "#CC6633", hjust = .5),
        axis.title.x = element_text(color = "#000066"),
        axis.title.y = element_text(color = "#000066"),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_fill_brewer(palette = 'Set2') +
  labs(
    x = "Posttest Score",
    y = "Density",
    title = paste(
      "Random Forest Final Model"),
    caption = paste(
      "(Data source: scores_test)")
    )
rf_plot

###### MODEL 3

#Ensemble used to find best method for pretest portion of model
#shows that the linear model(glm) results in the lowest RMSE score on scores
models <- c("lm", "glm", "rlm", "knn", "rf", "svmLinear")

set.seed(1, sample.kind = "Rounding")
fits <- lapply(models, function(model){ 
  print(model)
  train(posttest ~ pretest, method = model, data = scores)
}) 
names(fits) <- models

#Create table with results of ensemble shows "glm" as the best model for pretest

#limit digits for table simplicity
options(digits = 3)

train_rmse_table <- data_frame(Model = c("lm", "glm", "rlm", "knn", "rf", "svmLinear"),
                               RMSE = c(fits$lm$results$RMSE,
                                        fits$glm$results$RMSE,
                                        fits$rlm$results$RMSE[1],
                                        fits$knn$results$RMSE[2],
                                        fits$rf$results$RMSE,
                                        fits$svmLinear$results$RMSE))
#Make the table prettier
ensemble_table <- train_rmse_table %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)%>%
  row_spec(2, bold = T, color = '#CC6633')

ensemble_table

#Factor analysis of mixed data (FAMD) is used to analyze datasets with
#both quantitative and qualitative predictors.

#Prepare data for FAMD
train_famd <- subset(scores, select = c(-student_id))
test_famd <- subset(scores_test, select = c(-student_id))

#FAMD analysis
res.famd <- FAMD(train_famd, graph = FALSE)

#Scree plot - the percentages of inertia explained by each FAMD dimension
fviz_screeplot(res.famd)

# Plot of variables with similar values close together
fviz_famd_var(res.famd, repel = TRUE)

# Contribution to the first dimension
cont_1 <- fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
cont_2 <- fviz_contrib(res.famd, "var", axes = 2)

#Arrange contributions side by side
two_tables <- grid.arrange(cont_1,cont_2, ncol = 2)

#Plot of quantitative variables with colors indicating contribution
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#008073", "#cea400", "#f3590c"),
              repel = TRUE)

#Finding just the average using RMSE

mu <- mean(scores$posttest) 
basic_rmse <- RMSE(scores_test$posttest, mu)
rmse_results <- data_frame(Model = "Posttest Average", RMSE = basic_rmse)

#Adding pretest as predictor using glm
pretest_glm <- train(posttest ~ pretest, method = "glm", data = scores)

#Adding glm equation results from training set into scores, scores_test, and validation datasets
sum_glm <- summary(pretest_glm)

#intercept of glm
sum_glm$coefficients[1,1] 
#slope of glm
sum_glm$coefficients[2,1] 

#Adding prediction glm from training set to train, test, and validation datasets
scores_train <- scores %>%
  mutate(predicted_glm = sum_glm$coefficients[2,1]*pretest
         + sum_glm$coefficients[1,1], bp=posttest-predicted_glm)

scores_test <- scores_test %>%
  mutate(predicted_glm = sum_glm$coefficients[2,1]*pretest
         + sum_glm$coefficients[1,1], bp=posttest-predicted_glm)

scores_validation <- validation %>%
  mutate(predicted_glm = sum_glm$coefficients[2,1]*pretest
         + sum_glm$coefficients[1,1], bp=posttest-predicted_glm)

#Pretest effect RMSE results
bp_rmse <- RMSE(scores_test$predicted_glm, scores_test$posttest)

#RMSE results table adding pretest(glm) effect
#RMSE prediction with pretest shows a significant decrease in RMSE results.
pretest_rmse <- bind_rows(rmse_results,
                          data_frame(Model="Pretest Effect Model",
                                     RMSE = bp_rmse ))
#Make the table prettier
pretest_table <- pretest_rmse %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)

pretest_table


#FAMD results for dimension 1 of contribution predictors has "classroom" first. 
#randomForest doesn't include "classroom" in the model due to too many groups. 

#This calculates the classroom effect(bc) using Least Squares approximation.
class_bc <- scores_train %>% 
  group_by(classroom) %>%
  summarise(bc = mean(posttest - mu - bp))

#RMSE results with "classroom" effect
bc_score <- scores_test %>%
  left_join(class_bc, by = 'classroom') %>%
  mutate(class_pred = mu + bp + bc) %>%
  pull(class_pred)
bc_rmse <- RMSE(bc_score, scores_test$posttest)

#RMSE results table including adding "classroom" effect
class_rmse <- bind_rows(pretest_rmse,
                        data_frame(Model="Classroom Effect",
                                   RMSE = bc_rmse))

#From FAMD, some classrooms were used as predictors and others were not. 
#Regularization used on "classroom" as a predictor to account for some classrooms
#with limited posttest scores.

#This lambda was selected based on exploration of different lambda numbers
lambda <- seq(-.5, .5, 0.025)

class_bc_r <- sapply(lambda, function(x){
  mu <- 
    mean(scores_train$posttest)
  c_bc <-
    scores_train %>% 
    group_by(classroom) %>% 
    summarise(c_bc = sum(posttest - mu - bp)/(n()+x))
predicted <-
    scores_test %>% 
    left_join(c_bc, by='classroom') %>%
    mutate(class_pred = mu + bp + c_bc)%>%
    pull(class_pred)
  
  return(RMSE(predicted, scores_test$posttest))
})

#Visualize best lambda for "classroom" indicates minimum lambda
qplot(lambda,class_bc_r)

#Optimal penalty (lambda) for classroom predictor
class_lambda <- lambda[which.min(class_bc_r)]

#Best RMSE 
bc_reg_rmse <- min(class_bc_r)

options(digits = 5)
#RMSE results table adding Regularized "classroom" effect shows improved RMSE results
reg_class_rmse <- bind_rows(class_rmse,
                            data_frame(Model="Regularized Classroom Effect",
                                       RMSE = bc_reg_rmse ))

options(digits = 5)
#Make the table prettier
class_table <- reg_class_rmse %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)
class_table

#Classroom model for adding more predictions
class_bc_add <-
  scores_train %>% 
  group_by(classroom) %>% 
  summarise(bc = sum(posttest - mu - bp)/(n()+ class_lambda))

#The next highest predictor is "teaching method" for randomForest.
#This calculates the teaching method(bm).
method_bm <- scores_train %>% 
  left_join(class_bc_add, by ='classroom')%>%
  group_by(teaching_method) %>%
  summarise(bm = mean(posttest - mu - bp - bc))

#RMSE results with "teaching method" effect
bm_score <- scores_test %>%
  left_join(class_bc_add, by ='classroom') %>%
  left_join(method_bm, by ='teaching_method')%>%
  mutate(method_pred = mu + bp + bc + bm) %>%
  pull(method_pred)
bm_rmse <- RMSE(bm_score, scores_test$posttest)

#RMSE results table including adding teaching method effect slightly reduced RMSE results.
#This isn't surprising due to possible collinearity between "classroom" and "school"
options(digits = 8)
method_rmse <- bind_rows(reg_class_rmse,
                           data_frame(Model="Teaching Method Effect",
                                      RMSE = bm_rmse))
method_rmse
#Make the table prettier
method_table <- method_rmse %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)
method_table

#From Variable Importance, the next highest predictor is "school".
#This calculates the school effect(bs) using Least Squares approximation.
school_bs <- scores_train %>% 
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  group_by(school) %>%
  summarise(bs = mean(posttest - mu - bp - bc - bm))

#RMSE results with "school" effect
bs_score <- scores_test %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  mutate(school_pred = mu + bp + bc + bm + bs) %>%
  pull(school_pred)
bs_rmse <- RMSE(bs_score, scores_test$posttest)

#RMSE results table including adding "lunch" effect

school_rmse <- bind_rows(method_rmse,
                        data_frame(Model="Scool Effect",
                                   RMSE = bs_rmse))
school_rmse

#From Variable Importance, the next highest predictor is
#"n_student", the number of students in the class.
#This calculates adding the n_student effect(bn).
n_student_bn <- scores_train %>% 
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  group_by(n_student) %>%
  summarise(bn = mean(posttest - mu - bp -bc - bm - bs))

#RMSE results with "n_student" effect
bn_score <- scores_test %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  mutate(bn_pred = mu + bp + bc + bm + bs + bn) %>%
  pull(bn_pred)
bn_rmse <- RMSE(bn_score, scores_test$posttest)

#RMSE results table including adding "n_student" effect

n_students_rmse <- bind_rows(school_rmse,
                             data_frame(Model="n_Students in Class Effect",
                                        RMSE = bn_rmse))
n_students_rmse

#From Variable Importance, the next highest predictor is "lunch".
#This calculates the lunch effect(bl) using Least Squares approximation.
lunch_bl <- scores_train %>% 
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  group_by(lunch) %>%
  summarise(bl = mean(posttest - mu - bp - bc - bm - bs - bn))

#RMSE results with "lunch" effect
bl_score <- scores_test %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  mutate(l_pred = mu + bp + bc + bm + bs + bn + bl) %>%
  pull(l_pred)
bl_rmse <- RMSE(bl_score, scores_test$posttest)

#RMSE results table including adding "lunch" effect

lunch_rmse <- bind_rows(n_students_rmse,
                          data_frame(Model="Lunch Effect",
                                     RMSE = bl_rmse))
lunch_rmse


#From Variable Importance, the next highest predictor is "school_type",
#This calculates the school_type effect(bt) using Least Squares approximation.
type_bt <- scores_train %>% 
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  group_by(school_type) %>%
  summarise(bt = mean(posttest - mu - bp - bc - bm - bs - bn - bl))

#RMSE results with "school type" effect
bt_score <- scores_test %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  left_join(type_bt, by = 'school_type') %>%
  mutate(bt_pred = mu + bp + bc + bm + bs + bn + bl + bt) %>%
  pull(bt_pred)
bt_rmse <- RMSE(bt_score, scores_test$posttest)

#RMSE results table including adding "school type" effect

type_rmse <- bind_rows(lunch_rmse,
                         data_frame(Model="School Type Effect",
                                    RMSE = bt_rmse))
type_rmse

#From Variable Importance, the next highest predictor is "school setting",
#This calculates the school setting effect(bss) using Least Squares approximation.
setting_bss <- scores_train %>% 
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  left_join(type_bt, by = 'school_type') %>%
  group_by(school_setting) %>%
  summarise(bss = mean(posttest - mu - bp - bc - bm - bs - bn - bl - bt))

#RMSE results with "school setting" effect
bss_score <- scores_test %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  left_join(type_bt, by = 'school_type') %>%
  left_join(setting_bss, by = 'school_setting') %>%
  mutate(bss_pred = mu + bp + bc + bm + bs + bn + bl + bt + bss) %>%
  pull(bss_pred)
bss_rmse <- RMSE(bss_score, scores_test$posttest)

#RMSE results table including adding "school_setting" effect

setting_rmse <- bind_rows(type_rmse,
                       data_frame(Model="School Setting Effect",
                                  RMSE = bss_rmse))
setting_rmse

#From FAMD and VarImp, the final predictor is gender.
#This calculates the gender effect(bg).
gender_bg <- scores_train %>% 
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  left_join(type_bt, by = 'school_type') %>%
  left_join(setting_bss, by = 'school_setting') %>%
  group_by(gender) %>%
  summarise(bg = mean(posttest - mu - bp - bc - bm - bs - bn - bl - bt - bss))

#RMSE results with gender effect
bg_score <- scores_test %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  left_join(type_bt, by = 'school_type') %>%
  left_join(setting_bss, by = 'school_setting') %>%
  left_join(gender_bg, by = 'gender') %>%
  mutate(bg_pred = mu + bp + bc + bm + bs + bn + bl + bt + bss + bg) %>%
  pull(bg_pred)
bg_rmse <- RMSE(bg_score, scores_test$posttest)

#RMSE results table including adding gender effect

gender_rmse <- bind_rows(setting_rmse,
                       data_frame(Model="Gender Effect",
                                  RMSE = bg_rmse))
gender_table <- gender_rmse %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE,
                font_size = 14)
gender_table

#####
#This calculates the gender effect(bg) using Least Squares approximation getting rid of others.
type_bg_end <- scores_train %>% 
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  group_by(gender) %>%
  summarise(bg = mean(posttest - mu - bp - bc - bm - bs - bn - bl))

#RMSE results with "gender" effect (losing predictors) gives lower RMSE score
bg_score_end <- scores_test %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  left_join(type_bg_end, by = 'gender') %>%
  mutate(bg_pred = mu + bp + bc + bm + bs + bn + bl + bg) %>%
  pull(bg_pred)
model_3_best <- RMSE(bg_score_end, scores_test$posttest)

#RMSE results table including adding "gender" effect (ending)

gender_rmse_end <- bind_rows(lunch_rmse,
                             data_frame(Model="Gender Effect",
                                        RMSE = model_3_best))

#Make the table prettier
gender_results_end <- gender_rmse_end %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)%>%
  row_spec(9, bold = T, color = '#CC6633')

gender_results_end

#RMSE final model on scores_validation dataset (scores_validation includes
#glm prediction from training set)
bg_score_val <- scores_validation %>%
  left_join(class_bc_add, by = 'classroom') %>%
  left_join(method_bm, by = 'teaching_method') %>%
  left_join(school_bs, by = 'school') %>%
  left_join(n_student_bn, by = 'n_student') %>%
  left_join(lunch_bl, by = 'lunch') %>%
  left_join(type_bg_end, by = 'gender') %>%
  mutate(bg_pred = mu + bp + bc + bm + bs + bn + bl + bg) %>%
  pull(bg_pred)
model_3_validation <- RMSE(bg_score_val, scores_validation$posttest)

####
#Results from 3 models

#limit digits for table simplicity
options(digits = 4)

#Table for scores_test RMSE results
scores_test_results_table <- data_frame(Model = c("Model 1", "Model 2", "Model 3"),
                               "Test Set RMSE" = c(model_1_best, model_2_best, model_3_best))
#Make the table prettier
test_results <- scores_test_results_table %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)%>%
  row_spec(1, bold = T, color = '#CC6633')

test_results

#validation dataset applied to model 1, the best model


#Table for validation dataset RMSE results
validation_table <- data_frame(Model = c("Model 1", "Model 2", "Model 3"),
                                         "Validation RMSE" = c(model_1_validation, model_2_validation_update, model_3_validation))
#Make the table prettier
validation_results <- validation_table %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                latex_options = "hold_position",
                full_width = FALSE,
                font_size = 14)%>%
  row_spec(1, bold = T, color = '#bb0014')

validation_results
