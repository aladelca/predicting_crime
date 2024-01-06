#### Packages

library(ggplot2)
library(tidyverse)
library(cluster)
library(clusterCrit)
library(fpc)
library(ggfortify)
library(gbm)
library(randomForest)
library(sf)
library(rnaturalearth)
library(gridExtra)
library(corrplot)
library(caret)


raw_data = read.csv('dataset.csv')
nrow(raw_data)
data = read.csv('dataset.csv')
data = data[data$incident_id!=261784,]

#### Data into incident participant level

participant_type_columns =  data %>%
                            separate_rows(participant_type, sep = "\\|\\|") %>%
                            separate(participant_type, into = c("index", "type"), sep = "::")

participant_gender_columns =  data %>%
  separate_rows(participant_gender, sep = "\\|\\|") %>%
  separate(participant_gender, into = c("index", "gender"), sep = "::")

participant_age_columns =  data %>%
  separate_rows(participant_age, sep = "\\|\\|") %>%
  separate(participant_age, into = c("index", "age"), sep = "::")

participant_relationship_columns =  data %>%
  separate_rows(participant_relationship, sep = "\\|\\|") %>%
  separate(participant_relationship, into = c("index", "relationship"), sep = "::")

participant_status_columns =  data %>%
  separate_rows(participant_status, sep = "\\|\\|") %>%
  separate(participant_status, into = c("index", "status"), sep = "::")

final = participant_type_columns %>% 
        full_join(participant_gender_columns[,c('incident_id', 'index', 'gender')], by = c('incident_id','index')) %>% 
        full_join(participant_age_columns[,c('incident_id','index','age')], by = c('incident_id','index')) %>% 
        full_join(participant_relationship_columns[,c('incident_id','index','relationship')], by = c('incident_id','index')) %>% 
        full_join(participant_status_columns[,c('incident_id','index','status')], by = c('incident_id','index'))



df_age = final %>% filter(as.numeric(age)<=100)
ggplot(data = df_age, aes(x = as.numeric(age),fill = type) ) + geom_histogram(binwidth = 1)+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Distribution of Age', x = 'Age', y = 'Count')
  
### Feature engineering to get data from victims and attackers

only_victims = final %>% 
  filter(type == 'Victim') %>% 
  group_by(incident_id, gender) %>%
  summarise(frequency = n()) %>%
  pivot_wider(names_from = gender, values_from = frequency, values_fill = 0) %>% 
  rename('victim_male' = 'Male', 'victim_no_gender' = 'NA','victim_female'=  'Female')

only_subject = final %>% 
  filter(type == 'Subject-Suspect') %>% 
  group_by(incident_id, gender) %>%
  summarise(frequency = n()) %>%
  pivot_wider(names_from = gender, values_from = frequency, values_fill = 0) %>% 
  rename('subject_male' = 'Male', 'subject_no_gender' = 'NA','subject_female'=  'Female')

only_victims_age = final %>% 
  filter(type == 'Victim') %>% 
  group_by(incident_id) %>% 
  summarise(victim_mean_age = mean(as.integer(age), na.omit = TRUE))

only_subject_age = final %>% 
  filter(type == 'Subject-Suspect') %>% 
  group_by(incident_id) %>% 
  summarise(subject_mean_age = mean(as.integer(age), na.omit = TRUE))


df_clustering = data %>% 
  left_join(only_victims, by = 'incident_id') %>% 
  left_join(only_subject, by = 'incident_id') %>% 
  left_join(only_victims_age, by = 'incident_id') %>% 
  left_join(only_subject_age, by = 'incident_id')
  
nrow(df_clustering)
nrow(data)

cluster_vars = c('incident_id','n_killed','n_injured','latitude','longitude','victim_male','victim_female','subject_male','subject_female')

df_cluster = df_clustering[,cluster_vars]
df_cluster = df_cluster %>% filter(!is.na(latitude) & !is.na(longitude)) %>% 
  mutate(victim_male = ifelse(is.na(victim_male),0,victim_male)) %>% 
  mutate(victim_female = ifelse(is.na(victim_female),0,victim_female)) %>% 
  mutate(subject_female = ifelse(is.na(subject_female),0,subject_female)) %>% 
  mutate(subject_male = ifelse(is.na(subject_male),0,subject_male))
  

sd(df_cluster$n_injured)
mean(df_clustering$n_injured)

a = ggplot(data = df_cluster, aes(x = as.numeric(n_killed)) ) + geom_histogram(binwidth = 1, fill = 'skyblue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Histogram of Fatal Victims', x = 'n_killed', y = 'Count')

b = ggplot(data = df_cluster, aes(x = as.numeric(n_killed)) ) + geom_boxplot(binwidth = 1, fill = 'skyblue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Distribution of Fatal Victims', x = 'n_killed')


grid.arrange(a,b,ncol = 2)


a = ggplot(data = df_cluster, aes(x = as.numeric(n_injured)) ) + geom_histogram(binwidth = 1, fill = 'orange')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Histogram of Victims', x = 'n_injured', y = 'Count')

b = ggplot(data = df_cluster, aes(x = as.numeric(n_injured)) ) + geom_boxplot(binwidth = 1, fill = 'orange')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Distribution of Victims', x = 'n_injured')


grid.arrange(a,b,ncol = 2)


a = ggplot(data = df_cluster, aes (x = n_injured, y = n_killed)) + geom_point()+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Relationship between number of fatal victims and number of victims', x = 'n_injured', y = 'n_killed')

a



a = ggplot(data = df_cluster, aes(x = victim_male) ) + geom_histogram(binwidth = 1, fill = 'blue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Histogram of Number of Male Victims', y = 'Count')

b = ggplot(data = df_cluster, aes(x = victim_male) ) + geom_boxplot(binwidth = 1, fill = 'blue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Distribution of Number of Male Victims')


grid.arrange(a,b,ncol = 2)



a = ggplot(data = df_cluster, aes(x = victim_female) ) + geom_histogram(binwidth = 1, fill = 'skyblue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Histogram of Number of Female Victims', y = 'Count')

b = ggplot(data = df_cluster, aes(x = victim_female) ) + geom_boxplot(binwidth = 1, fill = 'skyblue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Distribution of Number of Female Victims')


grid.arrange(a,b,ncol = 2)


a = ggplot(data = df_cluster, aes(x = subject_male) ) + geom_histogram(binwidth = 1, fill = 'blue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Histogram of Number of Male Attackers', y = 'Count')

b = ggplot(data = df_cluster, aes(x = subject_male) ) + geom_boxplot(binwidth = 1, fill = 'blue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Distribution of Number of Male Attackers')


grid.arrange(a,b,ncol = 2)


a = ggplot(data = df_cluster, aes(x = subject_male) ) + geom_histogram(binwidth = 1, fill = 'skyblue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Histogram of Number of Female Attackers', y = 'Count')

b = ggplot(data = df_cluster, aes(x = subject_male) ) + geom_boxplot(binwidth = 1, fill = 'skyblue')+
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Distribution of Number of Female Attackers')


grid.arrange(a,b,ncol = 2)

### Clustering model

for (i in c(2,3,4,5,6,7,8,9,10)){
  model = kmeans(df_cluster[,c('n_killed','n_injured','latitude','longitude','victim_male','victim_female','subject_male','subject_female')],i)
  n = model$tot.withinss
  results = append(results, n)
}


df_eval = data.frame(n_clusters = c(2,3,4,5,6,7,8,9,10),
                     within_distance = results)
elbow_plot = ggplot(df_eval, aes(x = n_clusters, y = within_distance)) + geom_line() + labs(title = "Selecting the number of clusters",
                                                                                           x = "Number of clusters",
                                                                                           y = "Within Distance")+ theme_minimal()+ theme(
  panel.background = element_rect(fill = "white"),
  #text = element_text(family = "serif", color = "black"),
  plot.title = element_text(face = "bold", size = 16),
  axis.text = element_text(size = 9)
)+
  scale_y_continuous(labels = scales::number_format(scale = 0.00001))



elbow_plot

pca = prcomp(df_cluster[,c('n_killed','n_injured','latitude','longitude','victim_male','victim_female','subject_male','subject_female')], scale = TRUE)
autoplot(pca, data = df_cluster[,c('n_killed','n_injured','latitude','longitude','victim_male','victim_female','subject_male','subject_female')], loadings = TRUE, loadings.label = TRUE) + labs(title = 'Principal Component Analysis')+  theme_minimal()+ theme(
  panel.background = element_rect(fill = "white"),
  #text = element_text(family = "serif", color = "black"),
  plot.title = element_text(face = "bold", size = 12),
  axis.text = element_text(size = 9)
)


df_cluster[,c('n_killed','n_injured','latitude','longitude','victim_male','victim_female','subject_male','subject_female','cluster')] %>% group_by(cluster) %>% 
  summarise(n_killed = mean(n_killed),
            latitude = mean(latitude),
            longitude = mean(longitude),
            victim_male = mean(victim_male),
            victim_female = mean(victim_female),
            subject_male = mean(subject_male),
            subject_female = mean(subject_female))


df_cluster$cluster = km.5$cluster
data = data %>% left_join(df_cluster[,c('incident_id','cluster')], by = 'incident_id')
final = final %>% left_join(df_cluster[,c('incident_id','cluster')], by = 'incident_id')
df_clustering = df_clustering %>% left_join(df_cluster[,c('incident_id','cluster')], by = 'incident_id')

calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


View(df_clustering)

### Feature engineering for machine learning model

model_data = df_clustering[!is.na(df_clustering$cluster),]

final_model_data = model_data %>% group_by(date,cluster) %>% 
  summarise(n_killed = sum(n_killed),
            n_injured = sum(n_injured),
            congressional_district = calculate_mode(congressional_district),
            num_guns_involved = sum(n_guns_involved),
            state_house_district = calculate_mode(state_house_district),
            state_senate_district = calculate_mode(state_senate_district),
            victim_male = sum(victim_male, na.omit=FALSE),
            victim_female = sum(victim_female,  na.omit=FALSE),
            victim_no_gender = sum(victim_no_gender,  na.omit=FALSE),
            subject_male = sum(subject_male,  na.omit=FALSE),
            subject_female = sum(subject_female,  na.omit=FALSE))

victim_age_variables = final %>% 
  filter(type == 'Victim') %>% 
  group_by(cluster, date) %>% 
  summarise(victim_mean_age = mean(as.integer(age), na.omit = FALSE),
            victim_max_age = max(as.integer(age), na.omit = FALSE),
            victim_min_age = min(as.integer(age), na.omit = FALSE),
            victim_std_age = sd(as.integer(age)))


df_map = df_clustering[,c('latitude','longitude','cluster')]
df_map = df_map %>% filter(!is.na(latitude) & !is.na(longitude))




ggplot(data = df_map, aes(x = latitude, y = longitude, color = as.factor(cluster))) + geom_point()+theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) + labs(title = 'Cluster Location')+
  guides(fill = guide_legend(title = "Cluster"))


subject_age_variables = final %>% 
  filter(type == 'Subject-Suspect') %>% 
  group_by(cluster, date) %>% 
  summarise(subject_mean_age = mean(as.integer(age), na.omit = TRUE),
            subject_max_age = max(as.integer(age), na.omit = TRUE),
            subject_min_age = min(as.integer(age), na.omit = TRUE),
            subject_std_age = sd(as.integer(age)))


final_model_data = final_model_data %>% 
  left_join(victim_age_variables, by = c('cluster', 'date')) %>%
  left_join(subject_age_variables, by = c('cluster', 'date'))


View(final)

final_model_data$date <- as.Date(final_model_data$date)

monthly_data = final_model_data %>%
  group_by(cluster, month = format(date, "%Y%m")) %>%
  summarise(total_killed = sum(n_killed))


data_2013 = final_model_data %>% filter(date < '2014-01-01')
data_2014 = final_model_data %>% filter(date < '2015-01-01' & date >= '2014-01-01')
data_2015 = final_model_data %>% filter(date < '2016-01-01' & date >= '2015-01-01')
data_2016 = final_model_data %>% filter(date < '2017-01-01' & date >= '2016-01-01')
data_2017 = final_model_data %>% filter(date < '2018-01-01' & date >= '2017-01-01')
data_2018 = final_model_data %>% filter(date < '2019-01-01' & date >= '2018-01-01')


ggplot(final_model_data) +
  geom_line(aes(x = date, y = n_killed)) +
  labs(title = "Number of Fatal Victims by Date",
       x = "Date",
       y = "Number of Fatal Victims"
       ) +
  theme_minimal()+ theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 9)
  ) 




### Target variable

final_model_data = final_model_data %>% arrange(cluster, date)
final_model_data = final_model_data %>% group_by( cluster) %>% mutate(n_injured_next_day = lead(n_injured))
final_model_data = final_model_data %>% group_by( cluster) %>% mutate(n_injured_previous_day = lag(n_injured))
final_model_data = final_model_data %>% group_by( cluster) %>% mutate(n_injured_previous_day_2 = lag(n_injured,n=2))
final_model_data = final_model_data %>% group_by( cluster) %>% mutate(n_injured_previous_day_3 = lag(n_injured,n=3))
final_model_data = final_model_data %>% group_by( cluster) %>% mutate(n_injured_previous_day_4 = lag(n_injured,n=4))
final_model_data = final_model_data %>% group_by( cluster) %>% mutate(n_injured_previous_week = lag(n_injured,n=7))
final_model_data_filtered = final_model_data %>% filter(date >= '2014-01-01')

final_model_data_filtered$weekday = wday(final_model_data_filtered$date, week_start = 1)
final_model_data_filtered$week = week(final_model_data_filtered$date)
final_model_data_filtered$month = month(final_model_data_filtered$date)

### Train and test dataset

data_cluster_1_train = final_model_data_filtered %>% filter(cluster == 1) %>% filter(!is.na(n_injured_next_day)) %>% filter(date<'2018-01-01')
data_cluster_2_train = final_model_data_filtered %>% filter(cluster == 2) %>% filter(!is.na(n_injured_next_day))%>% filter(date<'2018-01-01')
data_cluster_3_train = final_model_data_filtered %>% filter(cluster == 3) %>% filter(!is.na(n_injured_next_day))%>% filter(date<'2018-01-01')
data_cluster_4_train = final_model_data_filtered %>% filter(cluster == 4) %>% filter(!is.na(n_injured_next_day))%>% filter(date<'2018-01-01')
data_cluster_5_train = final_model_data_filtered %>% filter(cluster == 5) %>% filter(!is.na(n_injured_next_day))%>% filter(date<'2018-01-01')


data_cluster_1_test = final_model_data_filtered %>% filter(cluster == 1) %>% filter(!is.na(n_injured_next_day)) %>% filter(date>='2018-01-01')
data_cluster_2_test = final_model_data_filtered %>% filter(cluster == 2) %>% filter(!is.na(n_injured_next_day))%>% filter(date>='2018-01-01')
data_cluster_3_test = final_model_data_filtered %>% filter(cluster == 3) %>% filter(!is.na(n_injured_next_day))%>% filter(date>='2018-01-01')
data_cluster_4_test = final_model_data_filtered %>% filter(cluster == 4) %>% filter(!is.na(n_injured_next_day))%>% filter(date>='2018-01-01')
data_cluster_5_test = final_model_data_filtered %>% filter(cluster == 5) %>% filter(!is.na(n_injured_next_day))%>% filter(date>='2018-01-01')


### Modeling


model_boosting_1 = gbm(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
              data = data_cluster_1_train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 4)


model_rf_1 = randomForest(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                          n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                        data = data_cluster_1_train,
                        ntree=1000, importance = TRUE)

predictions_boosting_1 = predict(model_boosting_1, newdata = data_cluster_1_test, n.tree = 10000)
predictions_rf_1 = predict(model_rf_1, newdata = data_cluster_1_test, n.tree = 10000)
mean((predictions_boosting_1 - data_cluster_1_test$n_injured_next_day)^2)
mean((predictions_rf_1 - data_cluster_1_test$n_injured_next_day)^2)

data_plot_1 = data.frame(predictions_boosting = predictions_boosting_1, predictions_rf = predictions_rf_1, date = data_cluster_1_test$date, real = data_cluster_1_test$n_injured_next_day)

model_1 = ggplot(data_plot_1, aes(x = date)) +
  geom_line(aes(y = predictions_boosting, color = "Boosting"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = predictions_rf, color = "Random Forest"), linetype = "dashed", size = 1, alpha = 0.8) +
  geom_line(aes(y = real, color = "Real"), linetype = "solid", size = 1, alpha = 0.8) +
  labs(title = "Comparison for Cluster 1",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Boosting" = "blue", "Random Forest" = "red", "Real" = "#006400"), name = 'Models') +
  theme_minimal() +
  theme(legend.position = "top") + theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 8)
  )




model_boosting_2 = gbm(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                         n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                       data = data_cluster_2_train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 4)


model_rf_2 = randomForest(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                            n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                          data = data_cluster_2_train,
                          ntree=1000, importance = TRUE)

predictions_boosting_2 = predict(model_boosting_2, newdata = data_cluster_2_test, n.tree = 10000)
predictions_rf_2 = predict(model_rf_2, newdata = data_cluster_2_test, n.tree = 10000)
mean((predictions_boosting_2 - data_cluster_2_test$n_injured_next_day)^2)
mean((predictions_rf_2 - data_cluster_2_test$n_injured_next_day)^2)



data_plot_2 = data.frame(predictions_boosting = predictions_boosting_2, predictions_rf = predictions_rf_2, 
                         date = data_cluster_2_test$date, real = data_cluster_2_test$n_injured_next_day)

model_2 = ggplot(data_plot_2, aes(x = date)) +
  geom_line(aes(y = predictions_boosting, color = "Boosting"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = predictions_rf, color = "Random Forest"), linetype = "dashed", size = 1, alpha = 0.8) +
  geom_line(aes(y = real, color = "Real"), linetype = "solid", size = 1, alpha = 0.8) +
  labs(title = "Comparison for Cluster 2",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Boosting" = "blue", "Random Forest" = "red", "Real" = "#006400"), name = 'Models') +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(legend.position = "top") + theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 8)
  )




model_boosting_3 = gbm(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                         n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                       data = data_cluster_3_train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 4)


model_rf_3 = randomForest(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                            n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                          data = data_cluster_3_train,
                          ntree=1000, importance = TRUE)

predictions_boosting_3 = predict(model_boosting_3, newdata = data_cluster_3_test, n.tree = 10000)
predictions_rf_3 = predict(model_rf_3, newdata = data_cluster_3_test, n.tree = 10000)
mean((predictions_boosting_3 - data_cluster_3_test$n_injured_next_day)^2)
mean((predictions_rf_3 - data_cluster_3_test$n_injured_next_day)^2)


data_plot_3 = data.frame(predictions_boosting = predictions_boosting_3, predictions_rf = predictions_rf_3, 
                         date = data_cluster_3_test$date, real = data_cluster_3_test$n_injured_next_day)

model_3 = ggplot(data_plot_3, aes(x = date)) +
  geom_line(aes(y = predictions_boosting, color = "Boosting"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = predictions_rf, color = "Random Forest"), linetype = "dashed", size = 1, alpha = 0.8) +
  geom_line(aes(y = real, color = "Real"), linetype = "solid", size = 1, alpha = 0.8) +
  labs(title = "Comparison for Cluster 3",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Boosting" = "blue", "Random Forest" = "red", "Real" = "#006400"), name = 'Models') +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(legend.position = "top")+
  theme(legend.position = "top") + theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 8)
  )



model_boosting_4 = gbm(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                         n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                       data = data_cluster_4_train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 4)


model_rf_4 = randomForest(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                            n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                          data = data_cluster_4_train,
                          ntree=1000, importance = TRUE)






predictions_boosting_4 = predict(model_boosting_4, newdata = data_cluster_4_test, n.tree = 10000)
predictions_rf_4 = predict(model_rf_4, newdata = data_cluster_4_test, n.tree = 10000)
mean((predictions_boosting_4 - data_cluster_4_test$n_injured_next_day)^2)
mean((predictions_rf_4 - data_cluster_4_test$n_injured_next_day)^2)


data_plot_4 = data.frame(predictions_boosting = predictions_boosting_4, predictions_rf = predictions_rf_4, 
                         date = data_cluster_4_test$date, real = data_cluster_4_test$n_injured_next_day)

model_4 = ggplot(data_plot_4, aes(x = date)) +
  geom_line(aes(y = predictions_boosting, color = "Boosting"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = predictions_rf, color = "Random Forest"), linetype = "dashed", size = 1, alpha = 0.8) +
  geom_line(aes(y = real, color = "Real"), linetype = "solid", size = 1, alpha = 0.8) +
  labs(title = "Comparison for Cluster 4",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Boosting" = "blue", "Random Forest" = "red", "Real" = "#006400"), name = 'Models') +
  theme_minimal() +
  theme(legend.position = "top") + theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 8)
  )

model_boosting_5 = gbm(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                         n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                       data = data_cluster_5_train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 4)


model_rf_5 = randomForest(n_injured_next_day ~ n_injured + n_injured_previous_day + n_injured_previous_day_2 + 
                            n_injured_previous_day_3 + n_injured_previous_day_4 + n_injured_previous_week + weekday + week + month,
                          data = data_cluster_5_train,
                          ntree=1000, importance = TRUE)

predictions_boosting_5 = predict(model_boosting_5, newdata = data_cluster_5_test, n.tree = 10000)
predictions_rf_5 = predict(model_rf_5, newdata = data_cluster_5_test, n.tree = 10000)
mean((predictions_boosting_5 - data_cluster_5_test$n_injured_next_day)^2)
mean((predictions_rf_5 - data_cluster_5_test$n_injured_next_day)^2)

data_plot_5 = data.frame(predictions_boosting = predictions_boosting_5, predictions_rf = predictions_rf_5, 
                         date = data_cluster_5_test$date, real = data_cluster_5_test$n_injured_next_day)
model_5 = ggplot(data_plot_5, aes(x = date)) +
  geom_line(aes(y = predictions_boosting, color = "Boosting"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = predictions_rf, color = "Random Forest"), linetype = "dashed", size = 1, alpha = 0.8) +
  geom_line(aes(y = real, color = "Real"), linetype = "solid", size = 1, alpha = 0.8) +
  labs(title = "Comparison for Cluster 5",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Boosting" = "blue", "Random Forest" = "red", "Real" = "#006400"), name = 'Models') +
  theme_minimal() +
  theme(legend.position = "top")+theme(
    panel.background = element_rect(fill = "white"),
    #text = element_text(family = "serif", color = "black"),
    plot.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 8)
  )



### Graphs
corr_matrix = cor(data_cluster_1[,c('n_injured_next_day','n_injured','n_injured_previous_day','n_injured_previous_day_2',
                  'n_injured_previous_day_3','n_injured_previous_day_4','n_injured_previous_week','weekday')])

par(mfrow = c(1, 1))
corrplot(corr_matrix, method = 'color',addCoef.col = "black", tl.col = "black", tl.srt = 20)

### Model comparison

grid.arrange(model_1,model_2,model_3,model_4, model_5,ncol = 2, nrow = 3)


