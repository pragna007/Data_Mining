#!/usr/bin/env python
# coding: utf-8

# In[17]:


# Filepath
data_path <- '/content/breast-cancer.data'
names_path <- '/content/breast-cancer.names'


# In[18]:


library(tidyverse)
library(ggrepel)
library(ggplot2)
library(grid)
library(ggalt)
library(ggExtra)
library(ggcorrplot)
library(quantmod)
library(scales)
library(ggthemes)
library(ggfortify)
library(lubridate)
library(plyr)
library(zoo)
library(dplyr)
library(forecast)
library(ggdendro)


# # Task1

# In[19]:


#creating dataframe
df<- data.frame (
  data <- read.csv(data_path)
)
#adding column names to the dataframe
colnames(df) <- c("class", "age", "menopause","tumor_size","inv_nodes","node_caps","deg_malig","breast","breast_quad","irradiat")


# In[20]:


df


# In[ ]:


# finding missing values
library(dplyr)

df %>% filter_all(any_vars(. %in% c('?')))


# In[22]:


#replacing missing values with mode of that column
df$node_caps[df$node_caps == "?"] <- mode(df$node_caps)
df$breast_quad[df$breast_quad == "?"] <- mode(df$breast_quad)


# In[23]:


df %>% filter_all(any_vars(. %in% c('?'))) # checking for missing values


# In[24]:


#stat summary
summary(df)


# In[25]:


#Changing categorical variable into numerical variables
df$class = factor(df$class, levels = c('no-recurrence-events','recurrence-events'),  labels = c(0,1))
df$node_caps = factor(df$node_caps, levels = c('no','yes'),  labels = c(0,1))
df$irradiat = factor(df$irradiat, levels = c('no','yes'),  labels = c(0,1))
df$breast = factor(df$breast, levels = c('left','right'),  labels = c(0,1))
#df$breast_quad = factor(df$breast_quad, levels = c('right_low','right_up','left_low','left_up'),  labels = c(0,1,2,3))


# In[40]:


#stat summary
summary(df)


# In[32]:


# scatter plot
g<-ggplot(df, aes(x=tumor_size, y=age)) + 
    geom_point(                     # Adds points of data to plot
               col="steelblue",     # Change color of points
               size=3               # Change size of points
               )
plot(g)


# In[34]:


ggplot(df, aes(x = class)) +
  geom_bar(fill = "#fc9272") +
  ggtitle("Distribution of Diagnosis in the Entire Dataset") +
  theme_minimal() +
  theme(legend.position = "none")


# In[46]:


set.seed(3011) 
train_index <- sample(nrow(df), size = round(0.75 * nrow(df)), replace = FALSE)
train <- df[train_index,]
test <- df[-train_index,]


# In[48]:


lm <- glm(formula = class ~ ., data = train, family = binomial())
summary(lm)


# In[50]:


library(car)
vif(lm)


# In[42]:


best_decision_tree <- rpart(class ~., df = train,
                            control = rpart.control(minsplit = 11,
                                                    maxdepth = 10))
rpart.plot(x = best_decision_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE, yesno = 2)


# # **TASK 3**

# In[ ]:


create_train_test <- function(data, size = 0.8, train = TRUE) {
    n_row = nrow(data)
    total_row = size * n_row
    train_sample < - 1: total_row
    if (train == TRUE) {
        return (data[train_sample, ])
    } else {
        return (data[-train_sample, ])
    }
}


# In[ ]:


data_train <- create_train_test(df, 0.8, train = TRUE)
data_test <- create_train_test(df, 0.8, train = FALSE)
dim(data_train)


# In[ ]:





# # Credit Card

# In[1]:


data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data",header = FALSE)
data #Sample data
df<-data.frame(data)
df
#add column names to dataframe
colnames(df)<-c("Chk_Acct_Status","Mnth_dur","Crdt_Hist","Purpse","Crdt_Amt","Svgs_Acct","Emplymnt_Status","Instl_rate","Gndr","gurntors","Prsnt_Rsdnce","Property","age","othr_instlmnts","housing","exsting_crdts","job","no_people","telphne","frgn_wrkr","class")
df
df$class <- as.factor(df$class-1)
df


# In[5]:


trainrows <- sample(1:nrow(df), nrow(df) * 0.8)
train <- df[trainrows, ]
test <- df[-trainrows,]
dim(train)
library(rpart)
tree <- rpart(formula = class~., data = train,method = 'class')
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(tree)
#prp(tree)
acc<-c()
for(i in 1:nrow(test))
{
  #print(test[i,])
  #print(i)
  pre<-predict(tree,test[i,], type = 'class')
  table_mat <- table(test[i,]$class, pre)
  acc[i]<-(sum(diag(table_mat)) / sum(table_mat))*100
}
acc


# In[ ]:




