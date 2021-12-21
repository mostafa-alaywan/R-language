
###*************************************************************************
###*************************************************************************
###                                                                      ***
###                           REGRESSION TREES                           ***
###                                                                      ***
###*************************************************************************
###*************************************************************************



# Loading Packages  -------------------------------------------------------

library(rpart)
library(rpart.plot)
library(caret)

help(package= 'rpart')
help(package = 'rpart.plot')
help(package = 'caret')



# Data Preparation  -------------------------------------------------------

url = "https://github.com/mostafa-alaywan/Python/raw/main/Data/Cardiotocographic.csv"

cardio = read.csv(url, header = TRUE , sep=",")

str(cardio)

summary(cardio)

cardio$NSP = as.factor(cardio$NSP)



# Building and Plotting Tree Model Without Tuning The Hyperparameters ------------------
# NSP in terms the three variables :  LB , AC , FM 

# Building tree : 
nsp_tree <- rpart(NSP ~ LB+AC+FM , method = 'anova', data = cardio )

# Plotting  the Tree : first plot 
rpart.plot(nsp_tree ,    # tree model 
           type = 1 ,   # control the labels (0 -> 5)
           cex = 0.75   # control the text size
)

rpart.plot(nsp_tree ,  
           yesno = 2,   # control the yes/no text on the tree (0 -> 2)
           type = 1 ,   # control the labels (0 -> 5)
           cex = 0.75  
)

rpart.plot(nsp_tree ,   
           type = 1 ,   
           extra = 101, # control the number of observation + percentage in each node
           cex = 0.75   
)


# Checking the Hyperparamters by Default :  -------------------------------

attributes(nsp_tree)
nsp_tree$control



#  tuning parameters with rpart.control() function   ---------------------------------------
#  rpart.control is a function passed to rpart() 

?rpart.control

control_tree <- rpart( NSP ~ LB+AC+FM , method = 'anova' , data =cardio ,
                       control = rpart.control(cp = 0.02 , minsplit = 5)  )

rpart.plot(control_tree , yesno = 2 , type = 2 )


# Tuning the parameter cp with a set of values  ---------------------------
# using  "train" method from "caret library"

caret_rpart <- train(NSP ~ LB+AC+FM , method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.07, len = 30)),
                     data = cardio)


attributes(caret_rpart)

# plot the Accuracy according to cp Value : 
?ggplot.train
ggplot(caret_rpart)+theme_minimal()


# best cp value :
caret_rpart$bestTune

# draw a square around the the best cp value on the graph :
ggplot(caret_rpart , highlight = TRUE)+theme_minimal()

# Access the final model
my_final_model <-caret_rpart$finalModel

# plotting the final model using rpart.plot : 
rpart.plot(my_final_model , yesno = 2 , cex = 0.5 )
