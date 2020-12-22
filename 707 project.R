setwd("//hd.ad.syr.edu/02/669a4e/Documents/Desktop")
data <- read.csv("~/Desktop/mushroom.csv")
str(data)
View(colnames(data))
sum(!complete.cases(data))
View(data)
table(data$stalk.root)
table(data$class)
library(readr) # CSV file I/O, e.g. the read_csv function
library(caret)
library(randomForest)
library(caTools)   #<- For stratified split
library(rpart.plot)
library(ElemStatLearn)
library(klaR)
library(xgboost)
library(arulesViz)
library(ggplot2)
library(arules)
library(pROC)
library(gbm)
library(corrplot)
#-----------train & test-------------------------------------------------------------------------------
train_index<-createDataPartition(data$class,p=0.7,list=FALSE)
train <- data[train_index,]
test <- data[-train_index,]
table(train$class)
table(test$class)

#-----------explore and visuilize----------------------------------------------------------------------
#check how many species are in each category
class <- plyr::count(data$class)
print(sprintf("Edible: %d | Poisonous: %d | Percent of poisonous classes: %.1f%%",
              class$freq[1],class$freq[2], round(class$freq[1]/nrow(data)*100,1)))
#Visuilize
mushrooms <-data
mushrooms$veil.type <- NULL
str(mushrooms)
sapply(mushrooms, class)

#class
df1 <- data.frame("class"=c("edible","poisonous"),"percentage"=c(4208/(4208+3916),3916/(4208+3916)))
df1

pie <- ggplot(df1,aes(x="",y=percentage, fill=class))+
  geom_bar(stat="identity",width=1)+
  coord_polar("y",start=0)+geom_text(aes(label=paste0(class," ",round(percentage*100),"%")),position=position_stack(vjust=0.5))+
  labs(x=NULL,y=NULL, fill=NULL)+
  scale_fill_manual(values = c("#00BFC4","#F8766D"))+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5))

pie
colnames(data)
gg<-ggplot(mushrooms, aes(`class`, fill = `class`))
gg<-gg+geom_bar() + labs(title ="Class", x = "Class" , y = "count of class"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg
#cap shape:
gg1<-ggplot(mushrooms, aes(`cap.shape`, fill = `class`))
gg1<-gg1+geom_bar() + labs(title ="Class vs. Cap shape", x = "Cap shape" , y = "count of Cap shape"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg1
#cap.surface 
gg2<-ggplot(mushrooms, aes(`cap.surface`, fill = `class`)) 
gg2<-gg2+ geom_bar() + labs(title ="Class vs. Cap surface ", x = "Cap surface " , y = "count of Cap surface"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg2
#cap.color
gg3<-ggplot(mushrooms, aes(`cap.color`, fill = `class`)) 
gg3<-gg3+ geom_bar() + labs(title ="Class vs. Cap color ", x = "Cap color " , y = "count of Cap color"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg3
#bruises
gg4<-ggplot(mushrooms, aes(`bruises`, fill = `class`)) 
gg4<-gg4+ geom_bar() + labs(title ="Class vs. Bruises ", x = "Bruises " , y = "count of Bruises"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg4
#odor
gg5<-ggplot(mushrooms, aes(`odor`, fill = `class`)) 
gg5<-gg5+ geom_bar() + labs(title ="Class vs. Odor ", x = "Odor " , y = "Odor"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg5
#gill.attachment
gg6<-ggplot(mushrooms, aes(`gill.attachment`, fill = `class`)) 
gg6<-gg6+ geom_bar() + labs(title ="Class vs. Gill attachment ", x = "Gill attachment " , y = "count of Gill attachment"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg6
#gill.spacing
gg7<-ggplot(mushrooms, aes(`gill.spacing`, fill = `class`)) 
gg7<-gg7+ geom_bar() + labs(title ="Class vs. Gill spacing ", x = "Gill spacing " , y = "count of Gill spacing"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg7
#gill.size
gg8<-ggplot(mushrooms, aes(`gill.size`, fill = `class`)) 
gg8<-gg8+ geom_bar() + labs(title ="Class vs. Gill size ", x = "Gill size " , y = "count of Gill size"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg8
#gill.color
gg9<-ggplot(mushrooms, aes(`gill.color`, fill = `class`)) 
gg9<-gg9+ geom_bar() + labs(title ="Class vs. Gill color ", x = "Gill color " , y = "count of Gill color"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg9
#stalk.shape
gg10<-ggplot(mushrooms, aes(`stalk.shape`, fill = `class`)) 
gg10<-gg10+ geom_bar() + labs(title ="Class vs. Stalk shape ", x = "Stalk shape " , y = "count of Stalk shape"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg10
#stalk.root
gg11<-ggplot(mushrooms, aes(`stalk.root`, fill = `class`)) 
gg11<-gg11+ geom_bar() + labs(title ="Class vs. Stalk root ", x = "Stalk root " , y = "count of Stalk root"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg11
#stalk.surface.above.ring
gg12<-ggplot(mushrooms, aes(`stalk.surface.above.ring`, fill = `class`)) 
gg12<-gg12+ geom_bar() + labs(title ="Class vs. Stalk surface above ring ", x = "count of Stalk surface above ring " , y = "Stalk surface above ring"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg12
#stalk.surface.below.ring
gg13<-ggplot(mushrooms, aes(`stalk.surface.below.ring`, fill = `class`)) 
gg13<-gg13+ geom_bar() + labs(title ="Class vs. Stalk surface below ring ", x = "count of Stalk surface below ring " , y = "Stalk surface below ring"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg13
#stalk.color.above.ring
gg14<-ggplot(mushrooms, aes(`stalk.color.above.ring`, fill = `class`)) 
gg14<-gg14+ geom_bar() + labs(title ="Class vs. Stalk color above ring ", x = "count of Stalk color above ring " , y = "Stalk color above ring"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg14
#stalk.color.below.ring
gg15<-ggplot(mushrooms, aes(`stalk.color.below.ring`, fill = `class`)) 
gg15<-gg15+ geom_bar() + labs(title ="Class vs. Stalk color below ring ", x = "count of Stalk color below ring " , y = "Stalk color below ring"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg15
#veil.color 
gg16<-ggplot(mushrooms, aes(`veil.color`, fill = `class`)) 
gg16<-gg16+ geom_bar() + labs(title ="Class vs.Veil color ", x = "Veil color " , y = "count of Veil color"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg16
#ring.number
gg17<-ggplot(mushrooms, aes(`ring.number`, fill = `class`)) 
gg17<-gg17+ geom_bar() + labs(title ="Class vs. Ring number ", x = "Ring number " , y = "count of Ring number"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg17
#population
gg18<-ggplot(mushrooms, aes(`population`, fill = `class`)) 
gg18<-gg18+ geom_bar() + labs(title ="Class vs. Population ", x = "Population " , y = "count of Population"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg18
#habitat 
gg19<-ggplot(mushrooms, aes(`habitat`, fill = `class`)) 
gg19<-gg19+ geom_bar() + labs(title ="Class vs. Habitat ", x = "Habitat " , y = "Habitat"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg19
#ring type
gg20<-ggplot(mushrooms, aes(`ring.type`, fill = `class`)) 
gg20<-gg20+ geom_bar() + labs(title ="Class vs. ring type ", x = "ring type " , y = "ring type"  )+scale_fill_manual(values = c("#00BFC4","#F8766D"))
gg20

str(data)
#jittor
#gill.attachment
g1<-ggplot(mushrooms, aes(x= `gill.attachment`, y= `class`, fill = `class`, color = `class`)) + geom_jitter()+scale_colour_manual(values = c("#00BFC4","#F8766D"))+
  labs(x = "gill attachment" , y = "class"  )
g1
#odor
g2<-ggplot(mushrooms, aes(x= `odor`, y= `class`, fill = `class`, color = `class`))  + geom_jitter() + scale_colour_manual(values = c("#00BFC4","#F8766D"))+
  labs(x = "odor" , y = "class"  )
g2
#population
g3<-ggplot(mushrooms, aes(x= `population`, y= `class`, fill = `class`, color = `class`))  + geom_jitter() + scale_colour_manual(values = c("#00BFC4","#F8766D"))+
  labs(x = "population" , y = "class"  )
g3
#veil color
g4<-ggplot(mushrooms, aes(x= `veil.color`, y= `class`, fill = `class`, color = `class`))  + geom_jitter() + scale_colour_manual(values = c("#00BFC4","#F8766D"))+
  labs(x = "veil color" , y = "class"  )
g4
#-----------Association Rules--------------------------------------------------------------------------
library(arules)
#Create association rules 
#edible
rules_record_e <- apriori(data = data, parameter = list(support = 0.1, conf = 0.5,minlen = 3, maxlen = 5),
                 appearance = list(default = "lhs", rhs = c("class=e")),
                 control = list(verbose = F))
inspect(head(sort(rules_record_e, by = "lift", descreasing = T), 15))
plot(rules_record_e,measure=c("support","lift"),shading="confidence",main="Scatter Plot for 8561 Rules(e) ")
#poi
rules_record_p <- apriori(data = data, parameter = list(support = 0.1, conf = 0.5,minlen = 2, maxlen = 5),
                         appearance = list(default = "lhs", rhs = c("class=p")),
                         control = list(verbose = F))
inspect(head(sort(rules_record_p, by = "lift", descreasing = T), 15))
plot(rules_record_p,measure=c("support","lift"),shading="confidence",main = "Scatter Plot for 8561 Rules(p)")

#Tune parameters
rules_record_e1 <- apriori(data = data, parameter = list(support = 0.4, conf = 0.5,minlen = 2, maxlen = 5),
                        appearance = list(default = "lhs", rhs = c("class=e")),
                        control = list(verbose = F))
inspect(head(sort(rules_record_e1, by = "lift", descreasing = T), 15))

rules_record_e2 <- apriori(data = data, parameter = list(support = 0.41, conf = 0.8,minlen = 2, maxlen = 5),
                        appearance = list(default = "lhs", rhs = c("class=e")),
                        control = list(verbose = F))
inspect(head(sort(rules_record_e2, by = "lift", descreasing = T), 15))

rules_record_p1 <- apriori(data = data, parameter = list(support = 0.4, conf = 0.5,minlen = 2, maxlen = 5),
                           appearance = list(default = "lhs", rhs = c("class=p")),
                           control = list(verbose = F))
inspect(head(sort(rules_record_p1, by = "lift", descreasing = T), 15))

rules_record_p2 <- apriori(data = data, parameter = list(support = 0.392, conf = 0.8,minlen = 2, maxlen = 5),
                           appearance = list(default = "lhs", rhs = c("class=p")),
                           control = list(verbose = F))
inspect(head(sort(rules_record_p2, by = "lift", descreasing = T), 15))

#Freqency
fac_var<-sapply(data,is.factor)
freqdata<-as(data[,fac_var],"transactions")
freqent_items<-eclat(freqdata,parameter = list(support=0.4,minlen=3,maxlen=5))
inspect(head(freqent_items,3))
itemFrequencyPlot(freqdata,topN=10,type ="absolute",main="Item frequency")

#remove redundant
rules_e<-apriori(data = data, parameter = list(support = 0.4, conf = 0.5,minlen = 2, maxlen = 5),
               appearance = list(default = "lhs", rhs = c("class=e")),
               control = list(verbose = F))
subset_rules_e<-which(colSums(is.subset(rules_e,rules_e))>1)
rules_e<-sort(rules_e[-subset_rules_e],by="lift",decreasing=T)
inspect(head(rules_e,5))

rules_p<-apriori(data = data, parameter = list(support = 0.4, conf = 0.5,minlen = 2, maxlen = 5),
                 appearance = list(default = "lhs", rhs = c("class=p")),
                 control = list(verbose = F))
subset_rules_p<-which(colSums(is.subset(rules_p,rules_p))>1)
rules_p<-sort(rules_p[-subset_rules_p],by="lift",decreasing=T)
inspect(head(rules_p,5))


#-----------decisin tree--------------------------------------------------------------------------------
library(caret)
library(rpart)
library(rattle)
train$veil.type<-NULL
test$veil.type<-NULL
train$spore.print.color<-NULL
train_x<-train[,-1]
train_y<-train[,1]

dt_model <- train(x=train_x,y=train_y,method="rpart",metric="Accuracy",
                  trControl=trainControl(method="boot"))
                 
print(dt_model)
print(dt_model$finalModel)
fancyRpartPlot(dt_model$finalModel,main = "Decision Tree")

plot(varImp(dt_model),main="Decision Tree - Variable Importance Plot")
dt_pred<-predict(dt_model,newdata=test)
confusionMatrix(dt_pred,test$class)
dt_pred_prob<-predict(dt_model,newdata=test,type="prob")
dt_roc_curve<-roc(test$class,dt_pred_prob$e)
plot(dt_roc_curve,main="ROC Curve-Decision Tree")
auc(test$class,dt_pred_prob$e)
True_Class <- factor(c("e","e","p","p"))
Predicted_Class <- factor(c("e","p","e","p"))
Y<-c(1262,0,29,1145)
df_tree <- data.frame(True_Class, Predicted_Class, Y)

dtg1<-ggplot(data =  df_tree, mapping = aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#99CCFF", high = "#006699") +
  theme_bw() + theme(legend.position = "none")

#tune
dt_model2 <- train(x=train_x,y=train_y,method="rpart",metric="Accuracy",
                  trControl=trainControl(method="boot"),
                  tuneGrid=expand.grid(cp=seq(0,0.01,0.001)))
print(dt_model2)
print(dt_model2$finalModel)
fancyRpartPlot(dt_model2$finalModel)
plot(varImp(dt_model2),main="RPART - Variable Importance Plot")
#model pre-pruning:
dt_model_preprune <- train(x=train_x,y=train_y,method="rpart",metric="Accuracy",
                           tuneLength = 8,
                           control = rpart.control(minsplit = 50, minbucket = 20, maxdepth = 5))
print(dt_model_preprune$finalModel)
fancyRpartPlot(dt_model_preprune$finalModel)
plot(varImp(dt_model_preprune),main="RPART - Variable Importance Plot")
#model post-pruning
dt_model_postprune <- prune(dt_model2$finalModel, cp = 0.2)
print(dt_model_postprune)
fancyRpartPlot(dt_model_postprune)



#----------Random Forest----------------------------------------------------------------------------------
rf_model<-train(x=train_x,y=train_y,method="rf")
rf_model
varimp_rf <- varImp(rf_model)
plot(varimp_rf, main = "Variable Importance with Random Forest")


rf_pred<-predict(rf_model,newdata=test)
confusionMatrix(rf_pred,test$class)
rf_pred_prob<-predict(rf_model,newdata=test,type="prob")
rf_roc_curve<-roc(test$class,rf_pred_prob$e)
plot(rf_roc_curve)
auc(test$class,rf_pred_prob$e)
rf_True_Class <- factor(c("e","e","p","p"))
rf_Predicted_Class <- factor(c("e","p","e","p"))
rf_Y<-c(1262,0,0,1174)
df_rf <- data.frame(rf_True_Class, rf_Predicted_Class, rf_Y)

ggplot(data =  df_rf, mapping = aes(x = rf_True_Class, y = rf_Predicted_Class)) +
  geom_tile(aes(fill = rf_Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", rf_Y)), vjust = 1) +
  scale_fill_gradient(low = "#99CCFF", high = "#006699") +
  theme_bw() + theme(legend.position = "none")
#---------------------gbm---------------------------
gbm_model <- train(x=train_x,y=train_y, method = "gbm")
gbm_model
varimp_gbm <- varImp(gbm_model)
varimp_gbm
plot(varimp_gbm, main = "Variable Importance with Gradiant Boosting")

gbm_pred<-predict(gbm_model,newdata=test)
confusionMatrix(gbm_pred,test$class)
gbm_pred_prob<-predict(gbm_model,newdata=test,type="prob")
gbm_roc_curve<-roc(test$class,gbm_pred_prob$e)
plot(gbm_roc_curve)
auc(test$class,gbm_pred_prob$e)
gbm_True_Class <- factor(c("e","e","p","p"))
gbm_Predicted_Class <- factor(c("e","p","e","p"))
gbm_Y<-c(1262,0,0,1174)
df_gbm <- data.frame(gbm_True_Class, gbm_Predicted_Class, gbm_Y)

ggplot(data =  df_gbm, mapping = aes(x = gbm_True_Class, y = gbm_Predicted_Class)) +
  geom_tile(aes(fill = gbm_Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", gbm_Y)), vjust = 1) +
  scale_fill_gradient(low = "#99CCFF", high = "#006699") +
  theme_bw() + theme(legend.position = "none")

#-------------bag------------------------
bag_model <- train(x=train_x,y=train_y, method = "treebag")
bag_model

varimp_bag <- varImp(bag_model)
varimp_bag
plot(varimp_bag, main = "Variable Importance with Gradiant Boosting")

bag_pred<-predict(gbm_model,newdata=test)
confusionMatrix(bag_pred,test$class)
bag_pred_prob<-predict(bag_model,newdata=test,type="prob")
bag_roc_curve<-roc(test$class,bag_pred_prob$e)
plot(bag_roc_curve)
auc(test$class,bag_pred_prob$e)
bag_True_Class <- factor(c("e","e","p","p"))
bag_Predicted_Class <- factor(c("e","p","e","p"))
bag_Y<-c(1262,0,0,1174)
df_bag <- data.frame(bag_True_Class, bag_Predicted_Class, bag_Y)

ggplot(data =  df_bag, mapping = aes(x = bag_True_Class, y = bag_Predicted_Class)) +
  geom_tile(aes(fill = bag_Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", bag_Y)), vjust = 1) +
  scale_fill_gradient(low = "#99CCFF", high = "#006699") +
  theme_bw() + theme(legend.position = "none")

#-------------compare
model_comparison <- resamples(list(tree1=dt_model,tree2=dt_model2,
                                   tree_preprune=dt_model_preprune,
                                   RandomForest=rf_model,
                                   GBM=gbm_model,
                                   Bagging=bag_model))
summary(model_comparison)
scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))

bwplot(model_comparison, scales = scales)





