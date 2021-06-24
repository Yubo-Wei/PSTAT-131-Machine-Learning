library(tidyverse) 
library(ROCR) 
library(tree) 
library(maptree) 
library(class) 
library(lattice) 
library(ggridges) 
library(superheat)
rm(list = ls())
setwd("~/Desktop/pstat131/hw3")
drug_use <- read_csv('drug.csv',
                     col_names = c('ID','Age','Gender','Education','Country','Ethnicity',
                                   'Nscore','Escore','Oscore','Ascore','Cscore','Impulsive',
                                   'SS','Alcohol','Amphet','Amyl','Benzos','Caff','Cannabis',
                                   'Choc','Coke','Crack','Ecstasy','Heroin','Ketamine',
                                   'Legalh','LSD','Meth','Mushrooms','Nicotine','Semer','VSA'))
drug_use <- drug_use %>% mutate_at(as.ordered, .vars=vars(Alcohol:VSA))
drug_use <- drug_use %>%
  mutate(Gender = factor(Gender, labels=c("Male", "Female"))) %>%
  mutate(Ethnicity = factor(Ethnicity, labels=c("Black", "Asian", "White",
                                                "Mixed:White/Black", "Other",
                                                "Mixed:White/Asian",
                                                "Mixed:Black/Asian"))) %>%
  mutate(Country = factor(Country, labels=c("Australia", "Canada", "New Zealand",
                                            "Other", "Ireland", "UK", "USA")))                                  
        
# 1a
drug_use = drug_use %>% mutate(recent_cannabis_use=ifelse(Cannabis >= "CL3","Yes","No"))%>% 
  mutate(recent_cannabis_use = factor(recent_cannabis_use,levels=c("No", "Yes")))

# 1b
drug_use_subset <- drug_use %>% select(Age:SS, recent_cannabis_use)
set.seed(1) 
training.indices <- sample(1:nrow(drug_use_subset), 1500)
drug_use_train <- drug_use_subset[training.indices,]
drug_use_test <- drug_use_subset[-training.indices,]
dim(drug_use_train)
dim(drug_use_test)
# 1c
glm.fit <- glm(recent_cannabis_use ~ ., data = drug_use_train, family = binomial) 
summary(glm.fit)
# 2
tree_parameters = tree.control(nobs=nrow(drug_use_train), minsize=10, mindev=1e-3)
drugtree = tree(recent_cannabis_use~.,control = tree_parameters,data = drug_use_train)
# 2a
set.seed(1)
cv = cv.tree(drugtree, FUN = prune.misclass, K=10)
best.size.cv = 6
plot(cv$size , cv$dev, type="b", 
     xlab = "Number of leaves, \'best\'", ylab = "CV Misclassification Error",
     col = "red", main="CV")
abline(v = best.size.cv, lty=2)

# 2b
drugtree.pruned = prune.misclass(drugtree, best=best.size.cv)
draw.tree(drugtree.pruned, nodeinfo=TRUE,cex = 0.5)
# The variable 'Country' is split ﬁrst in this decision tree.

# 2c
# Predict on test set 
predictions = predict(drugtree.pruned, drug_use_test, type="class")
# get the true response of the test data
truth = drug_use_test$recent_cannabis_use
# Obtain confusion matrix
confusion_matrix = table(truth, predictions)
confusion_matrix

# get true positive rate
true_positive_rate = confusion_matrix[2,2]/sum(confusion_matrix[2,])
true_positive_rate

# get false positive rate
false_positive_rate = confusion_matrix[1,2]/sum(confusion_matrix[1,])
false_positive_rate

# 3a 
library(ROCR)
prob_log_testing = predict(glm.fit,drug_use_test,type="response")
pred_log = prediction(prob_log_testing, truth)
perf_log = performance(pred_log, measure="tpr", x.measure="fpr")

prob_tree_testing = predict(drugtree.pruned,newdata = drug_use_test)[,2]
pred_tree = prediction(prob_tree_testing,truth)
perf_tree = performance(pred_tree, measure="tpr", x.measure="fpr")

plot(perf_log, col=2, lwd=3, main="ROC curve") 
plot(perf_tree, col=2,lwd=3, main="ROC curve")
abline(0,1)
 # 3b
auc_log = performance(pred_log, "auc")@y.values
auc_log

auc_tree = performance(pred_tree, "auc")@y.values
auc_tree
# tree is larger


# 4a 
leukemia_data <- read_csv("leukemia_data.csv")
leukemia_data <- leukemia_data %>% 
  mutate(Type = factor(Type))
table(leukemia_data$Type)
# BCR-ABL least

# 4b
leukemia_data_wt = leukemia_data %>% select(-Type)
pr.out=prcomp(leukemia_data_wt, scale=TRUE, center = TRUE)
## get the variance explained by each principal component
pr.var=pr.out$sdev ^2
pve=pr.var/sum(pr.var)
## get cumulative pve
cumulative_pve = cumsum(pve)
## plot PVE and cumulative pve
par(mfrow=c(1, 2))
plot(pve, type="l", lwd=3)
plot(cumulative_pve, type="l", lwd=3)
dev.off()



pca$x[, 1:2]
# 4c

rainbow_colors <- rainbow(7) 
plot_colors <- rainbow_colors[leukemia_data$Type]
plot(pr.out$x[,1],pr.out$x[,2],xlab="PC1", ylab="PC2",cex=0.5)
text(pr.out$x[,1],pr.out$x[,2],labels = leukemia_data$Type,col=plot_colors,cex=0.6)
# T-ALL is most clearly separated from the others along the PC1 axis
## create a dataframe contain Type label and numeric PC1 value
pc_1 = data.frame(leukemia_data$Type,pr.out$x[,1])
colnames(pc_1) <- c('Tpye','PC1')
## sort the PC1 value
sorted <- pc_1[order(-abs(pc_1$PC1)),]
head(sorted,6)




# 4f
## load dendextend library
library(dendextend)
##subsetting to include only rows for which Type is either T-ALL, TEL-AML1, or Hyperdip50
leukemia_subset_1 <- filter(leukemia_data, Type == c("T-ALL","TEL-AML1","Hyperdip50"))
## exclude the first column Type
leukemia_subset = leukemia_subset_1 %>% select(-Type)
leukemia_subset_scaled <- scale(leukemia_subset[, -1], center=TRUE, scale=TRUE)
## calculate the distance matrix
subset_dist = dist(leukemia_subset_scaled,method='euclidean')
set.seed(1)
## Hierarchical Clustering using complete linkage
drug.hclust = hclust(subset_dist,method='complete')

## first plot
x = as.dendrogram(drug.hclust)
x %>% set_labels(leukemia_subset_1$Type[order.dendrogram(x)]) %>% 
  set("labels_col",k=3) %>% 
  set("branches_k_color", k = 3) %>%
  set("labels_cex", 0.3) %>% 
  plot(main='Three Groups for hclust',horiz = TRUE)

## second plot
y = as.dendrogram(drug.hclust)
y %>% set_labels(leukemia_subset_1$Type[order.dendrogram(y)]) %>% 
  set("labels_col",k=5) %>% 
  set("branches_k_color", k = 5) %>%
  set("labels_cex", 0.3) %>% 
  plot(main='Five Groups for hclust',horiz = TRUE)

                                   