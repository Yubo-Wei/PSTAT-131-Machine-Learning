rm(list=ls())
setwd("~/Desktop/pstat131/finalproject/")
library(tidyverse)
library(maps)
library(tree)
library(maptree)
library(ROCR)
library(glmnet)
library(gbm)
library(kableExtra)
library(randomForest)

election.raw <- read_delim("data/election/election.csv", delim = ",") %>% 
  mutate(candidate=as.factor(candidate))

census_meta <- read_delim("data/census/metadata.csv", delim = ";", col_names = FALSE) 
census <- read_delim("data/census/census.csv", delim = ",") 


# 4
election.raw %>% filter(fips == 2000)
election.raw <- filter(election.raw, fips != 2000)
dim(election.raw)
# reason: The county of those rows are NAs.

# 5
election_federal <- filter(election.raw, fips == "US")
election_state <- filter(election.raw, fips != "US" & is.na(county)) 
election <- filter(election.raw, !is.na(county))

# 6
candidate_votes <- election_federal %>% 
  select(candidate, votes)
candidate_votes <- candidate_votes[order(candidate_votes$votes),]
candidate_ordered <- factor(candidate_votes$candidate, levels = as.vector(candidate_votes$candidate))
candidate_votes <- candidate_votes %>% 
  mutate(pct = votes/sum(votes), candidate = candidate_ordered) 
ggplot(data = candidate_votes, aes(candidate, pct)) + 
  geom_col() +
  coord_flip()+ 
  labs(title = "2016 U.S. Presidential Election Candidate Votes", x = "Candidate", y = "Vote Count") + 
  geom_text(aes(label=votes), size = 2) 

# 7
county_group <- group_by(election, fips)
vote_group <- summarize(county_group, total = sum(votes))
county_group <- left_join(county_group, vote_group, by = "fips")
county_winner <- county_group %>%
  mutate(pct = votes/total) %>%
  top_n(n=1)

state_group <- group_by(election_state, state)
state_vote <- summarize(state_group, total = sum(votes))
state_group <- left_join(state_group, state_vote, by = "state")
state_winner <- state_group %>%
  mutate(pct = votes/total) %>%
  top_n(n = 1)

# 8
county <- map_data("county")
ggplot(data = county) + 
  geom_polygon(aes(x = long, y = lat, fill = subregion, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) 

# 9
states <- map_data("state")
states <- states %>%
  mutate(fips = state.abb[match(states$region, tolower(state.name))])
states <- left_join(states, state_winner, by="fips")

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color="white" ) + 
  coord_fixed(1.3) +
  ggtitle("Winning Candidate by State") +
  guides(fill=guide_legend(title = "Candidate"))

# 10
county <- map_data("county")
county_seperate <- separate(maps::county.fips , polyname, c("region", "subregion"), sep="," )
county_joined <- left_join(county_seperate, county, by=c("region", "subregion"))
county_joined$fips <- as.factor(county_joined$fips)
county <- left_join(county_joined, county_winner)

ggplot(data = county) + 
  geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color="white" ) + 
  coord_fixed(1.3) +
  ggtitle("Winning Candidate by County") +
  guides(fill=guide_legend(title = "Candidate"))

# 11 here we plot the income by County.
state_new <- na.omit(census) %>%
  group_by(State) %>%
  add_tally(TotalPop)
state_new <- cbind(state_new, weight = state_new$TotalPop/state_new$n)
state_new <- state_new %>%
  group_by(State) %>%
  summarise_at(vars(Income), funs(sum(. * weight)))
state_new <- state_new[order(state_new$Income, decreasing = F),]
state_ordered <- factor(state_new$State, levels = as.vector(state_new$State))
state_new <- state_new %>% 
  mutate(State = state_ordered) 
ggplot(state_new, aes(State, Income)) +
  geom_col() +
  coord_flip()+ 
  labs(title = "Income by State", x = "State", y = "Income") + 
  geom_text(aes(label=Income), size = 2) 

# 12
# First Variable
census.del <- census[complete.cases(census),] %>% 
  mutate(Men = 100 * (Men/TotalPop), 
         Employed = 100* (Employed/TotalPop),
         Citizen = 100* (Citizen/TotalPop)) %>%
  mutate(Minority = Hispanic + Black + Native + Asian + Pacific) %>% 
  select(-Hispanic, -Black, -Native, -Asian, -Pacific, -Walk, -PublicWork, -Construction, -Women, -White)
  
# Second Variable
census.subct <- census.del %>%
  group_by(State, County) %>%
  add_tally()
names(census.subct)[ncol(census.subct)] <- "CountyTotal"
census.subct <- mutate(census.subct, county_weight = TotalPop/CountyTotal)

# Third Variable
sum_county_weighted <- summarise_at(census.subct, .funs = funs(sum), .vars = vars("county_weight"))
names(sum_county_weighted)[ncol(sum_county_weighted)] <- "sum_county_weight"
census.ct <- left_join(census.subct, sum_county_weighted , by = c("State", "County"))
census.ct <- mutate(census.ct, county_weight = county_weight/sum_county_weight) %>%
  select(-CountyTotal, -sum_county_weight)

census.ct[4:27] <- census.ct[4:27]*census.ct$county_weight
census.ct <- census.ct %>% summarise_at(vars(Men:Minority), funs(sum))
head(census.ct)


# 13 
# county-level
pc1 <- prcomp(census.ct[3:26], scale=TRUE, center=TRUE)
dim(pc1$x)
# sub-county level
pc2 <- prcomp(census.subct[4:27], scale = TRUE, center = TRUE)
dim(pc2$x)

ct.pc = pc1$rotation[,1:2]
pc_1_sorted= sort(abs(ct.pc[,1]),decreasing = TRUE)
head(pc_1_sorted,3)
# IncomePerCap ChildPoverty Poverty
subct.pc = pc2$rotation[,1:2]
pc_1_sub_sorted= sort(abs(subct.pc[,1]),decreasing = TRUE)
head(pc_1_sub_sorted,3)
# IncomePerCap  Professional    Income 


# 14 
pr.var1 <- pc1$sd^2 
pve1 <- pr.var1 / sum(pr.var1) 
cum_pve1 <- cumsum(pve1)

pr.var2 <- pc2$sd^2 
pve2 <- pr.var2 / sum(pr.var2) 
cum_pve2 <- cumsum(pve2)

par(mfrow=c(2, 2))
plot(pve1, type="l", xlab="PC1", ylab="County PVE")
plot(cum_pve1, type="l", xlab="PC1", ylab="County Cumulative PVE")

plot(pve2, type="l", xlab="PC2", ylab="Subcounty PVE")
plot(cum_pve2, type="l", xlab="PC2", ylab="Subcounty Cumulative PVE")

PC1.Num <- min(which(cum_pve1>=0.9)[1])
PC2.Num <- min(which(cum_pve2>=0.9)[1])

# 15
census.ct_clust <- scale(census.ct[3:26])
census.ct_dist <- dist(census.ct_clust, method = "euclidean")
census.ct_hclust <- hclust(census.ct_dist, method = "complete")
clust1 <- cutree(census.ct_hclust, k = 10)

ct.pc <- data.frame(pc1$x[,1:5])
ct.pc_clust <- scale(ct.pc)
ct.pc_dist <- dist(ct.pc_clust, method = "euclidean")
ct.pc_hclust <- hclust(ct.pc_dist, method = "complete")
clust2 <- cutree(ct.pc_hclust, k = 10)

#San Mateo 
clust1[which(census.ct$County == "San Mateo")]
clust2[which(census.ct$County == "San Mateo")]

# Classification
tmpwinner <- county_winner %>% ungroup %>%
  mutate(state = state.name[match(state, state.abb)]) %>%               ## state abbreviations
  mutate_at(vars(state, county), tolower) %>%                           ## to all lowercase
  mutate(county = gsub(" county| columbia| city| parish", "", county))  ## remove suffixes
tmpcensus <- census.ct %>% mutate_at(vars(State, County), tolower)
election.cl <- tmpwinner %>%
  left_join(tmpcensus, by = c("state"="State", "county"="County")) %>% 
  na.omit
## save meta information
election.meta <- election.cl %>% select(c(county, fips, state, votes, pct, total))
## save predictors and class labels
election.cl <- election.cl %>% select(-c(county, fips, state, votes, pct, total))
set.seed(10) 
n <- nrow(election.cl)
in.trn <- sample.int(n, 0.8*n) 
trn.cl <- election.cl[ in.trn,]
tst.cl <- election.cl[-in.trn,]
set.seed(20) 
nfold <- 10
folds <- sample(cut(1:nrow(trn.cl), breaks=nfold, labels=FALSE))
calc_error_rate = function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}
records = matrix(NA, nrow=3, ncol=2)
colnames(records) = c("train.error","test.error")
rownames(records) = c("tree","logistic","lasso")


# 16 
candidate.tree <- tree(as.factor(candidate)~.,data = trn.cl, control = tree.control(nrow(trn.cl)))
cv <- cv.tree(candidate.tree, folds, FUN = prune.misclass, K = nfold)
best.size.cv <- min(cv$size[which(cv$dev == min(cv$dev))])# size is 6
draw.tree(candidate.tree, nodeinfo = T, cex = 0.5)
title("Unpruned Tree")

pruned_tree <- prune.misclass(candidate.tree, best.size.cv)
draw.tree(pruned_tree, nodeinfo = T, cex = 0.5)
title("Pruned Tree")

trn.clX <- select(trn.cl, -candidate)
trn.clY <- trn.cl$candidate
tst.clX <- select(tst.cl, -candidate)
tst.clY <- tst.cl$candidate

pred_pruned <- predict(pruned_tree, trn.clX, type = "class")
tr.error <- calc_error_rate(pred_pruned, trn.clY)
pred_tr <- predict(pruned_tree, tst.clX, type = "class")
ts.error <- calc_error_rate(pred_tr, tst.clY)
records[1,1] <- tr.error
records[1,2] <- ts.error

# 17
glm_fit <- glm(factor(candidate)~., data = trn.cl, family = "binomial")
summary(glm_fit)

glm_train <- predict(glm_fit, newdata = trn.cl, type = "response")
winner_train <- factor(ifelse(glm_train < 0.5, "Donald Trump", "Hillary Clinton"), 
                       levels=c("Donald Trump", "Hillary Clinton"))
Truth1 <- factor(ifelse(trn.cl$candidate == "Donald Trump","Donald Trump", "Hillary Clinton"))

glm_test <- predict(glm_fit, newdata = tst.cl, type = "response")
winner_test <- factor(ifelse(glm_test < 0.5, "Donald Trump", "Hillary Clinton"),
                      levels=c("Donald Trump", "Hillary Clinton"))
Truth2 <- factor(ifelse(tst.cl$candidate == "Donald Trump","Donald Trump", "Hillary Clinton"))

records[2,1] <- calc_error_rate(winner_train, Truth1)
records[2,2] <- calc_error_rate(winner_test, Truth2)

# 18
y.trn <- ifelse(trn.cl[,1] == "Donald Trump", 0, 1) 
x.trn <- model.matrix(candidate~. , trn.cl)[,-1] 
cv_lasso <- cv.glmnet(lambda = c(1, 5, 10, 50) * 1e-4, x.trn, y.trn, foldid = folds, 
                      alpha =1,family = "binomial") 
plot(cv_lasso) 
bestlambda <- cv_lasso$lambda.min #1e-04
abline(v = log(bestlambda), col="navy", lwd = 3, lty = 2) 
lasso_mod <- glmnet(x.trn, y.trn, alpha = 1, family = "binomial") 
coeff <- predict(lasso_mod, type = "coefficients", s = bestlambda) 

x.tst <- model.matrix(candidate~., tst.cl)[,-1] 
lasso_train_pred = predict(lasso_mod, newx = x.trn, s = bestlambda) 
lasso_train = ifelse(lasso_train_pred < 0.5, "Donald Trump","Hillary Clinton") 
las_train_err = calc_error_rate(as.tibble(lasso_train), trn.cl[,1]) 
lasso_test_pred = predict(lasso_mod, newx = x.tst, s = bestlambda) 
lasso_test = ifelse(lasso_test_pred < 0.5, "Donald Trump","Hillary Clinton") 
las_test_err = calc_error_rate(as.tibble(lasso_test), tst.cl[,1]) 

records[3,1] <- las_train_err
records[3,2] <- las_test_err
records
bestlambda

# 19
# tree
pred_tree <- prediction(as.numeric(pred_pruned), as.numeric(trn.cl$candidate))
perf_tree <- performance(pred_tree, measure = 'tpr', x.measure = 'fpr')
plot(perf_tree, col = "maroon", lwd = 3, main = "ROC Curves")
abline(0,1)
#logistic 
Pred_log_tst2 <- predict(glm.fit, tst.cl,type = 'class')
pred_log <- prediction(as.numeric(glm_train), as.numeric(trn.cl$candidate))
perf_log <- performance(pred_log, measure = 'tpr', x.measure = 'fpr')
plot(perf_log, add = TRUE, col = "forestgreen", lwd = 9)
abline(0,1)
#lasso
pred_lasso <- prediction(as.numeric(glm_train), as.numeric(trn.cl$candidate))
perf_lasso <- performance(pred_log, measure = 'tpr', x.measure = 'fpr')
plot(perf_lasso, add = TRUE, col = "lightblue", lwd = 3)
abline(0,1)
legend("bottomright", legend = c("decision tree", "log", "lasso"), 
       col = c("maroon","forestgreen", "lightblue"), lty = 1, cex = 0.7)

auc_tree <- performance(pred_tree, "auc")@y.values 
auc_tree
auc_log <- performance(pred_log, "auc")@y.values  
auc_log
auc_lasso <- performance(pred_lasso, "auc")@y.values 
auc_lasso


# 20
set.seed(1)
#Trump = 0, Clinton = 1
true_test <- as.numeric(ifelse(tst.cl$candidate == "Donald Trump", 0,1))
boost.elect.cl <- gbm(ifelse(candidate == "Donald Trump", 0,1)~., data = trn.cl, 
                      distribution = "bernoulli", n.trees = 800) 
#summary(boost.elect.cl, main = "Boosting Election.cl")
par(mfrow = c(1,2))
plot(boost.elect.cl, i = "Minority", ylab= "y(Minority)")
plot(boost.elect.cl, i = "SelfEmployed", ylab= "y(SelfEmployed)")
yhat.boost <- predict(boost.elect.cl, newdata = tst.cl, n.trees = 800, type = "response")
#confusion matrix
boost.error <- table(pred = yhat.boost, truth = true_test)
test.boost.error <- 1 - sum(diag(boost.error))/sum(boost.error) #0.9983713
record1 <- matrix(c(test.boost.error, test.boost.error), nrow = 1, ncol = 1)
colnames(record1) = c("test.error")
rownames(record1) = c("boosting")
kable(record1)



# bagging
set.seed(1)
trn.cl$candidate <- factor(trn.cl$candidate)
bag.elect.cl <- randomForest(candidate~., data = trn.cl, mtry = 10, importance = TRUE)
plot(bag.elect.cl)
legend("center", colnames(bag.elect.cl$err.rate), col = 1:4, cex = 0.8, fill = 1:4)
bag.elect.cl <- randomForest(candidate~., data = trn.cl, mtry = 10, ntree = 700, 
                             importance = TRUE)
yhat.bag <- predict(bag.elect.cl, newdata = tst.cl)
#confusion matrix
bag.error <- table(pred = yhat.bag, truth = true_test)
test.bag.error <- 1 - sum(diag(bag.error))/sum(bag.error)   #0.04885993
record1 <- matrix(c(test.boost.error, test.bag.error), nrow = 2, ncol = 1)
colnames(record1) = c("test.error")
rownames(record1) = c("boost error","bag error")
kable(record1)

