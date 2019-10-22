library(vcdExtra)
library(ggplot2)
library(corrplot)
library(rstanarm)
library(loo)
library(caret) # calibration()

options(mc.cores = parallel::detectCores())

# Load data:
data(ICU)

# Look at data:
dim(ICU)  # 200 cases, 22 variables
head(ICU) # See first rows
names(ICU)
str(ICU) # see ?ICU for details on each variable


# As adviced in the help file, remove 'race' and 'coma':
ICU$race <- NULL
ICU$coma <- NULL

# Fit 
icu.mod2 <- glm(died ~ age + cancer  + admit + uncons, data=ICU, family=binomial)
summary(icu.mod2)

icu.fit <- data.frame(ICU, prob=predict(icu.mod2, type="response"))

# combine categorical risk factors to a single string
risks <- ICU[, c("cancer", "admit", "uncons")]
risks[,1] <- ifelse(risks[,1]=="Yes", "Cancer", "")
risks[,2] <- ifelse(risks[,2]=="Emergency", "Emerg", "")
risks[,3] <- ifelse(risks[,3]=="Yes", "Uncons", "")
risks <- apply(risks, 1, paste, collapse="")
risks[risks==""] <- "(none)"
icu.fit$risks <- risks

library(ggplot2)
ggplot(icu.fit, aes(x=age, y=prob, color=risks)) +
  geom_point(size=2) +
  geom_line(size=1.25, alpha=0.5) +
  theme_bw() + ylab("Probability of death")



# From https://avehtari.github.io/modelselection/diabetes.html :

# first look at the data set using summary() and str() to understand what type of data are you working
# with
diabetes <- read.csv("Part 2//diabetes.txt", header = TRUE)
summary(diabetes)
str(diabetes)

# removing those observation rows with 0 in any of the variables
for (i in 2:6) {
  diabetes <- diabetes[-which(diabetes[, i] == 0), ]
}
# scale the covariates for easier comparison of coefficient posteriors
for (i in 1:8) {
  diabetes[i] <- scale(diabetes[i])
}

# modify the data column names slightly for easier typing
names(diabetes)[7] <- "dpf"
names(diabetes) <- tolower(names(diabetes))

n=dim(diabetes)[1]
p=dim(diabetes)[2]
str(diabetes)

corrplot(cor(diabetes[, c(9,1:8)]))

diabetes$outcome <- factor(diabetes$outcome)
# preparing the inputs
x <- model.matrix(outcome ~ . - 1, data = diabetes)
y <- diabetes$outcome

t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(outcome ~ ., data = diabetes,
                  family = binomial(link = "logit"), 
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = 123)

pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

round(coef(post1), 2)
round(posterior_interval(post1, prob = 0.9), 2)

loo1 <- loo(post1, save_psis = TRUE)

post0 <- update(post1, formula = outcome ~ 1, QR = FALSE)
loo0 <- loo(post0)
compare_models(loo0, loo1)

# Predicted probabilities
linpred <- posterior_linpred(post1)
preds <- posterior_linpred(post1, transform=TRUE)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)

round((mean(xor(pr[y==0]>0.5,as.integer(y[y==0])))+mean(xor(pr[y==1]<0.5,as.integer(y[y==1]))))/2,2)

# LOO predictive probabilities
ploo=E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(post1))$value
# LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(y==0))),2)

calPlotData<-calibration(y ~ pred + loopred, 
                         data = data.frame(pred=pred,loopred=ploo,y=y), 
                         cuts=10, class="1")
ggplot(calPlotData, auto.key = list(columns = 2))




# > calPlotData$data
#    calibModelVar       bin   Percent     Lower     Upper Count midpoint
# 1           pred   [0,0.1]  2.803738  1.035728  6.002365     6        5
# 2           pred (0.1,0.2] 14.285714  8.949706 21.197487    20       15
# 3           pred (0.2,0.3] 25.531915 17.093952 35.567328    24       25
# 4           pred (0.3,0.4] 38.095238 27.711315 49.344385    32       35
# 5           pred (0.4,0.5] 65.217391 49.750995 78.645645    30       45
# 6           pred (0.5,0.6] 43.750000 26.363812 62.337427    14       55
# 7           pred (0.6,0.7] 57.894737 40.821445 73.690176    22       65
# 8           pred (0.7,0.8] 73.333333 60.338966 83.925355    44       75
# 9           pred (0.8,0.9] 95.833333 85.745903 99.491353    46       85
# 10          pred   (0.9,1] 78.571429 59.046897 91.703939    22       95
# 11       loopred   [0,0.1]  3.738318  1.627520  7.232834     8        5
# 12       loopred (0.1,0.2] 14.285714  8.949706 21.197487    20       15
# 13       loopred (0.2,0.3] 29.787234 20.790064 40.099899    28       25
# 14       loopred (0.3,0.4] 41.860465 31.303669 52.994296    36       35
# 15       loopred (0.4,0.5] 50.000000 34.561157 65.438843    22       45
# 16       loopred (0.5,0.6] 46.153846 26.587122 66.629178    12       55
# 17       loopred (0.6,0.7] 65.000000 48.315555 79.371751    26       65
# 18       loopred (0.7,0.8] 67.741935 54.663985 79.060492    42       75
# 19       loopred (0.8,0.9] 88.000000 75.689868 95.466468    44       85
# 20       loopred   (0.9,1] 78.571429 59.046897 91.703939    22       95

cuts   <- round(seq(1,392,length.out=9),0)
counts <- rep(NA, 10)
myperc <- rep(NA, 10)
yvec <- as.numeric(y)-1
for (i in 1:10)
{
  counts[i] <- sum(yvec[order(pred)][cuts[i]:(cuts[i+1])])
  myperc[i] <- round(100*mean(yvec[order(pred)][cuts[i]:(cuts[i+1])]),1)
}


library(splines)
library(MASS)
ggplot(data = data.frame(pred=pred,loopred=ploo,y=as.numeric(y)-1), aes(x=loopred, y=y)) +
  stat_smooth(method='glm', formula = y ~ ns(x, 5), fullrange=TRUE) +
  geom_abline(linetype = 'dashed') +
  labs(x = "Predicted (LOO)", y = "Observed") +
  geom_jitter(height=0.02, width=0, alpha=0.3) +
  scale_y_continuous(breaks=seq(0,1,by=0.1)) +
  xlim(c(0,1))















