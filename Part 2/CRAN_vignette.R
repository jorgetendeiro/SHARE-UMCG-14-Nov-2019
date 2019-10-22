# From here:
# https://cran.r-project.org/web/packages/rstanarm/vignettes/rstanarm.html

library(rstanarm)
library(loo)

data("womensrole", package = "HSAUR3")
womensrole
total <- womensrole$agree + womensrole$disagree
total2 <- cbind(womensrole$agree, womensrole$disagree)
womensrole_glm_1 <- glm(cbind(agree, disagree) ~ education + gender,
                        data = womensrole, family = binomial(link = "logit"))
round(coef(summary(womensrole_glm_1)), 3)

# Comment on what p can tell us (about data|hyp instead of on hyp|data).

womensrole_bglm_1 <- stan_glm(cbind(agree, disagree) ~ education + gender,
                              data   = womensrole,
                              family = binomial(link = "logit"), 
                              prior  = student_t(df = 7), 
                              prior_intercept = student_t(df = 7),
                              cores  = 2, seed = 12345)
womensrole_bglm_1

ci95 <- posterior_interval(womensrole_bglm_1, prob = 0.95, pars = "education")
round(ci95, 3)

cbind(Median = coef(womensrole_bglm_1), MAD_SD = se(womensrole_bglm_1))
summary(residuals(womensrole_bglm_1)) # not deviance residuals
cov2cor(vcov(womensrole_bglm_1))

launch_shinystan(womensrole_bglm_1, ppd = FALSE)

y_rep <- posterior_predict(womensrole_bglm_1)
dim(y_rep)

par(mfrow = 1:2, mar = c(5,3.7,1,0) + 0.1, las = 3)
boxplot(sweep(y_rep[,womensrole$gender == "Male"], 2, STATS = 
                womensrole$total[womensrole$gender == "Male"], FUN = "/"), 
        axes = FALSE, main = "Male", pch = NA,
        xlab = "Years of Education", ylab = "Proportion of Agrees")
with(womensrole, axis(1, at = education[gender == "Male"] + 1, 
                      labels = 0:20))
axis(2, las = 1)
with(womensrole[womensrole$gender == "Male",], 
     points(education + 1,  agree / (agree + disagree), 
            pch = 16, col = "red"))
boxplot(sweep(y_rep[,womensrole$gender == "Female"], 2, STATS = 
                womensrole$total[womensrole$gender == "Female"], FUN = "/"), 
        axes = FALSE, main = "Female", pch = NA,
        xlab = "Years of Education", ylab = "")
with(womensrole, axis(1, at = education[gender == "Female"] + 1,
                      labels = 0:20))
with(womensrole[womensrole$gender == "Female",], 
     points(education + 1,  agree / (agree + disagree), 
            pch = 16, col = "red"))

womensrole_bglm_2 <- update(womensrole_bglm_1, formula. = . ~ . + I(education^2))

loo_bglm_1 <- loo(womensrole_bglm_1, k_threshold = 0.7)
loo_bglm_2 <- loo(womensrole_bglm_2,k_threshold = 0.7)

par(mfrow = 1:2, mar = c(5,3.8,1,0) + 0.1, las = 3)
plot(loo_bglm_1, label_points = TRUE)
plot(loo_bglm_2, label_points = TRUE)

loo_compare(loo_bglm_1, loo_bglm_2)
compare_models(loo_bglm_2, loo_bglm_1)

        newdata <- data.frame(agree = c(0,0), disagree = c(100,100), education = c(12,16),
                      gender = factor("Female", levels = c("Male", "Female")))
y_rep <- posterior_predict(womensrole_bglm_2, newdata)
summary(apply(y_rep, 1, diff))


