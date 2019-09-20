


library(vcdExtra)

# Load data:
data(ICU)

# Look at data:
dim(ICU)  # 200 cases, 22 variables
head(ICU) # See first rows
names(ICU)

# Here is the info of each variiable (see '?ICU'):
# - died:
#      Died before discharge?, a factor with levels No Yes
# - age:
#      Patient age, a numeric vector
# - sex:
#      Patient sex, a factor with levels Female Male
# - race:
#      Patient race, a factor with levels Black Other White. Also represented here as white.
# - service:
#      Service at ICU Admission, a factor with levels Medical Surgical
# - cancer:
#      Cancer part of present problem?, a factor with levels No Yes
# - renal:
#      History of chronic renal failure?, a factor with levels No Yes
# - infect:
#      Infection probable at ICU admission?, a factor with levels No Yes
# - cpr:
#      Patient received CPR prior to ICU admission?, a factor with levels No Yes
# - systolic:
#      Systolic blood pressure at admission (mm Hg), a numeric vector
# - hrtrate:
#      Heart rate at ICU Admission (beats/min), a numeric vector
# - previcu:
#      Previous admission to an ICU within 6 Months?, a factor with levels No Yes
# - admit:
#      Type of admission, a factor with levels Elective Emergency
# - fracture:
#      Admission with a long bone, multiple, neck, single area, or hip fracture? a factor with levels No Yes
# - po2:
#      PO2 from inital blood gases, a factor with levels >60 <=60
# - ph:
#      pH from inital blood gases, a factor with levels >=7.25 <7.25
# - pco:
#      PCO2 from inital blood gases, a factor with levels <=45 >45
# - bic:
#      Bicarbonate (HCO3) level from inital blood gases, a factor with levels >=18 <18
# - creatin:
#      Creatinine, from inital blood gases, a factor with levels <=2 >2
# - coma:
#      Level of unconsciousness at admission to ICU,	a factor with levels None Stupor Coma
# - white:
#      a recoding of race, a factor with levels White Non-white
# - uncons:
#      a recoding of coma a factor with levels No Yes
# 

# As adviced in the help file, remove 'race' and 'coma':
ICU$race <- NULL
ICU$coma <- NULL

# Fit 
icu.mod2 <- glm(died ~ age + cancer  + admit + uncons, data=ICU, family=binomial)
summary(icu.mod2)

anova(icu.mod2, icu.mod1, icu.full, test="Chisq")

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

# }



















