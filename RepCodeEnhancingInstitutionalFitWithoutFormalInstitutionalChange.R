###Replication code for 
###Enhancing Institutional Fit without Formal Institutional Change
###Authors: Daniel J. Read, Alexandra Carroll, Lisa Wainger
###Journal: Ecology and Society
###Questions: Daniel J. Read (read.dread@gmail.com)

#Load packages
library(tidyverse)
library(ordinal)
library(ggcorrplot)
library(sure)
library(cowplot)
library(generalhoslem)
library(broom.mixed)
library(MASS)

# Load data
file_path <- "your/file/path/RepDataEnhancingInstitutionalFitWithoutFormalInstitutionalChange.rda"
load(file = file_path)

# Limit data to those who evaluated a conservation practitioner
dat2 <- dat1 %>% filter(GR !=0)

# Test for endogeneity of Sharing Knowledge
# Step 1: First-stage regression (predict X_endog)
first_stage <- lm(SKb ~ FD + AC + YR, data = dat2)
summary(first_stage)

# Save residuals
dat2$resid <- residuals(first_stage)

# Step 2: Proportional odds model with residuals (2SRI)
dat2$GR <- factor(dat2$GR, ordered = TRUE)
second_stage <- polr(GR ~ SKb + UCb + BPb + resid, data = dat2, method = "logistic")
summary(second_stage)

# Step 3: Check significance of residual term
coefs <- summary(second_stage)$coefficients
pvals <- 2 * (1 - pnorm(abs(coefs[, "t value"])))  # Get p-values from t-values
cbind(coefs, "p value" = round(pvals, 4))


# Test for endogeneity of Understanding Context
# Step 1: First-stage regression (predict X_endog)
first_stage <- lm(UCb ~ FD + AC + YR, data = dat2)
summary(first_stage)

# Save residuals
dat2$resid <- residuals(first_stage)

# Step 2: Proportional odds model with residuals (2SRI)
dat2$GR <- factor(dat2$GR, ordered = TRUE)
second_stage <- polr(GR ~ SKb + UCb + BPb + resid, data = dat2, method = "logistic")
summary(second_stage)

# Step 3: Check significance of residual term
coefs <- summary(second_stage)$coefficients
pvals <- 2 * (1 - pnorm(abs(coefs[, "t value"])))  # Get p-values from t-values
cbind(coefs, "p value" = round(pvals, 4))


# Test for endogeneity of Being Professional
# Step 1: First-stage regression (predict X_endog)
first_stage <- lm(BPb ~ FD + AC + YR, data = dat2)
summary(first_stage)

# Save residuals
dat2$resid <- residuals(first_stage)

# Step 2: Proportional odds model with residuals (2SRI)
dat2$GR <- factor(dat2$GR, ordered = TRUE)
second_stage <- polr(GR ~ SKb + UCb + BPb + resid, data = dat2, method = "logistic")
summary(second_stage)

# Step 3: Check significance of residual term
coefs <- summary(second_stage)$coefficients
pvals <- 2 * (1 - pnorm(abs(coefs[, "t value"])))  # Get p-values from t-values
cbind(coefs, "p value" = round(pvals, 4))


#Correlation matrix
model.matrix(~0+., data=dat2) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


#Ordinal regression
#Model 1, criteria plus additional characteristics
mod1 <- clm(GR ~ SKb + UCb + BPb + TN + LO + FD, data = dat2, Hess = TRUE)
summary(mod1)

tidy(mod1, conf.int = TRUE, exponentiate = TRUE, effects = "fixed" )

nominal_test(mod1)

#Plot surrogate residuals
mod1_res <- grid.arrange(
  autoplot.clm(mod1, nsim = 10, what = "qq"),
  autoplot.clm(mod1, nsim = 10, what = "fitted", alpha = 0.5),
  autoplot.clm(mod1, nsim = 10, what = "covariate", x = factor(dat2$SKb), xlab = "Sharing relevant knowledge"),
  autoplot.clm(mod1, nsim = 10, what = "covariate", x = factor(dat2$UCb), xlab = "Understanding farm context"),
  autoplot.clm(mod1, nsim = 10, what = "covariate", x = factor(dat2$BPb), xlab = "Being professional"),
  autoplot.clm(mod1, nsim = 10, what = "covariate", x = dat2$TN, xlab = "Tenure"),
  autoplot.clm(mod1, nsim = 10, what = "covariate", x = dat2$LO, xlab = "BMP Takes Land Out"),
  autoplot.clm(mod1, nsim = 10, what = "covariate", x = dat2$FD, xlab = "Funding Source"),
  ncol = 3)

#Model 2, criteria only
mod2 <- clm(GR ~ SKb + UCb + BPb, data=dat2)
summary(mod2)

tidy(mod2, conf.int = TRUE, exponentiate = TRUE, effects = "fixed")

nominal_test(mod2)

#Plot surrogate residuals
mod2_res <- grid.arrange(
  autoplot.clm(mod2, nsim = 10, what = "qq"),
  autoplot.clm(mod2, nsim = 10, what = "fitted", alpha = 0.5),
  autoplot.clm(mod2, nsim = 10, what = "covariate", x = factor(dat2$SKb), xlab = "Sharing relevant knowledge"),
  autoplot.clm(mod2, nsim = 10, what = "covariate", x = factor(dat2$UCb), xlab = "Understanding farm context"),
  autoplot.clm(mod2, nsim = 10, what = "covariate", x = factor(dat2$BPb), xlab = "Being professional"),
  ncol = 2)

#Compare models
AIC(mod2, mod1)
anova(mod2, mod1)


# Plot number of practices adopted by CP grade
boxplot1 <- ggplot(dat2, aes(x = GR, y = CP)) +
  geom_boxplot(width = 0.75, color = "#0072B2") +
  geom_jitter(height = 0, width = 0.2, fill = "#999999") +
  scale_x_discrete(limits = rev) +
  labs(x = "Overall Grade", y = "# Conservation Practices Adopted", subtitle = "(b)")

boxplot1

boxplot2 <- ggplot(dat1, aes(x = metCP, y = CP)) +
  geom_boxplot(width = 0.35, color = "#0072B2") +
  geom_jitter(height = 0, width = 0.2, fill = "#999999") +
  labs(x = "Met a Conservation Practitioner", y = "# Conservation Practices Adopted", subtitle = "(a)")

boxplot2

fig4 <- plot_grid(
  boxplot2,
  boxplot1,
  ncol = 2
)

fig4

# T-test of whether farmers who met CPs adopted significantly more conservation practices
t.test(CP ~ metCP, data = dat1)
