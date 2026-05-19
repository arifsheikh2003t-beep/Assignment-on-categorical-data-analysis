
# PROBLEM 1: Lung Cancer Data Analysis (EDA & Models)
# Ensure required libraries are ready
library(dplyr)
library(ggplot2)
library(readr)
library(epitools)

# Load Dataset (Updated path string to standard format)
lung <- read.csv(file.choose(lung_disease.csv)

#  Basic EDA Questions
# i) Distribution of Age
print(summary(lung$Age))
ggplot(lung, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 20) +
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal()

# ii) Smokers vs Non-smokers
print(table(lung$Smoking))
print(prop.table(table(lung$Smoking)) * 100)

# iii) Income group frequency
print(table(lung$Income))

# iv) Percentage exposed to high pollution
high_pollution_pct <- mean(lung$Pollution == "High") * 100
print(high_pollution_pct)

# v) Overall prevalence of lung disease
print(prop.table(table(lung$LungDisease)) * 100)

# Association & Crosstab
# i & ii) Smoking vs Lung Disease Contingency Table
table_smoke <- table(lung$Smoking, lung$LungDisease)
print(table_smoke)
print(prop.table(table_smoke, margin = 1)) # Conditional proportions

# iii & iv) Pollution and Income across Lung Disease
table_pollution <- table(lung$Pollution, lung$LungDisease)
print(table_pollution)

table_income <- table(lung$Income, lung$LungDisease)
print(prop.table(table_income, margin = 1))

# Statistical Testing
# i) Chi-square Tests
print(chisq.test(table_smoke))
print(chisq.test(table_pollution))

# ii) Fisher's Exact Test
print(fisher.test(table_smoke))

# iii) Odds Ratio via epitools
print(oddsratio(table_smoke))

# Correlation & Interpretation 
# Convert categorical variables cleanly to binary integers
lung$Smoking_num <- ifelse(lung$Smoking %in% c("Yes", "Smoker"), 1, 0)
lung$Disease_num <- ifelse(lung$LungDisease %in% c("Yes", "1"), 1, 0)

print(cor(lung$Smoking_num, lung$Disease_num, use = "complete.obs"))
print(cor(lung$Age, lung$Disease_num, use = "complete.obs"))

# Logistic Regression 
# Explicitly transform strings/integers to clean factors
lung$Smoking     <- as.factor(lung$Smoking)
lung$Pollution   <- as.factor(lung$Pollution)
lung$Income      <- as.factor(lung$Income)
lung$LungDisease <- as.factor(lung$LungDisease)

# Basic Model
model1 <- glm(LungDisease ~ Smoking + Age + Pollution + Income, data = lung, family = binomial)
print(summary(model1))
print(exp(coef(model1)))    # Adjusted Odds Ratios
print(exp(confint(model1))) # Odds Ratio Profiles Confidence Interval

# Interaction Model
model2 <- glm(LungDisease ~ Smoking * Pollution + Age + Income, data = lung, family = binomial)
print(summary(model2))
print(exp(coef(model2)))

# Visualizations
ggplot(lung, aes(x = Smoking, fill = LungDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Smoking and Lung Disease Prevalence", y = "Proportion") +
  theme_minimal()

ggplot(lung, aes(x = Pollution, fill = LungDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Pollution and Lung Disease Prevalence", y = "Proportion") +
  theme_minimal()


# PROBLEM 2: One-Way ANOVA (Exercise Programs)

library(car)

set.seed(123)
exercise_data <- data.frame(
  Program    = rep(c("A", "B", "C"), each = 30),
  WeightLoss = c(rnorm(30, mean = 5, sd = 1.5),
                 rnorm(30, mean = 7, sd = 1.8),
                 rnorm(30, mean = 9, sd = 2.0))
)

# Descriptive summary via dplyr
exercise_summary <- exercise_data %>%
  group_by(Program) %>%
  summarise(Mean = mean(WeightLoss), SD = sd(WeightLoss), Min = min(WeightLoss), Max = max(WeightLoss), .groups = 'drop')
print(exercise_summary)

# Fit ANOVA
anova_model <- aov(WeightLoss ~ Program, data = exercise_data)
print(summary(anova_model))

# Check Model Assumptions 
# 1. Normality of Residuals
shapiro_test <- shapiro.test(residuals(anova_model))
print(shapiro_test)

qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "red")

# 2. Homogeneity of Variance
print(leveneTest(WeightLoss ~ Program, data = exercise_data))

# Post-Hoc Comparison & Visualization 
print(TukeyHSD(anova_model))

ggplot(exercise_data, aes(x = Program, y = WeightLoss, fill = Program)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Weight Loss with Standard Error Bars", y = "Weight Loss (Pounds)") +
  theme_minimal()

# PROBLEM 3: Anemia Levels in Nigeria

library(vcd)

anemia <- read.csv("C:/Users/Lenovo/Desktop/Education/41/Siddikursir/children anemia.csv")

# Formulate Contingency Table
cont_table <- table(anemia$Highest.educational.level, anemia$Anemia.level)
print(cont_table)
print(prop.table(cont_table, margin = 1) * 100)

# Chi-Square Independence Evaluation
chi_result <- chisq.test(cont_table)
print(chi_result)
print(chi_result$expected)

# PROBLEM 4: Fisher's Exact Test & Contribution Diagram

drug_table <- matrix(c(40, 10, 10, 40, 25, 25), nrow = 3, byrow = TRUE)
rownames(drug_table) <- c("Drug_A", "Drug_B", "Drug_C")
colnames(drug_table) <- c("No_Disease", "Disease")

print(fisher.test(drug_table))

# Post-Hoc Pairwise comparisons
print(fisher.test(drug_table[c(1,2), ])) # A vs B
print(fisher.test(drug_table[c(1,3), ])) # A vs C
print(fisher.test(drug_table[c(2,3), ])) # B vs C

# Adjusted alpha threshold calculation (Bonferroni)
alpha_adj <- 0.05 / 3
cat("Bonferroni Adjusted Significance Level Threshold:", alpha_adj, "\n")

# Contribution Plot via vcd package
assoc(drug_table, shade = TRUE, main = "Association Between Drug Treatment and Disease")

# PROBLEM 5: Binary Logistic GLM (Graduate School Admissions)
binary_data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
binary_data$rank <- as.factor(binary_data$rank)

model_binary <- glm(admit ~ gre + gpa + rank, data = binary_data, family = binomial)
print(summary(model_binary))
print(exp(coef(model_binary)))
print(exp(confint(model_binary)))

# PROBLEM 6: Poisson GLM (High School Awards)

poisson_data <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
poisson_data$prog <- factor(poisson_data$prog, levels = c(1, 2, 3), labels = c("General", "Academic", "Vocational"))

model_poisson <- glm(num_awards ~ prog + math, family = poisson(link = "log"), data = poisson_data)
print(summary(model_poisson))
print(exp(coef(model_poisson)))

# Dispersion Diagnostic check
dispersion_ratio <- model_poisson$deviance / model_poisson$df.residual
cat("Poisson Model Dispersion Ratio Calculation:", dispersion_ratio, "\n")

# PROBLEM 7: Negative Binomial GLM (School Absences)

library(MASS)
library(haven)

nb_data <- read_dta("https://stats.idre.ucla.edu/stat/data/nb_data.dta")
nb_data$prog <- factor(nb_data$prog, levels = c(1, 2, 3), labels = c("General", "Academic", "Vocational"))

model_nb <- glm.nb(daysabs ~ math + prog, data = nb_data)
print(summary(model_nb))
print(exp(coef(model_nb)))


# PROBLEM 8: Zero-Inflated Poisson GLM (Fish Catch Count)

library(pscl)

fish_data <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")

# Zero-Inflated Poisson Model
# Right of the pipe symbol (|) defines variables predicting structural zero inflation
model_zip <- zeroinfl(count ~ child + persons + camper + livebait | child + persons, data = fish_data, dist = "poisson")
print(summary(model_zip))
print(exp(coef(model_zip)))

# PROBLEM 9: Zero-Inflated Negative Binomial GLM (Fish Catch Alternative)

# Fixing prompt context mismatch (applying fish counts data appropriately to ZINB structure)
model_zinb <- zeroinfl(count ~ child + persons + camper + livebait | child + persons, data = fish_data, dist = "negbin")
print(summary(model_zinb))
print(exp(coef(model_zinb)))

# PROBLEM 10: Zero-Truncated Poisson GLM (Hospital Stay Duration)

library(countreg) # Ensure this package is loaded or use alternative methods

zt_data <- read_dta("https://stats.idre.ucla.edu/stat/data/ztp.dta")
zt_data$hmo  <- as.factor(zt_data$hmo)
zt_data$died <- as.factor(zt_data$died)

# Fit Zero-Truncated Poisson
model_ztp <- countreg::zerotrunc(stay ~ age + hmo + died, data = zt_data, dist = "poisson")
print(summary(model_ztp))
print(exp(coef(model_ztp)))

# Note: Standard profile confint fails on zerotrunc objects; using Wald test intervals:
ztp_se <- summary(model_ztp)$coefficients$count[, "Std. Error"]
ztp_ci <- cback <- cbind(Lower = coef(model_ztp) - 1.96 * ztp_se, Upper = coef(model_ztp) + 1.96 * ztp_se)
print(exp(ztp_ci))

# PROBLEM 11: Zero-Truncated Negative Binomial GLM (Hospital Stay Alternative)

model_ztnb <- countreg::zerotrunc(stay ~ age + hmo + died, data = zt_data, dist = "negbin")
print(summary(model_ztnb))
print(exp(coef(model_ztnb)))

# Wald transformation confidence interval calculations
ztnb_se <- summary(model_ztnb)$coefficients$count[, "Std. Error"]
ztnb_ci <- cbind(Lower = coef(model_ztnb)[1:length(ztnb_se)] - 1.96 * ztnb_se, 
                 Upper = coef(model_ztnb)[1:length(ztnb_se)] + 1.96 * ztnb_se)
print(exp(ztnb_ci))

# Model Selection Criterion Across zero-truncated models 
print(AIC(model_ztp, model_ztnb))