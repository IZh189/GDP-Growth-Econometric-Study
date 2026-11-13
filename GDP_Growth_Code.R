#Load the data amd clean it from N/A variables
# Dependent Variable: gamma (GDP Growth)
# Fixed Independent Variables: GDPSH60 (Initial Income), LIFEE060 (Life Expectancy), P60 (Primary Schooling)
data <- read.csv2("C:/Users/yzhil/Downloads/variablesforreplication.csv")

library(dplyr)

data_clean <- data %>%
  filter(!is.na(gamma), 
         !is.na(GDPSH60), 
         !is.na(LIFEE060), 
         !is.na(P60))


summary_stats <- summary(data_clean[c("gamma", "GDPSH60", "LIFEE060", "P60")])
print(summary_stats)

## Task 2: Run the 'fixed' regression
# The formula is: Growth ~ Income + Life Expectancy + Education
first_model <- lm(gamma ~ GDPSH60 + LIFEE060 + P60, data = data_clean)

summary(first_model)

# Regression A: Testing regional dummies with economic variables
model_1 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + safrica + laam + bmp1, data = data_clean)

summary(model_1)
# --- TASK 3: 10 UNIQUE ROBUSTNESS REGRESSIONS ---
# Every variable below appears in at least two different models.

# 1. Economic Investment & Policy
m1 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + EQINV + YrsOpen + BMS6087, data=data_clean)
summary(m1)
# 2. Politics & Institutions
m2 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + RULELAW + prightsb + civlibb + revcoup, data=data_clean)
summary(m2)
# 3. Geography & Culture
m3 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + laam + safrica + ABSLATIT + CONFUC, data=data_clean)
summary(m3)
# 4. Religion & Exports
m4 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + MUSLIM + PROT + BUDDHA + CATH + PRIEXP70, data=data_clean)

summary(m4)
# 5. Conflict, History & Trade
m5 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + wardum + SPAIN + NONEQINV + RERD + EcOrg + Mining, data=data_clean)
summary(m5)


# 6. Second check for Investment & Openness
m6 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + EQINV + YrsOpen + NONEQINV + RERD + EcOrg + Mining, data=data_clean)
summary(m6)
# 7. Second check for Politics & Institutions
m7 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + RULELAW +  safrica + civlibb + revcoup + wardum + SPAIN, data=data_clean)
summary(m7)
# 8. Second check for Geography & Culture
m8 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + laam + prightsb + ABSLATIT + CONFUC, data=data_clean)
summary(m8)

# 9. Second check for Market Premiums & Exports
m9 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + BMS6087 + PRIEXP70 + MUSLIM + PROT, data=data_clean)
summary(m9)
# 10. Final Mix of Religions
m10 <- lm(gamma ~ GDPSH60+LIFEE060+P60 + BUDDHA + CATH + civlibb + RULELAW , data=data_clean)
summary(m10)



