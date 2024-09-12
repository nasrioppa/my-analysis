data <- read.csv("data.csv")

# load all packages for this tutorial
library(psych)
library(car)
library(carData)
library(fastDummies)
library(emmeans)
library(rockchalk)

#descriptive stats
library(psych)
psych::describe(data[, 1:5])
#
table(data$Gender)
table(data$Pos)
table(data$WorkExp)


#mean
data$CSall <- rowMeans(data[, c("CS1", "CS2", "CS3", "CS4", "CS5", "CS6", "CS7", "CS8")], na.rm = TRUE)
data$RSall <- rowMeans(data[, c("RS1", "RS2", "RS3", "RS4", "RS5", "RS6", "RS7", "RS8", 
                                "RS9", "RS10")], na.rm = TRUE)
data$PSall <- rowMeans(data[, c("PS1", "PS2", "PS3", "PS4", "PS5", "PS6", "PS7")], na.rm = TRUE)

data$WEall <- rowMeans(data[, c("WE1", "WE2", "WE3", "WE4", "WE5", "WE6", "WE7", "WE8", 
                                "WE9", "WE10", "WE11", "WE12", "WE13", "WE14", "WE15")], na.rm = TRUE)
library(lm.beta)
library(sjPlot)

# Mean centering
data$CSall_c <- data$CSall - mean(data$CSall, na.rm = TRUE)
data$RSall_c <- data$RSall - mean(data$RSall, na.rm = TRUE)
data$PSall_c <- data$PSall - mean(data$PSall, na.rm = TRUE)

data$Gender <- as.numeric(factor(data$Gender, levels = c("M", "F", "LGBTQIA+")))
data$Pos <- as.numeric(factor(data$Pos, levels = c("Other", "Operator", "Expert",
                                                   "Supervisor", "Manager", "Director/senior executive")))
data$WorkExp <- as.numeric(factor(data$WorkExp, levels = c("less than 1", "1 to 5", "More than 5")))

#model control
model_control <- lm(WEall ~ Pos + WorkExp + Member, data = data) 
summary(model_control)
tab_model(model_control, show.std = TRUE)



# Step 2: Main effects  
model2 <- lm(WEall ~ CSall_c + RSall_c + PSall_c, data = data) 
summary(model2)
lm.beta(model2)
tab_model(model2, show.std = TRUE)

# (Linearity) non linear
crPlots(model2)
ncvTest(model2)
# (Normality) pass
plot(model2, 2)
# (Homoscedasticity)
plot(model2, 3)
spreadLevelPlot(model2)

#shapiro
shapiro.test(rstandard(model2))

# Step 2: Extract the residuals
residuals1 <- residuals(model2)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals1, "pnorm")  # Assuming normal distribution for comparison


# Step 3: Two-way interactions 
model3 <- lm(WEall ~ CSall_c*RSall_c + CSall_c*PSall_c + RSall_c * PSall_c, data = data)
summary(model3)
tab_model(model3, show.std = TRUE)

# (Linearity) non linear
ncvTest(model3)
# (Normality) pass
plot(model3, 2)
# (Homoscedasticity)
plot(model3, 3)
spreadLevelPlot(model3)

#shapiro
shapiro.test(rstandard(model3))

# Step 2: Extract the residuals
residuals2 <- residuals(model3)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals2, "pnorm")  # Assuming nor


# Step 4: Three-way interaction
model4 <- lm(WEall ~ CSall_c*RSall_c*PSall_c, data = data) 
summary(model4)
tab_model(model4, show.std = TRUE)

# (Linearity) non linear
ncvTest(model4)
# (Normality) pass
plot(model4, 2)
# (Homoscedasticity)
plot(model4, 3)
spreadLevelPlot(model4)

#shapiro
shapiro.test(rstandard(model4))

# Step 2: Extract the residuals
residuals3 <- residuals(model4)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals3, "pnorm")  # Assuming nor


# Compare models
anova(model_control, model2, model3, model4)

table234 <- apa.reg.table(model2,model3,model4, table.number = 2, filename = "table234.doc")
