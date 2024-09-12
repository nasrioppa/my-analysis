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
psych::describe(data)
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

hist(data$RSall, breaks=10, col="lightblue", xlab="Resilience Scores",
     main="Histogram of Resilience Scores")
boxplot(data$RSall, horizontal=TRUE)

hist(data$RSall, breaks=10, col="lightblue", xlab="Resilience Scores",
     main="Histogram of Challenge Scores")
boxplot(data$CSall, horizontal=TRUE)

hist(data$RSall, breaks=10, col="lightblue", xlab="Resilience Scores",
     main="Histogram of PsychSafety Scores")
boxplot(data$PSall, horizontal=TRUE)

# Mean centering
data$CSall_c <- data$CSall - mean(data$CSall, na.rm = TRUE)
data$RSall_c <- data$RSall - mean(data$RSall, na.rm = TRUE)
data$PSall_c <- data$PSall - mean(data$PSall, na.rm = TRUE)

# Check multicollinearity
library(car)
vif(lm(WEall ~ CSall_c * RSall_c * PSall_c, data = data))


library(apaTables)
# Step 1: Control variables
model_control <- lm(WEall ~ Pos + WorkExp + Gender, data = data)
summary(model_control)
table_control <- apa.reg.table(model_control, table.number = 1, filename = "control_vars.doc")
tab_model(model_control, show.std = TRUE)


# Hypothesis 1: Challenge stressors negatively relate to work engagement
model1c <- lm(WEall ~ CSall_c, data = data)
summary(model1c)

# (Linearity) non linear
crPlots(model1c)
ncvTest(model1c)
# (Normality) pass
plot(model1c, 2)
# (Homoscedasticity)
plot(model1c, 3)
spreadLevelPlot(model1c)

#shapiro
shapiro.test(rstandard(model1c))

# Step 2: Extract the residuals
residuals1 <- residuals(model1c)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals1, "pnorm")  # Assuming normal distribution for comparison

# Hypothesis 2: Resilience moderates the relationship between challenge stressors and work engagement 
model2c <- lm(WEall ~ CSall_c * RSall_c, data = data)
summary(model2c)
# (Linearity) non linear
ncvTest(model2c)
# (Normality) pass
plot(model2c, 2)
# (Homoscedasticity)
plot(model2c, 3)
spreadLevelPlot(model2c)

#shapiro
shapiro.test(rstandard(model2c))

# Step 2: Extract the residuals
residuals2 <- residuals(model2c)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals2, "pnorm")  # Assuming normal distribution for comparison

# Hypothesis 3: Psychological safety moderates the relationship between challenge stressors and work engagement
model3c <- lm(WEall ~ CSall_c * PSall_c, data = data)
summary(model3c)
# (Linearity) linear
ncvTest(model3c)
# (Normality) pass
plot(model3c, 2)
# (Homoscedasticity)
plot(model3c, 3)
spreadLevelPlot(model3c)

#shapiro
shapiro.test(rstandard(model3c))

# Step 2: Extract the residuals
residuals3 <- residuals(model3c)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals3, "pnorm")  # Assuming normal distribution for comparison


# Hypothesis 4: Three-way interaction among challenge stressors, resilience, and psychological safety predicts work engagement
model4c <- lm(WEall ~ CSall_c * RSall_c * PSall_c, data = data)
summary(model4c)
# (Linearity) linear
ncvTest(model4c)
# (Normality) pass
plot(model4c, 2)
# (Homoscedasticity)
plot(model4c, 3)
spreadLevelPlot(model4c)

shapiro.test(rstandard(model4c))
library(dgof)


# Step 2: Extract the residuals
residuals4 <- residuals(model4c)

# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals4, "pnorm")  # Assuming normal distribution for comparison


#tablee



# Define values for each variable at which to estimate marginal means
at_value <- list(
  CSall = c(mean(data$CSall) - sd(data$CSall),  # -1 SD
            mean(data$CSall),                    # Mean
            mean(data$CSall) + sd(data$CSall)   # +1 SD
  ),
  RSall = c(quantile(data$RSall, 0.25),  # 25th percentile
            quantile(data$RSall, 0.75)), # 75th percentile
  PSall = c(quantile(data$PSall, 0.25),  # 25th percentile
            quantile(data$PSall, 0.75))  # 75th percentile
)

# Load required package
library(MASS)

# Hypothesis 1: Challenge stressors negatively relate to work engagement
glm1 <- glm(WEall ~ CSall_c, data = data, family = gaussian(link = "identity"))
summary(glm1)

# Hypothesis 2: Resilience moderates the relationship between challenge stressors and work engagement
glm2 <- glm(WEall ~ CSall_c * RSall_c, data = data, family = gaussian(link = "identity"))
summary(glm2)

# Hypothesis 3: Psychological safety moderates the relationship between challenge stressors and work engagement
glm3 <- glm(WEall ~ CSall_c * PSall_c, data = data, family = gaussian(link = "identity"))
summary(glm3)

# Hypothesis 4: Three-way interaction among challenge stressors, resilience, and psychological safety predicts work engagement
glm4 <- glm(WEall ~ CSall_c * RSall_c * PSall_c, data = data, family = gaussian(link = "identity"))
summary(glm4)

library(ggplot2)
# ปรับรูปแบบกราฟ
apa_theme <- theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        strip.text = element_text(size = 10, face = "bold"))


#Graph for resilience hypo 2
model2 <- lm(WEall ~ CSall * RSall, data = data)
summary(model2)

# Estimate marginal means for WEall across CSall at different RSall combinations
RS_emm <- emmeans(model2, ~ RSall | CSall, at = at_value)

# Get confidence intervals for the estimated marginal means
emmip(model2, RSall ~ CSall, at = at_value, CIs = TRUE)

# Get confidence intervals for the estimated marginal means
RSemm_summary <- summary(RS_emm)

# Create a new column for the resilience combination
library(dplyr)
RSemm_summary$Resilience <- with(RSemm_summary,
                                factor(case_when(
                                  RSall == quantile(data$RSall, 0.75) ~ "High Resilience",
                                  RSall == quantile(data$RSall, 0.25) ~ "Low Resilience"
                                ), levels = c("High Resilience", "Low Resilience")))
library(ggplot2)

# Create bar plot with error bars
ggplot(RSemm_summary, aes(x = Resilience, y = emmean, fill = Resilience)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(0.8), color = "black") +
  scale_fill_manual(values = c("High Resilience" = "red", "Low Resilience" = "blue")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  labs(title = "Work Engagement by Resilience and Challenge Stressor",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

# Create line plot with error bars
ggplot(RSemm_summary, aes(x = CSall, y = emmean, color = Resilience)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("High Resilience" = "red", "Low Resilience" = "blue")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  labs(title = "Work Engagement by Resilience and Challenge Stressor",
       x = "Challenge Stressors",
       color = "Resilience",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme


#Graph for Hypo 3
model3 <- lm(WEall ~ CSall * PSall, data = data)
summary(model3)

# Estimate marginal means for WEall across CSall at different RSall combinations
PS_emm <- emmeans(model3, ~ PSall | CSall, at = at_value)

# Get confidence intervals for the estimated marginal means
emmip(model3, PSall ~ CSall, at = at_value, CIs = TRUE)

# Get confidence intervals for the estimated marginal means
PSemm_summary <- summary(PS_emm)

# Create a new column for the resilience combination
library(dplyr)
PSemm_summary$PsySafety <- with(PSemm_summary,
                                 factor(case_when(
                                   PSall == quantile(data$PSall, 0.75) ~ "High Psychological Safety",
                                   PSall == quantile(data$PSall, 0.25) ~ "Low Psychological Safety"
                                 ), levels = c("High Psychological Safety", "Low Psychological Safety")))
library(ggplot2)

# Create bar plot with error bars
ggplot(PSemm_summary, aes(x = PsySafety, y = emmean, fill = PsySafety)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(0.8), color = "black") +
  scale_fill_manual(values = c("High Psychological Safety" = "red", "Low Psychological Safety" = "blue")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  labs(title = "Work Engagement by Psychological Safety and Challenge Stressor",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

# Create line plot with error bars
ggplot(PSemm_summary, aes(x = CSall, y = emmean, color = PsySafety)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("High Psychological Safety" = "red", "Low Psychological Safety" = "blue")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  labs(title = "Work Engagement by Psychological Safety and Challenge Stressor",
       x = "Challenge Stressors",
       color = "Psychological Safety",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

#graph for hypo 4
model4 <- lm(WEall ~ CSall * RSall * PSall, data = data)

# Estimate marginal means for WEall across CSall at different RSall and PSall combinations
we_emm <- emmeans(model4, ~ RSall * PSall | CSall, at = at_value)

# Get confidence intervals for the estimated marginal means (optional)
emmip(model4, RSall * PSall ~ CSall, at = at_value, CIs = TRUE)

# Get confidence intervals for the estimated marginal means
emm_summary <- summary(we_emm)

# Create a new column for the resilience-psych safety combination
library(dplyr)
emm_summary$RSxPS <- with(emm_summary, 
                                factor(case_when(
                                  RSall == quantile(data$RSall, 0.75) & PSall == quantile(data$PSall, 0.75) ~ "High-High",
                                  RSall == quantile(data$RSall, 0.25) & PSall == quantile(data$PSall, 0.25) ~ "Low-Low",
                                  RSall == quantile(data$RSall, 0.75) & PSall == quantile(data$PSall, 0.25) ~ "High-Low",
                                  RSall == quantile(data$RSall, 0.25) & PSall == quantile(data$PSall, 0.75) ~ "Low-High"
                                ), levels = c("High-High", "Low-Low", "High-Low", "Low-High")))

# สร้างกราฟแท่งแยกตามกลุ่ม
ggplot(emm_summary, aes(x = RSxPS, y = emmean, fill = RSxPS)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(0.8), color = "black") +
  scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green3", "Low-High" = "purple4")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  labs(title = "Work Engagement by Resilience, Psychological Safety, and Challenge Stressor",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

# Create line plot with error bars
ggplot(emm_summary, aes(x = CSall, y = emmean, color = RSxPS)) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +  # use geom_smooth for straight lines
  scale_color_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green3", "Low-High" = "purple4")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  labs(title = "Work Engagement by Resilience, Psychological Safety, and Challenge Stressor",
       x = "Challenge Stressors",
       color = "Resilience x PsychSafety",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme





# Define values for each variable at which to estimate marginal means
at_value <- list(
  CSall = c(mean(data$CSall) - sd(data$CSall),  # -1 SD
            mean(data$CSall),                    # Mean
            mean(data$CSall) + sd(data$CSall)   # +1 SD
  ),
  RSall = c(mean(data$RSall) - sd(data$RSall),  # Median
            mean(data$RSall),
            mean(data$RSall) + sd(data$RSall)), # Median (ใช้ค่าเดียวกันเพื่อไม่ให้ error)
  PSall = c(mean(data$PSall) - sd(data$RSall),  # Median
            mean(data$PSall),
            mean(data$PSall) + sd(data$RSall))  # Median (ใช้ค่าเดียวกันเพื่อไม่ให้ error)
)

# สร้างตัวแปรใหม่เพื่อแบ่งกลุ่ม Resilience และ Psychological Safety ตามค่ากลาง
data$RSall_group <- ifelse(data$RSall > mean(data$RSall), "High Resilience", "Low Resilience")
data$PSall_group <- ifelse(data$PSall > mean(data$PSall), "High Psychological Safety", "Low Psychological Safety")

# Hypothesis 2: Resilience moderates the relationship between challenge stressors and work engagement
model2 <- lm(WEall ~ CSall * RSall_group, data = data)
summary(model2)

# Graph for resilience hypo 2
RS_emm <- emmeans(model2, ~ RSall_group | CSall, at = at_value)
emmip(model2, RSall_group ~ CSall, at = at_value, CIs = TRUE)
RSemm_summary <- summary(RS_emm)
RSemm_summary$Resilience <- RSemm_summary$RSall_group
library(ggplot2)
ggplot(RSemm_summary, aes(x = Resilience, y = emmean, fill = Resilience)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("High Resilience" = "red", "Low Resilience" = "blue")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  labs(title = "Work Engagement by Resilience and Challenge Stressor",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

ggplot(RSemm_summary, aes(x = CSall, y = emmean, linetype = Resilience, shape = Resilience)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("High Resilience" = "solid", "Low Resilience" = "dashed")) +
  scale_shape_manual(values = c("High Resilience" = 16, "Low Resilience" = 17)) + # ใช้รหัสรูปร่างที่ต้องการ
  scale_y_continuous(expand = c(0, 0.2)) +
  ylab("Work Engagement") +
  labs(x = "Challenge Stressors",
       linetype = "Resilience",
       shape = "Resilience",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

ggplot(RSemm_summary, aes(x = CSall, y = emmean, linetype = Resilience, shape = Resilience)) +
  geom_line(size = 1, aes(linetype = Resilience)) +
  geom_point(size = 3, aes(shape = Resilience)) +
  scale_linetype_manual(values = c("High Resilience" = "solid", 
                                   "Low Resilience" = "dashed"),
                        name = "Resilience") +
  scale_shape_manual(values = c("High Resilience" = 16,
                                "Low Resilience" = 17),
                     name = "Resilience") +
  scale_y_continuous(expand = c(0, 0.2)) +
  ylab("Work Engagement") +
  labs(x = "Challenge Stressors",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme +
  theme(text = element_text(family = "Sarabun"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))


# Hypothesis 3: Psychological safety moderates the relationship between challenge stressors and work engagement
model3 <- lm(WEall ~ CSall * PSall_group, data = data)
summary(model3)

# Graph for Hypo 3
PS_emm <- emmeans(model3, ~ PSall_group | CSall, at = at_value)
emmip(model3, PSall_group ~ CSall, at = at_value, CIs = TRUE)
PSemm_summary <- summary(PS_emm)
PSemm_summary$PsySafety <- PSemm_summary$PSall_group

ggplot(PSemm_summary, aes(x = PsySafety, y = emmean, fill = PsySafety)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("High Psychological Safety" = "red", "Low Psychological Safety" = "blue")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  labs(title = "Work Engagement by Psychological Safety and Challenge Stressor",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

library(showtext)
font_add_google("Sarabun", "sarabun")
showtext_auto()
library(sysfonts)
ggplot(PSemm_summary, aes(x = CSall, y = emmean, linetype = PsySafety, shape = PsySafety)) +
  geom_line(size = 1, aes(linetype = PsySafety)) +
  geom_point(size = 3, aes(shape = PsySafety)) +
  scale_linetype_manual(values = c("High Psychological Safety" = "solid", 
                                   "Low Psychological Safety" = "dashed"),
                        name = "Psychological Safety") +
  scale_shape_manual(values = c("High Psychological Safety" = 16,
                                "Low Psychological Safety" = 17),
                     name = "Psychological Safety") +
  scale_y_continuous(expand = c(0, 0.2)) +
  ylab("Work Engagement") +
  labs(x = "Challenge Stressors",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme +
  theme(text = element_text(family = "Sarabun")) # ปรับตำแหน่งของ legend

# Hypothesis 4: Three-way interaction among challenge stressors, resilience, and psychological safety predicts work engagement
model4 <- lm(WEall ~ CSall * RSall_group * PSall_group, data = data)
summary(model4)

# Graph for hypo 4
we_emm <- emmeans(model4, ~ RSall_group * PSall_group | CSall, at = at_value)
emmip(model4, RSall_group * PSall_group ~ CSall, at = at_value, CIs = TRUE)
emm_summary <- summary(we_emm)
emm_summary$RSxPS <- with(emm_summary, 
                          factor(case_when(
                            RSall_group == "High Resilience" & PSall_group == "High Psychological Safety" ~ "High-High",
                            RSall_group == "Low Resilience" & PSall_group == "Low Psychological Safety" ~ "Low-Low",
                            RSall_group == "High Resilience" & PSall_group == "Low Psychological Safety" ~ "High-Low",
                            RSall_group == "Low Resilience" & PSall_group == "High Psychological Safety" ~ "Low-High"
                          ), levels = c("High-High", "Low-Low", "High-Low", "Low-High")))

ggplot(emm_summary, aes(x = RSxPS, y = emmean, fill = RSxPS)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green3", "Low-High" = "purple4")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  apa_theme

ggplot(emm_summary, aes(x = RSxPS, y = emmean, group = RSxPS, fill = RSxPS)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), alpha = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                position = position_dodge(0.8), color = "black") +
  scale_fill_manual(values = c("High-High" = "red", 
                               "Low-Low" = "blue",
                               "High-Low" = "green3",
                               "Low-High" = "purple4")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  apa_theme +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Sarabun", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Create line plot with error bars
ggplot(emm_summary, aes(x = CSall, y = emmean, color = RSxPS)) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +  # use geom_smooth for straight lines
  scale_color_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green3", "Low-High" = "purple4")) +
  scale_y_continuous(expand = c(0, 0.2)) +
  ylab("Work Engagement") +
  labs(title = "Work Engagement by Resilience, Psychological Safety, and Challenge Stressor",
       x = "Challenge Stressors",
       color = "Resilience x PsychSafety",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

