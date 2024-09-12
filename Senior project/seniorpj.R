data <- read.csv("data.csv")

# load all packages for this tutorial
library(psych)
library(car)
library(carData)
library(fastDummies)
library(emmeans)
library(rockchalk)

#mean
data$CSall <- rowMeans(data[, c("CS1", "CS2", "CS3", "CS4", "CS5", "CS6", "CS7", "CS8")], na.rm = TRUE)
data$RSall <- rowMeans(data[, c("RS1", "RS2", "RS3", "RS4", "RS5", "RS6", "RS7", "RS8", 
                                "RS9", "RS10")], na.rm = TRUE)
data$PSall <- rowMeans(data[, c("PS1", "PS2", "PS3", "PS4", "PS5", "PS6", "PS7")], na.rm = TRUE)

data$WEall <- rowMeans(data[, c("WE1", "WE2", "WE3", "WE4", "WE5", "WE6", "WE7", "WE8", 
                                "WE9", "WE10", "WE11", "WE12", "WE13", "WE14", "WE15")], na.rm = TRUE)

#dummy 
#dum Pos; ref group = Other
data$PosDum <-  ifelse(data$Pos == "Operator", 1,0)
data$PosDum2 <- ifelse(data$Pos == "Expert", 1,0)
data$PosDum3 <- ifelse(data$Pos == "Supervisor", 1,0)
data$PosDum4 <- ifelse(data$Pos == "Manager", 1,0)
data$PosDum5 <- ifelse(data$Pos == "Director/senior executive", 1,0)

#dum WorkExp ;ref group = less than 1 year
data$WorkExpDum <- ifelse(data$WorkExp == "1 to 5", 1,0)
data$WorkExpDum2 <- ifelse(data$WorkExp == "More than 5", 1,0)

#dum Gender; ref group = M
data$GenDum <- ifelse(data$Gender == "F", 1,0)
data$GenDum2 <- ifelse(data$Gender == "LGBTQIA+", 1,0)

# Step 1: Control variables
model_control <- lm(WEall ~ PosDum + PosDum2 + PosDum3 + PosDum4 + PosDum5 + 
                            WorkExpDum + WorkExpDum2 + 
                            GenDum + GenDum2, data = data)
summary(model_control)

# Hypothesis 1: Challenge stressors negatively relate to work engagement
model1 <- lm(WEall ~ PosDum + PosDum2 + PosDum3 + PosDum4 + PosDum5 + 
                     WorkExpDum + WorkExpDum2 + 
                     GenDum + GenDum2 +
                     CSall, data = data)
summary(model1)
anova(model_control, model1)

# Hypothesis 2: Resilience moderates the relationship between challenge stressors and work engagement
model2 <- lm(WEall ~ PosDum + PosDum2 + PosDum3 + PosDum4 + PosDum5 + 
                     WorkExpDum + WorkExpDum2 + 
                     GenDum + GenDum2 + 
                     CSall * RSall, data = data)
summary(model2)
anova(model1, model2)

# Hypothesis 3: Psychological safety moderates the relationship between challenge stressors and work engagement
model3 <- lm(WEall ~ PosDum + PosDum2 + PosDum3 + PosDum4 + PosDum5 + 
                     WorkExpDum + WorkExpDum2 + 
                     GenDum + GenDum2 +
                     CSall * PSall, data = data)
summary(model3)
anova(model1, model3)

# Hypothesis 4: Three-way interaction among challenge stressors, resilience, and psychological safety predicts work engagement
model4 <- lm(WEall ~ PosDum + PosDum2 + PosDum3 + PosDum4 + PosDum5 + 
                     WorkExpDum + WorkExpDum2 + 
                     GenDum + GenDum2 + 
                     CSall*RSall*PSall, data = data)
summary(model4)
anova(model2, model4)
# Assuming model4 is your fitted model containing CSall, RSall, and PSall

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


# Estimate marginal means for WEall across CSall at different RSall and PSall combinations
we_emm <- emmeans(model4, ~ PSall * RSall | CSall, at = at_value)

# Get confidence intervals for the estimated marginal means (optional)
emmip(model4, PSall * RSall ~ CSall, at = at_value, CIs = TRUE)

# Get confidence intervals for the estimated marginal means
emm_summary <- summary(we_emm)

# Create a new column for the resilience-psych safety combination
library(dplyr)
emm_summary$combination <- with(emm_summary, 
                                factor(case_when(
                                  PSall == quantile(data$PSall, 0.75) & RSall == quantile(data$RSall, 0.75) ~ "High-High",
                                  PSall == quantile(data$PSall, 0.25) & RSall == quantile(data$RSall, 0.25) ~ "Low-Low",
                                  PSall == quantile(data$PSall, 0.75) & RSall == quantile(data$RSall, 0.25) ~ "High-Low",
                                  PSall == quantile(data$PSall, 0.25) & RSall == quantile(data$RSall, 0.75) ~ "Low-High"
                                ), levels = c("High-High", "Low-Low", "High-Low", "Low-High")))
library(ggplot2)
ggplot(emm_summary, aes(x = CSall, y = emmean, color = combination, linetype = combination)) +
  geom_point(position = position_dodge(0.2)) +
  geom_line(aes(group = interaction(PSall, RSall)), position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, position = position_dodge(0.2)) +
  xlab("Challenge Stressor") +
  ylab("Work Engagement") +
  theme_classic() +
  scale_color_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green", "Low-High" = "purple")) +
  scale_linetype_manual(values = c("High-High" = "solid", "Low-Low" = "solid", "High-Low" = "dashed", "Low-High" = "dashed")) +
  labs(title = "Work Engagement by Challenge Stressor, Resilience, and Psychological Safety")


library(ggplot2)

ggplot(emm_summary, aes(x = CSall, y = emmean, fill = combination)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(0.5)) +
  xlab("Challenge Stressor") +
  ylab("Work Engagement") +
  theme_classic() +
  scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green", "Low-High" = "purple")) +
  labs(title = "Work Engagement by Challenge Stressor, Resilience, and Psychological Safety")


library(ggplot2)

ggplot(emm_summary, aes(x = combination, y = emmean, fill = combination)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green3", "Low-High" = "purple4")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12, face = "bold")) +
  facet_wrap(~CSall, ncol = 2) +
  labs(title = "Work Engagement by Resilience and Psychological Safety")


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

# สร้างกราฟแท่งแยกตามกลุ่ม
ggplot(emm_summary, aes(x = combination, y = emmean, fill = combination)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "green3", "Low-High" = "purple4")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Work Engagement") +
  facet_wrap(~CSall, ncol = 2, labeller = label_both) +
  labs(title = "Work Engagement by Resilience, Psychological Safety, and Challenge Stressor",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

shapiro.test()
