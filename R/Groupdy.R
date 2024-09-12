data <- read.csv("data.csv")


# load all packages for this tutorial
library(psych)
library(car)
library(carData)
library(fastDummies)
library(emmeans)
library(rockchalk)

#mean
data$PSEall <- rowMeans(data[, c("PSE1", "PSE2", "PSE3", "PSE4")], na.rm = TRUE)
data$SEall  <- rowMeans(data[, c("SE1", "SE2", "SE3", "SE4", "SE5", "SE6", "SE7", "SE8", 
                                 "SE9", "SE10")], na.rm = TRUE)
data$CTall  <- rowMeans(data[, c("CT1", "CT2", "CT3", "CT4", "CT5", "CT6", "CT7", "CT8",
                                 "CT9", "CT10", "CT11","CT12")], na.rm = TRUE)

#test alpha
PSEall_items <- data[, c("PSE1", "PSE2", "PSE3", "PSE4")]
alpha(PSEall_items, check.keys=TRUE)

SEall_items <- data[, c("SE1", "SE2", "SE3", "SE4", "SE5", "SE6", "SE7", "SE8", 
                        "SE9", "SE10")]
alpha(SEall_items, check.keys=TRUE)

CTall_items <- data[, c("CT1", "CT2", "CT3", "CT4", "CT5", "CT6", "CT7", "CT8",
                        "CT9", "CT10", "CT11","CT12")]
alpha(CTall_items, check.keys=TRUE)


library(lm.beta)
library(sjPlot)


# Mean centering
data$PSEall_c <- data$PSEall - mean(data$PSEall, na.rm = TRUE)
data$CTall_c  <- data$CTall  - mean(data$CTall,  na.rm = TRUE)

#step 1
model1 <- lm(SEall ~ PSEall_c + CTall_c, data = data)
summary(model1)
tab_model(model1, show.std = TRUE)

# (Linearity) non linear
ncvTest(model1)
# (Normality) pass
plot(model1, 2)
# (Homoscedasticity)
plot(model1, 3)
spreadLevelPlot(model1)

#shapiro
shapiro.test(rstandard(model1))

# Step 2: Extract the residuals
residuals2 <- residuals(model1)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals2, "pnorm")  # Assuming nor


#step 2
model2 <- lm(SEall ~ PSEall_c * CTall_c, data = data)
summary(model2)
tab_model(model2, show.std = TRUE)
vif(model2)

library(apaTables)
table12 <- apa.reg.table(model1,model2, table.number = 1, filename = "table12.doc")


# (Linearity) non linear
ncvTest(model2)
# (Normality) pass
plot(model2, 2)
# (Homoscedasticity)
plot(model2, 3)
spreadLevelPlot(model2)

#shapiro
shapiro.test(rstandard(model2))

# Step 2: Extract the residuals
residuals3 <- residuals(model2)
# Step 3: Perform the Kolmogorov-Smirnov test
ks.test(residuals3, "pnorm")  # Assuming nor
anova(model1, model2)

# Define values for each variable at which to estimate marginal means
at_value <- list(
  PSEall = c(mean(data$PSEall) - sd(data$PSEall),  # -1 SD
            mean(data$PSEall),                    # Mean
            mean(data$PSEall) + sd(data$PSEall)   # +1 SD
  ),
  CTall = c(median(data$CTall),  # Median
            median(data$CTall)) # Median (ใช้ค่าเดียวกันเพื่อไม่ให้ error)
)

#แบ่งกลุ่ม CT ตาม PSE
data$CTall_group <- ifelse(data$CTall > median(data$CTall), "High Collectivistic", "Low Collectivistic")

# Hypothesis 2: 
model2g <- lm(SEall ~ PSEall * CTall_group, data = data)
summary(model2g)


# Graph for resilience hypo 2
CT_emm <- emmeans(model2g, ~ CTall_group | PSEall, at = at_value)
emmip(model2g, CTall_group ~ PSEall, at = at_value, CIs = TRUE)
CTemm_summary <- summary(CT_emm)
CTemm_summary$Collectivistic <- CTemm_summary$CTall_group

library(ggplot2)
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


ggplot(CTemm_summary, aes(x = Collectivistic, y = emmean, fill = Collectivistic)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(0.8), color = "black") +
  scale_fill_manual(values = c("High Collectivistic" = "red", "Low Collectivistic" = "blue")) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  ylab("Self-Esteem") +
  facet_wrap(~PSEall, ncol = 2, labeller = label_both) +
  labs(title = "Self-Esteem by Collectivistic and Perceived Social Exclusion",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme

ggplot(CTemm_summary, aes(x = PSEall, y = emmean, linetype = Collectivistic)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("High Collectivistic" = "solid", "Low Collectivistic" = "dashed")) +
  geom_text(aes(label = round(emmean, 2)), position = position_dodge(0.8), vjust = -0.25, size = 3) +
  scale_y_continuous(expand = c(0, 0.2)) +
  ylab("Self-Esteem") +
  labs(title = "Self-Esteem by Collectivistic and Perceived Social Exclusion",
       x = "Perceived Social Exclusion",
       linetype = "Collectivistic",
       caption = "Note. Error bars represent 95% confidence intervals.") +
  apa_theme 

# Load the apaTables package
library(apaTables)

# Correlation table
corr_table <- data[, c("PSEall", "SEall", "CTall")]

# Create an APA-style correlation table
apa_corr_table <- apa.cor.table(corr_table, filename = "apa_corr_table.doc", table.number = 2)

