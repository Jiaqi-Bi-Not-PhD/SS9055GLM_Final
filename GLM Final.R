## ----setup, include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------
## Load libraries
library(tidyverse)
library(ggplot2)
library(MASS)
library(cowplot)
library(DescTools)
library(arm)


## -------------------------------------------------------------------------------
## Load data
geographic <- read_csv("glimdonia_geographical_data.csv")
survey <- read_csv("Bi_Jiaqi_sample_data.csv")


## -------------------------------------------------------------------------------
## Probability of finding the orchid - Data Wrangling & Visualization
survey2 <- survey %>%
  mutate(Found = ifelse(Count != 0, 1, 0)) %>%
  dplyr::select(-Site)

grid_x_found <- ggplot(aes(x = x, y = Found), data = survey2) + 
  geom_jitter(width = 0, height = 0.05, color = "pink") + 
  geom_smooth() +
  labs(title = "X-Axis vs. Orchid Found") 

grid_y_found <- ggplot(aes(x = y, y = Found), data = survey2) + 
  geom_jitter(width = 0, height = 0.05, color = "lightblue") + 
  geom_smooth() +
  labs(title = "Y-Axis vs. Orchid Found")

plot_grid(grid_x_found, grid_y_found)
# X-Axis and Y-Axis mostly within (25,25) to (75, 75)


## -------------------------------------------------------------------------------
## Plot river vs. orchid found
ggplot(aes(x = River, fill = factor(Found)), data = survey2) +
  geom_bar(stat = "count", color = "black", position = position_dodge()) + 
  ylab("Count") + 
  labs(fill = "Found", title = "River vs. Orchid Found") +
  scale_fill_brewer(palette = "Paired") +
  ylab("Count of Site Where Orchid is Found") +
  theme(plot.title = element_text(hjust = 0.5)) 


## -------------------------------------------------------------------------------
## Plot terrain vs. orchid found
ggplot(aes(x = Terrain, fill = factor(Found)), data = survey2) +
  geom_bar(stat = "count", color = "black", position = position_dodge()) + 
  ylab("Count") + 
  labs(fill = "Found", title = "Terrain vs. Orchid Found") +
  scale_fill_brewer(palette = "Paired") +
  ylab("Count of Site Where Orchid is Found") +
  theme(plot.title = element_text(hjust = 0.5)) 


## -------------------------------------------------------------------------------
## Plot vegetation vs. orchid found
ggplot(aes(x = Vegetation, fill = factor(Found)), data = survey2) +
  geom_bar(stat = "count", color = "black", position = position_dodge()) + 
  ylab("Count") + 
  labs(fill = "Found", title = "Vegetation vs. Orchid Found") +
  scale_fill_brewer(palette = "Paired") +
  ylab("Count of Site Where Orchid is Found") +
  theme(plot.title = element_text(hjust = 0.5)) 


## -------------------------------------------------------------------------------
## Elevation vs. orchid Count
survey2 %>%
  filter(Found == 1) %>%
  ggplot(aes(x = Elevation, y = Count)) + 
  geom_point() + 
  labs(title = "Elevation vs. Orchid Counts (Only if Orchid is Found)") +
  ylab("Count of Orchid") +
  theme(plot.title = element_text(hjust = 0.5)) 

## Elevation and Vegetation Correlation
survey2 %>%
  ggplot(aes(x = Elevation, y = Vegetation)) + 
  geom_point() +
  labs(title = "Elevation & Vegetation Correlation Check") +
  theme(plot.title = element_text(hjust = 0.5)) 


## -------------------------------------------------------------------------------
## Data Wrangling - Considering glacial & Ocean & Beach 
## & None vegetation as necessary cause for not having the orchid
## These are causal of the orchid has 0 probability to be found
## based on the plot and common sense
survey3 <- survey2 %>%
  filter(!(Terrain %in% c("beach", "glacial", "ocean")))

geographic3 <- geographic %>%
  filter(!(Terrain %in% c("beach", "glacial", "ocean")))


## -------------------------------------------------------------------------------
## Logistic regression
prob_model_1 <- glm(Found ~ I(x^2) + I(y^2) + x*y + Terrain * (River + Elevation + Vegetation), 
                    family = binomial(), data = survey3)
prob_model_2 <- glm(Found ~ I(x^2) + I(y^2) + x*y + Terrain + Elevation + Vegetation + River, 
                    family = binomial(), data = survey3)
prob_model_3 <- glm(Found ~ I(x^2) + I(y^2) + Terrain + Elevation + Vegetation + River, 
                    family = binomial(), data = survey3)
prob_model_4 <- glm(Found ~ I(x^2) + I(y^2) + x*y + Terrain + Elevation + River, 
                    family = binomial(), data = survey3) 
# Elevation & Vegetation highly correlated

## AIC model selection
AIC(prob_model_1, prob_model_2, prob_model_3, prob_model_4)

## Confidence interval
summary(prob_model_4) 
a <- confint(prob_model_4)
a <- round(a, 3)
a


## -------------------------------------------------------------------------------
## Diagnostics! 
## Goodness of fit
## Hosmer-Lemeshow Test (10 groups)
HosmerLemeshowTest(fitted(prob_model_4), survey3$Found)

## Hosmer-Lemeshow Test (100 groups)
HosmerLemeshowTest(fitted(prob_model_4), survey3$Found, ngr = 100)
# Changing the group does not change the result that the Goodness of fit
# is not rejected. 

## Extract the coefficients
prob_fitted <- augment(prob_model_4)

## Residuals vs. fitted and covariates
prob_resid_x <- prob_fitted %>%
  ggplot(aes(x = x, y = .std.resid)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  ylab("Std. Residual") +
  xlab("X-Coordinates") +
  labs(title = "X-Coordinates vs. Standardized Residual (Logistic)") +
  theme(plot.title = element_text(hjust = 0.5))
  
prob_resid_y <- prob_fitted %>%
  ggplot(aes(x = y, y = .std.resid)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  ylab("Std. Residual") +
  xlab("Y-Coordinates") +
  labs(title = "Y-Coordinates vs. Standardized Residual (Logistic)") +
  theme(plot.title = element_text(hjust = 0.5))

prob_resid_ter <- prob_fitted %>%
  ggplot(aes(x = Terrain, y = .std.resid)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0) +
  ylab("Std. Residual") +
  xlab("Terrain") +
  labs(title = "Terrain vs. Standardized Residual (Logistic)") +
  theme(plot.title = element_text(hjust = 0.5))

prob_resid_river <- prob_fitted %>%
  ggplot(aes(x = River, y = .std.resid)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0) +
  ylab("Std. Residual") +
  xlab("River") +
  labs(title = "River vs. Standardized Residual (Logistic)") +
  theme(plot.title = element_text(hjust = 0.5))

prob_resid_ele <- prob_fitted %>%
  ggplot(aes(x = Elevation, y = .std.resid)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  ylab("Std. Residual") +
  xlab("Elevation") +
  labs(title = "Elevation vs. Standardized Residual (Logistic)") +
  theme(plot.title = element_text(hjust = 0.5))

## Binned residuals
binnedplot(fitted(prob_model_4), resid(prob_model_4))

## Cook's distance 4/126
prob_fitted %>%
  ggplot(aes(x = .fitted, y = .cooksd)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted Value") +
  ylab("Cook's Distance") +
  geom_hline(yintercept = 4/nrow(prob_fitted), linetype = "dashed") +
  labs(title = "Cook's Distance (Logistic)") +
  theme(plot.title = element_text(hjust = 0.5))
  


## -------------------------------------------------------------------------------
## Probability calculation to geo data
geographic3$probability <- predict(prob_model_4, geographic3, type = "response")


## -------------------------------------------------------------------------------
################ Predict the count #################

## X & Y axis vs. Count by River
grid_x_count <- ggplot(aes(x = x, y = Count, color = River), 
                       data = survey2) + 
  geom_point() + 
  geom_smooth(color = "blue", se = FALSE) +
  labs(title = "X-Axis vs. Orchid Count") +
  ylim(0, max(survey2$Count) + 10)

grid_y_count <- ggplot(aes(x = y, y = Count, color = River), 
                       data = survey2) + 
  geom_point() + 
  geom_smooth(color = "blue", se = FALSE) +
  labs(title = "Y-Axis vs. Orchid Count") +
  ylim(0, max(survey2$Count) + 10)

plot_grid(grid_x_count, grid_y_count)

## Terrain x, y axis
grid_x_count2 <- ggplot(aes(x = x, y = Count, color = Terrain), 
                       data = survey2) + 
  geom_point() + 
  geom_smooth(color = "blue", se = FALSE) +
  labs(title = "X-Axis vs. Orchid Count") +
  ylim(0, max(survey2$Count) + 10)

grid_y_count2 <- ggplot(aes(x = y, y = Count, color = Terrain), 
                       data = survey2) + 
  geom_point() + 
  geom_smooth(color = "blue", se = FALSE) +
  labs(title = "Y-Axis vs. Orchid Count") +
  ylim(0, max(survey2$Count) + 10)

plot_grid(grid_x_count2, grid_y_count2)


## -------------------------------------------------------------------------------
## Mean vs. Variance
ngrp <- 20
breaks <- c(0,quantile(survey3$Elevation,(1:(ngrp-1)/ngrp)),
            max(survey3$Elevation))

check <- survey3 %>%
  mutate(Group = cut(Elevation, breaks = breaks, 
                     labels = paste0("Group",1:ngrp))) %>%
  group_by(Group)%>%
  mutate(grp_Ele = mean(Elevation))

check %>%
  group_by(Group) %>%
  summarize(Mean = mean(Count), Var = var(Count)) %>%
  ggplot(aes(x = Mean, y = Var)) +
  geom_point()+
  geom_abline(intercept = 0 , slope = 1) +
  geom_smooth(method = "lm", formula = y ~ x - 1) +
  labs(title = "Mean Variance Equality Check") +
  theme(plot.title = element_text(hjust = 0.5))
  
## Negative-Binomial regression
count_model_1 <- glm.nb(Count ~ I(x^2) + I(y^2) + x*y + Terrain * (River + Elevation + Vegetation), data = survey3)
count_model_2 <- glm.nb(Count ~ I(x^2) + I(y^2) + x*y + Terrain + Elevation + Vegetation + River, data = survey3)
count_model_3 <- glm.nb(Count ~ I(x^2) + I(y^2) + Terrain + Elevation + Vegetation + River, data = survey3)
count_model_4 <- glm.nb(Count ~ I(x^2) + I(y^2) + x*y + Terrain + Elevation + River, data = survey3) 

# Clear definition of likelihood for Negative Binomial, AIC is good
AIC(count_model_1, count_model_2, count_model_3, count_model_4)
summary_nb <- summary(count_model_2)


## Confidence Interval for count
b <- coef(count_model_2)
round(data.frame(exp(b)), 3)
se_nb <- summary_nb$coefficient[,2]
b.ci <- data.frame(Upper = b + 1.96 * se_nb / sqrt(nrow(survey3)), 
          Lower = b - 1.96 * se_nb / sqrt(nrow(survey3)))
round(exp(b.ci), 3)
## Deviance Goodness of fit test
round(pchisq(128.04, 114, lower.tail = FALSE), 3)
#### p=0.174 => no evidence to reject the null that the model fits the data #### well

## Plot variance as a function of mean
check %>%
  group_by(Group) %>%
  summarize(Mean = mean(Count), Var = var(Count)) %>%
  ggplot(aes(x=Mean,y=Var)) +
  geom_point() +
  xlim(c(0,NA)) + 
  ylim(c(0,NA)) + 
  stat_function(fun=function(x) x + 1/2.46 * x^2) +
  labs(title = "Mean Variance Relationship in Negative Binomial") +
  theme(plot.title = element_text(hjust = 0.5))



## -------------------------------------------------------------------------------
## Diagnostics!
## Covariates vs. residuals
nb_fitted <- augment(count_model_2)
nb_fitted %>%
  ggplot(aes(x = x, y = .std.resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  labs(title = "X-Coordinates vs. Standardized Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))

nb_fitted %>%
  ggplot(aes(x = y, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Y-Coordinates vs. Standardized Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))

nb_fitted %>%
  ggplot(aes(x = `I(x^2)`, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "x^2 vs. Standardized Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))

nb_fitted %>%
  ggplot(aes(x = `I(y^2)`, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "y^2 vs. Std. Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))

nb_fitted %>%
  ggplot(aes(x = Elevation, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Elevation vs. Standardized Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))

nb_fitted %>%
  ggplot(aes(x = Terrain, y = .std.resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  labs(title = "Terrain vs. Standardized Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))
  

nb_fitted %>%
  ggplot(aes(x = River, y = .std.resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  labs(title = "River vs. Standardized Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))

nb_fitted %>%
  filter(Vegetation != "none") %>%
  ggplot(aes(x = Vegetation, y = .std.resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  labs(title = "Vegetation vs. Standardized Residual (Negative Binomial)") +
  theme(plot.title = element_text(hjust = 0.5))

## Cook's distance
nb_fitted %>%
  ggplot(aes(x = .fitted, y = .cooksd)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted Value") +
  ylab("Cook's Distance") +
  xlim(-5, 5) +
  geom_hline(yintercept = 4/nrow(nb_fitted), linetype = "dashed") +
  labs(title = "Cook's Distance for Negative Binomial Regression")



## -------------------------------------------------------------------------------
## Predictions for count
geographic3$Count <- predict(count_model_2, geographic3, type = "response")


## -------------------------------------------------------------------------------
## Map production
geo_map <- full_join(geographic, geographic3, by = c("x", "y"))

## Clean the data
geo_map <- geo_map %>%
  dplyr::select(-c("Elevation.y", "River.y", "Terrain.y", "Vegetation.y")) %>%
  rename(Elevation = Elevation.x,
         River = River.x,
         Terrain = Terrain.x,
         Vegetation = Vegetation.x) %>%
  mutate(probability = ifelse(is.na(probability), 0, probability),
         Count = ifelse(is.na(Count), 0, Count))
## Grid map plot (Probability)
geo_map %>%
  ggplot(aes(x = x, y = y, fill = probability)) +
  geom_tile(color = "black", size = 0.1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 100, 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(1, 100, 10)) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Probability Map") +
  theme(plot.title = element_text(hjust = 0.5))

## Grid map plot (Count)
geo_map %>%
  ggplot(aes(x = x, y = y, fill = Count)) +
  geom_tile(color = "black", size = 0.1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 100, 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(1, 100, 10)) +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Abundance Map") +
  theme(plot.title = element_text(hjust = 0.5))


