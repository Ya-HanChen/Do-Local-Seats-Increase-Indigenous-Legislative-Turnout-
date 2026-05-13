library(readxl)
library(dplyr)
library(devtools)
library(tidyverse)
library(rddensity)
library(rdd)
library(rdrobust)
library(devtools)
library(RDDtools)


Indi <- readxl::read_excel("/Users/chenyahan/Library/CloudStorage/Dropbox/Research/碩士論文/平地原住民/Indi_data_new.xlsx")
head(Indi)
summary(Indi$原住民)
table(Indi$議員應選席次)

Indi <- Indi%>%
  mutate(原住民選舉人數 = age20_39 + age40_59 + age60)

summary(Indi$原住民選舉人數)


summary(Indi$原住民[Indi$議員應選席次 <= 0])
summary(Indi$原住民[Indi$議員應選席次 ==1])
summary(Indi$原住民[Indi$議員應選席次 ==2])
summary(Indi$原住民[Indi$議員應選席次 >=3])



Indi_data <- filter(Indi, Indi$議員應選席次 <=3)
summary(Indi_data$原住民)



## running variable

## c1 
Indi_data <- Indi_data %>%
  mutate(
    c1 = case_when(
      縣市別 == "直轄市" & 議員年份 %in% c(1994, 1998, 2002, 2006) ~ 4000,
      縣市別 == "縣市"   & 議員年份 %in% c(1994, 1998, 2002, 2005) ~ 1500,
      縣市別 == "直轄市" & 議員年份 %in% c(2010, 2014, 2018, 2022) ~ 2000, 
      縣市別 == "縣市"   & 議員年份 %in% c(2009, 2014, 2018, 2022) ~ 1500,
      TRUE ~ NA_real_))

## c2 
Indi_data <- Indi_data %>%
  mutate(
    c2 = case_when(
      縣市別 == "直轄市" & 議員年份 %in% c(1994, 1998, 2002, 2006) ~ 14000,
      縣市別 == "縣市"   & 議員年份 %in% c(1994, 1998, 2002, 2005) ~ 11500,
      縣市別 == "直轄市" & 議員年份 %in% c(2010, 2014, 2018, 2022) ~ 12000, 
      縣市別 == "縣市"   & 議員年份 %in% c(2009, 2014, 2018, 2022) ~ 11500,
      TRUE ~ NA_real_))

## c3 
Indi_data <- Indi_data %>%
  mutate(
    c3 = case_when(
      縣市別 == "直轄市" & 議員年份 %in% c(1994, 1998, 2002, 2006) ~ 24000,
      縣市別 == "縣市"   & 議員年份 %in% c(1994, 1998, 2002, 2005) ~ 21500,
      縣市別 == "直轄市" & 議員年份 %in% c(2010, 2014, 2018, 2022) ~ 22000, 
      縣市別 == "縣市"   & 議員年份 %in% c(2009, 2014, 2018, 2022) ~ 21500,
      TRUE ~ NA_real_))



Indi_data <- Indi_data %>%
  mutate(
    mid12 = c1 + (c2 - c1) / 2,
    mid23 = c2 + (c3 - c2) / 2, 
    
    running = case_when(
      原住民 <= mid12 ~ (原住民 - c1),
      原住民 >  mid12 & 原住民 <= mid23 ~ (原住民 - c2),
      原住民 >  mid23 ~ (原住民 - c3),
      
      TRUE ~ NA_real_
    ) 
  )

summary(Indi_data$running) 

Indi_data <- Indi_data %>%
  mutate(
    treat   = as.integer(running >= 0)
  )


## standardized running variable (score)
Indi_data <- Indi_data %>%
  mutate(
    mid12 = c1 + (c2 - c1) / 2,
    mid23 = c2 + (c3 - c2) / 2, 
    score = case_when(
      原住民 <= mid12 ~ (原住民 - c1)/c1,
      原住民 >  mid12 & 原住民 <= mid23 ~ (原住民 - c2)/c2,
      原住民 >  mid23 ~ (原住民 - c3)/c3,
      
      TRUE ~ NA_real_
    ) 
  )

Indi_data <- Indi_data %>%
  mutate(
    treat_score   = as.integer(score >= 0)
  )

summary(Indi_data$score) 


######### validity of the RDD ########
## density test, Continuity-Based Approach
density <- rddensity(Indi_data$running, c = 0)
summary(density)
rdplotdensity(density, Indi_data$running)


# Local Randomization Approach
out <- rdrandinf(Indi_data$立委投票率, Indi_data$running, wl = -1000, wr = 1000, seed = 50)
summary(out)

# the sensitivity of the estimate to different bandwidths
test <- RDDdata(y = Indi_data$立委投票率, x = Indi_data$running, cutpoint = 0)
summary(test)
reg_para <- RDDreg_lm(RDDobject = test, order = 1)
reg_para
plot(reg_para)

bw_ik <- RDDbw_IK(test)
reg_nonpara <- RDDreg_np(RDDobject = test, bw = bw_ik)
print(reg_nonpara)
dens_test(reg_nonpara)
plot(x = reg_nonpara)
plotSensi(reg_nonpara, from = 2000, to = 20000, by = 100, level = 0.95)
plotPlacebo(reg_nonpara, level=95) ## estimating the RDD effect based on fake cutpoints

reg_nonpara_lminf <- RDDreg_np(RDDobject=test, inference="lm")
plotPlacebo(reg_nonpara_lminf, level=95) ## estimating the RDD effect based on fake cutpoints


plotSensi(reg_nonpara_lminf, from = 2000, to = 20000, by = 100, level = 0.95)

## histogram 
ggplot(Indi_data, aes(x = running)) +
  geom_histogram(bins = 20, fill = "grey70", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.9) +
  labs(
    title = "",
    x = "Distance to the Population Threshold", 
    y = "Number of Districts"
  ) +
  theme_classic(base_size = 14)

ggplot(Indi_data, aes(x = running)) +
  geom_histogram(bins = 20, fill = "grey70", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.9) +
  coord_cartesian(xlim = c(-5000, 5000)) +   # 👈 加這行
  labs(
    x = "Distance to the Population Threshold", 
    y = "Number of Districts"
  ) +
  theme_classic(base_size = 14)

## scatter plot
ggplot(Indi_data, aes(x = running, y = 立委投票率)) +
  geom_point(alpha = 0.3) +
  geom_vline(xintercept = 0, color = "red") +
  labs(x = "Running variable", y = "Legislative Turnout Rate")

ggplot(Indi_data, aes(x = running, y = 立委投票率)) +
  geom_point(alpha = 0.3) +
  geom_vline(xintercept = 0, color = "red") +
  coord_cartesian(xlim = c(-5000, 5000)) +
  labs(x = "Running variable", y = "Legislative Turnout Rate")

## difference in mean
mean(Indi_data$立委投票率[Indi_data$treat ==1])
mean(Indi_data$立委投票率[Indi_data$treat ==0])



## rdd 
# Global
h100 <- max(abs(Indi_data$running))
rdrobust(Indi_data$立委投票率, Indi_data$running, p=2, c=0, h=h100, all=TRUE)%>%
  summary()
rdplot(Indi_data$立委投票率, Indi_data$running, p=1, c=0, h=h100)

# optimal bandwidth
rdrobust(Indi_data$立委投票率, Indi_data$running, p=1, c=0,  all=TRUE, 
         bwselect="mserd", 
         kernel = "tri")%>%
  summary()
rdplot(Indi_data$立委投票率, Indi_data$running, p=1, c=0, x.label = "Population", y.label = "Turnout Rate", title = "")


# 10000
rdrobust(Indi_data$立委投票率, Indi_data$running, p=2, c=0, h=10000,  all=TRUE)%>%
  summary()
rdplot(Indi_data$立委投票率, Indi_data$running, p=1, c=0, h=10000)

# 5000
rdrobust(Indi_data$立委投票率, Indi_data$running, p=2, c=0, h=5000,  all=TRUE)%>%
  summary()
rdplot(Indi_data$立委投票率, Indi_data$running, p=1, c=0, h=5000)

# 2500
rdrobust(Indi_data$立委投票率, Indi_data$running, p=2, c=0, h=2500,  all=TRUE)%>%
  summary()
rdplot(Indi_data$立委投票率, Indi_data$running, p=1, c=0, h=2500)

# MSE 
rdrobust(Indi_data$立委投票率, Indi_data$running, p=2, c=0, bwselect = "mserd",  all=TRUE)%>%
  summary()
rdplot(Indi_data$立委投票率, Indi_data$running, p=1, c=0, bwselect = "mserd")

## figure
Indi_data$group_color <- ifelse(Indi_data$treat > 0, "Treated", "Untreated")

# Global
ggplot(Indi_data, aes(x = running, y = 立委投票率, color = group_color)) +
  geom_point(aes(shape = group_color), alpha = 0.8, size = 2) +
  
  # 括號內 degree=4 表示使用 4 次多項式回歸，se=TRUE 就會畫信賴區間
  geom_smooth(data = subset(Indi_data, running > 0),
              method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              color = "red") +
  
  geom_smooth(data = subset(Indi_data, running <= 0),
              method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              color = "red") +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
  scale_color_manual(values = c("Treated" = "blue", "Untreated" = "blue")) +
  scale_shape_manual(values = c("Treated" = 16, "Untreated" = 1)) +
  labs(x = "Distance to the Population Threshold",
       y = "Legislative Turnout Rate") +
  coord_cartesian(xlim = c(-5000, 5000), ylim = c(0, 1)) +
  theme_minimal()+
  theme(legend.position = "none") 

ggplot(Indi_data, aes(x = running, y = 立委投票率, color = group_color)) +
  geom_point(aes(shape = group_color), alpha = 0.8, size = 2) +
  geom_smooth(data = subset(Indi_data, running > 0 & running < 10000),
              method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              color = "red") +
  
  geom_smooth(data = subset(Indi_data, running <= 0 & running > -10000),
              method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              color = "red") +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
  scale_color_manual(values = c("Treated" = "blue", "Untreated" = "blue")) +
  scale_shape_manual(values = c("Treated" = 16, "Untreated" = 1)) +
  labs(x = "Distance to the Population Threshold",
       y = "Legislative Turnout Rate") +
  coord_cartesian(xlim = c(-5000, 5000), ylim = c(0, 1)) +
  theme_minimal()+
  theme(legend.position = "none") 


## mean
mean_left <- Indi_data %>%
  filter(running < 0 & running >-10000) %>%
  summarise(m = mean(立委投票率, na.rm = TRUE)) %>%
  pull(m)

mean_right <- Indi_data %>%
  filter(running >= 0 & running<10000) %>%
  summarise(m = mean(立委投票率, na.rm = TRUE)) %>%
  pull(m)

ggplot(Indi_data, aes(x = running, y = 立委投票率, color = group_color)) +
  geom_point(aes(shape = group_color), alpha = 0.6, size = 2) +
  
  geom_segment(aes(x = min(running), xend = 0,
                   y = mean_left, yend = mean_left),
               inherit.aes = FALSE,
               color = "red", linewidth = 1) +
  
  geom_segment(aes(x = 0, xend = max(running),
                   y = mean_right, yend = mean_right),
               inherit.aes = FALSE,
               color = "red", linewidth = 1) +
  
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 1) +
  
  scale_color_manual(values = c("Treated" = "blue", "Untreated" = "blue")) +
  scale_shape_manual(values = c("Treated" = 16, "Untreated" = 1)) +
  
  coord_cartesian(xlim = c(-5000, 5000), ylim = c(0, 1)) +
  
  labs(x = "Distance to the Population Threshold",
       y = "Legislative Turnout Rate") +
  theme_minimal() +
  theme(legend.position = "none")



## balance checks
Indi_data <- Indi_data %>%
  mutate(
    female_rate = Indi_data$女原住民/Indi_data$原住民, 
    male_rate = Indi_data$男原住民/Indi_data$原住民, 
    age0_19_rate = Indi_data$age0_19/Indi_data$原住民, 
    age20_39_rate = Indi_data$age20_39/Indi_data$原住民, 
    age40_59_rate = Indi_data$age40_59/Indi_data$原住民, 
    age60more_rate = Indi_data$age60/Indi_data$原住民, 
    age_all = age0_19_rate +age20_39_rate+age40_59_rate+age60more_rate
  )

summary(Indi_data$age_all)

mean(Indi_data$female_rate[Indi_data$treat ==1])
mean(Indi_data$female_rate[Indi_data$treat ==0])

mean(Indi_data$male_rate[Indi_data$treat ==1])
mean(Indi_data$male_rate[Indi_data$treat ==0])

rdrobust(Indi_data$female_rate, Indi_data$running, p=1, c=0,  all=TRUE, 
         bwselect="mserd", 
         kernel = "tri")%>%
  summary()

rdplot(Indi_data$female_rate, Indi_data$running, p=1, c=0, kernel = "tri",
       x.lim = c(-5000, 5000),
       y.lim = c(0, 1), 
       x.label = "Distance to the Population Threshold", 
       y.label = "Female Indigenous Population Rate",
       title = "Placebo Test: Female Indigenous Population Rate")

rdrobust(Indi_data$male_rate, Indi_data$running, p=1, c=0, all=TRUE, 
         bwselect="mserd", 
         kernel = "tri")%>%
  summary()

rdplot(Indi_data$male_rate, Indi_data$running, p=1, c=0, kernel = "tri",
       x.lim = c(-5000, 5000),
       y.lim = c(0, 1), 
       x.label = "Distance to the Population Threshold", 
       y.label = "Male Indigenous Population Rate",
       title = "Placebo Test: Male Indigenous Population Rate")


mean(Indi_data$age0_19_rate[Indi_data$treat ==1])
mean(Indi_data$age0_19_rate[Indi_data$treat ==0])

rdrobust(Indi_data$age0_19_rate, Indi_data$running, p=1, c=0,  all=TRUE, 
         bwselect="mserd", 
         kernel = "tri")%>%
  summary()

rdplot(Indi_data$age0_19_rate, Indi_data$running, p=1, c=0, kernel = "tri",
       x.lim = c(-5000, 5000),
       y.lim = c(0, 1), 
       x.label = "Distance to the Population Threshold", 
       y.label = "Indigenous Population Share (Aged 0–19)",
       title = "Placebo Test: Indigenous Population Share (Aged 0–19)")

mean(Indi_data$age20_39_rate[Indi_data$treat ==1])
mean(Indi_data$age20_39_rate[Indi_data$treat ==0])

rdrobust(Indi_data$age20_39_rate, Indi_data$running, p=1, c=0,  all=TRUE, 
         bwselect="mserd", 
         kernel = "tri")%>%
  summary()

rdplot(Indi_data$age20_39_rate, Indi_data$running, p=1, c=0, kernel = "tri",
       x.lim = c(-5000, 5000),
       y.lim = c(0, 1), 
       x.label = "Distance to the Population Threshold", 
       y.label = "Indigenous Population Share (Aged 20–39)",
       title = "Placebo Test: Indigenous Population Share (Aged 20–39)")

mean(Indi_data$age40_59_rate[Indi_data$treat ==1])
mean(Indi_data$age40_59_rate[Indi_data$treat ==0])

rdrobust(Indi_data$age40_59_rate, Indi_data$running, p=1, c=0,  all=TRUE, 
         bwselect="mserd", 
         kernel = "tri")%>%
  summary()

rdplot(Indi_data$age40_59_rate, Indi_data$running, p=1, c=0, kernel = "tri",
       x.lim = c(-5000, 5000),
       y.lim = c(0, 1), 
       x.label = "Distance to the Population Threshold", 
       y.label = "Indigenous Population Share (Aged 40–59)",
       title = "Placebo Test: Indigenous Population Share (Aged 40–59)")

mean(Indi_data$age60more_rate[Indi_data$treat ==1])
mean(Indi_data$age60more_rate[Indi_data$treat ==0])

rdrobust(Indi_data$age60more_rate, Indi_data$running, p=1, c=0,  all=TRUE, 
         bwselect="mserd", 
         kernel = "tri")%>%
  summary()

rdplot(Indi_data$age60more_rate, Indi_data$running, p=1, c=0, kernel = "tri",
       x.lim = c(-5000, 5000),
       y.lim = c(0, 1), 
       x.label = "Distance to the Population Threshold", 
       y.label = "Indigenous Population Share (Aged 60 +)",
       title = "Placebo Test: Indigenous Population Share (Aged 60 +)")

library(modelsummary)

models <- list(
  "Female" = lm(female_rate ~ treat, data = Indi_data),
  "Male" = lm(male_rate ~ treat, data = Indi_data),
  "Age 0-19" = lm(age0_19_rate ~ treat, data = Indi_data),
  "Age 20-39" = lm(age20_39_rate ~ treat, data = Indi_data),
  "Age 40-59" = lm(age40_59_rate ~ treat, data = Indi_data),
  "Age 60+" = lm(age60more_rate ~ treat, data = Indi_data)
)

modelsummary(models)


Indi_data <- Indi_data %>%
  mutate(
    dependency_ratio= (age0_19 + age60) / (age20_39 + age40_59)
  )
summary(Indi_data$dependency_ratio)

Indi_data <- Indi_data %>%
  mutate(high_dep = dependency_ratio > median(dependency_ratio))

rdrobust(y = Indi_data$立委投票率[Indi_data$high_dep == 1],
         x = Indi_data$running[Indi_data$high_dep == 1], 
         c=0, p=1)%>%
  summary()

rdrobust(y = Indi_data$立委投票率[Indi_data$high_dep == 0],
         x = Indi_data$running[Indi_data$high_dep == 0], 
         c=0, p=1)%>%
  summary()


ggplot(Indi, aes(x = 議員應選席次, y = 立委投票率)) +
  geom_point(alpha = 0.5, color = "#2C7BB6") +  
  geom_smooth(method = "loess", 
              se = TRUE, 
              color = "#D55E00", 
              fill = "#D55E00",
              size = 1.2) +                     
  labs(
    title = "",
    x = "Number of Reserved Seats in Local Council Elections",
    y = "Indigenous Turnout in Legislative Elections"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank()
  )

ggplot(Indi_data, aes(x = 議員應選席次, y = 立委投票率)) +
  geom_point(alpha = 0.5, color = "#2C7BB6") +  
  geom_smooth(method = "loess", 
              se = TRUE, 
              color = "#D55E00", 
              fill = "#D55E00",
              size = 1.2) +                     
  labs(
    title = "",
    x = "Number of Reserved Seats in Local Council Elections",
    y = "Indigenous Turnout in Legislative Elections"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank()
  )

