## sensitivity_analyses.R: sensitivity analyses of false killer whale age
## estimates 

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 13 Apr 2025

## --------------------------------------------------------------------------- ##

## load packages 
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)

## directory objects
results <- here("Results")
ests <- here("Estimates")

## read in estimates ## ------------------------------------------------------ ##
ages <- read.csv(here(ests, "Pc_photoid_age_ests_for_R_2025Mar27.csv"))

# review
summary(ages)
str(ages)
unique(ages$sex)
unique(ages$age_first)
unique(ages$id)
length(unique(ages$id))

## sensitivity: adjust ages as if we mis-classified age class ## ------------- ##
colnames(ages)

# over estimates
ages_over <- ages %>%
  mutate(
    age_best_new = case_when(
      age_first == 10 ~ age_best - 4, # mis-classified sub-adult as adult
      age_first == 6 ~ age_best - 3, # mis-classified juvenile as sub-adult
      age_first == 3 ~ age_best - 2, # mis-classified calf as juvenile
      age_first == 15 ~ age_best - 5, # mis-classified 10yr male as 15yr male with DPM
      age_first == 25 ~ age_best - 10, # mis-classified 15yr male as 25yr male with PM
      TRUE ~ age_best),
    age_min_new = case_when(
      age_first == 10 ~ age_min - 4, # mis-classified sub-adult as adult
      age_first == 6 ~ age_min - 3, # mis-classified juvenile as sub-adult
      age_first == 3 ~ age_min - 2, # mis-classified calf as juvenile
      age_first == 15 ~ age_min - 5, # mis-classified 10yr male as 15yr male with DPM
      age_first == 25 ~ age_min - 10, # mis-classified 15yr male as 25yr male with PM
    TRUE ~ age_min),
    age_max_new = case_when(
      age_first == 10 ~ age_max - 4, # mis-classified sub-adult as adult
      age_first == 6 ~ age_max - 3, # mis-classified juvenile as sub-adult
      age_first == 3 ~ age_max - 2, # mis-classified calf as juvenile
      age_first == 15 ~ age_max - 5, # mis-classified 10yr male as 15yr male with DPM
      age_first == 25 ~ age_max - 10, # mis-classified 15yr male as 25yr male with PM
    TRUE ~ age_max)
  )

# under estimates
ages_under <- ages %>%
  mutate(
    age_best_new = case_when(
      age_first == 10 ~ age_best + 4, # mis-classified sm adult as younger than it is
      age_first == 6 ~ age_best + 3, # mis-classified sm adult as sub-adult
      age_first == 3 ~ age_best + 2, # mis-classified sub-adult as juvenile
      age_first == 15 ~ age_best + 5, # mis-classified 20yr male as 15yr male with DPM
      age_first == 25 ~ age_best + 10, # mis-classified 35yr male as 25yr male with PM
    TRUE~ age_best),
    age_min_new = case_when(
      age_first == 10 ~ age_min + 4, # mis-classified sub-adult as adult
      age_first == 6 ~ age_min + 3, # mis-classified juvenile as sub-adult
      age_first == 3 ~ age_min + 2, # mis-classified calf as juvenile
      age_first == 15 ~ age_min + 5, # mis-classified 10yr male as 15yr male with DPM
      age_first == 25 ~ age_min + 10, # mis-classified 15yr male as 25yr male with PM
    TRUE ~ age_best),
    age_max_new = case_when(
      age_first == 10 ~ age_max + 4, # mis-classified sub-adult as adult
      age_first == 6 ~ age_max + 3, # mis-classified juvenile as sub-adult
      age_first == 3 ~ age_max + 2, # mis-classified calf as juvenile
      age_first == 15 ~ age_max + 5, # mis-classified 10yr male as 15yr male with DPM
      age_first == 25 ~ age_max + 10, # mis-classified 15yr male as 25yr male with PM
    TRUE ~ age_best)
  )

## plots ## ------------------------------------------------------------------ ##

# median values of original age estimates
meds <- ages %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best),
    med_min = median(age_min),
    med_max = median(age_max),
    max_best = max(age_best),
    max_min = max(age_min),
    max_max = max(age_max)
  )

# best original
best <- ggplot(ages, aes(x = age_best, color = sex)) +
  geom_density() +
  geom_density(data = ages, aes(x = age_best), color = "grey40") +
  geom_vline(xintercept = median(ages$age_best), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 
best

# min original
min <- ggplot(ages, aes(x = age_min, color = sex)) +
  geom_density() +
  geom_density(data = ages, aes(x = age_min), color = "grey40") +
  geom_vline(xintercept = median(ages$age_min), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 
min

# max original
max <- ggplot(ages, aes(x = age_max, color = sex)) +
  geom_density() +
  geom_density(data = ages, aes(x = age_max), color = "grey40") +
  geom_vline(xintercept = median(ages$age_max), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

max

# median estimates for overestimated ages
meds_over <- ages_over %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best_new),
    med_min = median(age_min_new),
    med_max = median(age_max_new)
  )

# best over
best_over <- ggplot(ages_over, aes(x = age_best_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over, aes(x = age_best_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over$age_best_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# min over
min_over <- ggplot(ages_over, aes(x = age_min_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over, aes(x = age_min_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over$age_min_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# max over
max_over <- ggplot(ages_over, aes(x = age_max_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over, aes(x = age_max_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over$age_max_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# median estimates for underestimated ages 
meds_under <- ages_under %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best_new),
    med_min = median(age_min_new),
    med_max = median(age_max_new)
  )

# best under
best_under <- ggplot(ages_under, aes(x = age_best_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under, aes(x = age_best_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under$age_best_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# min under
min_under <- ggplot(ages_under, aes(x = age_min_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under, aes(x = age_min_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under$age_min_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# max under
max_under <- ggplot(ages_under, aes(x = age_max_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under, aes(x = age_max_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under$age_max_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# combine
ggarrange(best, min, max, best_over, min_over, max_over, best_under, min_under,
          max_under, nrow = 3, ncol = 3, common.legend = T)

# save
ggsave(here("results","fkw_catalog_ages_sensitivity_over_under_est_ageclass_densplot_2025Apr08.png"),
       width = 9, height = 5, units = "in", bg = "white")

## do the same thing as above but for CR3+ ## -------------------------------- ##

# filter based on confidence rating
ages3 <- filter(ages, cr >= 3)
ages_over3 <- filter(ages_over, cr >= 3)
ages_under3 <- filter(ages_under, cr >= 3)

# get median estimates 
meds3 <- ages3 %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best),
    med_min = median(age_min),
    med_max = median(age_max)
  )

best3 <- ggplot(ages3, aes(x = age_best, color = sex)) +
  geom_density() +
  geom_density(data = ages3, aes(x = age_best), color = "grey40") +
  geom_vline(xintercept = median(ages3$age_best), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds3, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 
best3

min3 <- ggplot(ages3, aes(x = age_min, color = sex)) +
  geom_density() +
  geom_density(data = ages3, aes(x = age_min), color = "grey40") +
  geom_vline(xintercept = median(ages3$age_min), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds3, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 
min3

max3 <- ggplot(ages3, aes(x = age_max, color = sex)) +
  geom_density() +
  geom_density(data = ages3, aes(x = age_max), color = "grey40") +
  geom_vline(xintercept = median(ages3$age_max), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds3, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

max3

# median estimates for overestimated ages
meds_over3 <- ages_over3 %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best_new),
    med_min = median(age_min_new),
    med_max = median(age_max_new)
  )

best_over3 <- ggplot(ages_over3, aes(x = age_best_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over3, aes(x = age_best_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over3$age_best_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over3, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

min_over3 <- ggplot(ages_over3, aes(x = age_min_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over3, aes(x = age_min_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over3$age_min_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over3, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

max_over3 <- ggplot(ages_over, aes(x = age_max_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over3, aes(x = age_max_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over3$age_max_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over3, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# median age estimates for underestimated ages
meds_under3 <- ages_under3 %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best_new),
    med_min = median(age_min_new),
    med_max = median(age_max_new)
  )

best_under3 <- ggplot(ages_under3, aes(x = age_best_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under3, aes(x = age_best_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under3$age_best_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under3, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

min_under3 <- ggplot(ages_under3, aes(x = age_min_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under3, aes(x = age_min_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under3$age_min_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under3, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

max_under3 <- ggplot(ages_under3, aes(x = age_max_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under3, aes(x = age_max_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under3$age_max_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under3, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 


# combine
ggarrange(best3, min3, max3, best_over3, min_over3, max_over3, best_under3, min_under3,
          max_under3, nrow = 3, ncol = 3, common.legend = T)

# save
ggsave(here("results","fkw_catalog_ages_sensitivity_over_under_est_ageclass_densplot_CR3plus_2025Apr08.png"),
       width = 9, height = 5, units = "in", bg = "white")

## do the same thing as above but for CR4+ ## -------------------------------- ##

# filter based on confidence rating 
ages4 <- filter(ages, cr >= 4)
ages_over4 <- filter(ages_over, cr >= 4)
ages_under4 <- filter(ages_under, cr >= 4)

# median age estimates 
meds4 <- ages4 %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best),
    med_min = median(age_min),
    med_max = median(age_max)
  )

best4 <- ggplot(ages4, aes(x = age_best, color = sex)) +
  geom_density() +
  geom_density(data = ages4, aes(x = age_best), color = "grey40") +
  geom_vline(xintercept = median(ages4$age_best), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds4, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 
best4

min4 <- ggplot(ages4, aes(x = age_min, color = sex)) +
  geom_density() +
  geom_density(data = ages4, aes(x = age_min), color = "grey40") +
  geom_vline(xintercept = median(ages4$age_min), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds4, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 
min4

max4 <- ggplot(ages4, aes(x = age_max, color = sex)) +
  geom_density() +
  geom_density(data = ages4, aes(x = age_max), color = "grey40") +
  geom_vline(xintercept = median(ages4$age_max), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds4, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

max4

# median estimates for overestimated ages
meds_over4 <- ages_over4 %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best_new),
    med_min = median(age_min_new),
    med_max = median(age_max_new)
  )

best_over4 <- ggplot(ages_over4, aes(x = age_best_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over4, aes(x = age_best_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over4$age_best_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over4, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

min_over4 <- ggplot(ages_over4, aes(x = age_min_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over4, aes(x = age_min_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over4$age_min_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over4, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

max_over4 <- ggplot(ages_over, aes(x = age_max_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_over4, aes(x = age_max_new), color = "grey40") +
  geom_vline(xintercept = median(ages_over4$age_max_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_over4, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

# median age estimates for underestimated ages
meds_under4 <- ages_under4 %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best_new),
    med_min = median(age_min_new),
    med_max = median(age_max_new)
  )

best_under4 <- ggplot(ages_under4, aes(x = age_best_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under4, aes(x = age_best_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under4$age_best_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under4, aes(color = sex, xintercept = med_best), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

min_under4 <- ggplot(ages_under4, aes(x = age_min_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under4, aes(x = age_min_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under4$age_min_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under4, aes(color = sex, xintercept = med_min), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 

max_under4 <- ggplot(ages_under4, aes(x = age_max_new, color = sex)) +
  geom_density() +
  geom_density(data = ages_under4, aes(x = age_max_new), color = "grey40") +
  geom_vline(xintercept = median(ages_under4$age_max_new), linetype = "dashed") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  geom_vline(data = meds_under4, aes(color = sex, xintercept = med_max), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  labs(color = "") +
  theme_classic() 


# combine
ggarrange(best4, min4, max4, best_over4, min_over4, max_over4, best_under4, min_under4,
          max_under4, nrow = 3, ncol = 3, common.legend = T)

# save
ggsave(here("results","fkw_catalog_ages_sensitivity_over_under_est_ageclass_densplot_CR4plus_2025Apr08.png"),
       width = 9, height = 5, units = "in", bg = "white")

## compare distribution of age estimates with and without aux ## ------------- ##

# remove aux from age estimates
ages <- ages %>%
  mutate(
    age_best_nx = age_best - aux,
    age_min_nx = age_min - aux,
    age_max_nx = age_max - aux
  )

best_nx <- ggplot() +
  geom_density(data = ages, aes(x = age_best, color = sex)) +
  geom_density(data = ages, aes(x = age_best_nx, color = sex), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_best)) +
  geom_vline(xintercept = median(ages$age_best_nx), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
best_nx

min_nx <- ggplot() +
  geom_density(data = ages, aes(x = age_min, color = sex)) +
  geom_density(data = ages, aes(x = age_min_nx, color = sex), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_min)) +
  geom_vline(xintercept = median(ages$age_min_nx), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
min_nx

max_nx <- ggplot() +
  geom_density(data = ages, aes(x = age_max, color = sex)) +
  geom_density(data = ages, aes(x = age_max_nx, color = sex), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_max)) +
  geom_vline(xintercept = median(ages$age_max_nx), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
max_nx

# combine 
ggarrange(best_nx, min_nx, max_nx, ncol = 3, nrow = 1, common.legend = T)

# save
ggsave(here("results","fkw_catalog_ages_sensitivity_auxnoaux_densplot_bysex_2025Apr08.png"),
       width = 9, height = 3, units = "in", bg = "white")

nx_sm <- ages %>%
  group_by(sex) %>%
  summarise(
    med_best = median(age_best),
    med_min = median(age_min),
    med_max = median(age_max),
    med_best_nx = median(age_best_nx),
    med_min_nx = median(age_min_nx),
    med_max_nx = median(age_max_nx)
  )

## what if we don't know sex? ## --------------------------------------------- ##

# first, assume that we have confidence in PM males, but not DPM males. so if 
# individual was first seen as DPM, remove 5 years. also, remove any AUX related
# to genetic parentage 

ages <- ages %>%
  mutate(
    age_best_s = case_when(
      age_first == 15 & id != "HIPc132" ~ age_best - 5,
      aux_type == "parentage" ~ age_best - aux,
      TRUE ~ age_best
    ),
    age_min_s = case_when(
      age_first == 15 & id != "HIPc132" ~ age_min - 4,
      aux_type == "parentage" ~ age_min - aux,
      TRUE ~ age_min
    ),
    age_max_s = case_when(
      age_first == 15 & id != "HIPc132" & cr == 4 ~ age_max + 5,
      age_first == 15 & id != "HIPc132" & cr == 5 ~ age_max + 5,
      age_first == 15 & id != "HIPc132" & cr == 3 ~ 65,
      aux_type == "parentage" ~ age_max - aux,
      TRUE ~ age_max
    )
  )

# check
chk <- ages[,c(1:15,25:27)]

# make density plot 
best_s <- ggplot() +
  geom_density(data = ages, aes(x = age_best)) +
  geom_density(data = ages, aes(x = age_best_s), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_best)) +
  geom_vline(xintercept = median(ages$age_best_s), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
best_s

min_s <- ggplot() +
  geom_density(data = ages, aes(x = age_min)) +
  geom_density(data = ages, aes(x = age_min_s), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_min)) +
  geom_vline(xintercept = median(ages$age_min_s), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
min_s

max_s <- ggplot() +
  geom_density(data = ages, aes(x = age_max)) +
  geom_density(data = ages, aes(x = age_max_s), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_max)) +
  geom_vline(xintercept = median(ages$age_max_s), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
max_s

# combine 
ggarrange(best_s, min_s, max_s, ncol = 3, nrow = 1, common.legend = T)

# save
ggsave(here("results","fkw_catalog_ages_sensitivity_nogeneticsex_densplot_2025Apr08.png"),
       width = 9, height = 3, units = "in", bg = "white")

# add sex if we didn't have any sexually dimorphic characteristics 
ages <- ages %>%
  mutate(
    age_best_s2 = case_when(
      age_first == 15 & id != "HIPc132"~ age_best - 5,
      age_first == 25 ~ age_best - 15,
      aux_type == "parentage" ~ age_best - aux,
      id == "HIPc132" ~ age_best - 13,
      TRUE ~ age_best
    ),
    age_min_s2 = case_when(
      age_first == 15 & id != "HIPc132"~ age_min - 4,
      age_first == 25 ~ age_min - 9,
      aux_type == "parentage" ~ age_min - aux,
      id == "HIPc132" ~ age_min - 13,
      TRUE ~ age_min
    ),
    age_max_s2 = case_when(
      age_first %in% c(15,25) & cr == 4 ~ age_max + 5, 
      age_first %in% c(15,25) & cr == 5 ~ age_max + 5,
      age_first %in% c(15,25) & cr == 3 ~ 65, # this rule applies to HIPc132, so no additional conditions for this ID
      aux_type == "parentage" ~ age_max - aux,
      TRUE ~ age_max
    )
  )

s_sm <- ages %>%
  summarise(
    med_best = median(age_best),
    med_min = median(age_min),
    med_max = median(age_max),
    med_best_s = median(age_best_s),
    med_min_s = median(age_min_s),
    med_max_s = median(age_max_s),
    med_best_s2 = median(age_best_s2),
    med_min_s2 = median(age_min_s2),
    med_max_s2 = median(age_max_s2)
  )

# density plots
best_s2 <- ggplot() +
  geom_density(data = ages, aes(x = age_best)) +
  geom_density(data = ages, aes(x = age_best_s2), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_best)) +
  geom_vline(xintercept = median(ages$age_best_s2), linetype = "dashed") +
  xlab("Best age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
best_s2

min_s2 <- ggplot() +
  geom_density(data = ages, aes(x = age_min)) +
  geom_density(data = ages, aes(x = age_min_s2), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_min)) +
  geom_vline(xintercept = median(ages$age_min_s2), linetype = "dashed") +
  xlab("Minimum age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
min_s2

max_s2 <- ggplot() +
  geom_density(data = ages, aes(x = age_max)) +
  geom_density(data = ages, aes(x = age_max_s2), linetype = "dashed") +
  geom_vline(xintercept = median(ages$age_max)) +
  geom_vline(xintercept = median(ages$age_max_s2), linetype = "dashed") +
  xlab("Maximum age estimate (yrs)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(seq(0,65, by = 5)),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "") +
  theme_classic() 
max_s2

# combine
ggarrange(best_s, min_s, max_s, best_s2, min_s2, max_s2, ncol = 3, nrow = 2, common.legend = T)

# save
ggsave(here("results","fkw_catalog_ages_sensitivity_nogeneticsex_densplot_2025Apr08v2.png"),
       width = 9, height = 6, units = "in", bg = "white")


## flip max age rule: based on maximum age of age class above that when first seen ## 

ages <- ages %>%
  mutate(
    age_max_n = case_when(
      ac_first == "Calf" ~ span_first_biop + 5,
      ac_first == "Juvenile" ~ span_first_biop + 9,
      ac_first == "Sub-adult" & sex == "Female" ~ 65 - span_last_biop,
      ac_first == "Sub-adult" & sex == "Male" ~ span_first_biop + 25,
      age_first >= 10 ~ 65 - span_last_biop
    )
  )


bar <- ggplot(ages, aes(x = age_best, y = reorder(id_sampn, age_best), color = as.factor(cr))) +
  geom_errorbar(data = ages, aes(xmin = age_min,
                                 xmax = age_max,
                                 color = as.factor(cr)), size = 1.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  theme_classic() +
  xlab("Best age estimate (yrs)") +
  ylab("Individual*") +
  facet_wrap(~sex, scales = "free_y") +
  #ggtitle("Males") +
  labs(color = stringr::str_wrap("Confidence rating", 10)) +
  scale_color_manual(limits = c("2","3","4","5"),
                     values = rev(shuk5)) +
  #scale_color_brewer(palette = "Purples") +
  scale_x_continuous(limits = c(0, 65),
                     breaks = seq(0,60, by = 10)) +
  theme(
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    #title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.1),
    strip.background = element_rect(color = NA),
    strip.text = element_text(size = 11)
    
  )
bar

n_bar <- ggplot(ages, aes(x = age_best, y = reorder(id_sampn, age_best), color = as.factor(cr))) +
  geom_errorbar(data = ages, aes(xmin = age_min,
                               xmax = age_max_n,
                               color = as.factor(cr)), size = 1.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  theme_classic() +
  xlab("Best age estimate (yrs)") +
  ylab("Individual*") +
  facet_wrap(~sex, scales = "free_y") +
  #ggtitle("Males") +
  labs(color = stringr::str_wrap("Confidence rating", 10)) +
  scale_color_manual(limits = c("2","3","4","5"),
                     values = rev(shuk5)) +
  #scale_color_brewer(palette = "Purples") +
  scale_x_continuous(limits = c(0, 65),
                     breaks = seq(0,60, by = 10)) +
  theme(
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    #title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.1),
    strip.background = element_rect(color = NA),
    strip.text = element_text(size = 11)
    
  )

# combine
ggarrange(bar, n_bar, nrow = 2, ncol = 1, common.legend = T,
          labels = c("(a)","(b)"))

# save
ggsave(here("results","fkw_catalog_ages_sensitivity_flipmaxrule_barbplot_by_sex_2025Apr13.png"),
       width = 9, height = 9, units = "in", bg = "white")

