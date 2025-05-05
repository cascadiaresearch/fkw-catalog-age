## summary_stats_figures.R: create summary stats and plots of photo-ID based age
## estimates

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 13 Apr 2025
 
## --------------------------------------------------------------------------- ##

## load packages
library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)
library(PNWColors)

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

# bar plot with ests, min/max, color by sex  ------------------------------ #

# males
ms <- filter(ages, sex == "Male")
ms$cr_c <- as.numeric(ms$cr) # make confidence rating a factor
ms$cr_f <- factor(ms$cr, levels = c("2","3","4","5"))
summary(ms$cr_f)

# color palette
shuk5 <- pnw_palette("Shuksan", n=4, type = "discrete")

# male plot
males <- ggplot(ms, aes(x = age_best, y = reorder(id_sampn, age_best), color = cr_f)) +
  geom_errorbar(data = ms, aes(xmin = age_min,
                                 xmax = age_max,
                                 color = cr_f), size = 1.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  theme_classic() +
  xlab("Best age estimate (yrs)") +
  ylab("Individual*") +
  ggtitle("Males") +
  labs(color = stringr::str_wrap("Confidence rating", 10)) +
  scale_color_manual(limits = c("2","3","4","5"),
                     values = rev(shuk5)) +
  #scale_color_brewer(palette = "Purples") +
  scale_x_continuous(limits = c(0, 65),
                     breaks = seq(0,60, by = 10)) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    #title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.1)
    
  )

males


# Females
fs <- filter(ages, sex == "Female") 
fs$cr_f <- factor(fs$cr, levels = c("2","3","4","5")) # make confidence rating a factor
summary(fs$cr_f)

# color palette
shuk5 <- pnw_palette("Shuksan", n=4, type = "discrete")

# plot
females <- ggplot(fs, aes(x = age_best, y = reorder(id_sampn, age_best), color = cr_f)) +
  geom_errorbar(data = fs, aes(xmin = age_min,
                               xmax = age_max,
                               color = cr_f), size = 1.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  theme_classic() +
  xlab("Best age estimate (yrs)") +
  ylab("Individual*") +
  ggtitle("Females") +
  labs(color = stringr::str_wrap("Confidence rating", 10)) +
  scale_color_manual(limits = c("2","3","4","5"),
                     values = rev(shuk5)) +
  #scale_color_brewer(palette = "Purples") +
  scale_x_continuous(limits = c(0, 65),
                     breaks = seq(0,60, by = 10)) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    #title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.1)
    
  )

females

## make frequency plot of confidence ratings ## ----------------------------- ##

# females
fcr <- ggplot(fs, aes(x = cr, fill = as.factor(cr))) +
  geom_bar(color = "gray19", width = 0.5) +
  scale_fill_manual(values = rev(shuk5))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 24)) +
  theme_classic() +
  xlab("Confidence rating") +
  ylab("# individuals") +
  labs(fill = stringr::str_wrap("Confidence rating", 20)) +
  theme(
    axis.text = element_text(size = 11, color = "black"),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    legend.position = "bottom"
    
  )
fcr

# males
mcr <- ggplot(ms, aes(x = cr, fill = as.factor(cr))) +
  geom_bar(color = "gray19", width = .5) +
  scale_fill_manual(values = rev(shuk5))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 24)) +
  theme_classic() +
  xlab("Confidence rating") +
  ylab("# individuals") +
  labs(fill = stringr::str_wrap("Confidence rating", 20)) +
  theme(
    axis.text = element_text(size = 11, color = "black"),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    legend.position = "bottom"
    
  )
mcr

# create multipaneled plot 
dims4 <- ggarrange(females, males, fcr, mcr,
                   nrow = 2, ncol = 2, labels = c("(a)","(b)","(c)","(d)"),
                   common.legend = T)
dims4

# save
ggsave(here(results, "Pc_best_byID_sexes_cr_v2.jpg"), width = 9, height = 9, units = "in")


## summarize data ## --------------------------------------------------------- ##

# average age estimate by confidence rating 
ages %>%
  group_by(cr) %>%
  summarise(ave_age = mean(age_best))

# get average best age estimate by confidence rating and sex and adult/non adult
ages <- ages %>%
  mutate(
    ad = ifelse(age_best >= 10, "adult", "non-adult"),
    ad_first = ifelse(age_first >= 10, "adult", "non-adult")
  )

cr_sum <- ages %>%
  group_by(ad, cr) %>%
  summarise(
    avg = mean(age_best)
  )

# number of unique individuals by sex
ages %>%
  group_by(sex) %>%
  summarize(n = length(unique(id)))

# aux years
aux <- filter(ages, aux != 0)

# aux year summary stats
aux %>%
  group_by(aux_type) %>%
  summarise(
    n = n(),
    min = min(aux),
    max = max(aux),
    mean = mean(aux),
    median = median(aux)
  )

## make plot of auxiliary data ## ------------------------------------------- ##

# get ages without aux adjustment 
aux <- aux %>%
  mutate(
    age_best_nx = age_best - aux,
    age_min_nx = age_min - aux,
    age_max_nx = age_max - aux,
    aux_type2 = ifelse(aux_type == "uncertainty","age class",aux_type)
  )

# get ages that had no aux information
nx <- filter(ages, aux == 0)

ggplot(aux, aes(x = age_best_nx, y = age_best, color = aux_type2, size = aux)) +
  geom_point(alpha = 0.7) +
  geom_point(data = nx, aes(x = age_best, y = age_best), color = "black", alpha = 0.3) +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0,40)) +
  scale_x_continuous(limits = c(0,40)) +
  ylab("Best age estimate (yrs) with Aux") +
  xlab("Best age estimate (yrs) without Aux") +
  labs(size = "Aux (yrs)", color = "Aux type") +
  scale_color_manual(values = c("#fdc795","#94a9ff")) +
  #scale_color_manual(values = c("#fdc795","#94a9ff","#6dc29d")) +
  theme_bw() +
  theme(
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    legend.title = element_text(size = 11, color = "black", face = "italic"),
    legend.text = element_text(size = 10, color = "black")
    #legend.position = "inside",
    #legend.position.inside = c(0.85,0.25)
  )

ggsave(here(results, "Pc_best_aux_vs_noaux_v4.jpg"), width = 6, height = 4, units = "in")


