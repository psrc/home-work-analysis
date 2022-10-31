# TITLE: Commute time by industry & mode
# GEOGRAPHIES: Region
# SOURCE: 2020 5YR ACS PUMS
# AUTHOR: Eric Clute
# DATE MODIFIED: 10/31/2022

library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(stringr)
library(tidyverse)

setwd("J:/Projects/Home_Work_Connections/PUMS")

# Pull data

pums_raw <- get_psrc_pums(5,2020,"p", c("JWMNP","INDP","JWTRNS","COW"))

# Mutate data to create commute bins, mode bins, streamline/group by industry categories
# Filter uses the COW (Class of worker) variable. Universe excludes ages <16, non workers. 
#   "^(unemployed)" call removes unemployed workers from data. 

pums_workers <- pums_raw %>%
  filter((!grepl("^(Unemployed)", as.character(COW)) & !is.na(COW))) %>%
  mutate(
    commute_bin=factor(case_when(JWMNP < 15 ~ "Under 15 min",
                                 JWMNP < 30 ~ "15-30 min",
                                 JWMNP < 45 ~ "30-45 min",
                                 JWMNP < 60 ~ "45-60 min",
                                 JWMNP >=60 ~ "60+ min",
                                 !is.na(JWMNP) ~ "Else"),
                       levels=c("Under 15 min",
                                "15-30 min",
                                "30-45 min",
                                "45-60 min",
                                "60+ min",
                                "Else")),
    industry_bin=str_extract(as.character(INDP),"(\\w+)"),
    mode_bin=factor(case_when(grepl("^(Bicycle)", as.character(JWTRNS)) ~ "Bicycle",
                            grepl("(Bus|Ferryboat|rail)", as.character(JWTRNS)) ~ "Transit",
                            grepl("^(Car, truck, or van)", as.character(JWTRNS)) ~ "SOV",
                            grepl("^(Walked)", as.character(JWTRNS)) ~ "Walked",
                            grepl("^(Worked from home)", as.character(JWTRNS)) ~ "WFH",
                            grepl("^(Motorcycle|Taxicab|Other method)", as.character(JWTRNS)) ~ "Other",
                            !is.na(JWTRNS) ~ as.character(JWTRNS))))

# Create median/mean commute by industry

commutebyindustry_median <- pums_workers %>% filter(!is.na(JWMNP)) %>% psrc_pums_median("JWMNP", group_vars = "industry_bin")
commutebyindustryandmode_median <- pums_workers %>% filter(!is.na(JWMNP)) %>% psrc_pums_median("JWMNP", group_vars = c("industry_bin", "mode_bin"))

commutebyindustry_mean <- pums_workers %>% filter(!is.na(JWMNP)) %>% psrc_pums_mean("JWMNP", group_vars = "industry_bin")
commutebyindustryandmode_mean <- pums_workers %>% filter(!is.na(JWMNP)) %>% psrc_pums_mean("JWMNP", group_vars = c("industry_bin", "mode_bin"))

# Export to Excel for graphing

commutebyindustry_mean <- subset(commutebyindustry_mean, select = -c(DATA_YEAR,COUNTY))
commutebyindustry_meanmedian <- inner_join(commutebyindustry_median, commutebyindustry_mean, by = 'industry_bin')

library(openxlsx)

write.xlsx(commutebyindustry_meanmedian, "meanmediancommutebyindustry_raw.xlsx")

# Focus on just transit/SOV commutes

commutebyind_sovtransit_median <- commutebyindustryandmode_median %>% filter(mode_bin == "Transit" | mode_bin == "SOV")
commutebyind_sovtransit_mean <- commutebyindustryandmode_mean %>% filter(mode_bin == "Transit" | mode_bin == "SOV")

commutebyind_sovtransit_median_pivot <- commutebyind_sovtransit_median %>% 
  pivot_wider(id_cols = c( 'DATA_YEAR', 'COUNTY', 'industry_bin'),
              names_from = 'mode_bin',
              values_from = c('JWMNP_median','JWMNP_median_moe'))

commutebyind_sovtransit_mean_pivot <- commutebyind_sovtransit_mean %>% 
  pivot_wider(id_cols = c( 'industry_bin'),
              names_from = 'mode_bin',
              values_from = c('JWMNP_mean','JWMNP_mean_moe'))

# Export to Excel for graphing

commutebyind_sovtransit_meanmedian <- inner_join(commutebyind_sovtransit_median_pivot,
                                           commutebyind_sovtransit_mean_pivot, by = 'industry_bin')
library(openxlsx)

write.xlsx(commutebyind_sovtransit_meanmedian, "meanmediancommutebyindustry_SOVTransit_raw.xlsx")


# ---------------------------------------------------

# HISTOGRAM OF SRV INDUSTRY

srv_workers <- pums_workers$variables %>% 
  filter(industry_bin == 'SRV')

# interactive ggplot 

library(ggiraph)

g <- ggplot(srv_workers) +
  geom_histogram_interactive(aes(x = JWMNP, tooltip = JWMNP, fill = mode_bin)) +
  facet_wrap(vars(industry_bin),
             labeller = labeller(industry_bin = label_wrap_gen(width = 35))) +
  theme(strip.text.x = element_text(size = 8))

girafe(ggobj = g)

# static ggplot

ggplot(srv_workers) +
  geom_histogram(aes(x = JWMNP, fill = mode_bin)) +
  facet_wrap(vars(industry_bin),
             labeller = labeller(industry_bin = label_wrap_gen(width = 35))) +
  theme(strip.text.x = element_text(size = 8))


# HISTOGRAM OF ALL INDUSTRIES

library(ggiraph)

hist_allworkers <- ggplot(pums_workers) +
  geom_histogram_interactive(aes(x = JWMNP, tooltip = JWMNP, fill = mode_bin)) +
  facet_wrap(vars(industry_bin),
             labeller = labeller(industry_bin = label_wrap_gen(width = 35))) +
  theme(strip.text.x = element_text(size = 8))

girafe(ggobj = hist_allworkers)
