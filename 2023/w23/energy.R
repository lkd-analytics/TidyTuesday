# TidyTuesday - W23, 2023: Energy

# Load required libraries
library(tidyverse)
library(countrycode)
library(scales)
library(ggsci)
library(sysfonts)
library(glue)
library(patchwork)
library(showtext)
library(ggtext)
library(ggflags)

# Load data ----
energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')
#tt_data <- tidytuesdayR::tt_load(2023, week = 23)
#energy <- tt_data$energy

# Data manipulation ----
## Top 10 energy producers from 2011-2021 (Highlight top 4)
top10 <- energy %>% 
    select(!country) %>% 
    filter(year > 2010, year < 2022) %>% 
    group_by(year, iso_code) %>% 
    drop_na(iso_code) %>% 
    select(contains('electricity')) %>% 
    select(!c(electricity_demand, 
              electricity_share_energy,
              fossil_electricity,
              low_carbon_electricity,
              other_renewable_electricity,
              per_capita_electricity,
              renewables_electricity,
              electricity_generation,
              other_renewable_exc_biofuel_electricity)) %>% 
    rename_all(~str_remove(., '_([a-z]+)')) %>% # rename columns
    ungroup() %>% 
    select(!year) %>% 
    group_by(iso) %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    mutate(total_production = select(., biofuel:wind) %>% 
               rowSums(na.rm = TRUE),
           iso2 = tolower(countrycode(iso, origin = 'iso3c', destination = 'iso2c')),
           country = countrycode(iso, origin = 'iso3c', destination = 'country.name'),
           country = fct_reorder(country, total_production)) %>% 
    arrange(desc(total_production)) %>% 
    head(10)

## Top 4 comparison
top4_comparison <- energy %>% 
    select(!country) %>% 
    filter(year > 2010, year < 2022) %>% 
    filter(iso_code %in% c('RUS', 'USA', 'CHN', 'IND')) %>% 
    group_by(year, iso_code) %>%
    select(contains('share_elec')) %>% 
    select(!c(fossil_share_elec,
              other_renewables_share_elec,
              renewables_share_elec,
              low_carbon_share_elec,
              other_renewables_share_elec_exc_biofuel)) %>% 
    rename_all(~str_remove(., '_(.*)'))

### Convert to long format
top4_comparison_long <- top4_comparison %>% 
    pivot_longer(!c(year, iso),
                 names_to = 'type',
                 values_to = 'amount') %>% 
    mutate(year = as.Date(ISOdate(year, 1, 1)),
           type = str_to_title(type),
           country = countrycode(iso, origin = 'iso3c', destination = 'country.name'))

# Fonts, Caption & Styling ----
font_add('fa-brands', regular = 'fonts/fa-brands-400.ttf')
font_add('fa-solid', regular = 'fonts/fa-solid-900.ttf')
font_add_google(name = 'Heebo', family = 'heebo',
                regular.wt = 400,
                bold.wt = 700)
font <- 'heebo'

logo_col <- 'white'
bg_col <- '#0b5345'

twitter <- glue("<span style='font-family:fa-brands; color:{logo_col}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{logo_col}'>&#xf09b;</span>")
data_source <- 'Source: Our World in Data - Energy Data Explorer'
tidy_week <- '#TidyTuesday W23, 2023'
space <- glue("<span style='color:{bg_col};font-size:1px'>'</span>")
dash <- glue("<span style='color:{bg_col}'>--</span>")
caption <- glue("{twitter}{dash}@lkd_analytics{space}{dash} | 
                 {dash}{space}{github}{dash}lkd-analytics{space}{dash} | 
                 {dash}{space}{data_source}{space}{dash} |
                 {dash}{space}{tidy_week}")

showtext_auto()

# Visualization ----
## Top 10 
p1 <- ggplot(top10, aes(y = country, x = total_production, country = iso2)) +
    geom_segment(aes(yend = country, xend = 0), 
                 color = ifelse(top10$iso %in% c('CHN', 'USA', 'IND', 'RUS'), "orange", "grey"), 
                 linewidth = ifelse(top10$iso %in% c('CHN', 'USA', 'IND', 'RUS'), 3.5, 2)) +
    geom_point() +
    geom_flag(size = 15) +
    scale_x_continuous(limits = c(0, 80000),
                       labels = label_number(scale = 1e-3, suffix = "k")) +
    theme(text = element_text(color = 'white', family = font, size = 72),
          axis.text = element_text(color = 'white', size = 72, family = font),
          axis.title.x = element_text(color = 'white', size = 64, margin = margin(t = 10)),
          axis.text.y = element_text(margin = margin(r = -5)),
          axis.text.x = element_text(size = 60),
          panel.background = element_rect(fill = bg_col, colour = bg_col),
          panel.grid.major.x = element_line(color = 'grey80'),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(b = 30, t = 5, r = 50, l = 0),
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = bg_col, colour = bg_col)) +
    xlab('Total Energy Production (TWh)') +
    ylab('') +
    ggtitle('Top 10 Energy Producers (2011-2021)')

## Top 4
p2 <- ggplot(top4_comparison_long, aes(fill = type, x = year, y = amount, label = type)) +
    geom_bar(position = 'fill', stat = 'identity') +
    scale_fill_flatui('default') +
    scale_y_continuous(labels = percent) +
    facet_wrap(~factor(country, levels = c('China', 'United States', 'India', 'Russia')), 
               strip.position = 'top') +
    theme(text = element_text(color = 'white', family = font, size = 72),
          axis.text = element_text(color = 'white', family = font),
          axis.text.x = element_text(color = 'white', size = 54),
          axis.text.y = element_text(color = 'white', size = 54),
          axis.ticks = element_line(color = 'white'),
          legend.title = element_blank(),
          legend.background = element_rect(fill = '#0b5345', colour = '#0b5345'),
          legend.key = element_blank(),
          legend.key.size = unit(0.8, 'cm'),
          legend.position = 'right',
          legend.spacing.x = unit(0.2, 'cm'),
          panel.background = element_rect(fill = bg_col, colour = bg_col),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "white", fill = NA, linewidth = .5),
          plot.margin = margin(b = 30, t = 5, r = 0, l = 0),
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = bg_col, colour = bg_col),
          strip.background = element_rect(fill = bg_col, colour = 'white'),
          strip.text = element_text(color = 'white', family = font, size = 62),) +
    xlab('') +
    ylab('') +
    ggtitle('Top 4 Comparison')

# Add plot annotations ----
p1 + p2 +
    plot_annotation(
        title = 'Comparison of Top 4 Energy Producers between 2011 - 2021',
        subtitle = str_wrap('', width = 300),
        caption = caption,
        theme = theme(
            text = element_text(family = font, size = 72, lineheight = 0.3, colour = 'white'),
            plot.background = element_rect(fill = bg_col, colour = bg_col),
            plot.title = element_text(size = 150, hjust = 0.5, face = 'bold'),
            plot.caption = element_markdown(colour = 'white', hjust = 0.5, margin = margin(t = 20)),
            plot.margin = margin(t = 20, r = 50, b = 20, l = 50),
            legend.position = 'none'
        )
    )

# Preview output
tgutil::ggpreview(device = 'png', dpi = 320, width = 24, height = 12)

ggsave(filename = '2023/w23/energy.png', width = 24, height = 12, dpi = 320)
