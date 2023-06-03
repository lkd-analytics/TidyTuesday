# TidyTuesday - W22, 2023: Verified Oldest People

# Load required libraries
library(tidyverse)
library(treemapify)
library(countrycode)
library(scales)
library(ggsci)
library(sysfonts)
library(glue)
library(patchwork)
library(showtext)
library(ggtext)

# Load data ----
tt_data <- tidytuesdayR::tt_load(2023, week = 22) 
centenarians <- tt_data$centenarians

# Data manipulation ----
centenarians <- centenarians %>% 
    mutate(country = place_of_death_or_residence) %>% 
    # Fix country names
    mutate(country = case_when(country == 'France (French Guiana)' ~ 'France',
                               country == 'France (Martinique)' ~ 'France',
                               country == 'France (Saint Barthélemy)' ~ 'France',
                               TRUE ~ country)) %>% 
    # Change country names to abreviations and add continent
    mutate(country = countrycode(country, origin = 'country.name', destination = 'iso3c'),
           continent = countrycode(country, origin = 'iso3c', destination = 'continent'),
           continent = case_when(continent == 'Oceania' ~ 'Asia',
                                 TRUE ~ continent)) %>% 
    group_by(country) %>% 
    count(country, continent, sort = T)

# Fonts, Caption & Styling ----
font_add('fa-brands', regular = 'fonts/fa-brands-400.ttf')
font_add('fa-solid', regular = 'fonts/fa-solid-900.ttf')
font_add_google(name = 'Heebo', family = 'heebo',
                regular.wt = 400,
                bold.wt = 700)
font <- 'heebo'

logo_col <- 'black'
bg_col <- '#FFE5B4'

twitter <- glue("<span style='font-family:fa-brands; color:{logo_col}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{logo_col}'>&#xf09b;</span>")
data_source <- 'Source: frankiethull: Centenarians'
tidy_week <- '#TidyTuesday W22, 2023'
space <- glue("<span style='color:{bg_col};font-size:1px'>'</span>")
dash <- glue("<span style='color:{bg_col}'>--</span>")
caption <- glue("{twitter}{dash}@lkd_analytics{space}{dash} | 
                 {dash}{space}{github}{dash}lkd-analytics{space}{dash} | 
                 {dash}{space}{data_source}{space}{dash} |
                 {dash}{space}{tidy_week}")

showtext_auto()

# Visualization ----
g1 <- ggplot(centenarians, aes(area = n, 
                               fill = continent, 
                               label = paste(country, n, sep = '\n'), 
                               subgroup = continent)) +
    geom_treemap(fill = 'grey50') +
    geom_treemap(aes(alpha = n)) +
    geom_treemap_subgroup_border(colour = 'white', size = 3) +
    geom_treemap_subgroup_text(colour = 'black',
                               alpha = 0.3,
                               fontface = 'italic',
                               place = 'centre',
                               grow = TRUE) +
    geom_treemap_text(colour = 'white', 
                      place = 'centre',
                      grow = T) +
    theme(legend.position = 'none',
          plot.background = element_rect(fill = bg_col, color = bg_col)) +
    scale_fill_manual(values = c('#800000ff', '#0f425cff', '#233e30')) +
    scale_alpha_continuous(range = c(0.4, 10))

# Add plot annotations ----
g1 +
    plot_annotation(
        title = 'Verified Oldest People by Country & Continent',
        subtitle = str_wrap('Counts/numbers indicate how many of the 200 eldest 
                            verified people belong to a given country. I wonder 
                            what USA and Japan are doing differently?', width = 80),
        caption = caption,
        theme = theme(
            text = element_text(family = font, size = 42, lineheight = 0.3, colour = 'black'),
            plot.background = element_rect(fill = bg_col, colour = bg_col),
            plot.title = element_text(size = 84, hjust = 0.5, face = 'bold'),
            plot.subtitle = element_text(size = 48, hjust = 0.5, margin = margin(b = 20)),
            plot.caption = element_markdown(colour = 'black', hjust = 0.5, margin = margin(t = 20)),
            plot.margin = margin(t = 20, r = 50, b = 20, l = 50),
            legend.position = 'none'
        )
    )

# Preview output
tgutil::ggpreview(device = 'png', dpi = 300,
                  width = 8, height = 8)

ggsave(filename = '2023/w22/centenarians.png', width = 8, height = 8, dpi = 320)
