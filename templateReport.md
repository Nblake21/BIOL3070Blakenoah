Final Project
================
Noah Blake
2025-10-28

- [R Markdown](#r-markdown)
- [Background](#background)

``` r
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

## Background

Asthma is a chronic condition that causes airways to tighten up and
makes it harder to breathe. There are many pollutants that can be found
in the air, which contribute to this Asthma reaction. These include
Ozone, Nitrogen Dioxide, Sulfur Dioxide, Carbon Monoxide, and
Methane\[^1\]. These species react with cells in the body and break down
into reactive oxygen species, which are powerful oxidizers that damage
cellular tissue. This causes an inflammatory response, which restricts
the airways.

In the United States, there are many of these pollutants that are
released in different areas of the country as show below , which can
cause high rates of asthma to appear. The map below shows the prevalence
of asthma across different regions.

``` r
library(ggplot2)

ggplot(asthma_data, aes(x = region, y = prevalence)) +
  geom_boxplot() +
  labs(title = "Asthma Prevalence by U.S. Region",
       x = "Region",
       y = "Asthma Prevalence (%)") +
  theme_minimal()
```

<img src="templateReport_files/figure-gfm/box plot-1.png" style="display: block; margin: auto;" />

``` r
# U.S. states map data
states_map <- map_data("state")

# Put the states into regions in a dataframe
state_regions <- data.frame(
  state = tolower(state.name),
  region = state.region
)

# Merge map data with region info
map_data <- states_map %>%
  left_join(state_regions, by = c("region" = "state"))

# Plot
ggplot(map_data, aes(x = long, y = lat, group = group, fill = region.y)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(title = "Four U.S. Census Regions",
       fill = "Region") +
  theme_void()
```

<img src="templateReport_files/figure-gfm/region map-1.png" style="display: block; margin: auto;" />

``` r
library(ggplot2)

age <- data.frame(category = c(" 0-4", " 5-11", "12-17", "18-24", "25-34", "35-64", "65+"), value = c(1.4, 2.4, 2.0, 3.8, 6.4, 11.5, 27.1))

ggplot(age, aes(x = category, y = value)) +
  geom_bar(stat = "identity", fill = "darkred") + # stat="identity" uses y-values directly
  labs(title = "Asthma Deaths By Age",x = "Age (Years)",y = "Asthma-Related Deaths Per Million")
```

![](templateReport_files/figure-gfm/age%20and%20deaths-1.png)<!-- -->

``` r
library(ggplot2)
library(maps)
library(dplyr)

# Map data
states_map <- map_data("state")

# Merge map data with asthma data
map_data <- states_map %>%
  left_join(asthma_data, by = c("region" = "state_lower"))

ggplot(map_data, aes(long, lat, group = group, fill = prevalence)) +
  geom_polygon(color = "gray90", size = 0.3) +  # state borders
  geom_polygon(aes(fill = prevalence), color = "white") +
  coord_fixed(1.3) +
 scale_fill_gradientn(
  name = "Asthma Prevalence (%)",
  colors = c("yellow2", "orange", "red", "red4")
) +
  labs(
    title = "Asthma Prevalence by U.S. State",
    subtitle = "Higher prevalence shown in darker colors"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
```

<img src="templateReport_files/figure-gfm/prevalence by state-1.png" style="display: block; margin: auto;" />

``` r
ggplot(asthma_data, aes(x = population, y = prevalence)) +
  geom_point(aes(color=region)) +
  geom_text(aes(label = state_abrv), vjust = -0.5, size = 3)+
  labs(title = "Number of Individuals With Asthma vs. Asthma Prevalence by State",
       x = "Population",
       y = "Asthma Prevalence (%)") +
  theme_minimal()
```

<img src="templateReport_files/figure-gfm/scatter plot-1.png" style="display: block; margin: auto;" />

![](templateReport_files/figure-gfm/plot-state-data-1.png)<!-- -->
