Asthma Prevelence in the US
================
Noah Blake
2025-12-06

- [Abstract](#abstract)
- [Background](#background)
- [Question Framing](#question-framing)
- [Methods and Analysis](#methods-and-analysis)
- [Discussion](#discussion)
- [Conclusion](#conclusion)
  - [Refrences](#refrences)

## Abstract

This Project aims to dive deeper into the understanding of Asthma which
is a condition which makes breathing restrictive and overall health
struggles related to the respiratory system. We look at Data from the
CDC and Asthma prevalence across the united states to see about what
different factors may cause it to be higher or lower in different area.
The locus of this project looks at the measure of population density and
its effect which we hypothesis will show a positive correlation in the
Rate of asthma. We concluded however that in only one region of the US
(the Northeast) was this correlation positive and statisticaly
significant with the rest being null.

## Background

Asthma is a chronic respiratory condition characterized by airway
constriction and inflammation, leading to breathing difficulties.
Exposure to certain air pollutants—including ozone, nitrogen dioxide,
sulfur dioxide, carbon monoxide, and methane\[^1\]—can trigger and
worsen asthma symptoms. These pollutants interact with bodily tissues
and can generate reactive oxygen species, powerful oxidizing agents that
damage cells. This damage prompts an inflammatory response, further
tightening the airways and making breathing more difficult.(“Asthma &
Allergy Foundation of America,” n.d.)

In the United States, emissions of these pollutants vary regionally,
contributing to differences in asthma rates across the country. The
following map illustrates the geographic distribution of asthma
prevalence in relation to these airborne pollutants.

## Question Framing

Study question : does population density have an effect on asthma rates
Hypothesis : we Hypothesis that in areas of High population there will
be a higher rate of Asthma due to pollution effects

## Methods and Analysis

Recent asthma data from the CDC was gathered for analysis. Asthma
prevalence rates across different states and regions were compared using
bar plots. An ANOVA and a Tukey pairwise comparison were then run to
test for significant differences. Spatial hotspots were also mapped to
identify areas with overlapping patterns of higher asthma rates.

``` r
library(ggplot2)

ggplot(asthma_data, aes(x = region, y = prevalence, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("salmon", "chartreuse3", "skyblue", "violet")) +
  labs(title = "Asthma Prevalence by U.S. Region",
       x = "Region",
       y = "Asthma Prevalence (%)") +
       theme_minimal()
```

<img src="Final-Testing_files/figure-gfm/box plot-1.png" style="display: block; margin: auto;" />
Figure 1. Bar plot showing the median Asthma prevalence in each US
region along with outliars with the south with the lowest prevelance and
the Northeast at the highest.

``` r
library(ggplot2)

ggplot(asthma_data, aes(x = region, y = pop_density_sqmile, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("salmon", "green", "skyblue", "plum")) +
  labs(title = "Population Density by U.S. Region",
       x = "Region",
       y = "Population Density (per sq. mile)") +
  theme_minimal()
```

<img src="Final-Testing_files/figure-gfm/population density boxplot-1.png" style="display: block; margin: auto;" />
Figure 2. Bar plot showing the asthma prevalence by US region

``` r
library(maps)
library(ggmap)
```

    ## ℹ Google's Terms of Service: <https://mapsplatform.google.com>
    ##   Stadia Maps' Terms of Service: <https://stadiamaps.com/terms-of-service>
    ##   OpenStreetMap's Tile Usage Policy: <https://operations.osmfoundation.org/policies/tiles>
    ## ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.

``` r
# U.S. states map data
states_map <- map_data("state")

# Put the states into regions in a dataframe
state_regions <- data.frame(
  state = tolower(state.name),
  region = state.region
)

# Merge map data with region info
map_data_1 <- states_map %>%
  left_join(state_regions, by = c("region" = "state"))

# Plot
ggplot(map_data_1, aes(x = long, y = lat, group = group, fill = region.y)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(title = "Four U.S. Census Regions",
       fill = "Region") +
  theme_void()
```

<img src="Final-Testing_files/figure-gfm/region map-1.png" style="display: block; margin: auto;" />
Figure 3. Map showing Regions of the US referenced in previous analysis.

``` r
library(ggplot2)
library(maps)
library(dplyr)

# Map data
states_map <- map_data("state")

# Merge map data with asthma data
map_data_2 <- states_map %>%
  left_join(asthma_data, by = c("region" = "state_lower"))

ggplot(map_data_2, aes(long, lat, group = group, fill = prevalence)) +
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

<img src="Final-Testing_files/figure-gfm/prevalence by state-1.png" style="display: block; margin: auto;" />
Figure 4. Map showing Asthma prevelence on a state by state basis with
high being darker and lower being lighter.

``` r
# load necessary libraries

library("car")
library("lme4")

# create a linear model for population density per square mile and asthma prevalence by state

m1 <- lm(prevalence ~ pop_density_sqmile, data=asthma_data)

# perform statistical test

Anova(m1)
summary(m1)
```

``` r
# load necessary libraries
library(car)
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

``` r
library(emmeans)
```

    ## Welcome to emmeans.
    ## Caution: You lose important information if you filter this package's results.
    ## See '? untidy'

``` r
# ensure region is a factor
asthma_data$region <- factor(asthma_data$region)

# linear model
m2 <- lm(prevalence ~ region, data = asthma_data)

# perform ANOVA
Anova(m2)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: prevalence
    ##           Sum Sq Df F value Pr(>F)  
    ## region    16.928  3  3.6503 0.0192 *
    ## Residuals 71.109 46                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = prevalence ~ region, data = asthma_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8111 -0.7702  0.1231  0.7597  2.7500 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          11.7111     0.4144  28.258  < 2e-16 ***
    ## regionSouth          -1.5611     0.5180  -3.013  0.00419 ** 
    ## regionNorth Central  -1.5944     0.5483  -2.908  0.00558 ** 
    ## regionWest           -1.2342     0.5391  -2.289  0.02671 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.243 on 46 degrees of freedom
    ## Multiple R-squared:  0.1923, Adjusted R-squared:  0.1396 
    ## F-statistic:  3.65 on 3 and 46 DF,  p-value: 0.0192

``` r
# pairwise comparison (Tukey-like, adjusted)
pairs(emmeans(m2, "region"))
```

    ##  contrast                  estimate    SE df t.ratio p.value
    ##  Northeast - South           1.5611 0.518 46   3.013  0.0211
    ##  Northeast - North Central   1.5944 0.548 46   2.908  0.0276
    ##  Northeast - West            1.2342 0.539 46   2.289  0.1155
    ##  South - North Central       0.0333 0.475 46   0.070  0.9999
    ##  South - West               -0.3269 0.464 46  -0.704  0.8949
    ##  North Central - West       -0.3603 0.498 46  -0.724  0.8871
    ## 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

``` r
library(car)

# run linear model with correct variable names + dataset
r1 <- lm(prevalence ~ pop_density_sqmile, data = asthma_data)

# ANOVA table
Anova(r1)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: prevalence
    ##                    Sum Sq Df F value Pr(>F)
    ## pop_density_sqmile  0.343  1  0.1877 0.6668
    ## Residuals          87.694 48

``` r
# regression summary
summary(r1)
```

    ## 
    ## Call:
    ## lm(formula = prevalence ~ pop_density_sqmile, data = asthma_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5804 -0.8478 -0.0491  0.6314  2.6415 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        1.044e+01  2.404e-01  43.446   <2e-16 ***
    ## pop_density_sqmile 3.042e-04  7.022e-04   0.433    0.667    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.352 on 48 degrees of freedom
    ## Multiple R-squared:  0.003894,   Adjusted R-squared:  -0.01686 
    ## F-statistic: 0.1877 on 1 and 48 DF,  p-value: 0.6668

1 Asthma prevalence by region (Figure1) This chart shows us the Median
Asthma prevalence with the Highest being the Northeast and the lowest
being the south. This helps us to answer our hypothesis because it shows
us where the region with a high rate of asthma is so we can test it to
see if it correlates with high population density.

2 Population density of the US (Figure2) This chart shows us the Median
population density of the different US regions as outlined in (figure3)
with the Northeast being the highest and the west being the lowest. This
helps us answer our hypothesis because it shows what we were looking for
at least in the northeast region that there is a high population density
which fits with the higher rate of asthma.

3 Map of the United states with state by state rates of asthma. This map
was designed to try and help us to see a trend in the regions and the
rates of asthma in each state and how they might correlate with the
region which were helpful for understanding the out liar that was New
England. This helped to solidify the points that we found in the last
two analysis about the correlation we are looking for in the asthma
rates and population density.

4 Three Anova tests with Tukey-adjusted pairwise function were run to
test for the significance of the data that we found. Most all of these P
values which we found were high and showed that these correlations were
not statistically significant other than in the northeast which showed
that asthma prevalence was significantly higher from the other regions
with comparison p-values of 0.00419, 0.00558, and 0.02671 for the South,
North Central, and West regions respectively. This statistical analysis
showed significance in the Northeast showing that the effects are not
likely to be from random chance making them significant in our project.
however these tests did show that in other ares the p values were not
under the needed threshold of significance causing us to nullify drawing
any conclusions from them in our hypothesis.

# Discussion

With this data we look forward to a new identifies of these high asthma
rates in different regions. due to the limitations on the data that we
looked at being only from one year (2024) and not any more specifics we
aim in the future to collect and find better data which will show up
different factors like Age, Sex or Race to answer this question. Another
valuable extension of this project would be to collect county-level
asthma prevalence and population density data and analyze their
relationship using linear regression to determine whether population
density meaningfully predicts asthma rates.

# Conclusion

The first overall solid evidence that helps to give strength to our
hypothesis is the northeast region which had the highest found
population density and the highest rate of asthma. Overall however we
were not able to conclusively say that the population density is a
direct factor in the Asthma rates. We conclude that more can be done in
the future to find a better answer to this question.

## Refrences

1.  OpenAI. (2025). ChatGPT (Version 5.1) \[Large language model\].
    <https://chat.openai.com/> (used for creation of box plots and
    troubleshooting)

2.  Centers for Disease Control and Prevention. (2024, November 21).
    Most recent asthma data. U.S. Department of Health and Human
    Services.
    <https://www.cdc.gov/asthma-data/about/most-recent-asthma-data.html>

3.  <https://aafa.org/asthma/asthma-triggers-causes/air-pollution-smog-asthma/>
