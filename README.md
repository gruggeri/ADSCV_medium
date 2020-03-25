
# From static to animated timeseries: the R way

In the last months, many examples on how to plot timeseries of COVID-19
confirmed cases can be found all over the web and you might be
overloaded by the amount of data visualisations that each day are shared
on every platform. It is indeed hard, for any passionate in data
visualisation to not want to plot the data yourself. For this reason, in
this article, we will go through the necessary steps to create a
timeseries plot, inspired from the many examples that can be found on
the web: FT, Economist. As a second step we will then animate the chart,
always in R, using the package `{gganimate}`.

## Importing the data and the libraries

As a very first step in our R script or R notebook, we will want to
import all the libraries that we use in this little excerise. We start
by the omnipresent `{tidyverse}`, which we will use to import,
manipulate and plot the data. `{lubridate}` is our package of choice for
working with variables that store dates values, `{janitor}` is used to
clean the column names. `{rcartocolor}` contains some very nice color
scales, useful for cartography and not only, then we use `{gganimate}`
to finally animate the chart.

``` r
library(tidyverse)
library(lubridate)
library(janitor)
library(gganimate)
library(rcartocolor)
```

We can now import the data directly from the continuously updated
dataset put together by the John Hopkins University, shared on Github.

``` r
cases_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
```

In order to recreate the chart, inspired by John Burn-Murdoch work, we
need to tidy the data first, and then do some basic manipulation on it.
Since each column corresponds to the different dates, the first thing to
do would be to transform the data from wide format to long format: for
this we use the `pivot_longer()` function from the `{tidyr}` package. We
even tried to be smart here, and select for our pivoting, only the
columns that are named as a date in the following format
“digit/digit/digit”. The new column `date` needs to be transformed
into a date column, and for doing it we can use the smart `mdy()`
function from the `{lubridate}` package: where `m` stands for *month*,
`d` stands for `day` and `y` stands for `year`.

In the rest of the script we keep only those `country_regions` that we
want to plot. We also filter out the China data because it is a bit off
scale compared to the other countries, but this may not be valid in few
weeks time given the spread of the pandemic.

Since we have that some countries are ungrouped by subregions, we also
use the `group_by()` and `summarise()` to make sure that we have only
one value per day per country, which corresponds to the sum of cases of
all the regions in each country.

``` r
cases_data <- cases_raw %>%
  pivot_longer(cols= matches("\\d+/\\d+/\\d+"), 
               names_to = "date", 
               values_to = "cases") %>% 
  clean_names() %>% 
  transmute(country_region, date = mdy(date), cases) %>% 
  filter(!country_region %in% c("Others", "China", "Cruise Ship")) %>%
  group_by(country_region, date) %>% 
  summarise(cases = sum(cases))
```

If we just wanted to plot the raw timeseries, this would be enough. We
although want to go a bit further than this and plot the dates since the
100th case in each country. We also want to keep only the countries that
reached the 100th case from less than 6 days. Still by group, we can
calculate the days since the 100th case and after this we can `ungroup`
the data.

``` r
cases_data <- cases_data %>% 
  filter(cases >= 100)%>%
  filter(n() >= 5) %>%
  arrange(date, .by_group = TRUE)%>%
  mutate(days_since_100 = (date-first(date)) / ddays(1)) %>%
  ungroup()
```

Many visualisations that can be found on the web or in newspapers,
overlap to the country trends a dashed line that corresponds to the 33%
daily rise. We then add to our original `tibble` (which is a table in
`tidyverse` language).

``` r
cases_data <- cases_data %>% 
  bind_rows(
    tibble(country_region = "33% daily rise", 
           days_since_100 = 0:25) %>%
      mutate(cases = 100*1.33^days_since_100)
  ) %>% 
  mutate(line_type = ifelse(country_region == "33% daily rise", "2", "1")) 
```

We also create a new variable `line_type` that we will use to set the
line corresponding to the `33% daily rise` to be a dashed line.

``` r
ggplot(data = cases_data, 
       mapping = aes(x = days_since_100, 
                     y = cases, 
                     color = country_region)) +
  #geom_point(size = 0.8, alpha = 0.9, pch = 21)+
  geom_line(size = 0.7, alpha =0.9,
            mapping = aes(linetype = line_type)) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(100, 500, 2000,  10000, 60000)) +
  scale_color_carto_d(palette = "Safe")+
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  labs(x = "Number of days since 100th case", 
       y = "Total Number of Cases",
       title = "Total number of COVID-19 cases",
       subtitle =  "Outside of China",
       caption = "Data Source: John Hopkins University")
```

    ## Warning in carto_pal(n, pal): Number of colors (n) in the Safe palette should be between 3 and 12

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This was a first attempt but we can already see that there is one
problem with this visualisation: the colours. The number of countries
plotted is indeed too big for many categorical colour scale. If we
plotted each line in a different colour we would not be able to really
discern the countries. If we want to still use different colours for the
countries that we want to highlight we need to give an order to the
countries, based on the data (i.e. based on the maximum value of
`days_since_100`, for each country). Do do so we can use the
`fct_infreq()` from the `{forcats}` package.

``` r
cases_data <- cases_data %>% 
  mutate(country_region = fct_infreq(country_region))
```

``` r
ggplot(data = cases_data, 
       mapping = aes(x = days_since_100, 
                     y = cases, 
                     color = country_region)) +
  #geom_point(size = 0.8, alpha = 0.9, pch = 21)+
  geom_line(size = 0.7, alpha =0.9,
            mapping = aes(linetype = line_type)) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(100, 500, 2000,  10000, 60000)) +
  scale_color_carto_d(palette = "Safe")+
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  labs(x = "Number of days since 100th case", 
       y = "Total Number of Cases",
       title = "Total number of COVID-19 cases",
       subtitle =  "Outside of China",
       caption = "Data Source: John Hopkins University")
```

    ## Warning in carto_pal(n, pal): Number of colors (n) in the Safe palette should be between 3 and 12

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Annotating the plot

If we want to annotate the plot with the name of each country, we can
then use `geom_text()` in our `ggplot` call. We can place the annotation
next to the last point of each line, by using a different `tibble` for
the annotation. In `annotations` we will just keep the observation
corresponding to the maximum value of `days_since_100`, for each county.
If we want to only annotate the first 12 countries (12 because the
colour scale we have chosen has 12 colours), we can create a new
variable named `label_country`, that will be empty for all countries
apart from the first 12, ordered before using the `fct_reorder()`.

``` r
annotations <- cases_data %>% 
  group_by(country_region) %>% 
  filter(days_since_100 == max(days_since_100)) %>% 
  mutate(label_country = ifelse(country_region %in% levels(country_region)[1:12],
                                 as.character(country_region), ""))
```

Now we are ready to annotate the plot. To increase the visibility of the
labels we can use `geom_shadowtext()` from the `{shadowtext}` package,
which adds a little 3d subtle look, which makes the labels stick out a
little bit. We indeed all know, that the devil lies in the details.

``` r
ggplot(data = cases_data, 
       mapping = aes(x = days_since_100, 
                     y = cases, 
                     color = country_region)) +
  geom_line(size = 0.7, alpha =0.9,
            mapping = aes(linetype = line_type)) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(100, 500, 2000,  10000, 60000)) +
  shadowtext::geom_shadowtext(data = annotations, 
                              mapping = aes(x = days_since_100, 
                                            y = cases, 
                                            label = label_country),
                              hjust=-0.1, vjust = 0, bg.color = "white") +
  scale_color_carto_d(palette = "Safe")+
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  labs(x = "Number of days since 100th case", 
       y = "Total Number of Cases",
       title = "Total number of COVID-19 cases",
       subtitle =  "Outside of China",
       caption = "Data Source: John Hopkins University")
```

    ## Warning in carto_pal(n, pal): Number of colors (n) in the Safe palette should be between 3 and 12

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Now, how do we plot the coloured line on top of the grey lines? To do
this, we would need to reorder the `country_region` factors again, but
this would affect the way the the lines are coloured. How can we cope
with this?

The way to do it is to manually set the colour of the countries, and
then order the countries in the reverse order.

``` r
cases_data <- cases_data %>% 
  mutate(color_label = forcats::fct_collapse(country_region,
    "#D63D32" = "Switzerland",
    "#888888" = "Italy",
    "#6699CC" = "Iran",
    "#661100" = "Germany",
    "#882255" = "France",
    "#999933" = "United Kingdom",
    "#44AA99" = "US",
    "#332288" = "Spain",
    "#117733" = "Korea, South",
    "#DDCC77" = "Netherlands",
    "#CC503E" = "Hong Kong",
    "#1D6996" = "Singapore",
    "#855C75" = "Japan",
    "black"   = "33% daily rise",
    other_level = "grey90"),
    color_label = fct_relevel(color_label, "grey90")) %>%
  arrange(color_label) %>%
  mutate(country_region = fct_inorder(country_region)) %>% 
  mutate(country_label = ifelse(color_label == "grey90", "", as.character(country_region)))
```

``` r
plot_gg <- ggplot(data = cases_data, 
       mapping = aes(x = days_since_100, 
                     y = cases, 
                     color = color_label,
                     group = country_region)) +
  geom_point(size = 0.8, alpha = 0.9, pch = 21)+
  geom_line(mapping = aes(linetype = line_type), 
            size = 0.7, alpha =0.9) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(100, 500, 2000,  10000, 60000)) +
  shadowtext::geom_shadowtext(aes(label = paste0(" ",country_label)),
                              hjust=0, vjust = 0, bg.color = "white") +
  scale_color_identity()+
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  )+
  coord_cartesian(clip = "off") +
  labs(x = "Number of days since 100th case", 
       y = "Total Number of Cases",
       title = "Total number of COVID-19 cases",
       subtitle =  "Outside of China",
       caption = "Data Source: John Hopkins University")

plot_gg
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot_gg +
  transition_reveal(days_since_100)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.gif)<!-- -->

``` r
#anim_save("covid.gif")
```
