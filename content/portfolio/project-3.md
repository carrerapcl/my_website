---
categories:
- Data analytics
- Finance
- Fixed income
client: Self-work
date: "2020-10-05T12:14:34+06:00"
description: This is meta description.
draft: false
image: images/portfolio/yield.jpg
project_url: https://fred.stlouisfed.org/
title: Yield curve inversion
---

Every so often, we hear warnings from commentators on the "inverted yield curve" and its predictive power with respect to recessions. An explainer what a [inverted yield curve is can be found here](https://www.reuters.com/article/us-usa-economy-yieldcurve-explainer/explainer-what-is-an-inverted-yield-curve-idUSKBN1O50GA). If you'd rather listen to something, here is a great podcast from [NPR on yield curve indicators](https://www.podbean.com/media/share/dir-4zgj9-6aefd11)

In addition, many articles and commentators think that, e.g., [*Yield curve inversion is viewed as a harbinger of recession*](https://www.bloomberg.com/news/articles/2019-08-14/u-k-yield-curve-inverts-for-first-time-since-financial-crisis). One can always doubt whether inversions are truly a harbinger of recessions, and [use the attached parable on yield curve inversions](https://twitter.com/5_min_macro/status/1161627360946511873).

In our case we will look at US data and use the [FRED database](https://fred.stlouisfed.org/) to download historical yield curve rates, and plot the yield curves since 1999 to see when the yield curves flatten. If you want to know more, a very nice article that explains the [yield curve is and its inversion can be found here](https://fredblog.stlouisfed.org/2018/10/the-data-behind-the-fear-of-yield-curve-inversions/). 

First, we will use the `tidyquant` package to download monthly rates for different durations. 

```{r get_rates, warning=FALSE}
# Get a list of FRED codes for US rates and US yield curve; choose monthly frequency
# to see, eg., the 3-month T-bill https://fred.stlouisfed.org/series/TB3MS
tickers <- c('TB3MS', # 3-month Treasury bill (or T-bill)
             'TB6MS', # 6-month
             'GS1',   # 1-year
             'GS2',   # 2-year, etc....
             'GS3',
             'GS5',
             'GS7',
             'GS10',
             'GS20',
             'GS30')  #.... all the way to the 30-year rate

# Turn  FRED codes to human readable variables
myvars <- c('3-Month Treasury Bill',
            '6-Month Treasury Bill',
            '1-Year Treasury Rate',
            '2-Year Treasury Rate',
            '3-Year Treasury Rate',
            '5-Year Treasury Rate',
            '7-Year Treasury Rate',
            '10-Year Treasury Rate',
            '20-Year Treasury Rate',
            '30-Year Treasury Rate')

maturity <- c('3m', '6m', '1y', '2y','3y','5y','7y','10y','20y','30y')

# by default R will sort these maturities alphabetically; but since we want
# to keep them in that exact order, we recast maturity as a factor 
# or categorical variable, with the levels defined as we want
maturity <- factor(maturity, levels = maturity)

# Create a lookup dataset
mylookup<-data.frame(symbol=tickers,var=myvars, maturity=maturity)
# Take a look:
mylookup %>% 
  knitr::kable()

df <- tickers %>% tidyquant::tq_get(get="economic.data", 
                   from="1960-01-01")   # start from January 1960

glimpse(df)
```

Our dataframe `df` has three columns (variables):

- `symbol`: the FRED database ticker symbol
- `date`: already a date object
- `price`: the actual yield on that date

The first thing would be to join this dataframe `df` with the dataframe `mylookup` so we have a more readable version of maturities, durations, etc.


```{r join_data, warning=FALSE}

yield_curve <-left_join(df,mylookup,by="symbol")

```

###### Plotting the yield curve

This may seem long but it should be easy to produce the following three plots

###### Yields on US rates by duration since 1960

```{r yield_curve_1, echo=FALSE, out.width="100%"}

treasury_rates <- yield_curve %>% 
  select(date, price, var) 

# Re-ordering 
  treasury_rates$var <- factor(treasury_rates$var,
                               levels =
                c("3-Month Treasury Bill",
                "6-Month Treasury Bill",
                "1-Year Treasury Rate",
                "2-Year Treasury Rate",
                "3-Year Treasury Rate",
                "5-Year Treasury Rate",
                "7-Year Treasury Rate",
                "10-Year Treasury Rate",
                "20-Year Treasury Rate",
                "30-Year Treasury Rate" ))

# Plotting
ggplot(treasury_rates, aes(x = date, y = price, color = var)) +
  guides(color = FALSE) +
  geom_line() +
  facet_wrap(~var, ncol = 2) +
  
# Titles
labs (title = "Yields on U.S. Treasury rates since 1960",
      y = "%",
      x = "",
      color = "Components of GDP",
      caption = "Source: ST. Louis Federal Reserve Economic Database (FRED)") +
  theme(axis.title.x = element_blank()) +
  theme_bw()

```

###### Monthly yields on US rates by duration since 1999 on a year-by-year basis

```{r yield_curve_2, echo=FALSE, out.width="100%"}

year_yield_curve <- yield_curve %>% 
  mutate(year = year(date) )%>% 
  filter(year > 1998) %>%
  mutate(month = month(date))

# Plotting
ggdata <- year_yield_curve %>% 
  ggplot(aes(x = maturity, y = price, group = month, color = factor(year))) +
  geom_line() +
  facet_wrap("year", ncol = 4 ) +

# Adding titles and removing guides
labs(title = "US Yield Curve",
      y = "Yield (%)",
      x = "Maturity",
      color = "Components of GDP",
      caption = "Source: ST. Louis Federal Reserve Economic Database (FRED)") +
  guides(color = FALSE) +
  theme_bw()
ggdata

```

###### 3-month and 10-year yields since 1999

```{r yield_curve_3, echo=FALSE, out.width="100%"}

# Filtering data 1999
year_yield_curve2 <- yield_curve %>% 
  filter(var %in% c("3-Month Treasury Bill", "10-Year Treasury Rate")) %>% 
  filter( date >= as.Date("1999-01-01"))

# Re-ordering data set
year_yield_curve2$var <- factor(year_yield_curve2$var,
                               levels =
                c("3-Month Treasury Bill",
                "10-Year Treasury Rate"))

# Plotting
ggplot(year_yield_curve2, aes(x = date, y = price, color = var)) +
        geom_line(aes(group = var)) +

# Adding titles
  labs( title = "Yields on 3-month and 10-year US Treasury rates since 1999",
        y = "%",
        x = "",
        caption = "Source: ST. Louis Federal Reserve Economic Database (FRED)") +
  theme_bw()

```


According to [Wikipedia's list of recession in the United States](https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States), since 1999 there have been two recession in the US: between Mar 2001–Nov 2001 and between Dec 2007–June 2009. Does the yield curve seem to flatten before these recessions? Can a yield curve flattening really mean a recession is coming in the US? Since 1999, when did short-term (3 months) yield more than longer term (10 years) debt?


Besides calculating the spread (10year - 3months), there are a few things we need to do to produce our final plot

1. Setup data for US recessions 
2. Superimpose recessions as the grey areas in our plot
3. Plot the spread between 30 years and 3 months as a blue/red ribbon, based on whether the spread is positive (blue) or negative(red)


- For the first, the code below creates a dataframe with all US recessions since 1946

```{r setup_US-recessions, warning=FALSE}

# get US recession dates after 1946 from Wikipedia 
# https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States

recessions <- tibble(
  from = c("1948-11-01", "1953-07-01", "1957-08-01", "1960-04-01", "1969-12-01", "1973-11-01", "1980-01-01","1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01"),  
  to = c("1949-10-01", "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01") 
  )  %>% 
  mutate(From = ymd(from), 
         To=ymd(to),
         duration_days = To-From)

recessions

```

To add the gray shaded areas corresponding to recessions, we use `geom_rect()` and to color the ribbons blue/red we must see whether the spread is positive or negative and then use `geom_ribbon()`.

```{r, warning=FALSE}

# Pivot data
final_yield_curve <- yield_curve %>% 
  filter(symbol %in% c("GS10","TB3MS")) %>% 
  select(- symbol,- maturity) %>% 
  group_by(date) %>% 
  pivot_wider(names_from = var, 
              values_from=price) %>% 
  filter(date >= as.Date("1959-01-01"))
final_yield_curve$difference = final_yield_curve$`10-Year Treasury Rate` - final_yield_curve$`3-Month Treasury Bill`

# Filter data recessions
recessions1 <- recessions %>% 
  filter(From >= as.Date("1959-01-01")) %>% 
  filter(To >= as.Date("1959-01-01"))

# Plot graph
ggplot(final_yield_curve, aes(x = date, y = difference)) +
  geom_line() +
  
# Adding gray recession lines
geom_rect(data = recessions1, aes(NULL, NULL, xmin = From, xmax = To),
            ymin = -5, ymax = 5, colour = "grey", size = 0.5, alpha = 0.2) +
 
# Changing fill in color of graph  
geom_ribbon(aes(ymin = 0,
                  ymax = ifelse(difference > 0, difference, 0)), fill = "blue", alpha = 0.15) +
geom_ribbon(aes(ymin = ifelse(difference < 0,difference, 0), 
                  ymax = 0), 
              fill = "red", alpha = 0.15) +

# Adding rug 
geom_rug(sides = "b", alpha = 0.5, position = "jitter", aes(color = ifelse(difference < 0, "dark blue", "red"))) +
  guides(color = FALSE) +
  
# Adding titles
labs(title = "Yield Curve Inversion: 10 year minus 3 months U.S Treasury rates",
     subtitle = "Differences in % points, monthly averages. Shaded areas correspoind to recessions",
     x = "",
     y = "Difference (10 years - 3 months) yield in %",
     caption = "Source: TfL, London Data Store") + 
  theme_bw()

```
