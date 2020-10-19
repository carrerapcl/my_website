---
categories:
- Data analytics
- Data science
client: Self-work
date: "2020-09-25T12:14:34+06:00"
description: This is meta description.
draft: false
image: images/portfolio/item-1.png
project_url: none
title: Where Do People Drink The Most Beer, Wine And Spirits?
---

#### Project Requirements# Where Do People Drink The Most Beer, Wine And Spirits?

#### Project and data set

We are going to use the data about drinks consumption from library `fivethirtyeight`.

```{r, load_alcohol_data}
library(fivethirtyeight)
data(drinks)
```

The drinks data has 1 character variable and 4 numeric variables and there are no missing values we should worry about.

```{r glimpse_skim_data}
skim(drinks)
```

Below is a plot of the top 25 beer consuming countries.

```{r beer_plot}
drinks_beer <- drinks %>% 
  arrange(desc(beer_servings)) %>% 
  head(25)
ggplot(drinks_beer, aes(y = reorder(country, beer_servings), x = beer_servings)) + 
  geom_col() + 
  labs(title = "Global Beer Consumption", 
       y = "",
       x = "Beer Servings",
       caption = "Source: FiveThirtyEight") +
  theme_economist()
```

Next is a plot that shows the top 25 wine consuming countries.

```{r wine_plot}
drinks_wine <- drinks %>% 
  arrange(desc(wine_servings)) %>% 
  head(25)
ggplot(drinks_wine, aes(y = reorder(country, wine_servings), x = wine_servings)) + 
  geom_col() + 
  labs(title = "Global Wine Consumption", 
       y = "",
       x = "Wine Servings",
       caption = "Source: FiveThirtyEight") +
  theme_economist()
```

Finally, a plot that shows the top 25 spirit consuming countries.

```{r spirit_plot}
drinks_spirit <- drinks %>% 
  arrange(desc(spirit_servings)) %>% 
  head(25)
ggplot(drinks_spirit, aes(y = reorder(country, spirit_servings), x = spirit_servings)) + 
  geom_col() + 
  labs(title = "Global Spirit Consumption", 
       y = "",
       x = "Spirit Servings",
       caption = "Source: FiveThirtyEight") +
  theme_economist()
```

Across the board, there appears to be a strong cultural bias to the types of alcohol that countries consume. For example, Germany is fourth on the chart for global annual beer consumption per person and France tops the wine consumption chart. Furthermore, particularly for beer and spirit consumption, countries that are culturally tied through history have similar consumption patterns. Namibia tops the chart for beer consumption, likely due to its status as a former German colony. A similar pattern exists on the spirits chart as well. Many of the countries in the former Soviet Union and Eastern Bloc appear on that chart, likely due to the common consumption of vodka. 

Contrary to the beer and spirit categories, however, wine consumption is much less tied to shared cultural history and seems more reliant on shared geography and geographic proximity. Because wine is an alcoholic beverage that requires grapes grown in specific climates, it is not as easy to produce across the world. Therefore, the consumption of wine is heavily concentrated in Europe and thus close to the wine producing regions of France, Italy, and Portugal.