---
categories:
- Data analytics
- Data visualization
- Finance
- Economics
client: Self-work
date: "2020-10-05T12:14:34+06:00"
description: This is meta description.
draft: false
image: images/portfolio/gdp.png
project_url: unstats.un.org/unsd/snaama/
title: GDP components over time and among countries
---

At the risk of oversimplifying things, the main components of gross domestic product, GDP are personal consumption (C), business investment (I), government spending (G) and net exports (exports - imports). You can read more about GDP and the different approaches in calculating at the [Wikipedia GDP page](https://en.wikipedia.org/wiki/Gross_domestic_product).

The GDP data we will look at is from the [United Nations' National Accounts Main Aggregates Database](https://unstats.un.org/unsd/snaama/Downloads), which contains estimates of total GDP and its components for all countries from 1970 to today. We will look at how GDP and its components have changed over time, and compare different countries and how much each component contributes to that country's GDP. The file we will work with is [GDP and its breakdown at constant 2010 prices in US Dollars](http://unstats.un.org/unsd/amaapi/api/file/6) and it has already been saved in the Data directory. Have a look at the Excel file to see how it is structured and organised


```{r read_GDP_data, eval=FALSE}

UN_GDP_data  <-  read_excel(here::here("data", "Download-GDPconstant-USD-countries.xls"), # Excel filename
                sheet="Download-GDPconstant-USD-countr", # Sheet name
                skip=2) # Number of rows to skip

```

The first thing we need to do is to tidy the data, as it is in wide format. We will make it into long, tidy format, expressing all figures in billions (divide values by `1e9`, or $10^9$), and renaming the indicators into something shorter.

```{r reshape_GDP_data}

library(reshape)

tidy_GDP_data  <-  UN_GDP_data %>% 
  pivot_longer(cols = 4:51, names_to = "Year", values_to = "Obs") %>% 
  mutate(Obs = Obs / 1e9) %>%
  
  #Rename indicators:
  mutate(IndicatorName=case_when(IndicatorName=="Exports of goods and services" ~ "Exports",
                                 IndicatorName=="Imports of goods and services" ~ "Imports",
                                 IndicatorName=="General government final consumption expenditure" ~ "Gov_Expenditure",
                                 IndicatorName=="Gross capital formation" ~ "Capital_formation",
                                 IndicatorName=="Household consumption expenditure (including Non-profit institutions serving households)" ~ "Household_Consumption",
                                 IndicatorName=="Gross Domestic Product (GDP)" ~ "GDP")) %>% 
  
  #Filter only relevant indicators:
  filter(IndicatorName %in% c("Exports", "Imports", "Gov_Expenditure", "Capital_formation", "Household_Consumption", "GDP"))

glimpse(tidy_GDP_data)


# Let us compare GDP components for these 3 countries
country_list <- c("United States","India", "Germany")

```

We now produce a plot for comparing the GDP components over time for three countries: United States, India and Germany. We see that household consumption expenditure in the US has increased strongly.

```{r gdp1, echo=FALSE, out.width="100%"}

# knitr::include_graphics(here::here("images", "gdp1.png"), error = FALSE)

tidy_GDP_data_filt <- tidy_GDP_data %>% 
  filter(Country %in% country_list,
         IndicatorName %in% c("Exports", "Imports", "Gov_Expenditure", "Capital_formation", "Household_Consumption")) %>% 
  group_by(Country) %>% 
  
  ggplot() +
  geom_line(aes(x=Year, y=Obs, group=IndicatorName, color=IndicatorName)) +
  facet_wrap(~Country) +
  theme_bw() +
  scale_x_discrete(breaks = c(1970,1980,1990,2000,2010)) +
  labs(x = "",
       y = "Billion US$",
       title = "GDP components over time",
       subtitle = "In constant 2010 USD") +
  scale_color_discrete(name="Components of GDP")

tidy_GDP_data_filt

```

Secondly, recall that GDP is the sum of Household Expenditure (Consumption *C*), Gross Capital Formation (business investment *I*), Government Expenditure (G) and Net Exports (exports - imports). Even though there is an indicator `Gross Domestic Product (GDP)` in your dataframe, we would like to calculate it given its components discussed above.

We will also check the % difference between what we calculated as GDP and the GDP figure included in the dataframe.

```{r gdp2, echo=FALSE, out.width="100%"}

tidy_GDP_data_2 <- tidy_GDP_data %>% 

#Re-shape the table to wide format because it is easier to make this computations that way.
  
pivot_wider(names_from="IndicatorName", values_from="Obs") %>% 
  mutate(Net_Exports = Exports - Imports,
         GDP_Calculated = Net_Exports + Gov_Expenditure + Capital_formation + Household_Consumption,
         GDP_Difference_Percentage = ((GDP_Calculated - GDP) / GDP) * 100) %>% 
  select(Year, Country, GDP_Difference_Percentage)
tidy_GDP_data_2

#knitr::include_graphics(here::here("images", "gdp2.png"), error = FALSE)

```

Lastly, this last chart shows us the different dynamic among these three countries (Germany, India and United States). In Germany, in 40 years there have no been important variations in the proportion of household or government expenditure among the total GDP. However, the proportion of net exports did increase, something that did not happen in India and in the US when compared to 1970. Moreover, India has experienced an important reduction in household expenditure and an increase in gross capital formation. A possible explanation of this is that households save more money and it ends up in investments and creation of new family businesses.

```{r gdp2, echo=FALSE, out.width="100%"}

GDP_data_3 <- tidy_GDP_data %>% 

#We create variables that show each GDP component in proportion to the total GDP of the country for that year
pivot_wider(names_from="IndicatorName", values_from="Obs") %>% 
  mutate(Net_Exports = Exports - Imports,
         GDP_Calculated = Net_Exports + Gov_Expenditure + Capital_formation + Household_Consumption,
         Gov_Expenditure_prop = (Gov_Expenditure / GDP_Calculated) * 100,
         Net_Exports_prop = (Net_Exports / GDP_Calculated) * 100,
         Capital_formation_prop = (Capital_formation / GDP_Calculated) * 100,
         Household_Consumption_prop = (Household_Consumption / GDP_Calculated) * 100) %>% 
   select(Year, Country, Gov_Expenditure_prop, Net_Exports_prop, Capital_formation_prop, Household_Consumption_prop)
GDP_data_3

#Tidy the data again
tidy_GDP_data_3  <-  GDP_data_3 %>% 
  pivot_longer(cols = 3:6, names_to = "IndicatorName", values_to = "Obs")

#Create the plot
plot_tidy_GDP_data_3 <- tidy_GDP_data_3 %>% 
  filter(Country %in% country_list,
         IndicatorName %in% c("Net_Exports_prop", "Gov_Expenditure_prop", "Capital_formation_prop", "Household_Consumption_prop")) %>%
  group_by(Country) %>% 
  
  ggplot() +
  geom_line(aes(x=Year, y=Obs, group=IndicatorName, color=IndicatorName)) +
  facet_wrap(~Country) +
  theme_bw() +
  scale_x_discrete(breaks = c(1970,1980,1990,2000,2010)) +
  labs(x = "",
       y = "Proportion",
       title = "GDP and its breakdown at constant 2010 prices in US Dollars",
       subtitle = "In constant 2010 USD",
       caption = "Source: United Nations, https://unstats.un.org/unsd/snaama/Downloads") +
  scale_color_discrete(name="Components of GDP")

plot_tidy_GDP_data_3

```