---
categories:
- Data analytics
- Social impact
- Hypothesis testing
client: Self-work
date: "2020-10-12T12:14:34+06:00"
description: This is meta description.
draft: false
image: images/portfolio/youth.jpeg
project_url: cdc.gov/healthyyouth/data/
title: Youth Risk Behavior Surveillance
---


Every two years, the Centers for Disease Control and Prevention conduct the [Youth Risk Behavior Surveillance System (YRBSS)](https://www.cdc.gov/healthyyouth/data/yrbs/index.htm) survey, where it takes data from high schoolers (9th through 12th grade), to analyze health patterns. You will work with a selected group of variables from a random sample of observations during one of the years the YRBSS was conducted.

#### Load the data

This data is part of the `openintro` textbook and we can load and inspect it. There are observations on 13 different variables, some categorical and some numerical. The meaning of each variable can be found by bringing up the help file:

```{r load-libraries, echo=FALSE}

library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
library(tidyquant)

```

```{r}

data(yrbss)
glimpse(yrbss)

```

Before you carry on with your analysis, it's is always a good idea to check with `skimr::skim()` to get a feel for missing values, summary statistics of numerical variables, and a very rough histogram.

#### Exploratory Data Analysis

You will first start with analyzing the `weight` of participants in kilograms. Using visualization and summary statistics, describe the distribution of weights. How many observations are we missing weights from?
We are missing 1,004 observations for weight in the data set. 

```{r, eda_on_weight}

yrbss_weights <- yrbss %>% 
  filter(!weight %in% NA) %>% 
  summarise(mean = mean(weight),
            median = median(weight),
            max = max(weight),
            min = min(weight),
            sd = sd(weight))
yrbss_weights
ggplot(yrbss, aes(x = weight)) +
  geom_histogram() +
  theme_bw() +
  labs(title = "Weighty Children",
       subtitle = "Distribution of Weights for American High Schoolers",
       x = "Weight (kg)",
       y = "Count",
       caption = "Source: CDC")
       
```

Next, consider the possible relationship between a high schooler’s weight and their physical activity. Plotting the data is a useful first step because it helps us quickly visualize trends, identify strong associations, and develop research questions.

Let’s create a new variable `physical_3plus`, which will be `yes` if they are physically active for at least 3 days a week, and `no` otherwise.

```{r}

yrbss <- yrbss %>% 
  mutate(physical_3plus = ifelse(physically_active_7d >= 3, "yes", "no"))

yrbss %>% filter(!is.na(physical_3plus)) %>% 
  group_by(physical_3plus) %>% 
  summarise(count = n()) %>% 
  mutate(prop= count/sum(count))

```

Can you provide a 95% confidence interval for the population proportion of high schools that are *NOT* active 3 or more days per week?

```{r}

weight_ci <- yrbss %>% 
  specify(response = physical_3plus, success = "no") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop") %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
weight_ci

```
  
Make a boxplot of `physical_3plus` vs. `weight`. Is there a relationship between these two variables? What did you expect and why?

```{r, boxplot}

weight_box <- yrbss %>% 
  filter(!physical_3plus %in% NA) %>% 
  ggplot(aes(x = weight, y = physical_3plus)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Muscle Mass?",
       subtitle = "Distribution of Weights for American High Schoolers by Physical Activity",
       x = "Weight (kg)",
       y = "3+ Hours of Physcical Activity per Day",
       caption = "Source: CDC")
weight_box

```

#### Confidence Interval

Boxplots show how the medians of the two distributions compare, but we can also compare the means of the distributions using either a confidence interval or a hypothesis test. Note that when we calculate the mean/SD, etc weight in these groups using the mean function, we must ignore any missing values by setting the `na.rm = TRUE`.

```{r}
yrbss %>%
  group_by(physical_3plus) %>%
  filter(!is.na(physical_3plus)) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE),
            sd_weight = sd(weight, na.rm=TRUE),
            count = n(),
            se_weight = sd_weight/sqrt(count),
            t_critical = qt(0.975, count-1), 
            margin_of_error = t_critical * se_weight,
            lower = mean_weight - t_critical * se_weight,
            upper = mean_weight + t_critical * se_weight
            )

```

There is an observed difference of about 1.77kg (68.44 - 66.67), and we notice that the two confidence intervals do not overlap. It seems that the difference is at least 95% statistically significant. Let us also conduct a hypothesis test.

#### Hypothesis test with formula

Write the null and alternative hypotheses for testing whether mean weights are different for those who exercise at least times a week and those who don’t.
* Null hypothesis: true difference in means is equal to 0
* Alternative hypothesis: true difference in means is not equal to 0

```{r}

t.test(weight ~ physical_3plus, data = yrbss)

```

#### Hypothesis test with `infer`

Next, we will introduce a new function, `hypothesize`, that falls into the infer workflow. You will use this method for conducting hypothesis tests.

But first, we need to initialize the test, which we will save as `obs_diff`.

```{r}

obs_diff <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

```

Notice how you can use the functions specify and calculate again like you did for calculating confidence intervals. Here, though, the statistic you are searching for is the difference in means, with the order being yes - no != 0.

After you have initialized the test, you need to simulate the test on the null distribution, which we will save as null.

```{r}

null_dist <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

```

Here, `hypothesize` is used to set the null hypothesis as a test for independence, i.e., that there is no difference between the two population means. In one sample cases, the null argument can be set to *point* to test a hypothesis relative to a point estimate.

Also, note that the `type` argument within generate is set to permute, which is the argument when generating a null distribution for a hypothesis test.

We can visualize this null distribution with the following code:

```{r}

ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()

```

Now that the test is initialized and the null distribution formed, we can visualise to see how many of these null permutations have a difference of at least `obs_stat` of `r obs_diff %>% pull() %>% round(2)`?

We can also calculate the p-value for your hypothesis test using the function `infer::get_p_value()`.

```{r}

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```

This the standard workflow for performing hypothesis tests.