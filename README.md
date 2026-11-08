# STA220H1 – Assignment 1: Exploring the Netflix Dataset 
Hanzade Zehra Ucan 

1011766876 

Course: STA220H1 

November 6, 2026
                                                               


# Introduction and Thesis 


The Netflix dataset contains information about 5,990 users, including their demographic characteristics, subscription plans, and viewing preferences. The purpose of this analysis is to explore the data using graphical summaries in R and identify patterns in how users interact with Netflix. I focused on subscription choices, completion rates, and viewing behaviour to see whether certain characteristics are associated with differences in user engagement. 

 

My main research question is, "Are users' viewing habits more closely related to their Netflix engagement than their demographic characteristics or subscription plans?" This analysis explores the relationship between user engagement, measured by completion rate, and different user characteristics. By comparing completion rates across gender and subscription plans, as well as examining average session length and movie-watching preferences, I aim to identify which patterns are most noticeable in the dataset. 


 # Setup and Data Cleaning
 ### 1. Data Cleaning and Preparation

The original Netflix dataset contained 6,379 observations and 21 variables. Before starting the analysis, I checked the structure of the dataset, reviewed the summary statistics, and checked for missing values. There were no missing values in any of the variables.

First, duplicate user IDs were removed to ensure that each user appeared only once. This removed 185 duplicate records.

Next, users with ages below 13 or above 90 were removed because these values were considered outside the valid age range for this analysis. After removing duplicates, another 204 observations were excluded due to invalid ages.

I also converted the gender and subscription plan variables into factors so they could be treated as categorical variables in the analysis.

Finally, I created a new variable called `movie_group` to divide users into three groups based on their percentage of movie watching: low, medium, and high movie share. This allowed me to compare users with different viewing habits.

After all cleaning and preparation steps, the final dataset contained **5,990 users and 22 variables**. This cleaned dataset was used for the remaining analysis.

# R code and output for the cleaning steps:
```
library(tidyverse)

netflix_raw <- read_csv("netflix.csv")

str(netflix_raw)
summary(netflix_raw)

colSums(is.na(netflix_raw))

nrow(netflix_raw)

netflix <- netflix_raw |>
  distinct(user_id, .keep_all = TRUE) |>
  filter(age >= 13, age <= 90) |>
  mutate(
    gender = factor(gender,
                    levels = c("Female", "Male", "Other",
                               "Prefer not to say")),
    
    subscription_plan = factor(subscription_plan,
                               levels = c("Basic", "Standard",
                                          "Premium", "Premium+")),
    
    movie_group = cut_number(
      movie_watch_pct,
      3,
      labels = c("Low movie share",
                 "Medium movie share",
                 "High movie share")
    )
  )

levels(cut_number(netflix$movie_watch_pct, 3))
nrow(netflix)

nrow(netflix_raw) -
  nrow(distinct(netflix_raw, user_id))

nrow(distinct(netflix_raw, user_id)) - nrow(netflix)

table(netflix$movie_group)
```



# Response
 ```
 library(tidyverse)
> netflix_raw <- read_csv("netflix.csv")
Rows: 6379 Columns: 21                                                                                                                                        
── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────
Delimiter: ","
chr  (8): email, first_name, last_name, gender, country, state_province, subscription_plan, primary_device
dbl (12): user_id, age, household_size, subscription_start_year, subscription_start_month, subscription_start_day, aver...
lgl  (1): is_active

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> str(netflix_raw)
spc_tbl_ [6,379 × 21] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
 $ user_id                 : num [1:6379] 1 2 3 4 5 9 10 11 12 13 ...
 $ email                   : chr [1:6379] "figueroajohn@example.org" "blakeerik@example.com" "smiller@example.net" "mitchellclark@example.com" ...
 $ first_name              : chr [1:6379] "Erica" "Joshua" "Barbara" "Chelsea" ...
 $ last_name               : chr [1:6379] "Garza" "Bernard" "Williams" "Ferguson" ...
 $ age                     : num [1:6379] 43 38 32 11 21 24 24 67 41 32 ...
 $ gender                  : chr [1:6379] "Male" "Male" "Female" "Male" ...
 $ country                 : chr [1:6379] "USA" "USA" "USA" "USA" ...
 $ state_province          : chr [1:6379] "Massachusetts" "Texas" "Michigan" "Ohio" ...
 $ subscription_plan       : chr [1:6379] "Basic" "Premium+" "Standard" "Standard" ...
 $ is_active               : logi [1:6379] TRUE TRUE FALSE TRUE TRUE TRUE ...
 $ primary_device          : chr [1:6379] "Laptop" "Desktop" "Desktop" "Laptop" ...
 $ household_size          : num [1:6379] 1 2 3 2 6 5 1 2 2 3 ...
 $ subscription_start_year : num [1:6379] 2024 2024 2023 2024 2024 ...
 $ subscription_start_month: num [1:6379] 4 5 9 8 10 4 8 10 9 1 ...
 $ subscription_start_day  : num [1:6379] 8 24 22 21 28 13 4 7 26 18 ...
 $ average_watch_time      : num [1:6379] 0.89 3.29 0.59 3.38 1.24 0.58 1.01 1.99 1.44 0.95 ...
 $ movie_watch_pct         : num [1:6379] 58.1 43.3 27.8 16.6 19.5 38.5 73 51.8 38.2 34 ...
 $ tv_watch_pct            : num [1:6379] 41.9 56.7 72.2 83.4 80.5 61.5 27 48.2 61.8 66 ...
 $ sessions_per_week       : num [1:6379] 5 10 3 13 4 1 5 6 7 9 ...
 $ average_session_minutes : num [1:6379] 65.1 41.6 17.6 10 90.5 ...
 $ completion_rate_pct     : num [1:6379] 64.5 54.3 28.1 23.3 71.5 69 74.1 54.4 62.8 46.4 ...
 - attr(*, "spec")=
  .. cols(
  ..   user_id = col_double(),
  ..   email = col_character(),
  ..   first_name = col_character(),
  ..   last_name = col_character(),
  ..   age = col_double(),
  ..   gender = col_character(),
  ..   country = col_character(),
  ..   state_province = col_character(),
  ..   subscription_plan = col_character(),
  ..   is_active = col_logical(),
  ..   primary_device = col_character(),
  ..   household_size = col_double(),
  ..   subscription_start_year = col_double(),
  ..   subscription_start_month = col_double(),
  ..   subscription_start_day = col_double(),
  ..   average_watch_time = col_double(),
  ..   movie_watch_pct = col_double(),
  ..   tv_watch_pct = col_double(),
  ..   sessions_per_week = col_double(),
  ..   average_session_minutes = col_double(),
  ..   completion_rate_pct = col_double()
  .. )
 - attr(*, "problems")=<externalptr> 
> summary(netflix_raw)
    user_id         email            first_name         last_name              age            gender         
 Min.   :    1   Length:6379        Length:6379        Length:6379        Min.   : -7.00   Length:6379       
 1st Qu.: 2512   Class :character   Class :character   Class :character   1st Qu.: 27.00   Class :character  
 Median : 4918   Mode  :character   Mode  :character   Mode  :character   Median : 35.00   Mode  :character  
 Mean   : 4983                                                            Mean   : 35.05                     
 3rd Qu.: 7474                                                            3rd Qu.: 43.00                     
 Max.   :10000                                                            Max.   :109.00                     
   country          state_province     subscription_plan  is_active       primary_device     household_size 
 Length:6379        Length:6379        Length:6379        Mode :logical   Length:6379        Min.   :1.000  
 Class :character   Class :character   Class :character   FALSE:913       Class :character   1st Qu.:2.000  
 Mode  :character   Mode  :character   Mode  :character   TRUE :5466      Mode  :character   Median :2.000  
                                                                                             Mean   :2.861  
                                                                                             3rd Qu.:4.000  
                                                                                             Max.   :8.000  
 subscription_start_year subscription_start_month subscription_start_day average_watch_time movie_watch_pct
 Min.   :2022            Min.   : 1.000           Min.   : 1.00          Min.   :0.17       Min.   : 1.60  
 1st Qu.:2023            1st Qu.: 3.000           1st Qu.: 8.00          1st Qu.:1.20       1st Qu.:30.10  
 Median :2024            Median : 6.000           Median :16.00          Median :1.80       Median :42.50  
 Mean   :2024            Mean   : 6.439           Mean   :15.67          Mean   :2.13       Mean   :43.22  
 3rd Qu.:2024            3rd Qu.: 9.000           3rd Qu.:23.00          3rd Qu.:2.66       3rd Qu.:55.90  
 Max.   :2025            Max.   :12.000           Max.   :31.00          Max.   :8.00       Max.   :95.40  
  tv_watch_pct   sessions_per_week average_session_minutes completion_rate_pct
 Min.   : 4.60   Min.   : 0.00     Min.   : 10.00          Min.   :15.30      
 1st Qu.:44.10   1st Qu.: 4.00     1st Qu.: 49.10          1st Qu.:51.70      
 Median :57.50   Median : 7.00     Median : 64.80          Median :62.10      
 Mean   :56.78   Mean   : 7.41     Mean   : 65.29          Mean   :61.54      
 3rd Qu.:69.90   3rd Qu.:10.00     3rd Qu.: 81.20          3rd Qu.:72.10      
 Max.   :98.40   Max.   :32.00     Max.   :150.70          Max.   :98.30      
> colSums(is.na(netflix_raw))
                 user_id                    email               first_name                last_name 
                       0                        0                        0                        0 
                     age                   gender                  country           state_province 
                       0                        0                        0                        0 
       subscription_plan                is_active           primary_device           household_size 
                       0                        0                        0                        0 
 subscription_start_year subscription_start_month   subscription_start_day       average_watch_time 
                       0                        0                        0                        0 
         movie_watch_pct             tv_watch_pct        sessions_per_week  average_session_minutes 
                       0                        0                        0                        0 
     completion_rate_pct 
                       0 
> nrow(netflix_raw)
[1] 6379
> netflix <- netflix_raw |>
+   distinct(user_id, .keep_all = TRUE) |>
+   filter(age >= 13, age <= 90) |>
+   mutate(
+     gender = factor(gender,
+                     levels = c("Female", "Male", "Other",
+                                "Prefer not to say")),
+     
+     subscription_plan = factor(subscription_plan,
+                                levels = c("Basic", "Standard",
+                                           "Premium", "Premium+")),
+     
+     movie_group = cut_number(
+       movie_watch_pct,
+       3,
+       labels = c("Low movie share",
+                  "Medium movie share",
+                  "High movie share")
+     )
+   )
> levels(cut_number(netflix$movie_watch_pct, 3))
[1] "[1.6,34.3]"  "(34.3,51.1]" "(51.1,95.4]"
> nrow(netflix)
[1] 5990
> nrow(netflix_raw) -
+   nrow(distinct(netflix_raw, user_id))
[1] 185
> nrow(distinct(netflix_raw, user_id)) - nrow(netflix)
[1] 204
> table(netflix$movie_group)

   Low movie share Medium movie share   High movie share 
              1999               2005               1986 

 ```

<img width="447" height="230" alt="image" src="https://github.com/user-attachments/assets/a961147a-a6d9-4f25-acfe-ff07b6de3a41" />


# Plot 1 (Pie Chart): Subscription plan distribution

The pie chart shows how the 5,990 users in the cleaned dataset are distributed across the four Netflix subscription plans. It gives an overview of which plans are more common among the users in the sample. The differences in the sizes of the sections show whether users are concentrated in certain plans or whether subscriptions are more evenly distributed. This is useful context for the rest of the analysis because it shows how the sample is made up before comparing completion rates across subscription plans. We can see that the most common plan is the Standard Plan and the least common plan is the Premium+ Plan, based on the pie chart.

```
plan_counts <- netflix %>%
  count(subscription_plan) %>%
  mutate(
    percentage = round(100 * n / sum(n), 1),
    label = paste0(subscription_plan, "\n",
                   percentage, "%")
  )

plan_counts

ggplot(plan_counts, aes(x = "", y = n,
                        fill = subscription_plan)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Netflix Subscription Plans",
    fill = "Subscription Plan",
  ) +
  theme_void()
```

  subscription_plan     n percentage label         
  
  <fct>             <int>      <dbl> <chr>    
  
1 Basic              1192       19.9 "Basic\n19.9%"   

2 Standard           2106       35.2 "Standard\n35.2%"

3 Premium            2102       35.1 "Premium\n35.1%" 

4 Premium+            590        9.8 "Premium+\n9.8%" 





<img width="775" height="588" alt="image" src="https://github.com/user-attachments/assets/ad6e1143-2485-4c90-9d0d-5db0378bf6bd" />





# Plot 2 (Boxplot): Completion rate comparison by gender

The boxplot shows that completion rates are quite similar across the different gender groups. The average completion rates are all close to one another, ranging from approximately 61.3% to 62.2%. This suggests that gender does not show a large difference in the percentage of titles users finish. Although there are some small differences between the groups, their completion rates are generally at a similar level. This is interesting because it suggests that gender may not be a particularly strong factor in explaining differences in viewing completion within this dataset.


```
gender_summary <- netflix %>%
  group_by(gender) %>%
  summarise(
    users = n(),
    mean_completion = round(mean(completion_rate_pct), 2),
    median_completion = round(median(completion_rate_pct), 2),
    .groups = "drop"
  )

gender_summary

ggplot(netflix, aes(x = gender, y = completion_rate_pct, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Completion Rate Comparison by Gender",
    x = "Gender",
    y = "Completion Rate (%)",
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

  gender            users mean_completion median_completion
  
  <fct>             <int>           <dbl>             <dbl>
  
1 Female             2711            61.3              62  

2 Male               2680            61.8              62.2

3 Other               298            61.4              61.4

4 Prefer not to say   301            62.2              63.2




<img width="775" height="588" alt="image" src="https://github.com/user-attachments/assets/0df6ea03-a0ca-4f9a-8d74-53c8560d4d50" />



# Plot 3 (Boxplot): Completion rate by subscription plan
The boxplot comparing completion rates across subscription plans also shows relatively small differences. The average completion rates range from approximately 61.1% to 61.8%, meaning that users on different plans complete a fairly similar percentage of the titles they start. The Basic plan has a slightly higher average completion rate than the other plans, while Premium+ has a slightly lower average. However, these differences are small, so the plot does not suggest a strong relationship between subscription plan and completion rate. Having a more expensive subscription does not appear to mean that users are more likely to finish the titles they start.

```
plan_summary <- netflix %>%
  group_by(subscription_plan) %>%
  summarise(
    users = n(),
    mean_completion = round(mean(completion_rate_pct), 2),
    median_completion = round(median(completion_rate_pct), 2),
    .groups = "drop"
  )

plan_summary

ggplot(netflix, aes(x = subscription_plan,
                    y = completion_rate_pct,
                    fill = subscription_plan)) +
  geom_boxplot() +
  labs(
    title = "Completion Rate by Subscription Plan",
    x = "Subscription Plan",
    y = "Completion Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

subscription_plan users mean_completion median_completion

  <fct>             <int>           <dbl>             <dbl>
  
1 Basic              1192            61.8              62.4

2 Standard           2106            61.6              61.8

3 Premium            2102            61.5              62.2

4 Premium+            590            61.1              62.2




<img width="775" height="588" alt="image" src="https://github.com/user-attachments/assets/796eff41-43d0-40bb-a571-fb2141169578" />


# Plot 4 (Scatterplot): Average session length and completion rate
The scatterplot shows a positive relationship between average session length and completion rate. As average session length increases, completion rates generally tend to increase as well. The points are spread out, so the relationship is not perfect, but the upward-sloping trend line shows a noticeable overall pattern. The correlation is approximately 0.562, which indicates a moderate positive association between these two variables. This suggests that users who spend more time in an average viewing session also tend to complete a higher percentage of the titles they start.

```
cor(netflix$average_session_minutes,
    netflix$completion_rate_pct)

ggplot(netflix, aes(x = average_session_minutes,
                    y = completion_rate_pct)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, colour = "purple") +
  labs(
    title = "Average Session Length and Completion Rate",
    x = "Average Session Length (minutes)",
    y = "Completion Rate (%)",
  ) +
  theme_minimal()
```

[1] 0.5622028


<img width="775" height="588" alt="image" src="https://github.com/user-attachments/assets/917e72c0-876a-4f97-88ff-e0f9f32703cc" />


# Plot 5 (Histogram): Average session length by movie-watching group

The histograms compare average session lengths among users who spend a low, medium, or high percentage of their viewing time watching movies. There is a noticeable shift toward longer sessions as the movie-watching percentage increases. The average session length is approximately 51.9 minutes for the low movie-share group, 64.6 minutes for the medium group, and 79.7 minutes for the high group. This shows a clear difference in average session length between the three groups. Users who spend a larger share of their viewing time watching movies tend to have longer average sessions. This is one of the more noticeable patterns in the analysis and suggests that the type of content users watch may be related to how long their viewing sessions last.

```
movie_summary <- netflix %>%
  group_by(movie_group) %>%
  summarise(
    users = n(),
    mean_session = round(mean(average_session_minutes), 2),
    median_session = round(median(average_session_minutes), 2),
    .groups = "drop"
  )

movie_summary

ggplot(netflix, aes(x = average_session_minutes)) +
  geom_histogram(bins = 25, fill = "pink",
                 colour = "white") +
  facet_wrap(~ movie_group) +
  labs(
    title = "Average Session Length by Movie-Watching Group",
    x = "Average Session Length (minutes)",
    y = "Number of Users",
  ) +
  theme_minimal()
```


 movie_group        users mean_session median_session
 
  <fct>              <int>        <dbl>          <dbl>
  
1 Low movie share     1999         51.8           51.8

2 Medium movie share  2005         64.6           64.6

3 High movie share    1986         79.6           79.5



<img width="775" height="588" alt="image" src="https://github.com/user-attachments/assets/caee22f3-4e5e-4940-937d-51d6e7b47ea5" />




# Conclusion
Looking across all five plots, viewing behaviour shows more noticeable patterns than gender or subscription plan. Completion rates are relatively similar across gender and subscription groups, while average session length has a clearer positive relationship with completion rate. There is also a noticeable increase in average session length among users who spend a larger share of their viewing time watching movies. Overall, these findings suggest that users' viewing habits may be more informative when examining engagement than demographic characteristics or subscription choices. Since the analysis is based on graphical summaries, these patterns show associations rather than proving that one factor causes another.




















