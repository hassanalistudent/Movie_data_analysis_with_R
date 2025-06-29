library(tidyverse)
################### reading Data ###################################

movie_data <- read_csv("Movie Data Starter Project - Movie Data.csv")
View(movie_data)

sort(colSums(is.na(movie_data)), decreasing = TRUE)

###### clean the movie data #############

clean_movie_data <- movie_data %>% 
  select(where(~ sum(is.na(.))<=20))

View(clean_movie_data)


#### Number of movies per genre ##############

No_of_movies_per_genry<-clean_movie_data %>% 
  group_by(Genre_1) %>% 
  summarise(No_of_movies=n())
View(No_of_movies_per_genry)

ggplot(No_of_movies_per_genry,aes(x=Genre_1,y=No_of_movies))+
  geom_bar(stat = "identity",fill="skyblue")+
  labs(title = "Number of movies Per genre",x="Genre",y="No of Movies")+
  theme_minimal()

############ Total Box office revenue per genre #############

revenue_by_genre <- clean_movie_data %>%
  group_by(Genre_1) %>%
  summarise(`Box office revenue` =round(sum(`Box Office Revenue`)/1000000,1))

ggplot(revenue_by_genre,aes(x=Genre_1,y=`Box office revenue`))+
  geom_bar(stat = "identity",fill="skyblue")+
  geom_text(aes(label = paste0(round(`Box office revenue`, 1), "M")), size = 3.5, vjust = -0.5)+
  labs(title = "Box office revenue per genre",x="Genre",y="Box office revenue (In millions)")+
  theme_minimal()

######################## Budget per genere ###############################

budget_by_genre <- clean_movie_data %>%
  group_by(Genre_1) %>%
  summarise(`Budget` =round(sum(Budget)/1000000,1))

ggplot(budget_by_genre,aes(x=Genre_1,y=Budget))+
  geom_bar(stat = "identity",fill="skyblue")+
  geom_text(aes(label = paste0(round(Budget, 1), "M")), size = 3.5, vjust = -0.5)+
  labs(title = "Budget per genre",x="Genre",y="Budget (In millions)")+
  coord_flip()+
  theme_minimal()


####################### Top 10 loss making movies ####################

loss_movies <- clean_movie_data %>%
  mutate(Profit_loss = `Box Office Revenue` - Budget) %>%
  filter(Profit_loss < 0) %>% 
  mutate(Loss_Amount=-Profit_loss) %>% 
  arrange((desc(Loss_Amount))) %>% 
  arrange(Profit_loss) %>% 
  slice_head(n=10)
  

library(forcats)
library(scales)


loss_movies %>%
  mutate(`Movie Title` = fct_reorder(`Movie Title`, Loss_Amount)) %>%
  ggplot(aes(x = `Movie Title`, y = Loss_Amount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(Loss_Amount / 1e6, 1), "M")),
            size = 3.5, hjust = -0.1) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Top 10 Profit Losing Movies",
    x = "Movie",
    y = "Loss (in millions)"
  ) +
  coord_flip() +
  theme_minimal()



###################### top 10 profit making movies ########################

top10_profit <- profit_movies %>%
  arrange(desc(Profit_loss)) %>%
  slice_head(n = 10) %>%
  mutate(`Movie Title` = fct_reorder(`Movie Title`, Profit_loss))

ggplot(top10_profit, aes(x = `Movie Title`, y = Profit_loss)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(Profit_loss / 1e6, 1), "M")),
            size = 3.5, hjust = -0.1) +
  scale_y_continuous(labels = label_comma())+ 
  labs(
    title = "Top 10 Profit Making Movies",
    x = "Movie",
    y = "Profit (in millions)"
  ) +
  coord_flip() +
  theme_minimal()


##################### Which genre type movie is more expansive ? ###################


avg_cost_for_type <- clean_movie_data %>%
  group_by(Genre_1) %>% 
  summarise(Avg_cost=round(mean(Budget),2)) %>% 
  mutate(Genre_1 = fct_reorder(Genre_1, Avg_cost))
  
ggplot(avg_cost_for_type,aes(x=Genre_1,y=Avg_cost))+
  geom_bar(stat="identity",fill="skyblue")+
  geom_text(aes(label = paste0(round(Avg_cost / 1e6, 1), "M")),
            size = 3.5, hjust = -0.1) +
  scale_y_continuous(labels = label_comma())+
  labs(
    title = "Average Budget of Movie of each Genre",
    x = "Genre",
    y = "Budget (in millions)"
  ) +
  coord_flip() +
  theme_minimal()

######################### Most expansive movie of each genre #########

most_expensive_movies <- clean_movie_data %>%
  group_by(Genre_1) %>%
  slice_max(order_by = Budget, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(Budget))  # Optional: sort by budget

most_expensive_movies %>%
  mutate(Genre_1 = fct_reorder(Genre_1, Budget)) %>%
  ggplot(aes(x = Genre_1, y = Budget)) +
  geom_bar(stat = "identity", fill = "tomato") +
  geom_text(aes(label = paste0(`Movie Title`, " (", round(Budget / 1e6, 1), "M)")),
            hjust = -0.1, size = 3.5) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Most Expensive Movie by Genre",
    x = "Genre",
    y = "Budget (in millions)"
  ) +
  coord_flip() +
  theme_minimal()

############################ Flop directors #################


# Step 1: Compute movie counts by director and outcome
director_outcomes <- clean_movie_data %>%
  mutate(
    Profit_loss = `Box Office Revenue` - Budget,
    Outcome = if_else(Profit_loss > 0, "Successful", "Flop")
  ) %>%
  group_by(Director_1, Outcome) %>%
  summarise(movie_count = n(), .groups = "drop")

# Step 2: Compute total movie count per director
total_counts <- director_outcomes %>%
  group_by(Director_1) %>%
  summarise(total_movies = sum(movie_count), .groups = "drop")

# Step 3: Get top 10 directors with most flops
flop_counts <- director_outcomes %>%
  filter(Outcome == "Flop") %>%
  arrange(desc(movie_count)) %>%
  slice_head(n = 10) %>%
  select(Director_1)

# Step 4: Filter only those top flop directors
top_directors <- director_outcomes %>%
  filter(Director_1 %in% flop_counts$Director_1) %>%
  left_join(total_counts, by = "Director_1")

# Step 5: Plot - reorder based on total movie count
ggplot(top_directors, aes(x = reorder(Director_1, -total_movies), y = movie_count, fill = Outcome)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Directors with Most Flops (Sorted by Total Movies)",
    x = "Director",
    y = "Number of Movies",
    fill = "Movie Outcome"
  ) +
  coord_flip() +
  theme_minimal()


######################## top 10 successful directors ####################

# Step 1: Compute movie counts by director and outcome
director_outcomes <- clean_movie_data %>%
  mutate(
    Profit_loss = `Box Office Revenue` - Budget,
    Outcome = if_else(Profit_loss > 0, "Successful", "Flop")
  ) %>%
  group_by(Director_1, Outcome) %>%
  summarise(movie_count = n(), .groups = "drop")

# Step 2: Get top 10 directors with most successful movies
top_successful_directors <- director_outcomes %>%
  filter(Outcome == "Successful") %>%
  arrange(desc(movie_count)) %>%
  slice_head(n = 13) %>%
  select(Director_1, successful_count = movie_count)

# Step 3: Filter full outcomes for those top directors
top_directors <- director_outcomes %>%
  filter(Director_1 %in% top_successful_directors$Director_1) %>%
  left_join(top_successful_directors, by = "Director_1")

# Step 4: Plot - reorder by number of successful movies
ggplot(top_directors, aes(x = reorder(Director_1, -successful_count), y = movie_count, fill = Outcome)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Directors with Most Successful Movies (Stacked by Outcome)",
    x = "Director",
    y = "Number of Movies",
    fill = "Movie Outcome"
  ) +
  coord_flip() +
  theme_minimal()



#################################### Budget and revenue trend over the year ##############################

library(tidyverse)
library(lubridate)

# Parse date-time and extract year
yearly_data <- clean_movie_data %>%
mutate(Release_Date_Parsed = mdy_hm(`Release Date`)) %>%
mutate(Year = year(Release_Date_Parsed)) %>% 
  group_by(Year) %>%
  summarise(
    Total_Budget = round(sum(Budget, na.rm = TRUE) / 1e6, 1),
    Total_Revenue = round(sum(`Box Office Revenue`,na.rm = TRUE)/1e6,1),
    Movie_count= n()
  ) %>%
  arrange(Year)

# Plot 1: Total Budget
ggplot(yearly_data, aes(x = Year, y = Total_Budget)) +
  geom_line(color = "tomato", linewidth = 1.2) +
  geom_point(color = "tomato", size = 2) +
  labs(
    title = "Total Budget by Year",
    x = "Year",
    y = "Budget (in Millions USD)"
  ) +
  theme_minimal()

# Plot 2: Total Revenue
ggplot(yearly_data, aes(x = Year, y = Total_Revenue)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Total Revenue by Year",
    x = "Year",
    y = "Revenue (in Millions USD)"
  ) +
  theme_minimal()


## total movie count per year
ggplot(yearly_data, aes(x = Year, y = Movie_count)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Total Movies by Year",
    x = "Year",
    y = "Movie_count"
  ) +
  theme_minimal()


# Combine all cast columns into one long format
actor_appearance <- clean_movie_data %>%
  select(starts_with("Cast")) %>%
  pivot_longer(cols = everything(), names_to = "Cast_Role", values_to = "Actor") %>%
  filter(!is.na(Actor)) %>%
  group_by(Actor) %>%
  summarise(Appearances = n(), .groups = "drop") %>%
  arrange(desc(Appearances)) %>%
  slice_head(n = 15)

# Plot top 15 actors by appearances
ggplot(actor_appearance, aes(x = fct_reorder(Actor, Appearances), y = Appearances)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 15 Most Frequent Actors",
    x = "Actor",
    y = "Number of Movie Appearances"
  ) +
  theme_minimal()


# Merge cast info with revenue
actor_revenue <- clean_movie_data %>%
  select(`Box Office Revenue`, starts_with("Cast")) %>%
  pivot_longer(cols = starts_with("Cast"), names_to = "Cast_Role", values_to = "Actor") %>%
  filter(!is.na(Actor)) %>%
  group_by(Actor) %>%
  summarise(
    Average_Revenue = round(mean(`Box Office Revenue`, na.rm = TRUE) / 1e6, 1),
    Movie_Count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Revenue)) %>%
  filter(Movie_Count >= 3) %>%  # Optional: filter to avoid one-movie anomalies
  slice_head(n = 15)

# Plot top actors by average revenue
ggplot(actor_revenue, aes(x = fct_reorder(Actor, Average_Revenue), y = Average_Revenue)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 Actors by Average Revenue (Min 3 Movies)",
    x = "Actor",
    y = "Avg Box Office Revenue (in Millions)"
  ) +
  theme_minimal()



# Count how many times each director works in each genre
director_genre_freq <- clean_movie_data %>%
  filter(!is.na(Director_1), !is.na(Genre_1)) %>%
  group_by(Director_1, Genre_1) %>%
  summarise(Movie_Count = n(), .groups = "drop") %>%
  arrange(desc(Movie_Count))

# View top combinations
head(director_genre_freq, 10)

# Optional: visualize top 15 director-genre combos
top_director_genre <- director_genre_freq %>%
  slice_max(order_by = Movie_Count, n = 15)

ggplot(top_director_genre, aes(x = fct_reorder(paste(Director_1, Genre_1, sep = " - "), Movie_Count), y = Movie_Count)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 15 Director–Genre Combinations",
    x = "Director - Genre",
    y = "Movie Count"
  ) +
  theme_minimal()


# Compute profit and success metrics by director & genre
director_genre_success <- clean_movie_data %>%
  mutate(
    Profit = `Box Office Revenue` - Budget
  ) %>%
  filter(!is.na(Director_1), !is.na(Genre_1)) %>%
  group_by(Director_1, Genre_1) %>%
  summarise(
    Movies = n(),
    Avg_Revenue = round(mean(`Box Office Revenue`, na.rm = TRUE) / 1e6, 1),
    Avg_Profit = round(mean(Profit, na.rm = TRUE) / 1e6, 1),
    .groups = "drop"
  ) %>%
  filter(Movies >= 3) %>% # optional: only show directors with at least 3 movies in a genre
  arrange(desc(Avg_Profit))

# Top 10 director–genre combos by profit
top_success_director_genre <- director_genre_success %>%
  slice_max(order_by = Avg_Profit, n = 10)

ggplot(top_success_director_genre, aes(x = fct_reorder(paste(Director_1, Genre_1, sep = " - "), Avg_Profit), y = Avg_Profit)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(
    title = "Top 10 Most Profitable Director–Genre Combos",
    x = "Director - Genre",
    y = "Average Profit (in Millions)"
  ) +
  theme_minimal()



# Calculate ROI per movie
roi_data <- clean_movie_data %>%
  mutate(
    ROI = (`Box Office Revenue` - Budget) / Budget
  ) %>%
  filter(!is.na(ROI), Budget > 0, ROI != Inf) %>%
  arrange(desc(ROI)) %>%
  slice_head(n = 10)  # Top 10 ROI

# Plot top ROI movies
ggplot(roi_data, aes(x = fct_reorder(`Movie Title`, ROI), y = ROI)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = paste0(round(ROI * 100, 0), "%")), hjust = -0.1, size = 3.5) +
  labs(
    title = "Top 10 Movies by ROI",
    x = "Movie Title",
    y = "Return on Investment (ROI)",
    caption = "ROI = (Revenue - Budget) / Budget"
  ) +
  coord_flip() +
  theme_minimal()


# Step 1: Unnest director-actor pairs and calculate profit
director_actor_profit <- clean_movie_data %>%
  select(`Movie Title`, Director_1, starts_with("Cast"), Budget, `Box Office Revenue`) %>%
  mutate(Profit = `Box Office Revenue` - Budget) %>%
  pivot_longer(cols = starts_with("Cast"), names_to = "Cast_Role", values_to = "Actor") %>%
  filter(!is.na(Director_1), !is.na(Actor)) %>%
  group_by(Director_1, Actor) %>%
  summarise(
    Movie_Count = n(),
    Avg_Profit = round(mean(Profit, na.rm = TRUE) / 1e6, 1),
    Success_Rate = mean(Profit > 0),
    .groups = "drop"
  ) %>%
  filter(Movie_Count >= 2)  # Only consider recurring pairs

top_avg_profit_pairs <- director_actor_profit %>%
  arrange(desc(Avg_Profit)) %>%
  slice_head(n = 5)

ggplot(top_avg_profit_pairs, aes(x = fct_reorder(paste(Director_1, Actor, sep = " & "), Avg_Profit), y = Avg_Profit)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = paste0(Avg_Profit, "M")), hjust = -0.1, size = 3.5) +
  labs(
    title = "Top 5 Director–Actor Pairs by Average Profit",
    x = "Director & Actor",
    y = "Avg Profit (in Millions)"
  ) +
  coord_flip() +
  theme_minimal()



# Combine all cast columns into one long format
actor_appearance <- clean_movie_data %>%
  select(starts_with("Cast")) %>%
  pivot_longer(cols = everything(), names_to = "Cast_Role", values_to = "Actor") %>%
  filter(!is.na(Actor)) %>%
  group_by(Actor) %>%
  summarise(Appearances = n(), .groups = "drop") %>%
  arrange(desc(Appearances)) %>%
  slice_head(n = 15)

# Plot top 15 actors by appearances
ggplot(actor_appearance, aes(x = fct_reorder(Actor, Appearances), y = Appearances)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 15 Most Frequent Actors",
    x = "Actor",
    y = "Number of Movie Appearances"
  ) +
  theme_minimal()


# Merge cast info with revenue
actor_revenue <- clean_movie_data %>%
  select(`Box Office Revenue`, starts_with("Cast")) %>%
  pivot_longer(cols = starts_with("Cast"), names_to = "Cast_Role", values_to = "Actor") %>%
  filter(!is.na(Actor)) %>%
  group_by(Actor) %>%
  summarise(
    Average_Revenue = round(mean(`Box Office Revenue`, na.rm = TRUE) / 1e6, 1),
    Movie_Count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Revenue)) %>%
  filter(Movie_Count >= 3) %>%  # Optional: filter to avoid one-movie anomalies
  slice_head(n = 15)

# Plot top actors by average revenue
ggplot(actor_revenue, aes(x = fct_reorder(Actor, Average_Revenue), y = Average_Revenue)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 Actors by Average Revenue (Min 3 Movies)",
    x = "Actor",
    y = "Avg Box Office Revenue (in Millions)"
  ) +
  theme_minimal()



# Count how many times each director works in each genre
director_genre_freq <- clean_movie_data %>%
  filter(!is.na(Director_1), !is.na(Genre_1)) %>%
  group_by(Director_1, Genre_1) %>%
  summarise(Movie_Count = n(), .groups = "drop") %>%
  arrange(desc(Movie_Count))

# View top combinations
head(director_genre_freq, 10)

# Optional: visualize top 15 director-genre combos
top_director_genre <- director_genre_freq %>%
  slice_max(order_by = Movie_Count, n = 15)

ggplot(top_director_genre, aes(x = fct_reorder(paste(Director_1, Genre_1, sep = " - "), Movie_Count), y = Movie_Count)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 15 Director–Genre Combinations",
    x = "Director - Genre",
    y = "Movie Count"
  ) +
  theme_minimal()


# Compute profit and success metrics by director & genre
director_genre_success <- clean_movie_data %>%
  mutate(
    Profit = `Box Office Revenue` - Budget
  ) %>%
  filter(!is.na(Director_1), !is.na(Genre_1)) %>%
  group_by(Director_1, Genre_1) %>%
  summarise(
    Movies = n(),
    Avg_Revenue = round(mean(`Box Office Revenue`, na.rm = TRUE) / 1e6, 1),
    Avg_Profit = round(mean(Profit, na.rm = TRUE) / 1e6, 1),
    .groups = "drop"
  ) %>%
  filter(Movies >= 3) %>% # optional: only show directors with at least 3 movies in a genre
  arrange(desc(Avg_Profit))

# Top 10 director–genre combos by profit
top_success_director_genre <- director_genre_success %>%
  slice_max(order_by = Avg_Profit, n = 10)

ggplot(top_success_director_genre, aes(x = fct_reorder(paste(Director_1, Genre_1, sep = " - "), Avg_Profit), y = Avg_Profit)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(
    title = "Top 10 Most Profitable Director–Genre Combos",
    x = "Director - Genre",
    y = "Average Profit (in Millions)"
  ) +
  theme_minimal()



# Calculate ROI per movie
roi_data <- clean_movie_data %>%
  mutate(
    ROI = (`Box Office Revenue` - Budget) / Budget
  ) %>%
  filter(!is.na(ROI), Budget > 0, ROI != Inf) %>%
  arrange(desc(ROI)) %>%
  slice_head(n = 10)  # Top 10 ROI

# Plot top ROI movies
ggplot(roi_data, aes(x = fct_reorder(`Movie Title`, ROI), y = ROI)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = paste0(round(ROI * 100, 0), "%")), hjust = -0.1, size = 3.5) +
  labs(
    title = "Top 10 Movies by ROI",
    x = "Movie Title",
    y = "Return on Investment (ROI)",
    caption = "ROI = (Revenue - Budget) / Budget"
  ) +
  coord_flip() +
  theme_minimal()


# Step 1: Unnest director-actor pairs and calculate profit
director_actor_profit <- clean_movie_data %>%
  select(`Movie Title`, Director_1, starts_with("Cast"), Budget, `Box Office Revenue`) %>%
  mutate(Profit = `Box Office Revenue` - Budget) %>%
  pivot_longer(cols = starts_with("Cast"), names_to = "Cast_Role", values_to = "Actor") %>%
  filter(!is.na(Director_1), !is.na(Actor)) %>%
  group_by(Director_1, Actor) %>%
  summarise(
    Movie_Count = n(),
    Avg_Profit = round(mean(Profit, na.rm = TRUE) / 1e6, 1),
    Success_Rate = mean(Profit > 0),
    .groups = "drop"
  ) %>%
  filter(Movie_Count >= 2)  # Only consider recurring pairs

### 2A. Top 5 Pairs by Average Profit
top_avg_profit_pairs <- director_actor_profit %>%
  arrange(desc(Avg_Profit)) %>%
  slice_head(n = 5)

ggplot(top_avg_profit_pairs, aes(x = fct_reorder(paste(Director_1, Actor, sep = " & "), Avg_Profit), y = Avg_Profit)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = paste0(Avg_Profit, "M")), hjust = -0.1, size = 3.5) +
  labs(
    title = "Top 5 Director–Actor Pairs by Average Profit",
    x = "Director & Actor",
    y = "Avg Profit (in Millions)"
  ) +
  coord_flip() +
  theme_minimal()

# Label movies based on genre columns
genre_comparison <-movie_data %>%
  mutate(
    Genre_Type = if_else(!is.na(Genre_2), "Multi-genre", "Single-genre"),
    Profit = `Box Office Revenue` - Budget,
    Success = Profit > 0
  ) %>%
  group_by(Genre_Type) %>%
  summarise(
    Avg_Budget = round(mean(Budget, na.rm = TRUE) / 1e6, 1),
    Avg_Revenue = round(mean(`Box Office Revenue`, na.rm = TRUE) / 1e6, 1),
    Success_Rate = round(mean(Success, na.rm = TRUE) * 100, 1),
    Movie_Count = n(),
    .groups = "drop"
  )

print(genre_comparison)

########### Revenue Efficiency by Genre ##########################



genre_efficiency <- clean_movie_data %>%
  filter(!is.na(Genre_1)) %>%
  group_by(Genre_1) %>%
  summarise(
    Total_Revenue = sum(`Box Office Revenue`, na.rm = TRUE),
    Total_Budget = sum(Budget, na.rm = TRUE),
    Efficiency = round(Total_Revenue / Total_Budget, 2),
    .groups = "drop"
  ) %>%
  filter(Total_Budget > 0) %>%
  arrange(desc(Efficiency))

ggplot(genre_efficiency, aes(x = fct_reorder(Genre_1, Efficiency), y = Efficiency)) +
  geom_col(fill = "darkcyan") +
  geom_text(aes(label = Efficiency), hjust = -0.1, size = 3.5) +
  labs(
    title = "Genre Revenue Efficiency (Revenue-to-Budget Ratio)",
    x = "Genre",
    y = "Revenue / Budget Ratio"
  ) +
  coord_flip() +
  theme_minimal()

######################### Release Timing Impact ################################


release_timing <- clean_movie_data %>%
  mutate(
    Release_Date = mdy_hm(`Release Date`),
    Release_Month = month(Release_Date, label = TRUE, abbr = TRUE),  # e.g., Jan, Feb
    Release_Quarter = quarter(Release_Date, with_year = FALSE),
    Profit = `Box Office Revenue` - Budget,
    Success = Profit > 0
  )

by_month <- release_timing %>%
  group_by(Release_Month) %>%
  summarise(
    Avg_Revenue = round(mean(`Box Office Revenue`, na.rm = TRUE) / 1e6, 1),
    Avg_Profit = round(mean(Profit, na.rm = TRUE) / 1e6, 1),
    Success_Rate = round(mean(Success, na.rm = TRUE) * 100, 1),
    Movie_Count = n(),
    .groups = "drop"
  )

# Revenue
ggplot(by_month, aes(x = Release_Month, y = Avg_Revenue)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Revenue by Release Month", y = "Revenue (in Millions)", x = "Month") +
  theme_minimal()

# Profit
ggplot(by_month, aes(x = Release_Month, y = Avg_Profit)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Average Profit by Release Month", y = "Profit (in Millions)", x = "Month") +
  theme_minimal()

###########  Ensemble vs. Star-Centric Cast 
#Compare performance of movies with:
  
#Full cast (5 actors)

#Few cast members (1–2)

#Determine if bigger casts correlate with higher returns.



# Count non-NA cast members
cast_analysis <- clean_movie_data %>%
  mutate(
    Cast_Size = rowSums(!is.na(select(., starts_with("Cast")))),
    Cast_Type = case_when(
      Cast_Size <= 2 ~ "Star-Centric",
      Cast_Size >= 5 ~ "Ensemble",
      TRUE ~ "Mid-Sized"
    ),
    Profit = `Box Office Revenue` - Budget,
    Success = Profit > 0
  )


cast_type_summary <- cast_analysis %>%
  group_by(Cast_Type) %>%
  summarise(
    Avg_Budget = round(mean(Budget, na.rm = TRUE) / 1e6, 1),
    Avg_Revenue = round(mean(`Box Office Revenue`, na.rm = TRUE) / 1e6, 1),
    Avg_Profit = round(mean(Profit, na.rm = TRUE) / 1e6, 1),
    Success_Rate = round(mean(Success, na.rm = TRUE) * 100, 1),
    Movie_Count = n(),
    .groups = "drop"
  )
ggplot(cast_type_summary, aes(x = Cast_Type, y = Success_Rate, fill = Cast_Type)) +
  geom_col() +
  geom_text(aes(label = paste0(Success_Rate, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Success Rate by Cast Type",
    x = "Cast Type",
    y = "Success Rate (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Ensemble" = "darkorange", "Star-Centric" = "skyblue", "Mid-Sized" = "gray"))



ggplot(cast_type_summary, aes(x = Cast_Type, y = Avg_Revenue, fill = Cast_Type)) +
  geom_col() +
  geom_text(aes(label = paste0(Avg_Revenue, "M")), vjust = -0.5, size = 4) +
  labs(
    title = "Average Revenue by Cast Type",
    x = "Cast Type",
    y = "Avg Revenue (in Millions)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Ensemble" = "darkgreen", "Star-Centric" = "steelblue", "Mid-Sized" = "gray"))
