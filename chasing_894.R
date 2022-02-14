# Creates charts tracking goals scored over a career using gamelogs

# Libraries
library(tidyverse)
library(rvest)
library(janitor)

# Function: get_gamelogs to pull data for an individual player

get_gamelogs <- function(first_name,last_name,first_season,last_season) {
  
  ## Set variables
  count_seasons <- last_season - first_season + 1 # Count number of seasons 
  df = data.frame() # empty dataframe we'll store each season into. This will be all goals by Ovi
  # First letter of last name
  # First 5 letters of last name
  # First 2 letters of first name
  
  ## Collect game data season by season, and store in single df
  
  for (i in 1:count_seasons) {
    curr_season <- first_season + i - 1 # current season, found by adding counter to the first season
    
    url <- paste(
      "https://www.hockey-reference.com/players/",
      substr(last_name,1,1),
      "/",
      substr(last_name,1,5),
      substr(first_name,1,2),
      "01/gamelog/",
      curr_season,
      "#gamelog",
      sep="")
    
    try({
      df_curr_year <- read_html(url) %>%
        html_node("table") %>%
        html_table(header = FALSE) %>% # header set as False due to double header row
        slice(-1) %>% # Don't need the first header row
        row_to_names(row_number = 1) %>% # Use the second header row as the column name
        clean_names() %>% # Handle duplicate column names
        filter(rk != "Rk") %>% # Remove intermediate column header rows %>%
        select(date,g,age,g_2,a,s,toi)
      
      df <- rbind(df,df_curr_year) # append current season to main df
    },silent = TRUE
    )
  }
  
  ## Some modifications
  df <- tibble::rowid_to_column(df, "ID") # Add a game number column
  df <- df %>%
    mutate(cum_goal = cumsum(g_2), # Add a running total goal count
           name = last_name) # Add name tag
  
  return(df)
}

# Get gamelog data from hockey-reference.com

df_99 <- get_gamelogs('Wayne','Gretzky',1980,1999)
df_97 <- get_gamelogs('Connor','McDavid',2016,2022)
df_8 <- get_gamelogs('Alex','Ovechkin',2006,2022)
df_29 <- get_gamelogs('Leon','Draisaitl',2015,2022)
df_9 <- get_gamelogs('Gordie','Howe',1947,1980) # one more season 1980 that we need to add in
df_68 <- get_gamelogs('Jaromir','Jagr',1991,2018)


# All time scorers - bring into single df

df_all <- bind_rows(df_99, df_9, df_8, df_68)

# Plot data of Top 4 scorers
top_4 <- ggplot() +
  geom_line(data = df_all, mapping = aes(x = ID, y = cum_goal, color = name), size = 1) +
  geom_text(data = df_all %>% filter(ID == max(ID)),aes(label = name,
                                                        x = ID + 100,
                                                        y = cum_goal,
                                                        color = name)) +
  labs(title = "NHL All-Time Goal Scorers",
       subtitle = "Ovechkin's chase to 894",
       caption = "Gamelog data from www.hockey-reference.com",
       x = "Game Number", 
       y = "Cumulative Goals Scored") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(lineheight=.8, face="bold", size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

top_4

# References

# [1] https://jlaw.netlify.app/2020/12/01/exploring-nhl-stanley-cup-champion-s-points-percentage-in-four-ggplots/
#     Showed me how to export from hockey-reference

# [2] https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop
#     To loop through years