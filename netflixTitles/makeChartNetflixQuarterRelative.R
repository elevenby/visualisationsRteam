# Update all installed packages / problem dauert zu lange
# update.packages()

library(tidyverse) # for data manipulation
library(ggplot2) # for data visualization
library(gridExtra) # for arranging multiple plots
library(wordcloud) # for generating word clouds
library(dplyr)
library(tidyselect) 

# Update all installed packages
update.packages()

# this creates the tables
library(readr)  # Load the readr library for reading CSV files

# Read the "netflix_titles.csv" file into a data frame
df <- read_csv('data/netflix_titles.csv')

head(df)  # Display the first 6 rows of the data frame

df$date_added <- as.Date(df$date_added, format = "%m/%d/%Y")


# Extract quarter from 'date_added' column
library(lubridate)
df$quarter_added <- quarters(df$date_added)



# Define a function to get the quarter name
quarter_expression <- function(i) {
  if (i == 1) {
    return('1st Quarter')
  } else if (i == 2) {
    return('2nd Quarter')
  } else if (i == 3) {
    return('3rd Quarter')
  } else {
    return('4th Quarter')
  }
}

# Convert 'date_added' column from character to date format
df$date_added <- as.Date(df$date_added)

# Calculate the number of attractions added per quarter and store the results in a data frame
df$quarter_added <- quarters(df$date_added)
qrt_df <- table(df$quarter_added)

# Calculate the percentage of attractions added per quarter and store the results in a data frame
prct_qrt <- data.frame(Prop = round(prop.table(qrt_df), 2))
prct_qrt <- t(prct_qrt)


library(ggplot2)
library(dplyr)

# Create a bar plot
types_df %>%
  ggplot(aes(x = percent, y = type, fill = type)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12, family = "Ubuntu", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, family = "Ubuntu", color = "black", face = "bold")) +
  ggtitle("Attraction type in Netflix by percent (%)") +
  
  # Add text labels for percentage and type
  geom_text(aes(label = paste0(percent, "%")), x = 0, y = seq_along(type), hjust = 0, size = 7, family = "Ubuntu", fontface = "bold", color = "white") +
  geom_text(aes(label = ifelse(row_number() == 1, "Movie", "TV Show")), x = 0, y = seq_along(type) + 0.3, hjust = 0, size = 3, family = "Ubuntu", fontface = "bold", color = "white") +
  
  # Remove spines
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank()) +
  
  # Set x-axis limit
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0))

            
            
            
            
            
            
            
            
            