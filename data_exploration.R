# Setup -------------------------------------------------------------------

# Load relevant libraries

library(tidyverse)
library(readxl)

# Read in 538 election denial data, and seperate the house and statewide datasets

ed <- 
  read_csv("data/raw/fivethirtyeight_election_deniers.csv") %>% 
  
  mutate(District = gsub("At-Large", "AL", District))  %>% 
  
  mutate(Source = gsub("To", "to", Source))

house_ed <- ed %>% 
  
  filter(Office == "Representative")

state_ed <- ed %>% 
  
  filter(Office != "Representative")

# Read in 2022 district-level data aggregated from Dave's

districts <-
  read_excel("data/raw/cd_cart_hex_118.xlsx") %>% 
  
  # Remove duplicate rows with hourglass positioning
  
  filter(View == "Cartogram") %>% 
  
  # Fix lean column
  
  mutate(lean = substr(PVI,1L,1L)) %>% 

  # Remove unnecessary bits

  select(State, dist, PVI, metric) %>% 
  
  # Fix names
  
  rename("District" = "dist")

# Import wikipedia sourced state PVIs

states <- 
  read_csv("data/raw/2022_redraw_workbook.csv") %>% 
  
  # Remove overturned maps
  
  filter(!grepl("Overturned", State)) %>% 
  
  # Remove unnecessary bits
  
  select(State, PVI, `Share GOP`) %>% 
  
  # Fix names
  
  rename("metric" = "Share GOP")


# Join --------------------------------------------------------------------

# Join House PVIS to the deniers

house <- 
  left_join(
  house_ed,
  districts,
  on = c("State","District"))

# Join State PVIS to deniers

state <- 
  left_join(
  state_ed,
  states,
  by = "State")

# Diagnose NAs

colSums(is.na(state))

# Combine

ed <- rbind(house,state)

# Clear

rm(districts, house, house_ed, state, state_ed, states)

# Diagnostics -------------------------------------------------------------

# Re-read ED to avoid problems

ed <- 
  read_csv("data/raw/fivethirtyeight_election_deniers.csv") %>% 
  
  mutate(District = gsub("At-Large", "AL", District)) %>% 
  
  mutate(Source = gsub("To", "to", Source),
         PVI = "State") # Placeholder

# Evaluate NAs

nas <-
  data.frame(colSums(is.na(ed)), row.names = NULL)

names(nas) <- c("Sum of NAs")

nas["Column Name"] <- names(ed)

na_plot <- ggplot(data = nas, aes(x = `Column Name`, y = `Sum of NAs`)) +
  geom_bar(stat = "identity", fill = "Black") +
  theme_classic() + 
  labs(title="Missing Data by Column")

ggsave("images/na_plot.png", device = png)

# View specifics

source_na_table <- ed %>% filter(is.na(Source)) %>% select(Stance) %>% table %>% data.frame

names(source_na_table) <- c("Stance", "Frequency")

source_na_plot <- ggplot(data = source_na_table, aes(x = Stance, y = Frequency)) +
  geom_bar(stat = "identity", fill = "DarkRed") +
  theme_classic() + 
  labs(title="Context of Missing Sources")

ggsave("images/source_na_plot.png", device = png)

url_na_table <- ed %>% filter(is.na(URL)) %>% select(Source) %>% table %>% data.frame

names(url_na_table) <- c("Source", "Frequency")

url_na_plot <- ggplot(data = url_na_table, aes(x = Source, y = Frequency)) +
  geom_bar(stat = "identity", fill = "DarkBlue") +
  theme_classic() + 
  labs(title="Context of Sources Missing URLs")

ggsave("images/na_source_plot.png", device = png)


# Distributions -----------------------------------------------------------

table(ed$Office)
table(ed$Stance)

pie = data.frame(table(ed$Stance))
names(pie) <- c("Stance","Quantity")

pie <- ggplot(pie, aes(x="", y=Quantity, fill=Stance)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  labs(title="Distribution of 2020 Election Stances")

ggsave("images/pie.png", device = png)
