# Setup -------------------------------------------------------------------

# Load relevant libraries

library(tidyverse)
library(readxl)

# Read in 538 election denial data, and seperate the house and statewide datasets

ed <- 
  read_csv("data/raw/fivethirtyeight_election_deniers.csv") %>% 
  
  mutate(dist = gsub("At-Large", "AL", District), .keep = "unused")  %>% 
  
  mutate(Source = gsub("To", "to", Source))

# Diagnostics -------------------------------------------------------------

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


# Correct Known Errors ----------------------------------------------------

ed %>% filter(is.na(Stance)) %>% select(State, dist) %>% print(n=18) %>% arrange()
# CA 30, CA 29, CA 10, CA 37, CA 15, CA 16, CA 34, NY 13, NY 9, MA 4 not in data
# CA2, Douglas Brower, No Comment
# RI1, Allan Waters, No Comment
# RI2, Allan Fung, Accepted with Reservations
# DE0, Lee Murphy, No Comment
# NH1, Karoline Leavitt, Fully denied
# IL17, Esther Joy King, No Comment
# NH2, Robert Burns, Accepted with Reservations

names(ed)
unique(ed$Stance)
unique(ed$dist)
CA2 <- c("Douglas Brower","No","California","Representative","No comment",NA,NA,NA,"2")
RI1 <- c("Allan R. Waters","No","Rhode Island","Representative","No comment",NA,NA,NA,"1")
RI2 <- c("Allan W. Fung","No","Rhode Island","Representative","Accepted with reservations",NA,NA,NA,"2")
DEAL <- c("Lee Murphy","No","Delaware","Representative","No comment",NA,NA,NA,"AL")
NH2 <- c("Robert Burns","No","New Hampshire","Representative","No comment",NA,NA,NA,"2")
NH1 <- c("Karoline Leavitt","No","New Hampshire","Representative","Fully denied",NA,NA,NA,"1")
#IL17 <- c(" Esther Joy King","No","Illinois","Representative","No comment",NA,NA,NA,"17")
lost <- data.frame(rbind(CA2,RI1, RI2, DEAL, NH1, NH2))
names(lost) <- names(ed)
ed <- rbind(ed,lost)



# Merge with Partisan Data ------------------------------------------------

# Import cartogram from previous project, which combines Cook 2022 ratings
# with homemade coordinate positions

cart <- 
  read_excel("data/raw/cd_cart_hex_118.xlsx") %>% 
  
  # Fix gross col name
  
  mutate(ST_dist = `ST#`, .keep = "unused") %>% 
  
  # remove unnecessary and unlisenced variables
  
  select(State, dist, PVI, metric, pvi_range, View, ST_dist, ST)

# Create primary data frame for analysis

df <- 
  
  # Call the cartogram / bowtie coords and PVI data
  
  cart %>% 
  
  # Join with the House election denier data
  
  left_join(
    ed %>% 
      filter(Office == "Representative") %>% 
      select(-c(Source,URL,Note)),
    on = c("State", "dist"))

# Pull data from NCSL:
# https://www.ncsl.org/research/elections-and-campaigns/primary-types.aspx
# Nebraska Sourced Seperately
# https://www.openprimaries.org/states_nebraska/

# Baseline is states are open

list_closed <- c("Delaware", "Maryland", "New York", "Florida", "Nevada", "Oregon", "Kentucky", "New Mexico", "Pennsylvania", "Nebraska")

list_partially_closed <- c("Connecticut", "Oklahoma", "Idaho", "South Dakota", "North Carolina", "Utah")

list_partially_open <- c("Illinois", "Ohio", "Indiana", "Tennessee", "Iowa", "Wyoming")

list_mostly_open <- c("Arizona", "Maine", "New Jersey", "Colorado", "Massachusetts", "Rhode Island", "Kansas", "New Hampshire", "West Virginia")

top_level <- c("Washington", "California", "Louisiana", "Alaska")

# Note: Georgia having a general election runoff is irrelevant

# create combined list

non_open <- c(list_closed, list_partially_closed, list_partially_open, list_mostly_open, top_level)

length(non_open)

# Check lengths by summing the non-open states to the number of open states

if((length(unique(non_open)) + 15 - 50) == 0) {print("All Accounted For")} else {print("check data")}

# Create a list of states that require majorities to win a primary (most runoff systems):
# https://ballotpedia.org/Runoff_election

majority_primaries <- c("Alabama", "Arkansas", "Georgia", "Mississippi", "Oklahoma", "South Carolina", "South Dakota", "Texas")

# Note: Vermont excluded; Vermont is only for ties (not really relevant to Congressional)
# Note: North Carolina excluded; NC has a 30% +1 rule for runoffs, making it non-majority
# Note: Georgia excluded due to only having general election runoffs
# Note: Louisiana funky, might have to add to this list

# create new categorical columns
df <- 
  df %>% 
  
  # Create a dummy that is 1 when a state requires a majority
  
  mutate(
    req_majority = 
      ifelse(
        State %in% 
          majority_primaries, 
        1, 
        0)) %>% 
  
  # Create a likert scale for primary openness
  
  mutate(
    primary_scale =
      case_when(
        State %in% top_level ~ 0,
        State %in% list_mostly_open ~ 2,
        State %in% list_partially_open ~ 3,
        State %in% list_partially_closed ~ 4,
        State %in% list_closed ~ 5) 
    
    # Add var to reflect unlisted states being open
    
    %>% replace_na(1))

# Read in fundraising data
# https://www.fec.gov/campaign-finance-data/all-candidates-file-description/

fec_key <- read_csv("data/raw/fec_key.csv")

# https://www.fec.gov/data/browse-data/?tab=bulk-data

fundraising <- read_delim("data/raw/weball22.txt", col_names = F) %>% 
  set_names(fec_key$`Column name`) %>%
  filter(CAND_PTY_AFFILIATION == "REP") %>% 
  filter(startsWith(CAND_ID, "H")) %>% 
  select(CAND_OFFICE_ST,CAND_ID,CAND_NAME) %>% 
  rename(ST = CAND_OFFICE_ST) %>% 
  mutate(last_name = tolower(gsub(",.*$", "", CAND_NAME)))

# Find Problems in Candidate suffixes

#df %>% mutate(last_name = sub(".*\\s", "", Candidate)) %>% select(last_name) %>% arrange(nchar(last_name)) %>% unique() %>% print(n = 40)

# enumerate issues and make it regex friendly

problems <- c(" III", " II", " Sr.", " Jr.")
find.string <- paste(unlist(problems), collapse = "|")

# Create last name and add variable

df <- df %>% 
  mutate(Candidate2 = gsub(find.string, replacement = "", x = Candidate)) %>% 
  mutate(last_name = tolower(sub(".*\\s", "", Candidate2)), 
         dist = as.integer(gsub("AL", 0, dist)),
         .keep = "unused") %>% 
  filter(View == "Cartogram")

combo_list <- df %>% select(last_name, ST) %>% mutate(combo = paste0(ST,last_name)) %>% pull(combo)
clean <- fundraising %>% group_by(last_name, ST) %>% filter(n()>1) %>% ungroup() %>% mutate(combo = paste0(ST,last_name)) %>% filter(combo %in% combo_list)
write.csv(clean, "data/processed/view_dups.csv")

fundraising <- fundraising %>% left_join(read_csv("data/raw/view_dups_tagged.csv") %>% select(FEC_ID,8) %>% set_names(c("CAND_ID","Keep")), by = "CAND_ID") %>% mutate(Keep = replace_na(Keep, "Meh")) %>% filter(Keep != "Drop")

df <- df %>% left_join(fundraising, by = c("ST","last_name"))

# clean environment

rm(list=setdiff(ls(), "df"))

# Create key for primary openness

primaries_key <- 
  data.frame(
    primary_scale = 
      c(0:5),
    primary_type = 
      c("All Party", "Open", "Open to Unaffiliated", "Partially Open", "Partially Closed", "Closed")
  )
c(0:5)
# Create new variable to highlight better competitive bins

df <- 
  df %>% 
  mutate(
    comp_scale =
      case_when(
        metric > .6 ~ 3,
        metric > .55 ~ 2,
        metric > .52 ~ 1,
        metric > .47 ~ 0,
        metric > .44 ~ -1,
        metric > .39 ~ -2,
        metric > 0 ~ -3)) 

# create key for competitiveness

comp_key <-
  data.frame(
    comp_scale = 
      unique(df$comp_scale),
    comp_key = 
      c("Solid R", "Solid D", "Likely R", "Competitive", "Lean R", "Lean D", "Likely D")) %>% 
      arrange(comp_scale)

# Create scale stance metric

stance_key <- 
  data.frame(
    Stance = 
      unique(df$Stance), 
    Stance_scale = 
      c(-2,1,0,2,-1,0,NA),
    Stance_key = 
      c("Denial","Partial Acceptance","No Comment","Acceptance","Some Denial","No Comment","Not Tracked by 538")) %>% 
  arrange(Stance_scale)

# Add stance scale to df

df <- df %>% 
  left_join(stance_key) %>% 
  left_join(comp_key) %>% 
  left_join(primaries_key)


df %>% group_by(ST_dist) %>% filter(n()>1) %>% ungroup()



# Explore -----------------------------------------------------------------



df %>%
  select(comp_scale, Stance_scale) %>%
  # Summarize data by count
  group_by(comp_scale, Stance_scale) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(comp_scale, total_count, fill = Stance_scale)) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill")

df %>%
  select(primary_scale, Stance_scale) %>%
  # Summarize data by count
  group_by(primary_scale, Stance_scale) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(primary_scale, total_count, fill = Stance_scale)) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill")

df %>%
  select(req_majority, Stance_scale) %>%
  # Summarize data by count
  group_by(req_majority, Stance_scale) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(req_majority, total_count, fill = Stance_scale)) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill")





# Scrap -------------------------------------------------------------------

lm.stance<- lm(Stance_scale ~ req_majority + comp_scale, data = df)
summary(lm.stance)

# https://www.opensecrets.org/open-data/api-documentation

os <- 
  read_csv("data/raw/os_cand_ids.csv") %>% 
  filter(Party == "R") %>% 
  mutate(ST = substr(DistIDRunFor, start = 1, stop = 2),
         dist = as.integer(substr(DistIDRunFor, start = 3, stop = 4)),
         last_name = tolower(gsub(",.*$", "", CRPName))) %>% 
  drop_na 

sing_state <- c("SD","ND","VT","DE","AK","WY")

os$sing_state <- ifelse(os$ST %in% sing_state, 0, 1)
if_else(os$sing_state == 0, 0, os$dist)

length(unique(victors$DistIDRunFor))
