# Setup -------------------------------------------------------------------

# Load relevant libraries

library(tidyverse)
library(readxl)
library(scales)

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

state <- 
  read_csv("data/raw/2022_redraw_workbook.csv") %>%
  mutate(ST_metric = `Share GOP`) %>% 
  select(ST, ST_metric) %>% distinct()

# Create primary data frame for analysis

df <- 
  
  # Call the cartogram / bowtie coords and PVI data
  
  cart %>% 
  
  # Join with the House election denier data
  
  left_join(
    ed %>% 
      filter(Office == "Representative") %>% 
      select(-c(Source,URL,Note)),
    on = c("State", "dist")) %>% 
  
  # Add State 
  
  left_join(state, on = "ST")


# Add Primary Election Data -----------------------------------------------

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


# Add Fundraising Data ----------------------------------------------------

# Read in fundraising data
# https://www.fec.gov/campaign-finance-data/all-candidates-file-description/

fec_key <- read_csv("data/raw/fec_key.csv")

# https://www.fec.gov/data/browse-data/?tab=bulk-data

fundraising <- read_delim("data/raw/weball22.txt", col_names = F) %>% 
  set_names(fec_key$`Column name`) %>%
  filter(CAND_PTY_AFFILIATION == "REP") %>% 
  filter(startsWith(CAND_ID, "H")) %>% 
  select(-c(CAND_ICI, PTY_CD, CAND_PTY_AFFILIATION, COH_BOP, COH_COP, CAND_OFFICE_DISTRICT, SPEC_ELECTION, PRIM_ELECTION, RUN_ELECTION, GEN_ELECTION, GEN_ELECTION_PRECENT)) %>% 
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

# Pull risky data and examine pre merge

#combo_list <- df %>% select(last_name, ST) %>% mutate(combo = paste0(ST,last_name)) %>% pull(combo)
#clean <- fundraising %>% group_by(last_name, ST) %>% filter(n()>1) %>% ungroup() %>% mutate(combo = paste0(ST,last_name)) %>% filter(combo %in% combo_list)
#write.csv(clean, "data/processed/view_dups.csv")

# Import drop list and remove join-unfriendly observations

fundraising <- fundraising %>% left_join(read_csv("data/raw/view_dups_tagged.csv") %>% select(FEC_ID,8) %>% set_names(c("CAND_ID","Keep")), by = "CAND_ID") %>% mutate(Keep = replace_na(Keep, "Keep")) %>% filter(Keep != "Drop") %>% select(-c(Keep))

# Join the working df and the fundraising data

df <- df %>% left_join(fundraising, by = c("ST","last_name"))


# Create Keys and Add Data ------------------------------------------------

# Create key for primary openness

primaries_key <- 
  data.frame(
    primary_scale = 
      c(0:5),
    primary_type = 
      c("All Party", "Open", "Open to \nUnaffiliated", "Partially \nOpen", "Partially \nClosed", "Closed"),
    primary_type_reduced = 
      c("All Party", "Open", "Restricted", "Restricted", "Restricted", "Closed"),
    primary_scale_reduced = 
      c(0,1,3,3,3,5))

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
      c("Full Denial","Acceptance with \nReservations","No Comment","Acceptance","Raised Questions","No Comment","Not Tracked by 538"),
    stance_scale_reduced = 
      c(-1,-1,0,1,-1,0,NA),
    stance_key_reduced = 
      c("Questioned Election","Questioned Election","No Comment","Acceptance","Questioned Election","No Comment","Not Tracked by 538")
    ) %>% 
  arrange(Stance_scale)

# Add stance scale to df

df <- df %>% 
  left_join(stance_key) %>% 
  left_join(comp_key) %>% 
  left_join(primaries_key)

# Check to ensure Alaska is the only state 
df %>% group_by(ST_dist) %>% filter(n()>1) %>% ungroup()

# Add Partisan Districting Data -------------------------------------------

df <- df %>% 
  mutate(dist_pol = metric - ST_metric)

# Clean and Compute Fundraising -------------------------------------------

df <- df %>% 
  mutate(
    fund_self = CAND_CONTRIB + CAND_LOANS,
    fund_indiv = TTL_INDIV_CONTRIB,
    fund_party = POL_PTY_CONTRIB,
    fund_other = OTHER_POL_CMTE_CONTRIB + TRANS_FROM_AUTH + OTHER_LOANS,
    .keep = "unused") %>% 
  mutate(
    fund_tot = fund_self + fund_indiv + fund_party + fund_other,
    fund_self_pct = fund_self / fund_tot,
    fund_indiv_pct = fund_indiv / fund_tot,
    fund_party_pct = fund_party / fund_tot,
    fund_other_pct = fund_other / fund_tot) %>% 
  select(-c(INDIV_REFUNDS, CMTE_REFUNDS, DEBTS_OWED_BY, TRANS_TO_AUTH, TTL_DISB, OTHER_LOAN_REPAY, CVG_END_DT, Office, View, CAND_LOAN_REPAY)) %>%
  rename_all(tolower)

names(df) <- gsub("cand_", "fec_", names(df))

# Clean and Save ----------------------------------------------------------

# Write df
write.csv(df, "data/processed/main_data.csv")

# clean environment

rm(list=setdiff(ls(), "df"))

# Explore -----------------------------------------------------------------

library(scales)

#stance_by_fundraising <-
df %>% select(incumbent, stance_scale, stance_key, fund_self_pct, fund_indiv_pct, fund_party_pct, fund_other_pct) %>% 
  na.omit() %>% group_by(stance_scale, stance_key, incumbent) %>% 
  summarize(
    avg_self_fund = mean(fund_self_pct), 
    avg_indiv_fund = mean(fund_indiv_pct),
    avg_party_fund = mean(fund_party_pct),
    avg_other_fund = mean(fund_other_pct)) %>% 
  pivot_longer(avg_self_fund:avg_other_fund, names_to = "fund_type", values_to = "avg") %>% 
  ggplot(aes(reorder(stance_key, -stance_scale), avg, fill = fund_type)) +
  # Add Bar
  geom_bar(stat = "identity", position = "stack") +
  #scale_fill_manual(values = stance_colors, name = NULL) +
  labs(x = "Denial Status", y = "Porportion of Fundraising", title = "Fundraising") +
  scale_y_continuous(labels = percent) + 
  facet_wrap(~ incumbent)
theme_classic() 

#stance_by_fundraising <-
df %>% select(incumbent, stance_scale, stance_key, fund_self, fund_indiv, fund_other) %>% # fund_party, 
  na.omit() %>% group_by(stance_scale, stance_key, incumbent) %>% 
  summarize(
    avg_self_fund = mean(fund_self), 
    avg_indiv_fund = mean(fund_indiv),
    #avg_party_fund = mean(fund_party),
    avg_other_fund = mean(fund_other)) %>% 
  pivot_longer(avg_self_fund:avg_other_fund, names_to = "fund_type", values_to = "avg") %>% 
  ggplot(aes(reorder(stance_key, -stance_scale), avg, fill = fund_type)) +
  # Add Bar
  geom_bar(stat = "identity", position = "dodge") +
  #scale_fill_manual(values = stance_colors, name = NULL) +
  labs(x = "Denial Status", y = "Porportion of Fundraising", title = "Fundraising") +
  facet_wrap(~ incumbent) +
  theme_classic() 

#ggsave("images/stance_by_fundraising.png", device = png)

stance_colors <- c("purple","lavender","grey","red","darkred","black")

stance_by_comp <- df %>%
  select(comp_scale, comp_key, stance_key, stance_scale) %>%
  # Summarize data by count
  group_by(comp_scale, comp_key, stance_key, stance_scale) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(reorder(comp_key, comp_scale), total_count, fill = reorder(stance_key, -stance_scale))) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = stance_colors, name = NULL) +
  labs(x = "District Lean", y = "Porportion of Candidates", title = "Election Denial by Competitiveness") +
  scale_y_continuous(labels = percent) + 
  theme_classic()

ggsave("images/stance_by_comp.png", device = png)

stance_by_primary_open <-
  df %>%
  select(primary_scale, primary_type, stance_scale, stance_key) %>%
  # Summarize data by count
  group_by(primary_scale, primary_type, stance_scale, stance_key) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(reorder(primary_type, primary_scale), total_count, fill = reorder(stance_key, -stance_scale))) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = stance_colors, name = NULL) +
  labs(x = "Primary Type", y = "Porportion of Candidates", title = "Election Denial by Primary Openess") +
  scale_y_continuous(labels = percent) + 
  theme_classic()

ggsave("images/stance_by_primary_open.png", device = png)

stance_by_primary_open_reduced <-
  df %>%
  filter(incumbent == "No") %>% 
  filter(state != "California") %>% 
  select(primary_scale_reduced, primary_type_reduced, stance_scale, stance_key) %>%
  # Summarize data by count
  group_by(primary_scale_reduced, primary_type_reduced, stance_scale, stance_key) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(reorder(primary_type_reduced, primary_scale_reduced), total_count, fill = reorder(stance_key, -stance_scale))) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = stance_colors, name = NULL) +
  labs(x = "Primary Type", y = "Porportion of Candidates", title = "Election Denial by Primary Openess (Non-Incumbents)") +
  scale_y_continuous(labels = percent) + 
  theme_classic()

ggsave("images/stance_by_primary_open_reduced.png", device = png)


stance_by_primary_thresh <-
  df %>%
  select(req_majority, stance_scale, stance_key) %>%
  mutate(req_majority = ifelse(req_majority == 1, "Majority", "Plurality")) %>% 
  # Summarize data by count
  group_by(req_majority, stance_scale, stance_key) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(req_majority, total_count, fill = reorder(stance_key, -stance_scale))) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = stance_colors, name = NULL) +
  labs(x = "Primary Victory Threshold", y = "Porportion of Candidates", title = "Election Denial by Primary Threshold") +
  scale_y_continuous(labels = percent) + 
  theme_classic()

ggsave("images/stance_by_primary_thresh.png", device = png)

stance_by_incumbency <-
  df %>%
  select(incumbent, stance_scale, stance_key) %>%
  drop_na() %>% 
  # Summarize data by count
  group_by(incumbent, stance_scale, stance_key) %>%
  summarise(total_count=n()) %>%
  # Plot
  ggplot(aes(incumbent, total_count, fill = reorder(stance_key, -stance_scale))) +
  # Add Bar
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = stance_colors, name = NULL) +
  labs(x = "Incumbency Status", y = "Porportion of Candidates", title = "Election Denial by Incumbency") +
  scale_y_continuous(labels = percent) + 
  theme_classic()

ggsave("images/stance_by_incumbency.png", device = png)

data_summary <- function(x) {
  m <- median(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

stance_by_distortion <-
  df %>% 
  select(stance_key,dist_pol,stance_scale, incumbent) %>% 
  filter(stance_key != "Not Tracked by 538") %>% 
  ggplot(aes(reorder(stance_key, -stance_scale),dist_pol, fill = reorder(stance_key, -stance_scale))) + 
  geom_violin() +
  stat_summary(fun.data=data_summary, 
               geom="pointrange", 
               color="black",
               show.legend = FALSE) +
  scale_fill_manual(values = stance_colors, 
                    name = NULL) + 
  labs(title = "Figure 2.B: District Distortion", y = "District Distortion", x = "Stance") + 
  scale_y_continuous(
    breaks=c(-.2, 0, .2),
    labels=c("More Democratic", "Neutral", "More Republican")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = .5) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90, hjust = .5, vjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank())

ggsave("images/stance_by_distorition.png", device = png)

stance_by_distortion_lim <-
  df %>% 
  select(stance_key_reduced,dist_pol,stance_scale_reduced, comp_scale, incumbent) %>% 
  filter(stance_key_reduced != "Not Tracked by 538") %>% 
  ggplot(aes(reorder(stance_key_reduced, -stance_scale_reduced),dist_pol, fill = reorder(stance_key_reduced, -stance_scale_reduced))) + 
  geom_violin() +
  stat_summary(fun.data=data_summary, 
               geom="pointrange", 
               color="black",
               show.legend = FALSE) +
  scale_fill_manual(values = c("purple", "grey", "darkred"), 
                    name = NULL) + 
  labs(title = "Figure 2.A: District Distortion", y = "District Distortion", x = "Stance") + 
  scale_y_continuous(
    breaks=c(-.2, 0, .2),
    labels=c("More Democratic", "Neutral", "More Republican")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = .5) +
  #facet_wrap(~ incumbent) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90, hjust = .5, vjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank())

ggsave("images/stance_by_distorition_lim.png", device = png)

stance_by_polarity <- 
  df %>% 
  select(stance_key,metric,stance_scale) %>% 
  filter(stance_key != "Not Tracked by 538") %>% 
  ggplot(aes(reorder(stance_key, -stance_scale),metric, fill = reorder(stance_key, -stance_scale))) + 
  geom_violin() +
  stat_summary(fun.data=data_summary, 
               geom="pointrange", 
               color="black",
               show.legend = FALSE) +
  scale_fill_manual(values = stance_colors, 
                    name = NULL) + 
  labs(title = "Figure 2.B: District Polarity", y = "District PVI", x = "Stance") + 
  scale_y_continuous(
    breaks=c(.3,.5,.7),
    labels=c("D+20", "Even", "R+20")) +
  geom_hline(yintercept=.5, linetype="dashed", color = "black", alpha = .5) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90, hjust = .5, vjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank())

ggsave("images/stance_by_polarity.png", device = png)

stance_by_polarity_lim <-
  df %>% 
  select(stance_key_reduced,metric,stance_scale_reduced, comp_scale, incumbent) %>% 
  filter(stance_key_reduced != "Not Tracked by 538") %>% 
  ggplot(aes(reorder(stance_key_reduced, -stance_scale_reduced),metric, fill = reorder(stance_key_reduced, -stance_scale_reduced))) + 
  geom_violin() +
  stat_summary(fun.data=data_summary, 
               geom="pointrange", 
               color="black",
               show.legend = FALSE) +
  scale_fill_manual(values = c("purple", "grey", "darkred"), 
                    name = NULL) + 
  labs(title = "Figure 2.A: District Polarity", y = "District Polarity", x = "Stance") + 
  scale_y_continuous(
    breaks=c(.3,.5,.7),
    labels=c("D+20", "Even", "R+20")) +
  geom_hline(yintercept=.5, linetype="dashed", color = "black", alpha = .5) +
  #facet_wrap(~ incumbent) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90, hjust = .5, vjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank())

ggsave("images/stance_by_polarity_lim.png", device = png)

lemon::grid_arrange_shared_legend(stance_by_distortion, stance_by_polarity, ncol = 2)

df %>% 
  select(stance_key, fund_indiv, metric, stance_scale) %>% 
  ggplot(aes(metric,fund_indiv, color = reorder(stance_key, -stance_scale))) +
  #geom_line(stat = "summary_bin", binwidth = .03) +
  geom_point() +
  scale_color_manual(values = stance_colors, name = NULL) +
  theme_classic()

library(maps)
states <- map_data("state")

primary_type_by_state <-
  df %>% 
  select(state, primary_type_reduced, primary_scale_reduced) %>% 
  mutate(region = tolower(state)) %>% 
  distinct() %>% 
  select(region,primary_type_reduced, primary_scale_reduced) %>% 
  inner_join(map_data("state"),by = "region") %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill = reorder(primary_type_reduced, primary_scale_reduced)), 
               color="white", size = 0.2) +
  scale_fill_brewer(palette = "OrRd", name = "Primary Type") + 
  labs(title = "Primary Type by State",
       caption = "*Alaska (All Party) and Hawaii (Open) Exlcuded for Concision") +
  theme_void() + 
  theme(plot.title = element_text(size=22), plot.caption = element_text(size=10))

ggsave("images/primary_type_by_state.png", device = png)

primary_thresh_by_state <-
  df %>% 
  select(state, req_majority) %>% 
  mutate(req_majority = ifelse(req_majority == 1, "Majority", "Plurality")) %>% 
  mutate(region = tolower(state)) %>% 
  distinct() %>% 
  select(region, req_majority) %>% 
  inner_join(map_data("state"),by = "region") %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill = req_majority), 
               color="white", size = 0.2) +
  scale_fill_brewer(palette = "OrRd", name = "Primary Threshold") + 
  labs(title = "Primary Threshold by State",
       caption = "*Alaska (Plurality) and Hawaii (Majority) Exlcuded for Concision") +
  theme_void() + 
  theme(plot.title = element_text(size=22), plot.caption = element_text(size=10))

ggsave("images/primary_thresh_by_state.png", device = png)


length(df %>% filter(is.na(stance)))
sum(table(df$stance))

