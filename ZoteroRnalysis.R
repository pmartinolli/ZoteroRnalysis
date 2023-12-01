#---
#ZoteroRanalysis
#---

# This file is supposed to contain all the information needed to understand, process, and produce data



# What is it doing ? 
# This R code can analyze a Zotero library of references & can produce graphics and tabular statistics



# Why ?
# To better understand your library of references
# For example :
# What is the distribution of the year of publication ? Did the publications happened long after the journal were created of is it a new academic outlet ?
# What are the main journals of the articles ? It can give an idea where to publish later
# What are the main publishers of the books and book sections ?
# What are the main languages of the references ?
# What are the main authors of the studies ? 
# Are authors single authors or multiples authors ?
# What are the main topics of the studies ? (NB: you will need to have indexed your corpus of references with your own thesaurus)
# How are the topics are distributed through the years ?
# What look like a word cloud of the titles of the studies ? 



# What do you need ?
# 1. Zotero installed
# With a library of references
#    The more cleaned are the references, the better is the analysis
#    The better indexed are the references, also the better is the analysis
# 2. R and R Studio installed
# With all the packages 
#    You will find them along the way with #install.packages("NAME")
#    The first time you start this program you need to remove the # before and Run the line
#    Because it takes time, once installed you can put back the # before to skip that step
# 3. OpenRefine installed
#    If you want to reconcile your data with open linked data online
#    It means your will semi-automatically retrieve more data (if it is indexed in Wikidata) to enrich your own data
#    For example, with the name of the Journal we will retrieve the Date of creation of the journal (Inception) or the Country of origin of the journal
#    Yes, it's awesome!
# 4. any PDF viewer, to open some graphics
# 5. OpenOffice Calc, to open the csv tabular extractions 










# Let's stat. 

# Create a new working folder on your computer
# Exemple : MyZoteroAnalysis¸
# Go to Zotero > My library > Right click > Export > Format : CSV (Unicode UTF-8) : Export the file "My library.csv" into your new working folder

# Copy this ZoteroRnalysis.R file into this working folder
# Open R Studio > File > Open File... > ZoteroRnalysis.R
# Go to this line with R Studio (it should be green)





# Go to RStudio > Session > Set Working Directory > To source file location
# Go in the Console frame 
# Copy the text after ">" in the console (it should start with "setwd...")
# Paste and replace the following line (because that line is for my computer) : 

setwd("C:/Users/martinop/OneDrive - Universite de Montreal/perso/en_cours/MyZoteroAnalysis")





# Let's install some packages
# Remove the # before the two lines after this block (the one names install.packages(""))
# Then, put the cursor on the line (that should have changed the color)
# Then click gently on the Run button at the top right of this frame
# Some stuff will happen in the Console under
# when its finished with a "blue >", repeat with the second line
# When its finished it means the packages are installed forever on your computer
# You can put back the two # to skip that step next times you run this analysis (because as you can see it takes time)

# install.packages("ggplot2")
# install.packages("plotly")

# You will encounter other lines like this along the way
# Apply the same treatment
# Some people put all the installation of packages at the begining of the code 
# but, as I cherish localized citations, I wanted to link the pieces of code 
# with their relevant localized packages 





# Let's call these packages
# Put the cursor on the line under (named Library(ggplot2)), then click Run
# And Click Run again 

library(ggplot2)
library(plotly)

# The two lines were executed, and in doing so, the two packages (already installed) were invoked 



















# Create a specific folder named "output" to place the outputs of the analysis
output_folder <- "output"

# Check if the folder already exists
if (!dir.exists(output_folder)) {
  # If it doesn't exist, create the folder
  dir.create(output_folder)
  cat("Folder created:", output_folder, "\n")
} else {
  cat("Folder already exists:", output_folder, "\n")
}






# Lets load the Zotero file into R

# If needed replace "My library.csv" with the actual file path or URL of your CSV file
file_name <- "My library.csv"

# Read the CSV file into a variable (here a data_frame) with specific options
ZOTEROLIB <- read.table(file_name, header = TRUE, sep = ",", encoding = "UTF-8")

# Now all the references are loaded into this big data frame named ZOTEROLIB
# Sometimes we will run the analysis on subsets of this big collection

# For example, if we want to analyze only the journal articles that are peer-reviewed 
# (ie. that are indexed with a tag named peer-reviewed) and that match an another tag
# NB: all my personal tags are starting with the "_" character to signal they are from my thesaurus

# Creating a subset of the dataframe to match certain documents only 
# Item.Type = "journalArticle" 
# and Tags contains "_peer reviewed" 
# and Tags contains "_TTRPG"

# Create a first subset based on your criteria
subset1_ZOTEROLIB <- subset(ZOTEROLIB, 
                            Item.Type == "journalArticle" & 
                            grepl("_peer reviewed", Manual.Tags) & 
                            grepl("_TTRPG", Manual.Tags))


# Create an another subset based on the criteria (Journal article OR book OR book section, AND TTRPG)
subset2_ZOTEROLIB <- subset(ZOTEROLIB, 
                           (Item.Type == "journalArticle" | 
                            Item.Type == "book" | 
                            Item.Type == "bookSection" ) & 
                            grepl("_TTRPG", Manual.Tags))











# Let's start with some basic analysis

## ANALYSIS : Peer-reviewed articles distributed by year

# Assuming your data frame is named DF with a column Publication.Year
DF <- subset1_ZOTEROLIB

# Count the observations for each date
date_counts <- table(DF$Publication.Year)

# Create a sequence of years from the minimum to the maximum, excluding NA
all_years <- seq(min(as.numeric(DF$Publication.Year), na.rm = TRUE), 
                 max(as.numeric(DF$Publication.Year), na.rm = TRUE), 
                 by = 1)

# Create a data frame with all years
all_years_df <- data.frame(Year = all_years)

# Merge the existing data with the data frame containing all years
merged_data <- merge(all_years_df, data.frame(Year = names(date_counts), Count = as.numeric(date_counts)), by = "Year", all.x = TRUE)

# Replace NAs with 0 for the count column
merged_data$Count[is.na(merged_data$Count)] <- 0

# Create a bar plot
# Assuming merged_data is your data frame with columns 'Year' and 'Count'
gg_plot <- ggplot(merged_data, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(title = "Peer-reviewed Journal Articles Distributed by Year", x = "Date", y = "Count") +
  theme_minimal()

# After runing the line under, a graphic is supposed to be displayed in the frame at the right of this one
print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "pr_journalarticles_by_year.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Export the data as a CSV file with column names
file_path <- file.path(output_folder, "pr_journalarticles_by_year.csv")
write.csv(merged_data, file = file_path, row.names = FALSE)








# ANALYSIS : Journal titles most frequently utilized for peer-reviewed articles

# Assuming your data frame is named DF with a column Publication.Title
DF <- subset1_ZOTEROLIB

# Use table to count occurrences of each Publication.Title
title_counts <- table(DF$Publication.Title)

# Sort the counts in descending order and select the top 15
top_titles <- head(sort(title_counts, decreasing = TRUE), 15)
all_titles <- sort(title_counts, decreasing = TRUE)

# Create a data frame for the top titles
top_titles_df <- data.frame(Publication.Title = names(top_titles), Count = as.numeric(top_titles))
all_titles_df <- data.frame(Publication.Title = names(all_titles), Count = as.numeric(all_titles))

# Assuming top_titles_df is your data frame with columns 'Publication.Title' and 'Count'
gg_plot <- ggplot(top_titles_df, aes(x = reorder(`Publication.Title`, Count), y = Count)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 15 Journal titles most frequently utilized for peer-reviewed articles", x = "Journal Titles", y = "Count") +
  theme_minimal() +
  coord_flip()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "journal_titles_counts.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Export the table as a CSV file with column names
file_path <- file.path(output_folder, "all_journal_titles_counts.csv")
write.csv(all_titles_df, file = file_path, row.names = FALSE)









# ANALYSIS : Listing the top publishers of books and book sections

DF <- subset2_ZOTEROLIB
presses_count <- table(DF$Publisher)

# Merging some data that are inconsistents
library(dplyr)

DF <- DF %>%
  mutate(Publisher = case_when(
    grepl("Wiley", Publisher) ~ "Wiley",
    grepl("McFarland", Publisher) ~ "McFarland",
    grepl("MIT Press", Publisher) ~ "MIT Press",
    grepl("ETC Press", Publisher) ~ "ETC Press",
    grepl("Springer", Publisher) ~ "Springer",
    TRUE ~ Publisher
  ))


# Create the table of publisher counts
presses_count <- table(DF$Publisher)

# Sort the counts in descending order and select the top 15
top_presses <- head(sort(presses_count, decreasing = TRUE), 15)
all_presses <- sort(presses_count, decreasing = TRUE)

# Create a data frame for the top titles
top_presses_df <- data.frame(Publisher = names(top_presses), Count = as.numeric(top_presses))
all_presses_df <- data.frame(Publisher = names(all_presses), Count = as.numeric(all_presses))

# Remove rows with NA or empty Publisher
top_presses_df <- subset(top_presses_df, !is.na(Publisher) & nzchar(Publisher))

gg_plot <- ggplot(top_presses_df, aes(x = reorder(`Publisher`, Count), y = Count)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 15 Publishers", x = "Journal Titles", y = "Count") +
  theme_minimal() +
  coord_flip()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "top_presses_counts.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Export the table as a CSV file with column names
file_path <- file.path(output_folder, "all_presses_counts.csv")
write.csv(all_presses_df, file = file_path, row.names = FALSE)










# ANALYSIS : Language used in the journal articles

# Assuming your data frame is named DF with a column Language
DF <- subset1_ZOTEROLIB

# Use table to count occurrences of each language code
language_counts <- table(DF$Language)

# Convert language_counts to a data frame
language_df <- data.frame(language = names(language_counts), count = as.numeric(language_counts))

# Create the pie chart
gg_plot <- ggplot(language_df, aes(x = "", y = count, fill = language)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +  # You can choose a different color palette
  labs(title = "Distribution of Language Codes", fill = "Language")

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "language_counts.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Use write.csv to export the language counts to a CSV file
file_path <- file.path(output_folder, "language_counts.csv")
write.csv(language_counts, file = file_path, row.names = FALSE)







# ANALYSIS : Most mentioned authors in journal articles  

# Assuming your data frame is named DF with a column Authors
DF <- subset1_ZOTEROLIB

# Convert Authors column to character to ensure consistency
DF$Author <- as.character(DF$Author)

# Split authors into individual names (assuming names are separated by commas)
individual_authors <- strsplit(DF$Author, ";")

# Flatten the list of individual authors into a single vector
all_authors <- unlist(individual_authors)

# Remove leading and trailing whitespaces from author names
all_authors <- trimws(all_authors)

# Create a data frame with the authors and their counts
authors_counts_df <- data.frame(Author = names(table(all_authors)), Count = as.numeric(table(all_authors)))

# Sort the data frame by count in descending order
authors_counts_df <- authors_counts_df[order(-authors_counts_df$Count), ]

# Merge authors with the same name
merged_authors_counts_df <- aggregate(Count ~ Author, data = authors_counts_df, sum)

# Use write.csv to export the merged authors counts to a CSV file
file_path <- file.path(output_folder, "all_authors_counts.csv")
write.csv(merged_authors_counts_df, file = file_path, row.names = FALSE)

# Order the data frame by count in descending order and select the top 20 authors
top_authors <- head(merged_authors_counts_df[order(-merged_authors_counts_df$Count), ], 20)

# Assuming top_titles_df is your data frame with columns 'Author' and 'Count'
gg_plot <- ggplot(top_authors, aes(x = reorder(`Author`, Count), y = Count)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 20 Authors with the highest frequency of mentions in articles", x = "Author", y = "Count") +
  theme_minimal() +
  coord_flip()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "top_authors.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Use write.csv to export the merged authors counts to a CSV file
file_path <- file.path(output_folder, "top_authors_counts.csv")
write.csv(top_authors, file = file_path, row.names = FALSE)








 
# ANALYSIS : All your tags in journal articles

# This analysis is assuming you have indexed all your references with tags starting by "_"

# Assuming your data frame is named DF with a column Tags
DF <- subset1_ZOTEROLIB

# Convert Tags column to character to ensure consistency
DF$Manual.Tags <- as.character(DF$Manual.Tags)

# Split tags into individual names (assuming tags are separated by commas)
individual_tags <- strsplit(DF$Manual.Tags, ";")

# Flatten the list of individual tags into a single vector
all_tags <- unlist(individual_tags)

# Remove leading and trailing whitespaces from tag names
all_tags <- trimws(all_tags)

# Create a data frame with the tags and their counts
tags_counts_df <- data.frame(Tag = names(table(all_tags)), Count = as.numeric(table(all_tags)))

# Sort the data frame by count in descending order
tags_counts_df <- tags_counts_df[order(-tags_counts_df$Count), ]

# Merge tags with the same name
merged_tags_counts_df <- aggregate(Count ~ Tag, data = tags_counts_df, sum)

# Use write.csv to export the merged tags counts to a CSV file
file_path <- file.path(output_folder, "all_tags_counts.csv")
write.csv(merged_tags_counts_df, file = file_path, row.names = FALSE)

# Order the data frame by count in descending order and select the top 150 tags
top_tags <- head(merged_tags_counts_df[order(-merged_tags_counts_df$Count), ], 150)

# Use write.csv to export the top tags to a CSV file
file_path <- file.path(output_folder, "top_tags_counts.csv")
write.csv(top_tags, file = file_path, row.names = FALSE)










# ANALYSIS : Treemaping only the tags that are capitalized (1st rank subject-headings)

# It is assuming that you have two levels of tags in your controlled list of tags (your thesaurus)
# The first level of tags is the generic level, these tags are all written in capital letters
# The second level of tags is the specific level, these tags are all written in non capital letters
# You will find information to build you thesaurus here : https://github.com/pmartinolli/TM-MyThesaurus
# Aussi dans le Manuel pratique de recherche documentaire (PLU6058), chapitre 15.2 : https://bib.umontreal.ca/multidisciplinaire/plu6058 (in French)
# An example of two level thesaurus : https://github.com/pmartinolli/TM-MyThesaurus/blob/master/files/TTRPG-simple-thesaurus.pdf

# Filter only the tags written in capital letters
capital_top_tags <- top_tags[grep("^_[A-Z]+$", top_tags$Tag), ]

# Remove the tag _TTRPG from the list (because it's the main tag so it appears everywhere)
capital_top_tags <- capital_top_tags[capital_top_tags$Tag != "_TTRPG", ]

# Filter tags with count 5 and higher
filtered_cap_tags <- capital_top_tags[capital_top_tags$Count >= 5, ]

# Assuming filtered_cap_tags is your data frame with columns 'Tag' and 'Count'
gg_plot <- ggplot(filtered_cap_tags, aes(x = reorder(`Tag`, Count), y = Count)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 1st rank tags", x = "Tags", y = "Count") +
  theme_minimal() +
  coord_flip()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "top_cap_tags.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Create a treemap plot with plotly
treemap_plot <- plot_ly(
  data = filtered_cap_tags,
  labels = ~Tag,
  parents = ~"",
  values = ~Count,
  type = "treemap",
  hoverinfo = "label+value+percent root"
) %>%
  layout(title = "Top Tags Treemap (Capitalized = 1st rank in the subject headings thesaurus)")

# Save the plot to an HTML file
file_path <- file.path(output_folder, "top_cap_tags_treemap.html")
htmlwidgets::saveWidget(treemap_plot, file = file_path)

# Display the plot
treemap_plot










# ANALYSIS : Treemaping only the tags that are NOT capitalized (2nd rank subject-headings)

# Filter tags with count 5 and higher
filtered_tags <- top_tags[top_tags$Count >= 5, ]

# Filter only the tags NOT written in capital letters
filtered_tags <- filtered_tags[grep("^_[^A-Z]+$", filtered_tags$Tag), ]

# Filter tags that start with "_"
filtered_tags <- filtered_tags[grepl("^_", filtered_tags$Tag), ]

# Remove the tag _ZOTEROLIB and _peer reviewed from the list
filtered_tags <- filtered_tags[filtered_tags$Tag != "_TTRPG", ]
filtered_tags <- filtered_tags[filtered_tags$Tag != "_peer reviewed", ]

# Assuming you have already created the 'filtered_tags' data frame

# Assuming filtered_tags is your data frame with columns 'Tag' and 'Count'
gg_plot <- ggplot(head(filtered_tags[order(-filtered_tags$Count), ], 30), aes(x = reorder(Tag, Count), y = Count)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 30 tags - 2nd rank in the subject headings", x = "Tags", y = "Count") +
  theme_minimal() +
  coord_flip()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "top_noncap_tags.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)


# Create a treemap plot with plotly
treemap_plot <- plot_ly(
  data = filtered_tags,
  labels = ~Tag,
  parents = ~"",
  values = ~Count,
  type = "treemap",
  hoverinfo = "label+value+percent root"
) %>%
  layout(title = "Top Tags Treemap (Count >= 5) (2nd rank in the subject headings thesaurus)")

# Save the plot to an HTML file
file_path <- file.path(output_folder, "noncap_tags_treemap.html")
htmlwidgets::saveWidget(treemap_plot, file = file_path)

# Display the plot
treemap_plot










# ANALYSIS : the distribution of the 1st rank tags per year

# Assuming your data frame is named DF with a column Tags
DF <- subset1_ZOTEROLIB

# Create an empty dataframe DDFF
DDFF <- data.frame()

# Iterate over unique Publication.Year values
for (year in unique(DF$Publication.Year)) {
  # Extract tags for the current year
  year_tags <- unlist(strsplit(gsub(" ", "", DF$Manual.Tags[DF$Publication.Year == year]), ";"))
  
  # Initialize a data frame for the current year
  year_df <- data.frame(
    Publication.Year = rep(year, length(filtered_cap_tags$Tag)),
    Tags = filtered_cap_tags$Tag,
    Count = 0
  )
  
  # Iterate over tags and update Count using regex
  for (i in seq_along(year_df$Tags)) {
    tag <- year_df$Tags[i]
    regex_pattern <- paste0("\\b", tag, "\\b")  # Use word boundaries to match whole tags
    year_df$Count[i] <- sum(grepl(regex_pattern, DF$Manual.Tags[DF$Publication.Year == year]))
  }
  
  # Bind the dataframe to DDFF
  DDFF <- rbind(DDFF, year_df)
}

# Reset row names
rownames(DDFF) <- NULL

# Plot the bar chart
gg_plot <- ggplot(DDFF, aes(x = Publication.Year, y = Count, fill = Tags)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution of tags throught the years", 
       x = "Publication Year", 
       y = "Count") +
  theme_minimal()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "tags_distributed_by_year.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Use write.csv to export the top tags to a CSV file
file_path <- file.path(output_folder, "tags_distributed_by_year.csv")
write.csv(DDFF, file = file_path, row.names = FALSE)












# ANALYSIS : Number of authors per work

# Load the dplyr package
library(dplyr)

# Function to count authors
count_authors <- function(authors) {
  # Use strsplit to split authors by ';'
  author_list <- unlist(strsplit(authors, ';'))
  # Return the length of the list
  return(length(author_list))
}



DF <- subset1_ZOTEROLIB

# Use mutate to create a new column 'AuthorCount'
DF <- DF %>%
  mutate(AuthorCount = sapply(Author, count_authors))


# Group by 'Publication.Year' and 'AuthorCount' and count the number of items
bubble_data <- DF %>%
  group_by(Publication.Year, AuthorCount) %>%
  summarize(Number_of_Items = n())

# Create the bubble chart
gg_plot <- ggplot(bubble_data, aes(x = Publication.Year, y = AuthorCount, size = Number_of_Items)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 15)) +
  labs(title = "Bubble Chart - Number of Authors per work",
       x = "Publication Year",
       y = "Number of Authors per work",
       size = "Number of articles")

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "nb_author_per_work_per_year.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Use write.csv to export the top tags to a CSV file
file_path <- file.path(output_folder, "nb_author_per_work_per_year.csv")
write.csv(bubble_data, file = file_path, row.names = FALSE)












## RECONCILIATION with WIKIDATA

# We are going to use OpenRefine and Wikidata to match the names of the journals with their 
# respectives QID and then retrieve the date of inception (creation date) of the journals 
# if that data is indexed in Wikidata

# 1. Install and open the software OpenRefine.
# 2. Create a new project with the file output/all_journal_titles_counts.csv
# 3. Reconcile Publication.Title (details not described, a tutorial here https://www.wikidata.org/wiki/User:Pmartinolli/Tutoriel_chercheur)
# 4. Add a new column based on this reconciled data : Qid
# 5. Add a new column based on this reconciled data : Inception
# 6. Add a new column based on this reconciled data : CountryOfOrigin 
# 7. Add a new column based on CountryOfOrigin : Qid_CoO 
# 6. Export the results into a file journal_titles_reconciled.csv and put that file in the working folder (at the same root level as "My library.csv")


# Load the data
# Read the CSV file into a variable (e.g., data_frame) with specific options
file_path <- "journal_titles_reconciled.csv"
journal_titles_reconciled <- read.table(file_path, header = TRUE, sep = ",", encoding = "UTF-8")

# Turn the date in this format "2014-08-01T00:00:00Z" into "YYYY"
# install.packages("lubridate")
library(lubridate)

# Convert the 'Inception' column to a datetime object
journal_titles_reconciled$Inception <- ymd_hms(journal_titles_reconciled$Inception)

# Extract the year and create a new column 'Year'
journal_titles_reconciled$Year <- year(journal_titles_reconciled$Inception)






# Add a new field in the data frame corresponding to the year the Journal was created (Publication.Inception)
# and adding also a calculated field :
# PubAfterInception = Publication Year of the article minus Publication.Inception of the journal
# It is supposed to reflect if an article is published in a new journal or in an old traditional one

# Initialize a new column 'Publication.Inception' in ZOTEROLIB with NAs
ZOTEROLIB$Publication.Inception <- NA

# Loop through each row in ZOTEROLIB
for (i in 1:nrow(ZOTEROLIB)) {
  # Get the Publication.Title for the current row in ZOTEROLIB
  current_title <- ZOTEROLIB$Publication.Title[i]
  
  # Check if there is a match in data$Publication.Title
  match_row <- match(current_title, journal_titles_reconciled$Publication.Title)
  
  # If there is a match, assign the corresponding Inception value
  if (!is.na(match_row)) {
    ZOTEROLIB$Publication.Inception[i] <- as.numeric(journal_titles_reconciled$Year[match_row])
  }
}

# Create a new field 'PubAfterInception' in ZOTEROLIB
ZOTEROLIB$PubAfterInception <- NA

# Check if 'Publication.Inception' is not NA and calculate 'PubAfterInception'
ZOTEROLIB$PubAfterInception[!is.na(ZOTEROLIB$Publication.Inception)] <- 
  ZOTEROLIB$Publication.Year[!is.na(ZOTEROLIB$Publication.Inception)] - 
  ZOTEROLIB$Publication.Inception[!is.na(ZOTEROLIB$Publication.Inception)]



# Rebuilt a new subset1 (because we changed the ZOTEROLIB data frame)

# Creating a subset of the dataframe to match certain documents only 
# Item.Type = "journalArticle" 
# and Tags contains "_peer reviewed" 
# and Tags contains "_TTRPG"

# Create a first subset based on your criteria
subset1_ZOTEROLIB <- subset(ZOTEROLIB, 
                            Item.Type == "journalArticle" & 
                              grepl("_peer reviewed", Manual.Tags) & 
                              grepl("_TTRPG", Manual.Tags))

# Remove rows with NA values in PubAfterInception using complete.cases
subset1_ZOTEROLIB <- subset1_ZOTEROLIB[complete.cases(subset1_ZOTEROLIB$PubAfterInception), ]

# Create a ggplot bar plot for the distribution of PubAfterInception in the subset
ggplot(subset1_ZOTEROLIB, aes(x = factor(PubAfterInception))) +
  geom_bar() +
  labs(title = "Article's publication year following the establishment of the journal",
       x = "Years after creation",
       y = "Count") +
  theme_minimal()




# Add a new field in the data frame corresponding to the Country of Origin of the Journal and the Qid of this CoO

# Initialize a new column 'CountryofOrigin' in ZOTEROLIB with NAs
ZOTEROLIB$CountryOfOrigin <- NA
ZOTEROLIB$Qid_CoO <- NA

# Loop through each row in ZOTEROLIB
for (i in 1:nrow(ZOTEROLIB)) {
  # Get the Publication.Title for the current row in ZOTEROLIB
  current_title <- ZOTEROLIB$Publication.Title[i]
  
  # Check if there is a match in data$Publication.Title
  match_row <- match(current_title, journal_titles_reconciled$Publication.Title)
  
  # If there is a match, assign the corresponding values
  if (!is.na(match_row)) {
    ZOTEROLIB$CountryOfOrigin[i] <- journal_titles_reconciled$CountryOfOrigin[match_row]
    ZOTEROLIB$Qid_CoO[i] <- journal_titles_reconciled$Qid_CoO[match_row]
  }
}


# Rebuilt a new subset1 (because we changed the ZOTEROLIB data frame, again)

# Creating a subset of the dataframe to match certain documents only 
# Item.Type = "journalArticle" 
# and Tags contains "_peer reviewed" 
# and Tags contains "_TTRPG"

# Create a first subset based on your criteria
subset1_ZOTEROLIB <- subset(ZOTEROLIB, 
                            Item.Type == "journalArticle" & 
                              grepl("_peer reviewed", Manual.Tags) & 
                              grepl("_TTRPG", Manual.Tags))

# Remove rows with NA values in CountryOfOrigin using complete.cases
subset1_ZOTEROLIB <- subset1_ZOTEROLIB[complete.cases(subset1_ZOTEROLIB$CountryOfOrigin), ]

# Calculate the countries with counting the number of publications
CountryOfOrigin_count <- subset1_ZOTEROLIB %>%
  group_by(CountryOfOrigin) %>%
  summarise(Count = n())
# Remove NA
CountryOfOrigin_count <- subset(CountryOfOrigin_count, !is.na(CountryOfOrigin) & CountryOfOrigin != "")


# Create a bar plot
# Assuming merged_data is your data frame with columns 'Year' and 'Count'
gg_plot <- ggplot(CountryOfOrigin_count, aes(y = reorder(`CountryOfOrigin`, Count), x = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(title = "Journal's country of origin", y = "Country of origin", x = "Count") +
  theme_minimal()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "journals_CountryOfOrigin.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Export the data as a CSV file with column names
file_path <- file.path(output_folder, "journals_CountryOfOrigin.csv")
write.csv(CountryOfOrigin_count, file = file_path, row.names = FALSE)














## Creating a word cloud
# based on the code by Céline Van den Rul at https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a 

# install.packages("wordcloud")
# install.packages("wordcloud2")
# install.packages("RColorBrewer") 
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)


# install.packages("tm")
library(tm) 
#Create a vector containing only the text
text <- subset1_ZOTEROLIB$Title
# Create a corpus  
docs <- Corpus(VectorSource(text))

# Clean the texts
library(dplyr)
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("french"))

# Create a document-term-matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Generate the word cloud, method 1
set.seed(1234) 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# Generate the word cloud, method 2
wordcloud2(data=df, size=1.6, color='random-dark')











#####################

# Credits

# ChatGPT 3.5 by OpenAI for a lot of help with back and forth feedbacks on my R code
# Caroline Patenaude, Data librarian at Université de Montréal for teaching me R & OpenRefine 
# Céline Van den Rul at https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a for word clouds
# David Tingle at https://davidtingle.com/misc/bib for ideas of analysis to perform



