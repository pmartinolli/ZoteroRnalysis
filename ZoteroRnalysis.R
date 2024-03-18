#---
#ZoteroRnalysis, version 1.12
#---

## TODOLIST : commenting the code and changing the name of the variables for better readability




### METADATA

# Author : Pascal Martinolli
# Date (version 1.0) : 2023-12-01
# Last version of the code available at https://github.com/pmartinolli/ZoteroRnalysis/
# GPL-3.0 license https://github.com/pmartinolli/ZoteroRnalysis/blob/main/LICENSE
# This project and an example about TTRPGs is bloggued and discussed at https://jdr.hypotheses.org/1907 (in French)







### WHAT IS IT DOING ?

# This R code can analyze a Zotero library of references & can produce graphics and tabular statistics
# Optionally, it can retrieve information from Wikidata to enrich the original information





### WHY ?

# 1. To learn R Studio with a fun, useful and easy practice
# 2. To better understand your library of references, for example :
#     What is the distribution of the year of publication ? Did the publications happened long after the journal were created of is it a new academic outlet ?
#     What are the main journals of the articles ? It can give an idea where to publish later
#     What are the main publishers of the books and book sections ?
#     What are the main languages of the references ?
#     What are the main authors of the studies ?
#     Are authors single authors or multiples authors ?
#     What are the main topics of the studies ? (NB: you will need to have indexed your corpus of references with your own thesaurus)
#     How are the topics are distributed through the years ?
#     What look like a word cloud of the titles of the studies ?
#     What is the distribution of Master and PhD thesis (by year, country, number of pages)





### WHAT DO YOU NEED ?

# This file is supposed to contain all the information needed to understand, process, and produce data

# 1. Zotero installed
#    With a library of references
#       - The more cleaned are the references, the better is the analysis
#       - The better indexed are the references, also the better is the analysis

# 2. R and R Studio installed
#    With all the packages (that should be retrieved and installed at the first run of this code)

# 3. OpenRefine installed (optional)
#    If you want to reconcile your data with open linked data online
#    It means your will semi-automatically retrieve more data (if it is indexed in Wikidata) to enrich your own data
#    For example, with the name of the journal we will retrieve the date of creation of the journal (Inception) and the country of origin of the journal
#    Yes, it's awesome!

# 4. Any PDF viewer, to open some graphics in pdf

# 5. OpenOffice Calc, to open (and maybe edit) the csv files






### CREDITS

# ChatGPT 3.5 by OpenAI for a lot of help with back and forth feedback on my R code
# Caroline Patenaude, Data librarian at Université de Montréal for teaching me R & OpenRefine
# Céline Van den Rul at https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a for word clouds
# David Tingle at https://davidtingle.com/misc/bib for ideas of analysis to perform
# Zotero development team
# R and R Studio development team
# OpenRefine development team
# Wikidata development team and community of contributors






#####################






# Let's stat.


# Create a new working folder on your computer
# Example : MyZoteroAnalysis



# Go to Zotero > My library > Right click > Export > Format : CSV (Unicode UTF-8)
# Export the file "My library.csv" into your new working folder "MyZoteroAnalysis"



# Copy this "ZoteroRnalysis.R" file into this working folder
# Open the "ZoteroRnalysis.R" file (with R Studio > File > Open File... or double-click on it from the folder)
# Go to this line within R Studio (it should be green, because it's a comment and all comments are ignored by the program so we can write anything there, especially everything that will make the code more understandable)



# Go to RStudio > Session > Set Working Directory > To source file location
# Go in the Console frame
# Copy the text after ">" in the console (it should start with "setwd...")
# Paste and replace the line following that block of comments (because that line is for my computer)
# Then, put the cursor on that line and click Run on the top-right of this frame

setwd("C:/Users/martinop/OneDrive - Universite de Montreal/perso/en_cours/MyZoteroAnalysis")








# Now let's install some packages

# Put the cursor under that block and click Run on the top-right of this frame
# If a package is not installed yet, it will be installed (some stuff will happen in the console under)
# If a package is installed, nothing will happen (just some blues lines in the console)

# Load ggplot2 library if not already loaded
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Repeat

# Load plotly library if not already loaded
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)





# Now you will Run all the line that are not starting with >
# As you can see, Run bring you every time after the lines that were executed so you can just keep clicking on run
# And watch what going on in the console, and the viewer panel sometimes



# Create a specific folder named "output" to place all the outputs of the analysis (PDF, csv and everything else)
output_folder <- "output"

# Check if the folder already exists
if (!dir.exists(output_folder)) {
  # If it doesn't exist, create the folder
  dir.create(output_folder)
  cat("Folder created:", output_folder, "\n")
} else {
  cat("Folder already exists:", output_folder, "\n")
}






# Let's load the Zotero references file into R

# If needed replace "My library.csv" with the actual file name (or URL) of your CSV file
file_name <- "My library.csv"





# Read the CSV file into a variable (here a data_frame) with specific options
ZOTEROLIB <- read.table(file_name, header = TRUE, sep = ",", encoding = "UTF-8")

# Function to print variable name and class
print_variable_info <- function(x) {
  var_name <- deparse(substitute(x))
  var_class <- class(x)
  print(paste(var_name, " was created. It's a", var_class, "(class)"))
}
print_variable_info(ZOTEROLIB)

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

# Other Item.Type ??
# Here is a short list from https://www.zotero.org/support/kb/item_types_and_fields
#   book
#   bookSection
#   conferencePaper
#   journalArticle
#   magazineArticle
#   newspaperArticle
#   thesis
#   webpage
# Important : they are case sensitive, they use no space, and their 1st letter is lowercase









# Let's start with some basic analysis







## ANALYSIS : Peer-reviewed articles distributed by year


# Assuming your data frame is named DF with a column Publication.Year
DF <- subset1_ZOTEROLIB

# Assuming Publication.Year is initially stored as character or factor
# Convert it to character if it's factor
if (is.factor(DF$Publication.Year)) {
  DF$Publication.Year <- as.character(DF$Publication.Year)
}

# Remove any non-numeric characters and convert to numeric
DF$Publication.Year <- as.numeric(gsub("[^0-9]", "", DF$Publication.Year))

# Check for missing values and handle them if necessary
if (any(is.na(DF$Publication.Year))) {
  # Handle missing values: remove them or impute as needed
  DF <- na.omit(DF)
}

# Count the observations for each date
date_counts <- table(DF$Publication.Year)

# Create a sequence of years from the minimum to the maximum, excluding NA
all_years <- seq(min(as.numeric(DF$Publication.Year), na.rm = TRUE),
                 max(as.numeric(DF$Publication.Year), na.rm = TRUE),
                 by = 1)


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
  labs(title = "Top 15 Academic Journals", x = "Journal Titles", y = "Count") +
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

# Load dplyr library if not already loaded
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Merging some data that are inconsistent
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
top_authors <- head(merged_authors_counts_df[order(-merged_authors_counts_df$Count), ], 30)

# Assuming top_titles_df is your data frame with columns 'Author' and 'Count'
gg_plot <- ggplot(top_authors, aes(x = reorder(`Author`, Count), y = Count)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 30 Authors with the highest frequency of mentions in articles", x = "Author", y = "Count") +
  theme_minimal() +
  coord_flip()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "top_authors.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Use write.csv to export the merged authors counts to a CSV file
file_path <- file.path(output_folder, "top_authors_counts.csv")
write.csv(top_authors, file = file_path, row.names = FALSE)



## Export the authors_count_df as csv to reconcile with Wikidata and sort the « assumed » gender of the authors based on their first names
file_path <- file.path(output_folder, "all_authors_counts.csv")
write.csv(authors_counts_df, file = file_path, row.names = FALSE)

# Open this csv (all_authors_counts.csv )in OpenRefine
# Column Author > Add a column based on this column
#               > New column name = firstname
#               > replace value by
#                 value.split(',')[1].trim().replace(/^[A-Za-z]\.? /, '').split(' ')[0]
# Column Author > Add a column based on this column
#               > New column name = givenname_reco
#               > replace value by
#                 value.split(',')[1].trim().replace(/^[A-Za-z]\.? /, '').split(' ')[0]
# Column givenname_reco > Reconcile > Start reconciling > Wikidata : Type = female given name + Start reconciling
# Click on the facet at the right : None (become orange)
# Column givenname_reco > Reconcile > Start reconciling > Wikidata : Type = male given name + Start reconciling
# Column givenname_reco > Reconcile > Start reconciling > Wikidata : Type (in the box) = unisex given name + Start reconciling
# Column givenname_reco > Add a column based on reconciled value > P31 (instance of)
# Rename the column "instance of" into "instance.of"
# Export all as comma separated value : givenname_gender_reconciled.csv
# Pass through the csv data in LibreOffice Calc to correct some remaining mistakes

gender_data <- read.csv("givenname_gender_reconciled.csv", stringsAsFactors = FALSE)

# Initialize an empty list to store rows for the new DataFrame
author_year_gender_rows <- list()

# Iterate through each row of the original DataFrame
for (i in 1:nrow(DF)) {
  # Split the authors and corresponding years
  authors <- trimws(strsplit(DF$Author[i], ";")[[1]])
  year <- DF$Publication.Year[i]

  # Create a row for each author with corresponding year and gender
  for (author in authors) {
    # Find matches in gender_data based on whether the Author column contains the author value
    matching_row <- gender_data[grepl(author, gender_data$Author, ignore.case = TRUE), ]
    if (nrow(matching_row) > 0) {
      gender <- matching_row$`instance.of`
      # Take the first value if gender has multiple values
      gender <- gender[1]
    } else {
      # If no gender information found for author, set it as "Unknown"
      gender <- "Unknown"
    }
    author_year_gender_rows[[length(author_year_gender_rows) + 1]] <- c(Author = author,
                                                                        Publication.Year = year,
                                                                        Gender = gender)
  }
}

# Combine all rows into a DataFrame
DFgender <- do.call(rbind, author_year_gender_rows)
# Convert DF2 to data frame explicitly
DFgender <- as.data.frame(DFgender)

# Convert "Gender" column to a factor with specified levels
DFgender$Gender <- factor(DFgender$Gender, levels = c("male given name", "female given name", "unisex given name", NA))

# Plot the distribution of author_year_gender_rows through the years
gg_plot <- ggplot(DFgender, aes(x = Publication.Year, fill = Gender)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Authors by Gender over Years",
       x = "Year", y = "Count") +
  scale_fill_manual(values = c("lightblue", "pink", "green", "gray"),
                    labels = c("Male", "Female", "Unisex", "NA")) +
  theme_minimal()

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "gender_distribution_byyear.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Use write.csv to export the merged authors counts to a CSV file
file_path <- file.path(output_folder, "gender_distribution_byyear.csv")
write.csv(DFgender, file = file_path, row.names = FALSE)







# Open all_authors_counts.csv in OpenRefine
# Column Author > Add a column based on this column
#               > New column name = ORCID_reco
#               > replace value by
#                 value => value.split(',')[1].trim() + ' ' + value.split(',')[0].trim()
# Column ORCID_reco > Reconcile : Add service
# Column ORCID_reco > Reconcile > Start reconciling > Person
# Column ORCID_reco > Reconcile > Action > Match each cell to its best candidate
# Column ORCID_reco > Reconcile > Add entity identifier column
# Name = ORCID_ID
# To verify in LibreOffice Calc here is the formulate to do the same in a new column and compare with ORCID_reco : =REGEX(A2; "(.*), (.*)"; "$2 $1")




# Open all_authors_counts.csv in OpenRefine
# Column Author > Add a column based on this column
#               > New column name = Wikidata_reco
# Column Wikidata_reco > Reconcile > Start reconciling > Human (Q5)
# Column ORCID_reco > Reconcile > Action > Match each cell to its best candidate
# Column ORCID_reco > Reconcile > Add entity identifier column
# Name = ORCID_ID
# To verify in LibreOffice Calc here is the formulate to do the same in a new column and compare with ORCID_reco : =REGEX(A2; "(.*), (.*)"; "$2 $1")








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

# TL;DR, your thesaurus should be organized like this :
#
# _PSYCHOLOGY
#    _therapy
#    _mental disorder
#    _cognition
#    _well-being
#
# Every time you add a specific tag (non capital letter), you should add also the related generic TAG (capital letter)




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










# ANALYSIS : the distribution of 3 tags only

# Assuming your data frame is named DF with a column Tags
DF <- subset1_ZOTEROLIB

# Create an empty dataframe DDFF
DDFF <- data.frame()

# Create a small set of 4 tags
small_set_tags <- tags_counts_df %>%
  filter(Tag %in% c("_therapy", "_mental disorder", "_well-being", "_anxiety"))

#small_set_tags <- tags_counts_df %>%
#  filter(Tag %in% c("_moral panic"))

# Iterate over unique Publication.Year values
for (year in unique(DF$Publication.Year)) {
  # Extract tags for the current year
  year_tags <- unlist(strsplit(gsub(" ", "", DF$Manual.Tags[DF$Publication.Year == year]), ";"))

  # Initialize a data frame for the current year
  year_df <- data.frame(
    Publication.Year = rep(year, length(small_set_tags$Tag)),
    Tags = small_set_tags$Tag,
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
  labs(title = "Distribution of 4 selected tags throught the years",
       x = "Publication Year",
       y = "Count") +
  theme_minimal()

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "small_set_tags_distributed_by_year.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Use write.csv to export the top tags to a CSV file
file_path <- file.path(output_folder, "small_set_tags_distributed_by_year.csv")
write.csv(DDFF, file = file_path, row.names = FALSE)















# ANALYSIS : Number of authors per work

# Load dplyr library if not already loaded
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
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

# Load wordcloud library if not already loaded
if (!requireNamespace("wordcloud", quietly = TRUE)) {
  install.packages("wordcloud")
}
library(wordcloud)

# Load RColorBrewer library if not already loaded
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
library(RColorBrewer)

# Load wordcloud2 library if not already loaded
if (!requireNamespace("wordcloud2", quietly = TRUE)) {
  install.packages("wordcloud2")
}
library(wordcloud2)

# Load tm library if not already loaded
if (!requireNamespace("tm", quietly = TRUE)) {
  install.packages("tm")
}
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














## Analysis : Thesis


# Load tidyr library if not already loaded
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
library(tidyr)

# Load dplyr library if not already loaded
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)


subset2_ZOTEROLIB <- subset(ZOTEROLIB,
                            (Item.Type == "thesis" &
                              grepl("_TTRPG", Manual.Tags)))

DF <- subset2_ZOTEROLIB

# Remove all Type of thesis = Bachelor stuff (starts by B or R of D or H or I or C or S)
DF <- DF %>%
  filter(!grepl("^[BRDHCS]", Type))

# Normalized Type field into TypeNormalized
# if Type starts by T P or D, then its a PhD
# if Type starts by M, then its a Master
DF <- DF %>%
  mutate(TypeNormalized = case_when(
    grepl("^[TPD]", Type) ~ "PhD",
    grepl("^M", Type) ~ "Master",
    TRUE ~ Type
  ))

# Now select only the PhD and the Master
DF <- DF %>%
  filter(TypeNormalized %in% c("PhD", "Master"))

# Create a bar plot
# Assuming DF is your data frame with columns 'Year' and 'Count'
gg_plot <- ggplot(DF, aes(x = Publication.Year, fill = TypeNormalized)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "count") +
  labs(title = "Master and Doctoral Thesis Distributed by Year", x = "Year", y = "Count") +
  theme_minimal()

# After runing the line under, a graphic is supposed to be displayed in the frame at the right of this one
print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "thesis_by_year.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)

# Export the data as a CSV file with column names
# Create separate columns for Count PhD and Count Master
DF_counts <- DF %>%
  group_by(Publication.Year, TypeNormalized) %>%
  summarise(Count = n())

# Pivot the data to wide format
DF_counts_wide <- pivot_wider(DF_counts, names_from = TypeNormalized, values_from = Count)  #library(tidyr)

file_path <- file.path(output_folder, "thesis_by_year.csv")
write.csv(DF_counts_wide, file = file_path, row.names = FALSE)






# Load tidyverse library if not already loaded
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)



# Filter rows where TypeNormalized is "PhD" and Num.Pages is not NA
filtered_df_phd <- DF %>% filter(TypeNormalized == "PhD" & !is.na(Num.Pages))
# Filter rows where TypeNormalized is "Master" and Num.Pages is not NA
filtered_df_master <- DF %>% filter(TypeNormalized == "Master" & !is.na(Num.Pages))

# Calculate the average of Num.Pages for PhD
average_num_pages_phd <- mean(filtered_df_phd$Num.Pages, na.rm = TRUE)
# Calculate the average of Num.Pages for Master
average_num_pages_master <- mean(filtered_df_master$Num.Pages, na.rm = TRUE)

# Create a scatter plot with averages displayed
gg_plot <- ggplot() +
  geom_point(data = filtered_df_phd, aes(x = jitter(rep(2, nrow(filtered_df_phd)), amount = 0.1), y = Num.Pages), color = "red") +
  geom_text(data = filtered_df_master, aes(x = 1, y = average_num_pages_master,
                                           label = sprintf("Avg (Master): %.2f", average_num_pages_master)),
            vjust = -0.5, hjust = -0.5, color = "darkgreen", size = 6) +
  geom_point(data = filtered_df_master, aes(x = jitter(rep(1, nrow(filtered_df_master)), amount = 0.1), y = Num.Pages), color = "green") +
  geom_text(data = filtered_df_phd, aes(x = 2, y = average_num_pages_phd,
                                        label = sprintf("Avg (PhD): %.2f", average_num_pages_phd)),
            vjust = -0.5, hjust = 1.3, color = "darkred", size = 6) +
  labs(title = "Average Number of Pages for PhD and Master Thesis (excluding NA)",
       x = " ",
       y = "Number of Pages")

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "avg_nb_pages_per_thesis.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)






# Exporting the list of universities for another round of reconciliation with Wikidata data to retrieve Countries

# Replace 'DF$Publisher' with the column you want to export
column_to_export <- DF$Publisher

# Convert the column to a data frame with a custom name
df_to_export <- data.frame(Universities = column_to_export)

# Specify the file name
file_name <- paste0(output_folder, "/", "universities2reconcile.csv")

# Export the data frame to a CSV file
write.csv(df_to_export, file = file_name, row.names = FALSE)


# Open this csv with OpenRefine
# Duplicate the first column into a new column named UniversitiesWD
# Reconcile the UniversitiesWD column (to keep the first column identical and allow joining the data later)
# Add a new column based on the reconciled data : UniversitiesWD.QID
# Add a new column based on the reconciled data : Country
# Add a new column based on the reconciled data : Country.QID

# Export the file as comma-separated value "universities-reconciled.csv"
# "Universities", "UniversitiesWD", "UCountry", "UniversitiesWD.QID", "UCountry.QID"
# and put it at the root of the working folder









# Load the data

# Read the CSV file into a variable (e.g., data_frame) with specific options
file_path <- "universities-reconciled.csv"
universities_reconciled <- read.csv(file_path, header = FALSE, col.names = c("Universities", "UniversitiesWD", "UCountry", "UniversitiesWD.QID", "UCountry.QID"))

# merge the data
DF <- merge(DF, universities_reconciled, by.x = 'Publisher', by.y = 'Universities', all = FALSE)
DF <- distinct(DF, Key, .keep_all = TRUE)

count_data <- DF %>%
  group_by(UCountry, TypeNormalized) %>%
  summarize(count = n())

# Create the bar plot with reordered x-axis and categorized bars
gg_plot <- ggplot(count_data, aes(x = reorder(UCountry, count), y = count, fill = TypeNormalized)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "Country Distribution", x = "Country", y = "Count") +
  coord_flip()  # Use coord_flip() to flip the x and y axes

print(gg_plot)

# Save the ggplot to a PDF file
file_path <- file.path(output_folder, "thesis_by_countries.pdf")
ggsave(file_path, plot = gg_plot, width = 8, height = 6)


file_path <- file.path(output_folder, "thesis_by_countries.csv")
write.csv(count_data, file = file_path, row.names = FALSE)












## Final Export of the enriched ZOTEROLIB data frame into a csv

file_name <- "My library.csv"
ZOTEROLIB <- read.table(file_name, header = TRUE, sep = ",", encoding = "UTF-8")

# Full join with journal_titles_reconciled
ZOTEROLIB <- full_join(ZOTEROLIB, journal_titles_reconciled, by = c('Publication.Title' = 'Publication.Title'), relationship = "many-to-many")
# Full join with universities_reconciled
ZOTEROLIB <- full_join(ZOTEROLIB, universities_reconciled, by = c('Publisher' = 'Universities'), relationship = "many-to-many")

# Remove duplicates based on the Key field
ZOTEROLIB <- distinct(ZOTEROLIB, Key, .keep_all = TRUE)

# Export the data as a CSV file with column names
file_path <- file.path(output_folder, "My_library_enriched.csv")
write.csv(ZOTEROLIB, file = file_path, row.names = FALSE)











