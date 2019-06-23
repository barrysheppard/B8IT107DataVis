
# Converting a semi-colon seperated array in a csv file into multiple columns
# populated with Y or N depending if the array contained that value or not


#######################################
# Normal prep code                    #
#######################################

# This clears the workspace environment
rm(list = ls())
# This sets the working directory to the same as the file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This installs all the packages needed if not already loaded
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load("caTools")

# Start by loading the data
df <- read.csv("data/survey_results_public_01.csv")

# df$DevType is a text string seperated by semicolons. Each value is a 
# developer type. The total number of dev types is 25 with a 25 Other option
# allowing free format text.
# We want to look for the 'Data or business analyst' and the 'Data scientist
# or machine learning specialist' roles.



# Starting by adding new column defaulting to false
df$data_analyst <- FALSE 
# Find the matches and mark them as TRUE instead
matches <- grep('Data or business analyst', df$DevType)
df$data_analyst[matches] <- TRUE

# Starting by adding new column defaulting to false
df$data_scientist <- FALSE 
# Find the matches and mark them as TRUE instead
matches <- grep('Data scientist or machine learning specialist', df$DevType)
df$data_scientist[matches] <- TRUE


# Next we want to do similar with languages worked.
# The variable in question is df$LanguageWorkedWith
# The full list is
languages <- c("Assembly", "Bash/Shell/PowerShell","C","C++","Clojure", "Dart",
               "Elixir", "Erlang", "F#", "Go", "HTML/CSS", "Java", "JavaScript",
               "Kotlin", "Objective-C", "PHP", "Python", "R", "Ruby", "Rust",
               "Scala", "SQL", "Swift", "TypeScript","VBA", "WebAssembly")

# Loop through the list of langauges searching and adding new column
for (language in languages) {
  # Starting by adding new column defaulting to false
  new_column <- paste("language_",language, sep = "")
  df[, as.character(new_column)] <- FALSE
  # Find the matches and mark them as TRUE instead
  # As some of the items are single characters which would have lots of 
  # false positives we're enclosing the variables with semi-colons
  match <- paste(";",language,";", sep = "")
  # The fixed = TRUE stops the C++ being treated as a special operator
  matches <- grep(match, df$LanguageWorkedWith, fixed = TRUE)
  df[matches, as.character(new_column)] <- TRUE
}


# Save the file
write.csv(df,'data/survey_results_updated_01.csv')

# Repeat for the 02 file by changing the read and write variable names
