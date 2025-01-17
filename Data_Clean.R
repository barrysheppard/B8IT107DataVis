
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
if (!require("pacman")) install.packages("pacman")
pacman::p_load("countrycode")

# Start by loading the data
df1 <- read.csv("data/survey_results_public_01.csv")
df2 <- read.csv("data/survey_results_public_02.csv")
df <- rbind(df1, df2)

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

# We also want a simple variable showing the split across all 3 possible
df$data_role[df$data_analyst & df$data_scientist] <- 'Both'
df$data_role[!df$data_analyst & df$data_scientist] <- 'Data Scientist'
df$data_role[df$data_analyst & !df$data_scientist] <- 'Data Analyst'
df$data_role[!df$data_analyst & !df$data_scientist] <- 'None'


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
  # One problem with this is the start and last entry don't have ;
  # so in this case we're adding them
  # The fixed = TRUE stops the C++ being treated as a special operator
  matches <- grep(match, paste(";",df$LanguageWorkedWith,";", sep = ""), fixed = TRUE)
  df[matches, as.character(new_column)] <- TRUE
}

# We also want a single variable to see R and Python usage
df$python_or_r[df$language_Python & df$language_R] <- 'Both'
df$python_or_r[!df$language_Python & df$language_R] <- 'R Only'
df$python_or_r[df$language_Python & !df$language_R] <- 'Python only'
df$python_or_r[!df$language_Python & !df$language_R] <- 'None'

# Similar again situation for interview questions
# The variable in question is df$LastInt
# The full list is
interview_questions <- c("Write any code",
                         "Write code by hand (e.g., on a whiteboard)",
                         "Complete a take-home project",
                         "Solve a brain-teaser style puzzle",
                         "Interview with people in peer roles",
                         "Interview with people in senior / management roles")

# Loop through the list of langauges searching and adding new column
# As details are multi-word this time we need to add a different suffix
# So we're going to go with numbers and alias it later in tableau
question_number <- 1
for (question in interview_questions) {
  # Starting by adding new column defaulting to false
  new_column <- paste("question_",question_number, sep = "")
  df[, as.character(new_column)] <- FALSE
  # Find the matches and mark them as TRUE instead
  # As some of the items are single characters which would have lots of 
  # false positives we're enclosing the variables with semi-colons
  match <- paste(";",question,";", sep = "")
  # One problem with this is the start and last entry don't have ;
  # so in this case we're adding them
  # The fixed = TRUE stops the C++ being treated as a special operator
  matches <- grep(match, paste(";",df$LastInt,";", sep = ""), fixed = TRUE)
  df[matches, as.character(new_column)] <- TRUE
  question_number <- question_number + 1
}

df$code_test <- df$question_1 | df$question_2 | df$question_3 | df$question_4

# For presenting the data, I wanted to be able to show regional areas.
# To do this, I need to map all the countries onto regions.
# Luckily there is a countrycode package that makes this nice and easy

df$continent <- countrycode(df$Country, 'country.name', 'continent')
df$region <- countrycode(df$Country, 'country.name', 'region')
# There are some with NA as the Country was not listed
# For whatever reason it is Namibia
# df[df$Country == "Namibia",]

df$Ireland[df$Country == 'Ireland'] <- 'Ireland'
df$Ireland[!df$Country == 'Ireland'] <- 'Not Ireland'




misctech <- c("Ansible",
              "Apache Spark",
              "Chef",
              "Cordova",
              "CryEngine",
              "Flutter",
              "Hadoop",
              ".NET",
              ".NET Core",
              "Node.js",
              "Pandas",
              "Puppet",
              "React Native",
              "TensorFlow",
              "Torch/PyTorch",
              "Unity 3D",
              "Unreal Engine",
              "Xamarin"
             )


# Loop through the list of langauges searching and adding new column
for (tech in misctech) {
  # Starting by adding new column defaulting to false
  new_column <- paste("tech",tech, sep = "")
  df[, as.character(new_column)] <- FALSE
  # Find the matches and mark them as TRUE instead
  # As some of the items are single characters which would have lots of 
  # false positives we're enclosing the variables with semi-colons
  match <- paste(";",tech,";", sep = "")
  # One problem with this is the start and last entry don't have ;
  # so in this case we're adding them
  # The fixed = TRUE stops the C++ being treated as a special operator
  matches <- grep(match, paste(";",df$MiscTechWorkedWith,";", sep = ""), fixed = TRUE)
  df[matches, as.character(new_column)] <- TRUE
}


# Dev Environment

environments <- c("Android Studio",
                  "Atom",
                  "Coda",
                  "Eclipse",
                  "Emacs",
                  "IntelliJ",
                  "IPython / Jupyter",
                  "Komodo",
                  "Light Table",
                  "NetBeans",
                  "Notepad++",
                  "PHPStorm",
                  "PyCharm",
                  "RStudio",
                  "RubyMine",
                  "Sublime Text",
                  "TextMate",
                  "Vim",
                  "Visual Studio",
                  "Visual Studio Code",
                  "Xcode",
                  "Zend"
)


# Loop through the list of langauges searching and adding new column
for (env in environments) {
  # Starting by adding new column defaulting to false
  new_column <- paste("env_",env, sep = "")
  df[, as.character(new_column)] <- FALSE
  # Find the matches and mark them as TRUE instead
  # As some of the items are single characters which would have lots of 
  # false positives we're enclosing the variables with semi-colons
  match <- paste(";",env,";", sep = "")
  # One problem with this is the start and last entry don't have ;
  # so in this case we're adding them
  # The fixed = TRUE stops the C++ being treated as a special operator
  matches <- grep(match, paste(";",df$DevEnviron,";", sep = ""), fixed = TRUE)
  df[matches, as.character(new_column)] <- TRUE
}

# Need to convert the money to Euros
# After looking at the data the conversion used was 1.145750
df$CompEuro <- round(df$CompTotal / 1.145750, 0)


# remove all put the unique id respondent and the new variables
columns_kept <- c("Respondent", "data_analyst", "data_scientist", "data_role",
                  "language_Assembly",
                  "language_Bash/Shell/PowerShell", "language_C", "language_C++",
                  "language_Clojure", "language_Dart", "language_Elixir", 
                  "language_Erlang", "language_F#", "language_Go", "language_HTML/CSS",
                  "language_Java",  "language_JavaScript", "language_Kotlin",
                  "language_Objective-C", "language_PHP",  "language_Python",
                  "language_R",  "language_Ruby", "language_Rust",  "language_Scala",
                  "language_SQL", "language_Swift", "language_TypeScript", "language_VBA",
                  "language_WebAssembly", "region", "continent", "question_1",
                  "question_2", "question_3", "question_4", "question_5",
                  "question_6", "techAnsible","techApache Spark", "techChef",                      
                  "techCordova", "techCryEngine", "techFlutter",  "techHadoop",                    
                  "tech.NET", "tech.NET Core", "techNode.js",  "techPandas",
                  "techPuppet", "techReact Native", "techTensorFlow",
                  "techTorch/PyTorch", "techUnity 3D", "techUnreal Engine",             
                  "techXamarin", "python_or_r", "Ireland",
                  "env_Android Studio", "env_Atom","env_Coda", "env_Eclipse",
                  "env_Emacs", "env_IntelliJ", "env_IPython / Jupyter",
                  "env_Komodo", "env_Light Table", "env_NetBeans",
                  "env_Notepad++", "env_PHPStorm", "env_PyCharm", "env_RStudio",
                  "env_RubyMine", "env_Sublime Text", "env_TextMate", "env_Vim",                       
                  "env_Visual Studio", "env_Visual Studio Code", "env_Xcode",
                  "env_Zend", "code_test", "CompEuro"
                  )
output_df <- df[,columns_kept]

     
# Save the file
write.csv(output_df,'data/survey_calculated_columns.csv')
