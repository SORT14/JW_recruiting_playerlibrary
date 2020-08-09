#Load necessary packages
#library(readxl)
library(stringdist)
library(dplyr)
#library(writexl)

# #Insert variables
# #Insert the tab number of each data set in the excel file
# a_sheetindex <- c(1)
# b_sheetindex <- c(2)
# #Insert the column(s) or column range to be inserted from the specified tab in the excel file (ie. C:C for one column, C:E for multiple columns, etc)
# a_columns <- paste("A:B")
# b_columns <- paste("A:C")

df1 <- read.csv("mini_1.csv")
df2 <- read.csv("mini_2.csv")

#Establish the threshold for an acceptable cutoff for desired results (this may take several attempts; the closer to 1 the closer the match)
threshold <- 0.94 # min of 0.94 similary

# #Insert Sheet # and Column # or range of columns from datasets in Excel File
# a_df <- read_excel(file.choose(), a_sheetindex, range = cell_cols(a_columns))
# #names(df_a) <- c("LastName", "FirstName")
# b_df <- read_excel(file.choose(), b_sheetindex, range = cell_cols(b_columns))
# #names(df_b) <- c("LastName", "FirstName")



#Establish keys to be matched for each dataset
df1_name <- paste(gsub("[^[:alnum:]]", "", df1$last), gsub("[^[:alnum:]]", "", df1$first), sep = "")
df2_name <- paste(gsub("[^[:alnum:]]", "", df2$last), gsub("[^[:alnum:]]", "", df2$first), sep = "")
df2_nameid <- df2$Orig_PlayerPersonID


#Format columns
match <- character()
max_dist <- integer()
sorted_matches <- character()
sorted_matches_ids <- character()

#Run loop function on cleaned keys to be matched
for (i in 1:length(df1_name) ) {
  match_dist <- stringsim(c(tolower(gsub("[^[:alnum:]]", "", df1_name)))[i], c(tolower(gsub("[^[:alnum:]]", "", df2_name))), method = "jw", p=0.1)
  # matchdist <- stringdist(a_df_name[i], b_df_name) # several methods available
  
  match_dist <- ifelse(match_dist < threshold, NA, match_dist)
  sorted_matches[i] <- paste(df2_name[order(match_dist, na.last=NA, decreasing = TRUE)], collapse = " | ")
  max_dist[i] <- tryCatch(ifelse(is.integer(which.max(match_dist)), match_dist[which.max(match_dist)],NA), error = function(e){NA})
  match[i]<-ifelse(length(df2_name[which.max(match_dist)])==0,NA,
                   df2_name[which.max(match_dist)] )
}

#Run 2nd loop function to bring in IDs of matched records
for (i in 1:length(df1_name) ) {
  match_dist <- stringsim(c(tolower(gsub("[^[:alnum:]]", "", df1_name)))[i], c(tolower(gsub("[^[:alnum:]]", "", b_df_name))), method = "jw", p=0.1)
  # matchdist <- stringdist(a_df_name[i], b_df_name) # several methods available
  
  match_dist <- ifelse(match_dist < threshold, NA, match_dist)
  sorted_matches_ids[i] <- paste(df2_nameid[order(match_dist, na.last=NA, decreasing = TRUE)], collapse = " | ")
  max_dist[i] <- tryCatch(ifelse(is.integer(which.max(match_dist)), match_dist[which.max(match_dist)],NA), error = function(e){NA})
  match[i]<-ifelse(length(df2_name[which.max(match_dist)])==0,NA,
                   df2_name[which.max(match_dist)] )
}

#Make results data frame
results <- data.frame(df1_name=df1_name, match=match, similarity=max_dist, sorted_matches=sorted_matches, sorted_matches_ids=sorted_matches_ids, stringsAsFactors = F)
names(results) <- c("NameKeyA", "Top Matched NameKeyB", "Similarity", "Sorted Matched NameKeys", "Sorted Matched IDs")

# Save out an excel file with the results
#saved_results <- list(a_df, b_df, results)
#write_xlsx(results, "FuzzyMatch_Results.xlsx")

write.csv(results, "FuzzyMatch_Results.csv")
