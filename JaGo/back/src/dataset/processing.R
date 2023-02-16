process <- function(df, col_categories) {

  # Loop over each column in the dataframe
  for (i in seq_along(df)) {

    # Get the category of the current column
    cat <- col_categories[i]

    # Clean and transform the column based on its category
    if (cat == "quantitative continue") {
      # Clean the column (remove missing values, convert to numeric)
      df[[i]] <- as.numeric(as.character(df[[i]]))
      df[[i]][is.na(df[[i]])] <- mean(df[[i]], na.rm = TRUE)

    } else if (cat == "quantitative discrète") {
      # Clean the column (remove missing values, convert to integer)
      df[[i]] <- as.integer(as.character(df[[i]]))
      df[[i]][is.na(df[[i]])] <- as.integer(round(mean(df[[i]], na.rm = TRUE)))

    } else if (cat == "qualitative nominale") {
      # Clean the column (remove missing values, convert to factor)
      df[[i]] <- as.factor(as.character(df[[i]]))

    } else if (cat == "qualitative ordinale") {
      # Clean the column (remove missing values, convert to factor)
      df[[i]] <- as.factor(as.character(df[[i]]))
      df[[i]] <- factor(df[[i]], ordered = TRUE)
    } else {
      print("Unknown category, skip the column")
      next
    }
  }
  # Return the cleaned and transformed dataframe
  return(df)
}