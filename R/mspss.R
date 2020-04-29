# The mspss function takes a data frame containing MSPSS data and outputs the same
# data frame with the addition of new columns containing the MSPSS subscale scores
# The first argument takes in a data frame containing the MSPSS data. The second
# argument takes in the name of the column containing item 1 of the MSPSS. Name
# of the column has to be enclosed with quotes ("").

mspss <- function(df, MSPSS_1) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing MSPSS_1 argument
  if (missing(MSPSS_1)) {
    stop("Please specify the column name of Q1, in quotes, of the MSPSS from your
         data frame here")
  }

  # Test for the scenario where a non-existent column name is provided in the
  # MSPSS_1 argument
  if (!MSPSS_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Test for the scenario where there is no data in the data frame
  if (nrow(df) == 0) {
    stop("Your data frame does not contain any data. Please use a data frame
         containing MSPSS data")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the MSPSS_1 argument and call it the index.
  index <- which(colnames(df) == MSPSS_1)

  # Using the index as a reference point, test whether the 11th column after
  # the index (representing MSPSS item 12) exceed the total number of columns
  # in the df argument. If so, this might mean that the data frame do not
  # contain the complete set of 12 items
  if (index + 11 > ncol(df)) {
    stop("You do not have the complete set of 12 items for the MSPSS")
  }

  # Store the items belonging to the Significant Other subscale as positions
  # relative to the index in a temporary vector. Add the index to each element
  # of the temporary vector to obtain their actual column numbers and store the
  # results in a vector
  sig_other_vector <- index + c(0, 1, 4, 9)

  # Compute the Significant Other subscale scores by averaging the scores across
  # all items in the sig_other_vector. Store the results in a new column called
  # sig_other
  df[, "sig_other"] <- round(rowMeans(df[, sig_other_vector], na.rm = TRUE), 2)

  # Store the items belonging to the Family subscale as positions relative to the
  # index in a temporary vector. Add the index to each element of the temporary
  # vector to obtain their actual column numbers and store the results in a vector
  family_vector <- index + c(2, 3, 7, 10)

  # Compute the Family subscale scores by averaging the scores across all items
  # in the family_vector. Store the results in a new column called family
  df[, "family"] <- round(rowMeans(df[, family_vector], na.rm = TRUE), 2)

  # Store the items belonging to the Friends subscale as positions relative to the
  # index in a temporary vector. Add the index to each element of the temporary
  # vector to obtain their actual column numbers and store the results in a vector
  friends_vector <- index + c(5, 6, 8, 11)

  # Compute the Friends subscale scores by averaging the scores across all items
  # in the friends_vector. Store the results in a new column called friends
  df[, "friends"] <- round(rowMeans(df[, friends_vector], na.rm = TRUE), 2)

  # Store the items belonging to the Total scale as positions relative to the
  # index in a temporary vector. Add the index to each element of the temporary
  # vector to obtain their actual column numbers and store the results in a vector
  total_vector <- index + c(0:11)

  # Compute the Total scale scores by averaging the scores across all items
  # in the total_vector. Store the results in a new column called total
  df[, "total"] <- round(rowMeans(df[, total_vector], na.rm = TRUE), 2)

  # Return the data frame with the newly created subscale columns
  return(df)
}
