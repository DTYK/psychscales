# The apq function takes a data frame containing APQ data and outputs the same
# data frame with the addition of new columns containing the APQ subscale scores
# The first argument takes in a data frame containing the APQ data. The second
# argument takes in the name of the column containing item 1 of the APQ. Name
# of the column has to be enclosed with quotes ("").

apq <- function(df, APQ_1) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing APQ_1 argument
  if (missing(APQ_1)) {
    stop("Please specify the column name of Q1, in quotes, of APQ from your
         data frame here")
  }

  # Test for the scenario when a non-existent column name is provided in the
  # APQ_1 argument
  if (!APQ_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Test if there is no data in the data frame
  if (nrow(df) == 0) {
    stop("Your data frame does not contain any data. Please use a data frame
         containing APQ data")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the APQ_1 argument and call it the index.
  index <- which(colnames(df) == APQ_1)

  # Using the index as a reference point, test whether the 41st column after
  # the index (representing APQ item 42) exceed the total number of columns in
  # the df argument. If so, this might mean that the data frame do not contain
  # the complete set of 42 items
  if (index + 41 > ncol(df)) {
    stop("You do not have the complete set of 42 items for the APQ")
  }

  # Store the items belonging to the Involvement subscale as positions relative
  # to the index in a temporary vector. Add the index to each element of the
  # temporary vector to obtain their actual column numbers and store the
  # results in a vector
  involvement_vector <- index + c(0, 3, 6, 8, 10, 13, 14, 19, 22, 25)

  # Compute the Involvement subscale scores by summing up the items in the
  # involvement vector. Store the results in a new column called involvement
  df[, "involvement"] <- rowSums(df[, involvement_vector], na.rm = TRUE)

  # Store the items belonging to the Positive Parenting subscale as positions
  # relative to the index in a temporary vector. Add the index to each element
  # of the temporary vector to obtain their actual column numbers and store the
  # results in a vector
  parenting_vector <- index + c(1, 4, 12, 15, 17, 26)

  # Compute the Positive Parenting subscale scores by summing up the items in
  # the parenting vector. Store the results in a new column called
  # positive_parenting
  df[, "positive_parenting"] <- rowSums(df[, parenting_vector], na.rm = TRUE)

  # Store the items belonging to the Poor Monitoring/Supervision subscale as
  # positions relative to the index in a temporary vector. Add the index to each
  # element of the temporary vector to obtain their actual column numbers and
  # store the results in a vector
  monitoring_vector <- index + c(5, 9, 16, 18, 20, 23, 27, 28, 29, 31)

  # Compute the Poor Monitoring/Supervision subscale scores by summing up the
  # items in the monitoring vector. Store the results in a new column called
  # poor_monitoring
  df[, "poor_monitoring"] <- rowSums(df[, monitoring_vector], na.rm = TRUE)

  # Store the items belonging to the Inconsistent Discipline subscale as
  # positions relative to the index in a temporary vector. Add the index to each
  # element of the temporary vector to obtain their actual column numbers and
  # store the results in a vector
  discipline_vector <- index + c(2, 7, 11, 21, 24, 30)

  # Compute the Inconsistent Discipline subscale scores by summing up the items
  # in the discipline vector. Store the results in a new column called discipline
  df[, "discipline"] <- rowSums(df[, discipline_vector], na.rm = TRUE)

  # Store the items belonging to the Corporal Punishment subscale as positions
  # relative to the index in a temporary vector. Add the index to each element
  # of the temporary vector to obtain their actual column numbers and store the
  # results in a vector
  punishment_vector <- index + c(32, 34, 38)

  # Compute the Corporal Punishment subscale scores by summing up the items in
  # the punishment vector. Store the results in a new column called punishment
  df[, "punishment"] <- rowSums(df[, punishment_vector], na.rm = TRUE)

  # Return the data frame with the newly created subscale columns
  return(df)
}
