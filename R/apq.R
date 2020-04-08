apq <- function(df, start) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing start argument
  if (missing(start)) {
    stop("Please specify the column name of Q1 of the APQ from your
         data frame in quotes here")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the very first column of the APQ data frame
  # and call it the index
  index <- which(colnames(df) == start)

  # Using the index as a reference point, if the number of columns following
  # the index is greater than the total number of columns in the APQ data frame,
  # it might mean that the data frame do not have the complete set of 42 items
  # for the APQ
  if (index + 41 > ncol(df)) {
    stop("You do not have the complete set of 42 items for the APQ")
  }

  # Store the items in the Involvement subscale as positions relative to the
  # index in a vector
  involvement_vector <- c(0, 3, 6, 8, 10, 13, 14, 19, 22, 25)

  # Add the index to each element of the Involvement subscale vector to
  # obtain the actual column numbers in the APQ data frame
  indexed_involvement <- involvement_vector + index

  # Sum the items in the Involvement subscale and store it in a new column
  # called involvement
  df[, "involvement"] <- rowSums(df[, indexed_involvement], na.rm = TRUE)

  # Store the items in the Positive Parenting subscale as positions relative
  # to the index in a vector
  positive_parenting_vector <- c(1, 4, 12, 15, 17, 26)

  # Add the index to each element of the Positive Parenting vector to
  # obtain the actual column numbers in the APQ data frame
  indexed_positive_parenting <- positive_parenting_vector + index

  # Sum the items in the Positive Parenting subscale and store it in a new column
  # called positive_parenting
  df[, "positive_parenting"] <- rowSums(df[, indexed_positive_parenting],
                                        na.rm = TRUE)

  # Store the items in the Poor Monitoring/Supervision subscale as positions
  # relative to the index in a vector
  poor_monitoring_vector <- c(5, 9, 16, 18, 20, 23, 27, 28, 29, 31)

  # Add the index to each element of the Poor Monitoring/Supervision vector to
  # obtain the actual column numbers in the APQ data frame
  indexed_poor_monitoring <- poor_monitoring_vector + index

  # Sum the items in the Poor Monitoring/Supervision subscale and store it in a
  # new column called poor_monitoring
  df[, "poor_monitoring"] <- rowSums(df[, indexed_poor_monitoring],
                                     na.rm = TRUE)

  # Store the items in the Inconsistent Discipline subscale as positions
  # relative to the index in a vector
  discipline_vector <- c(2, 7, 11, 21, 24, 30)

  # Add the index to each element of the Inconsistent Discipline vector to
  # obtain the actual column numbers in the APQ data frame
  indexed_discipline <- discipline_vector + index

  # Sum the items in the Inconsistent Discipline subscale and store it in a
  # new column called discipline
  df[, "discipline"] <- rowSums(df[, indexed_discipline], na.rm = TRUE)

  # Store the items in the Corporal Punishment subscale as positions relative
  # to the index in a vector
  punishment_vector <- c(32, 34, 38)

  # Add the index to each element of the Corporal Punishment vector to obtain
  # the actual column numbers in the APQ data frame
  indexed_punishment <- punishment_vector + index

  # Sum the items in the Corporal Punishment subscale and store it in a new
  # column called punishment
  df[, "punishment"] <- rowSums(df[, indexed_punishment], na.rm = TRUE)

  # Return the data frame with the newly created subscale columns
  return(df)
}
