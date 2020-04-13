# The pcsq function takes a data frame containing PCSQ data and outputs the same
# data frame with the addition of new columns containing the PCSQ subscale scores
# The first argument takes in a data frame containing the PCSQ data. The second
# argument takes in the name of the column containing item 1 of the PCSQ. Name
# of the column has to be enclosed with quotes ("").

pcsq <- function(df, PCSQ_1) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing MSPSS_1 argument
  if (missing(PCSQ_1)) {
    stop("Please specify the column name of Q1, in quotes, of the PCSQ from your
         data frame here")
  }

  # Test for the scenario when a non-existent column name is provided in the
  # PCSQ_1 argument
  if (!PCSQ_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the PCSQ_1 argument and call it the index.
  index <- which(colnames(df) == PCSQ_1)

  # Using the index as a reference point, test whether the 13th column after
  # the index (representing PCSQ item 14) exceed the total number of columns
  # in the df argument. If so, this might mean that the data frame do not
  # contain the complete set of 14 items
  if (index + 13 > ncol(df)) {
    stop("You do not have the complete set of 14 items for the PCSQ")
  }

  # Store the items belonging to the Community Integration subscale as positions
  # relative to the index in a vector
  integration_vector <- c(0, 1, 3)

  # Add the index to each element of the Community Integration subscale vector to
  # obtain the actual column numbers in the data frame
  indexed_integration <- integration_vector + index

  # Using the above vector, obtain the mean of the items and store the result
  # in a new column called integration
  df[, "integration"] <- round(rowMeans(df[, indexed_integration], na.rm = TRUE), 2)

  # Store the items belonging to the Community Participation subscale as positions
  # relative to the index in a vector
  participation_vector <- c(2, 4, 5, 6, 7, 8)

  # Add the index to each element of the Community Participation subscale vector
  # to obtain the actual column numbers in the data frame
  indexed_participation <- participation_vector + index

  # Using the above vector, obtain the mean of the items and store the result in
  # a new column called participation
  df[, "participation"] <- round(rowMeans(df[, indexed_participation], na.rm = TRUE), 2)

  # Store the items belonging to the Community Organizations subscale as positions
  # relative to the index in a vector
  organizations_vector <- c(9, 10, 11, 12, 13)

  # Add the index to each element of the Community Organizations subscale vector
  # to obtain the actual column numbers in the data frame
  indexed_organizations <- organizations_vector + index

  # Using the above vector, obtain the mean of the items and store the result in
  # a new column called organizations
  df[, "organizations"] <- round(rowMeans(df[, indexed_organizations], na.rm = TRUE), 2)

  # Store the items belonging to the Total scale as positions relative to the
  # index in a vector
  total_vector <- c(0:13)

  # Add the index to each element of the Total scale vector to obtain the actual
  # column numbers in the data frame
  indexed_total <- total_vector + index

  # Using the above vector, obtain the mean of the items and store the result in
  # a new column called total
  df[, "total"] <- round(rowMeans(df[, indexed_total], na.rm = TRUE), 2)

  # Return the data frame with the newly created subscale columns
  return(df)
}
