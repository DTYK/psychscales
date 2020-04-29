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

  # Test for missing PCSQ_1 argument
  if (missing(PCSQ_1)) {
    stop("Please specify the column name of Q1, in quotes, of the PCSQ from your
         data frame here")
  }

  # Test for the scenario where a non-existent column name is provided in the
  # PCSQ_1 argument
  if (!PCSQ_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Test for the scenario where there is no data in the data frame
  if (nrow(df) == 0) {
    stop("Your data frame does not contain any data. Please use a data frame
         containing PCSQ data")
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

  # Reverse score item 3 and store the output in a new column called PCSQ_3r
  df[, "PCSQ_3r"] <- 6 - df[, index + 2]

  # Reverse score item 9 and store the output in a new column called PCSQ_9r
  df[, "PCSQ_9r"] <- 6 - df[, index + 8]

  # Store the items belonging to the Community Integration subscale as positions
  # relative to the index in a temporary vector. Add the index to each element
  # of the temporary vector to obtain their actual column numbers and store the
  # results in a vector
  integration_vector <- c(index + c(0, 1, 3), which(colnames(df) == "PCSQ_3r"))

  # Compute the Community Integration subscale scores by averaging the scores
  # across all items in the integration vector. Store the results in a new column
  # called integration
  df[, "integration"] <- round(rowMeans(df[, integration_vector]), 2)

  # Store the items belonging to the Community Participation subscale as positions
  # relative to the index in a temporary vector. Add the index to each element
  # of the temporary vector to obtain their actual column numbers and store the
  # results in a vector
  participation_vector <- c(index + c(4, 5, 6, 7), which(colnames(df) == "PCSQ_9r"))

  # Compute the Community Participation subscale scores by averaging the scores
  # across all items in the participation vector. Store the results in a new
  # column called participation
  df[, "participation"] <- round(rowMeans(df[, participation_vector]), 2)

  # Store the items belonging to the Community Organizations subscale as positions
  # relative to the index in a temporary vector. Add the index to each element
  # of the temporary vector to obtain their actual column numbers and store the
  # results in a vector
  org_vector <- index + c(9, 10, 11, 12, 13)

  # Compute the Community Organizations subscale scores by averaging the scores
  # across all items in the org vector. Store the results in a new column called
  # organizations
  df[, "organizations"] <- round(rowMeans(df[, org_vector]), 2)

  # Store the items belonging to the Total scale as positions relative to the
  # index in a temporary vector. Add the index to each element of the temporary
  # vector to obtain their actual column numbers and store the results in a vector
  total_vector <- c(index + c(0, 1, 3:7, 9:13), which(colnames(df) == "PCSQ_3r"),
                    which(colnames(df) == "PCSQ_9r"))

  # Compute the Total scale scores by averaging the scores across all items in
  # the total vector. Store the results in a new column called total
  df[, "total"] <- round(rowMeans(df[, total_vector]), 2)

  # Remove the reverse scored columns that were temporarily created for
  # the computation of the CNS score
  df[, c("PCSQ_3r", "PCSQ_9r")] <- NULL

  # Return the data frame with the newly created subscale columns
  return(df)
}
