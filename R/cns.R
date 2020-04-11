# The cns function takes a data frame containing Connectedness to Nature (CNS) data and
# outputs the same data frame with the addition of a new column containing the CNS
# score. The first argument takes in a data frame containing the CNS data. The second
# argument takes in the name of the column containing item 1 of the CNS. Name of the
# column has to be enclosed with quotes ("").

cns <- function(df, CNS_1) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing MSPSS_1 argument
  if (missing(CNS_1)) {
    stop("Please specify the column name of Q1, in quotes, of the CNS from your
         data frame here")
  }

  # Test for the scenario when a non-existent column name is provided in the
  # MSPSS_1 argument
  if (!CNS_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the CNS_1 argument and call it the index.
  index <- which(colnames(df) == CNS_1)


  # Using the index as a reference point, test whether the 13th column after
  # the index (representing CNS item 14) exceed the total number of columns
  # in the df argument. If so, this might mean that the data frame do not
  # contain the complete set of 14 items
  if (index + 13 > ncol(df)) {
    stop("You do not have the complete set of 14 items for the CNS")
  }

  # Reverse score item 4 and store the output in a new column called CNS_4r
  df[, "CNS_4r"] <- 6 - df[, index + 3]

  # Reverse score item 12 and store the output in a new column called CNS_12r
  df[, "CNS_12r"] <- 6 - df[, index + 11]

  # Reverse score item 14 and store the output in a new column called CNS_14r
  df[, "CNS_14r"] <- 6 - df[, index + 13]

  # Compute the CNS score and store it in a new column called CNS_score
  df[, "CNS_score"] <- round(rowMeans(df[, c(index:(index + 2),
                                       which(colnames(df) == "CNS_4r"),
                                       (index + 4):(index + 10),
                                       which(colnames(df) == "CNS_12r"),
                                       index + 12,
                                       which(colnames(df) == "CNS_14r"))],
                                      na.rm = TRUE), 2)

  # Remove the reverse scored columns that were temporarily created for
  # the computation of the CNS score
  df$CNS_4r <- NULL
  df$CNS_12r <- NULL
  df$CNS_14r <- NULL

  # Return the data frame with the newly created CNS column
  return(df)
}
