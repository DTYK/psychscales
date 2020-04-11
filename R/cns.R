# The cns function takes a data frame containing Connectedness to Nature (CNS) data and
# outputs the same data frame with the addition of a new column containing the CNS
# score. The first argument takes in a data frame containing the CNS data. The second
# argument takes in the name of the column containing item 1 of the CNS. Name
# of the column has to be enclosed with quotes ("").

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
}
