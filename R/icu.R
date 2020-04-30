# The icu function takes a data frame containing ICU data and outputs the same
# data frame with the addition of new columns containing the ICU subscale scores
# The first argument takes in a data frame containing the ICU data. The second
# argument takes in the name of the column containing item 1 of the ICU. Name
# of the column has to be enclosed with quotes ("").

icu <- function(df, ICU_1) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing ICU_1 argument
  if (missing(ICU_1)) {
    stop("Please specify the column name of Q1, in quotes, of the ICU from your
         data frame here")
  }

  # Test for the scenario where a non-existent column name is provided in the
  # ICU_1 argument
  if (!ICU_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Test for the scenario where there is no data in the data frame
  if (nrow(df) == 0) {
    stop("Your data frame does not contain any data. Please use a data frame
         containing ICU data")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the ICU_1 argument and call it the index.
  index <- which(colnames(df) == ICU_1)

  # Using the index as a reference point, test whether the 23rd column after
  # the index (representing ICU item 24) exceed the total number of columns
  # in the df argument. If so, this might mean that the data frame do not
  # contain the complete set of 24 items
  if (index + 23 > ncol(df)) {
    stop("You do not have the complete set of 24 items for the ICU")
  }

  # Create a function to reverse-score items. Function takes in data frame
  # containing ICU data and the name of the column that needs to be reverse-
  # scored. Function outputs the same data frame with the addition of the
  # reverse scored column. Reverse-scored columns are given the same name with
  # the addition of a r suffix
  reverse_score <- function(df, column) {

    df[, paste0(column, "r")] <- 5 - df[, column]

    return(df)
  }

  # Extract the names of the columns that require reverse-scoring based on
  # their position relative to the index and store it in a vector
  reverse_vector <- names(df)[index + c(0, 2, 4, 7, 12:16, 18, 22, 23)]

  # Using a for loop, extract the column names for each element in the reverse
  # scoring vector and use each column name as input in the reverse_score
  # function
  for (i in reverse_vector) {
    df <- reverse_score(df, i)
  }

  # Store the column numbers of items belonging to the Callousness subscale in
  # a vector. For direct-scored items, obtain the column numbers by adding the
  # index to the position of each element relative to the index. For the
  # reverse-scored items, obtain the column numbers by searching for their names
  # derived from the concatenation of their original names and a r suffix
  cal_vector <- append(index + c(1, 3, 6, 8, 9, 10, 11, 17, 19, 20),
                       which(names(df) == paste0(names(df)[index + 7], "r")))

  # Using the above vector, obtain the sum of the items and store the result
  # in a new column called callousness
  df[, "callousness"] <- rowSums(df[, cal_vector])

  # Store the column numbers of items belonging to the Uncaring subscale in
  # a vector. Obtain the column numbers by searching for their names derived
  # from the concatenation of their original names and a r suffix
  uncar_vector <- c(which(names(df) == paste0(names(df)[index + 2], "r")),
                    which(names(df) == paste0(names(df)[index + 4], "r")),
                    which(names(df) == paste0(names(df)[index + 12], "r")),
                    which(names(df) == paste0(names(df)[index + 14], "r")),
                    which(names(df) == paste0(names(df)[index + 15], "r")),
                    which(names(df) == paste0(names(df)[index + 16], "r")),
                    which(names(df) == paste0(names(df)[index + 22], "r")),
                    which(names(df) == paste0(names(df)[index + 23], "r")))

  # Using the above vector, obtain the sum of the items and store the result in
  # a new column called uncaring
  df[, "uncaring"] <- rowSums(df[, uncar_vector])

  # Store the column numbers of items belonging to the Unemotional subscale in
  # a vector. For direct-scored items, obtain the column numbers by adding the
  # index to the position of each element relative to the index. For the
  # reverse-scored items, obtain the column numbers by searching for their names
  # derived from the concatenation of their original names and a r suffix
  unemo_vector <- c(which(names(df) == paste0(names(df)[index + 0], "r")),
                    index + 5, which(names(df) == paste0(names(df)[index + 13], "r")),
                    which(names(df) == paste0(names(df)[index + 18], "r")),
                    index + 21)

  # Using the above vector, obtain the sum of the items and store the result in
  # a new column called unemotional
  df[, "unemotional"] <- rowSums(df[, unemo_vector])

  # Using a for loop, remove the newly created reverse-scored columns using
  # the reverse_vector
  for (i in reverse_vector) {
    df[, paste0(i, "r")] <- NULL
  }

  # Return the data frame with the newly created subscale columns
  return(df)
}
