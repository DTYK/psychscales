# The pbi function takes a data frame containing PBI data and outputs the same
# data frame with the addition of new columns containing the PBI subscale scores
# The first argument takes in a data frame containing the PBI data. The second
# argument takes in the name of the column containing item 1 of the PBI. Name
# of the column has to be enclosed with quotes (""). The function assumes that
# the data frame is structured such that the responses of the mother form are
# presented before/on the left to the responses of the father form

pbi <- function(df, PBI_mother_1) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing PBI_mother_1 argument
  if (missing(PBI_mother_1)) {
    stop("Please specify the column name of Q1, in quotes, of the PBI mother form
    from your data frame here")
  }

  # Test for the scenario when a non-existent column name is provided in the
  # PBI_mother_1 argument
  if (!PBI_mother_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the PBI_mother_1 argument and call it the
  # mother_index.
  mother_index <- which(colnames(df) == PBI_mother_1)

  # Using the mother_index object, use it to identify the column number of
  # item 1 of the father form. Store this as the father_index
  father_index <- mother_index + 25

  # Using the mother_index as a reference point, test whether the 49th column
  # after the index (representing PBI father form item 25) exceed the total
  # number of columns in the df argument. If so, this might mean that the data
  # frame do not contain the complete set of 50 items (25 for mother, 25 for
  # father)
  if (mother_index + 49 > ncol(df)) {
    stop("You do not have the complete set of 50 items for the PBI (25 for mother,
         25 for father)")
  }

  # Create a function to reverse-score items. Function takes in data frame
  # containing PBI data and the name of the column that needs to be reverse-
  # scored. Function outputs the same data frame with the addition of the
  # reverse scored column. Reverse-scored columns are given the same name with
  # the addition of a r suffix
  reverse_score <- function(df, column) {

    df[, paste0(column, "r")] <- 5 - df[, column]

    return(df)
  }

  # Extract the names of the columns that require reverse-scoring based on
  # their position relative to the mother_index and store it in a vector
  reverse_vector <- names(df)[mother_index + c(1, 2, 3, 6, 13, 14, 15, 17, 20, 21,
                                           23, 24, 26, 27, 28, 31, 38, 39, 40,
                                           42, 45, 46, 48, 49)]

  # Using a for loop, extract the column names for each element in the reverse
  # scoring vector and use each column name as input in the reverse_score
  # function
  for (i in reverse_vector) {
    df <- reverse_score(df, i)
  }

  # Store the items belonging to the Care subscale as positions relative to the
  # mother_index in a vector. For the reverse-scored items belonging to the
  # Care subscale, obtain their column numbers by searching for the concatenation
  # of their original column names (by their position relative to the mother
  # index) and a r suffix
  care_vector <- c(0, which(names(df) == paste0(names(df)[mother_index + 1], "r")),
                   which(names(df) == paste0(names(df)[mother_index + 3], "r")),
                   4, 5, 10, 11,
                   which(names(df) == paste0(names(df)[mother_index + 13], "r")),
                   which(names(df) == paste0(names(df)[mother_index + 15], "r")),
                   16, which(names(df) == paste0(names(df)[mother_index + 17], "r")),
                   which(names(df) == paste0(names(df)[mother_index + 23], "r")))

  # Add the mother_index to each element of the Care subscale vector to obtain
  # the actual column numbers in the data frame for the mother form
  indexed_care_mother <- care_vector + mother_index

  # Using the above vector, obtain the sum of the items and store the result in
  # a new column called mother_care
  df[, "mother_care"] <- rowSums(df[, indexed_care_mother], na.rm = TRUE)

  # Store the items belonging to the Overprotection subscale as positions
  # relative to the mother_index in a vector. For the reverse-scored items
  # belonging to the Overprotection subscale, obtain their column numbers by
  # searching for the concatenation of their original column names (by their
  # position relative to the mother index) and a r suffix
  overprotection_vector <- c(which(names(df) == paste0(names(df)[mother_index + 2], "r")),
                             which(names(df) == paste0(names(df)[mother_index + 6], "r")),
                             7, 8, 9, 12,
                             which(names(df) == paste0(names(df)[mother_index + 14], "r")),
                             18, 19,
                             which(names(df) == paste0(names(df)[mother_index + 20], "r")),
                             which(names(df) == paste0(names(df)[mother_index + 21], "r")),
                             22,
                             which(names(df) == paste0(names(df)[mother_index + 24], "r")))

  # Add the mother_index to each element of the Overprotection subscale vector
  # to obtain the actual column numbers in the data frame for the mother form
  indexed_overprotection_mother <- overprotection_vector + mother_index

  # Using the above vector, obtain the sum of the items and store the result in
  # a new column called mother_overprotection
  df[, "mother_overprotection"] <- rowSums(df[, indexed_overprotection_mother], na.rm = TRUE)

  # Store the items belonging to the Care subscale as positions relative to the
  # father_index in a vector. For the reverse-scored items belonging to the
  # Care subscale, obtain their column numbers by searching for the concatenation
  # of their original column names (by their position relative to the father
  # index) and a r suffix
  care_vector <- c(father_index, which(names(df) == paste0(names(df)[father_index + 1], "r")),
                   which(names(df) == paste0(names(df)[father_index + 3], "r")),
                   father_index + 4, father_index + 5, father_index + 10,
                   father_index + 11,
                   which(names(df) == paste0(names(df)[father_index + 13], "r")),
                   which(names(df) == paste0(names(df)[father_index + 15], "r")),
                   father_index + 16,
                   which(names(df) == paste0(names(df)[father_index + 17], "r")),
                   which(names(df) == paste0(names(df)[father_index + 23], "r")))

  # Using the above vector, obtain the sum of the items and store the result in
  # a new column called father_care
  df[, "father_care"] <- rowSums(df[, care_vector], na.rm = TRUE)

  # Store the items belonging to the Overprotection subscale as positions
  # relative to the father_index in a vector. For the reverse-scored items
  # belonging to the Overprotection subscale, obtain their column numbers by
  # searching for the concatenation of their original column names (by their
  # position relative to the father index) and a r suffix
  overprotection_vector <- c(which(names(df) == paste0(names(df)[father_index + 2], "r")),
                             which(names(df) == paste0(names(df)[father_index + 6], "r")),
                             father_index + 7, father_index + 8, father_index + 9,
                             father_index + 12,
                             which(names(df) == paste0(names(df)[father_index + 14], "r")),
                             father_index + 18, father_index + 19,
                             which(names(df) == paste0(names(df)[father_index + 20], "r")),
                             which(names(df) == paste0(names(df)[father_index + 21], "r")),
                             father_index + 22,
                             which(names(df) == paste0(names(df)[father_index + 24], "r")))

  # Using the above vector, obtain the sum of the items and store the result in
  # a new column called father_overprotection
  df[, "father_overprotection"] <- rowSums(df[, overprotection_vector], na.rm = TRUE)

  # Return the data frame with the newly created subscale columns
  return(df)
}
