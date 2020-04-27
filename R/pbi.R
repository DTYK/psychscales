# The pbi function takes a data frame containing PBI data and outputs the same
# data frame with the addition of new columns containing the PBI subscale scores
# The first argument takes in a data frame containing the PBI data. The second
# argument takes in the name of the column containing item 1 of the PBI. Name of
# the column has to be enclosed with quotes (""). The function assumes that the
# data frame is structured such that the responses of the Mother form are
# presented first/before/on the left of the responses of the father form. If the
# responses of the Father form are presented first, set the third argument, flip,
# to TRUE

pbi <- function(df, PBI_1, flip = FALSE) {

  # Test for missing data frame argument
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test for missing PBI_1 argument
  if (missing(PBI_1)) {
    stop("Please specify the column name of Q1, in quotes, of the PBI from your
         data frame here")
  }

  # Test for the scenario when a non-existent column name is provided in the
  # PBI_1 argument
  if (!PBI_1 %in% names(df)) {
    stop("Column name does not exist")
  }

  # Test if there is no data in the data frame
  if (nrow(df) == 0) {
    stop("Your data frame does not contain any data. Please use a data frame
         containing PBI data")
  }

  # Convert df argument into a data frame
  df <- as.data.frame(df)

  # Identify the column number of the PBI_1 argument and call it the index.
  index <- which(colnames(df) == PBI_1)

  # Using the index as a reference point, test whether the 49th column after the
  # index (representing PBI father form item 25) exceed the total
  # number of columns in the df argument. If so, this might mean that the data
  # frame do not contain the complete set of 50 items (25 for mother, 25 for
  # father)
  if (index + 49 > ncol(df)) {
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
  # their position relative to the index and store it in a vector
  reverse_vector <- names(df)[index + c(1, 2, 3, 6, 13, 14, 15, 17, 20, 21,
                                           23, 24, 26, 27, 28, 31, 38, 39, 40,
                                           42, 45, 46, 48, 49)]

  # Using a for loop, extract the column names for each element in the reverse
  # scoring vector and use each column name as input in the reverse_score
  # function
  for (i in reverse_vector) {
    df <- reverse_score(df, i)
  }

  # Store the column numbers of items belonging to the Mother Care subscale in
  # a vector. For direct-scored items, obtain the column numbers by adding the
  # index to the position of each element relative to the index. For the
  # reverse-scored items, obtain the column numbers by searching for their names
  # derived from the concatenation of their original names and a r suffix
  m_care_vector <- c(index + c(0, 4, 5, 10, 11, 16),
                          which(names(df) == paste0(names(df)[index + 1], "r")),
                          which(names(df) == paste0(names(df)[index + 3], "r")),
                          which(names(df) == paste0(names(df)[index + 13], "r")),
                          which(names(df) == paste0(names(df)[index + 15], "r")),
                          which(names(df) == paste0(names(df)[index + 17], "r")),
                          which(names(df) == paste0(names(df)[index + 23], "r")))

  # Compute the Mother Care subscale scores by summing up the items in the
  # m_care_vector. Store the results in a new column called mother_care
  df[, "mother_care"] <- rowSums(df[, m_care_vector])

  # Store the column numbers of items belonging to the Mother Overprotection
  # subscale in a vector. For direct-scored items, obtain the column numbers by
  # adding the index to the position of each element relative to the index. For
  # the reverse-scored items, obtain the column numbers by searching for their
  # names derived from the concatenation of their original names and a r suffix
  m_overprot_vector <- c(index + c(7, 8, 9, 12, 18, 19, 22),
                         which(names(df) == paste0(names(df)[index + 2], "r")),
                         which(names(df) == paste0(names(df)[index + 6], "r")),
                         which(names(df) == paste0(names(df)[index + 14], "r")),
                         which(names(df) == paste0(names(df)[index + 20], "r")),
                         which(names(df) == paste0(names(df)[index + 21], "r")),
                         which(names(df) == paste0(names(df)[index + 24], "r")))

  # Compute the Mother Overprotection subscale scores by summing up the items in
  # the m_overprot_vector. Store the results in a new column called
  # mother_overprotection
  df[, "mother_overprotection"] <- rowSums(df[, m_overprot_vector])

  # Store the column numbers of items belonging to the Father Care subscale in
  # a vector. For direct-scored items, obtain the column numbers by adding the
  # index to the position of each element relative to the index. For the
  # reverse-scored items, obtain the column numbers by searching for their names
  # derived from the concatenation of their original names and a r suffix
  f_care_vector <- c(index + c(25, 29, 30, 35, 36, 41),
                     which(names(df) == paste0(names(df)[index + 26], "r")),
                     which(names(df) == paste0(names(df)[index + 28], "r")),
                     which(names(df) == paste0(names(df)[index + 38], "r")),
                     which(names(df) == paste0(names(df)[index + 40], "r")),
                     which(names(df) == paste0(names(df)[index + 42], "r")),
                     which(names(df) == paste0(names(df)[index + 48], "r")))

  # Compute the Father Care subscale scores by summing up the items in the
  # f_care_vector. Store the results in a new column called father_care
  df[, "father_care"] <- rowSums(df[, f_care_vector])

  # Store the column numbers of items belonging to the Father Overprotection
  # subscale in a vector. For direct-scored items, obtain the column numbers by
  # adding the index to the position of each element relative to the index. For
  # the reverse-scored items, obtain the column numbers by searching for their
  # names derived from the concatenation of their original names and a r suffix
  f_overprot_vector <- c(index + c(32, 33, 34, 37, 43, 44, 47),
                         which(names(df) == paste0(names(df)[index + 27], "r")),
                         which(names(df) == paste0(names(df)[index + 31], "r")),
                         which(names(df) == paste0(names(df)[index + 39], "r")),
                         which(names(df) == paste0(names(df)[index + 45], "r")),
                         which(names(df) == paste0(names(df)[index + 46], "r")),
                         which(names(df) == paste0(names(df)[index + 49], "r")))

  # Compute the Father Overprotection subscale scores by summing up the items in
  # the f_overprot_vector. Store the results in a new column called
  # father_overprotection
  df[, "father_overprotection"] <- rowSums(df[, f_overprot_vector])

  # Using a for loop, remove the newly created reverse-scored columns using
  # the reverse_vector
  for (i in reverse_vector) {
    df[, paste0(i, "r")] <- NULL
  }

  # If the responses of the Father form are presented first, swap the naming
  # of the Care and Overprotection columns between fathers and mothers
  if (flip == TRUE) {

    names(df)[which(names(df) == "mother_care"):
                which(names(df) == "father_overprotection")] <-
      c("father_care", "father_overprotection", "mother_care",
        "mother_overprotection")
  }

  # Return the data frame with the newly created subscale columns
  return(df)
}
