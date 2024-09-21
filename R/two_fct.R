#' @title Create a Mapping Table of Original and Modified Factor Levels
#' @description Creates a data frame mapping the original factor levels to the modified levels.
#' @param original_factor The original factor vector before modification.
#' @param modified_factor The modified factor vector after modification.
#' @return A data frame containing the mapping of original to modified levels.
#' @examples
#' # Original and modified factor vectors
#' original_factor <- factor(c('apple', 'banana', 'cherry'))
#' modified_factor <- factor(c('apple_fruit', 'banana_fruit', 'cherry_fruit'))
#'
#' # Create mapping table
#' fct_mapping(original_factor, modified_factor)
#' @export
#' @author Kai Guo
fct_mapping <- function(original_factor, modified_factor) {
  # Parameter validation
  if (!is.factor(original_factor) || !is.factor(modified_factor)) {
    stop("Both 'original_factor' and 'modified_factor' must be factor vectors.")
  }
  if (length(levels(original_factor)) != length(levels(modified_factor))) {
    stop("Both factor vectors must have the same number of levels.")
  }

  # Create mapping data frame
  mapping_df <- data.frame(
    Original_Level = levels(original_factor),
    Modified_Level = levels(modified_factor),
    stringsAsFactors = FALSE
  )

  return(mapping_df)
}

#' @title Merge Similar Factor Levels
#' @description Merges levels of a factor that are similar based on string distance.
#' @importFrom stats hclust as.dist cutree
#' @importFrom stringdist stringdistmatrix
#' @param factor_vec A factor vector to modify.
#' @param max_distance A numeric value specifying the maximum string distance for merging levels.
#' @param method The method for computing string distance (default is 'lv' for Levenshtein distance).
#' @return A factor vector with similar levels merged.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'appel', 'banana', 'bananna', 'cherry'))
#'
#' # Merge similar levels
#' fct_merge_similar(factor_vec, max_distance = 1)
#' @export
#' @author Kai Guo
fct_merge_similar <- function(factor_vec, max_distance = 1, method = 'lv') {
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.numeric(max_distance) || max_distance < 0) {
    stop("The 'max_distance' must be a non-negative numeric value.")
  }
  if (!is.character(method) || length(method) != 1) {
    stop("The 'method' must be a single string specifying the distance method.")
  }


  levels_vec <- levels(factor_vec)
  distance_matrix <- stringdistmatrix(levels_vec, levels_vec, method = method)

  clusters <- hclust(as.dist(distance_matrix))
  groups <- cutree(clusters, h = max_distance)

  mapping <- tapply(levels_vec, groups, function(x) x[1])
  new_levels <- mapping[as.character(groups)]

  factor_vec_merged <- factor(factor_vec, levels = levels_vec, labels = new_levels)

  return(factor_vec_merged)
}
#' @title Rename Factor Levels Using Data Frame Mapping
#' @description Renames the levels of a factor vector based on a mapping provided in a data frame.
#' @param factor_vec A factor vector to modify.
#' @param mapping_df A data frame with two columns: 'old' and 'new', representing old and new level names.
#' @return A factor vector with levels renamed.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('A', 'B', 'C'))
#'
#' # Mapping data frame
#' mapping_df <- data.frame(old = c('A', 'B'), new = c('Alpha', 'Beta'))
#'
#' # Rename levels
#' fct_rename_levels(factor_vec, mapping_df)
#' @export
#' @author Kai Guo
fct_rename_levels <- function(factor_vec, mapping_df) {
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.data.frame(mapping_df) || !all(c('old', 'new') %in% names(mapping_df))) {
    stop("The 'mapping_df' must be a data frame with columns 'old' and 'new'.")
  }

  levels_vec <- levels(factor_vec)
  rename_map <- setNames(as.character(mapping_df$new), as.character(mapping_df$old))
  new_levels <- ifelse(levels_vec %in% names(rename_map), rename_map[levels_vec], levels_vec)

  factor_vec_renamed <- factor(factor_vec, levels = levels_vec, labels = new_levels)

  return(factor_vec_renamed)
}
#' @title Sort Factor Levels Based on Another Vector or Column
#' @description Sorts the levels of a factor vector based on the values of another vector or a column from a data frame. Handles cases where the sorting vector may contain `NA`s. Optionally reorders the data vector's elements to align with the new levels' order.
#' @param factor_vec A factor vector whose levels are to be sorted.
#' @param by A vector or data frame column used as the basis for sorting. Must be the same length as `factor_vec`.
#' @param decreasing Logical. Should the sorting be in decreasing order? Default is \code{FALSE}.
#' @param na_last Logical. Should `NA` values be put last? Default is \code{TRUE}.
#' @param inplace Logical. If \code{TRUE}, returns a new factor vector with elements reordered to align with the new levels' order. If \code{FALSE}, returns a new factor vector with only the levels' order adjusted, leaving the data vector's elements' order unchanged. Defaults to \code{FALSE}.
#' @return A factor vector with levels sorted based on `by`. Depending on the \code{inplace} parameter, the data vector's elements may also be reordered.
#' @examples
#' # Example using a vector without reordering data elements
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date'))
#' by_vec <- c(2, 3, 1, NA)
#' sorted_factor <- fct_sort(factor_vec, by = by_vec)
#' print(sorted_factor)
#' # [1] apple  banana cherry date
#' # Levels: cherry apple banana date
#'
#' # Example using a vector and reordering data elements
#' sorted_factor_inplace <- fct_sort(factor_vec, by = by_vec, inplace = TRUE)
#' print(sorted_factor_inplace)
#' # [1] cherry apple banana date
#' # Levels: cherry apple banana date
#'
#' # Example using a data frame column without reordering data elements
#' data <- data.frame(
#'   Category = factor(c('apple', 'banana', 'cherry', 'date')),
#'   Value = c(2, 3, 1, NA)
#' )
#' sorted_factor_df <- fct_sort(data$Category, by = data$Value)
#' print(sorted_factor_df)
#' # [1] apple  banana cherry date
#' # Levels: cherry apple banana date
#'
#' # Example using a data frame column and reordering data elements
#' sorted_factor_df_inplace <- fct_sort(data$Category, by = data$Value, inplace = TRUE)
#' print(sorted_factor_df_inplace)
#' # [1] cherry apple banana date
#' # Levels: cherry apple banana date
#' @export
fct_sort <- function(factor_vec, by, decreasing = FALSE, na_last = TRUE, inplace = FALSE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (length(factor_vec) != length(by)) {
    stop("The 'factor_vec' and 'by' must be of the same length.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }
  if (!is.logical(na_last) || length(na_last) != 1) {
    stop("The 'na_last' parameter must be a single logical value.")
  }
  if (!is.logical(inplace) || length(inplace) != 1) {
    stop("The 'inplace' parameter must be a single logical value.")
  }

  # Handle NA values in 'by'
  na_indices <- is.na(by)
  if (any(na_indices)) {
    if (na_last) {
      # Assign a value that will place NA values last in sorting
      by[na_indices] <- ifelse(decreasing, -Inf, Inf)
    } else {
      # Assign a value that will place NA values first in sorting
      by[na_indices] <- ifelse(decreasing, Inf, -Inf)
    }
  }

  # Create a data frame for sorting
  df <- data.frame(
    factor = factor_vec,
    by = by,
    stringsAsFactors = FALSE
  )

  # Order based on 'by'
  df <- df[order(df$by, decreasing = decreasing), ]

  # Get unique levels in order
  ordered_levels <- unique(as.character(df$factor))

  # Reorder factor levels
  factor_vec_ordered <- factor(factor_vec, levels = ordered_levels)

  if (inplace) {
    # Reorder the data vector's elements to align with the new levels' order
    # Create a mapping of levels to their new order
    level_order <- setNames(seq_along(ordered_levels), ordered_levels)

    # Assign an order value to each element based on its level
    element_order <- level_order[as.character(factor_vec_ordered)]

    # Handle NA by assigning Inf to place them at the end
    element_order[is.na(element_order)] <- Inf

    # Get the order of elements
    reordered_indices <- order(element_order, na.last = TRUE)

    # Reorder the data vector
    reordered_data <- factor_vec_ordered[reordered_indices]

    return(reordered_data)
  } else {
    return(factor_vec_ordered)
  }
}
####
#' @title Sort Factor Levels Using a Custom Function
#' @description Reorders the levels of a factor vector based on a custom function applied to each level. Optionally reorders the data vector's elements to align with the new levels' order.
#' @param factor_vec A factor vector to sort.
#' @param sort_func A function that takes a character vector (the levels) and returns a vector of the same length to sort by.
#' @param decreasing Logical. Should the sort be decreasing? Default is \code{FALSE}.
#' @param inplace Logical. If \code{TRUE}, returns a new factor vector with elements reordered to align with the new levels' order. If \code{FALSE}, returns a new factor vector with only the levels' order adjusted, leaving the data vector's elements' order unchanged. Defaults to \code{FALSE}.
#' @return A factor vector with levels reordered according to the custom function. Depending on the \code{inplace} parameter, the data vector's elements may also be reordered.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry'))
#'
#' # Sort levels by reverse alphabetical order without reordering data elements
#' sorted_custom <- fct_sort_custom(factor_vec, function(x) -rank(x))
#' print(sorted_custom)
#' # [1] apple  banana cherry
#' # Levels: cherry banana apple
#'
#' # Sort levels by reverse alphabetical order and reorder data elements
#' sorted_custom_inplace <- fct_sort_custom(factor_vec, function(x) -rank(x), inplace = TRUE)
#' print(sorted_custom_inplace)
#' # [1] cherry banana apple
#' # Levels: cherry banana apple
#'
#' # Sort levels by length of the level name without reordering data elements
#' sorted_custom_length <- fct_sort_custom(factor_vec, function(x) nchar(x))
#' print(sorted_custom_length)
#' # [1] apple  banana cherry
#' # Levels: apple cherry banana
#'
#' # Sort levels by length of the level name and reorder data elements
#' sorted_custom_length_inplace <- fct_sort_custom(factor_vec, function(x) nchar(x), inplace = TRUE)
#' print(sorted_custom_length_inplace)
#' # [1] apple  cherry banana
#' # Levels: apple cherry banana
#' @export
#' @author Kai Guo
fct_sort_custom <- function(factor_vec, sort_func, decreasing = FALSE, inplace = FALSE) {
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.function(sort_func)) {
    stop("The 'sort_func' must be a function.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }
  if (!is.logical(inplace) || length(inplace) != 1) {
    stop("The 'inplace' parameter must be a single logical value.")
  }

  levels_vec <- levels(factor_vec)
  sort_keys <- sort_func(levels_vec)

  if (length(sort_keys) != length(levels_vec)) {
    stop("The 'sort_func' must return a vector of the same length as the input levels.")
  }

  ordered_levels <- levels_vec[order(sort_keys, decreasing = decreasing)]
  factor_vec_ordered <- factor(factor_vec, levels = ordered_levels)

  if (inplace) {
    # Reorder the data vector's elements to align with the new levels' order
    # Create a mapping of levels to their new order
    level_order <- setNames(seq_along(ordered_levels), ordered_levels)

    # Assign an order value to each element based on its level
    element_order <- level_order[as.character(factor_vec_ordered)]

    # Handle NA by assigning Inf to place them at the end
    element_order[is.na(element_order)] <- Inf

    # Get the order of elements
    reordered_indices <- order(element_order, na.last = TRUE)

    # Reorder the data vector
    reordered_data <- factor_vec_ordered[reordered_indices]

    return(reordered_data)
  } else {
    return(factor_vec_ordered)
  }
}

#' @title Concatenate Multiple Factor Vectors
#' @description Combines multiple factor vectors into a single factor, unifying the levels.
#' @param ... Factor vectors to concatenate.
#' @return A single factor vector containing all elements and unified levels.
#' @examples
#' # Example factor vectors
#' factor_vec1 <- factor(c('apple', 'banana'))
#' factor_vec2 <- factor(c('cherry', 'date'))
#'
#' # Concatenate factors
#' concatenated_factor <- fct_concat(factor_vec1, factor_vec2)
#' levels(concatenated_factor)
#' @export
#' @author Kai Guo
fct_concat <- function(...) {
  factors <- list(...)

  if (!all(sapply(factors, is.factor))) {
    stop("All inputs must be factor vectors.")
  }

  combined_levels <- unique(unlist(lapply(factors, levels)))
  combined_values <- unlist(lapply(factors, as.character))

  concatenated_factor <- factor(combined_values, levels = combined_levels)

  return(concatenated_factor)
}

