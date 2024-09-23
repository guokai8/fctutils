#' @title Replace a Factor Level and Optionally Insert at Specified Position
#' @description Replaces a specified level in a factor vector with a new level. If a position is provided, the new level is inserted at the specified position among the levels; otherwise, the original level order is preserved.
#' @param factor_vec A factor vector in which a level will be replaced.
#' @param old_level A character string specifying the level to be replaced.
#' @param new_level A character string specifying the new level to replace the old level.
#' @param position Optional. A positive integer specifying the position to insert the new level in the levels vector. If \code{NULL}, the original level order is preserved. Default is \code{NULL}.
#' @return A factor vector with the level replaced and the new level optionally inserted at the specified position.
#' @examples
#' #
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'fig', 'grape'))
#'
#' # replace 'banana' as 'blueberry', and keep original order
#' fct_replace(factor_vec, old_level = 'banana', new_level = 'blueberry')
#'
#' # replace 'banana' as 'blueberry'
#' fct_replace(factor_vec, old_level = 'banana', new_level = 'blueberry', position = 2)
#' @export
#' @author Kai Guo
fct_replace <- function(factor_vec, old_level, new_level, position = NULL) {
  #
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.character(old_level) || length(old_level) != 1) {
    stop("The 'old_level' parameter must be a single character string.")
  }
  if (!is.character(new_level) || length(new_level) != 1) {
    stop("The 'new_level' parameter must be a single character string.")
  }
  if (!is.null(position) && (!is.numeric(position) || position <= 0 || position != as.integer(position))) {
    stop("The 'position' parameter must be a positive integer or NULL.")
  }

  #
  current_levels <- levels(factor_vec)

  #
  if (!(old_level %in% current_levels)) {
    stop("The 'old_level' does not exist in the factor levels.")
  }

  #
  factor_vec <- as.character(factor_vec)
  factor_vec[factor_vec == old_level] <- new_level

  #
  current_levels[current_levels == old_level] <- new_level

  #
  if (sum(current_levels == new_level) > 1) {
    current_levels <- unique(current_levels)
  }

  #
  if (!is.null(position)) {
    #
    if (position > length(current_levels)) {
      #
      position <- length(current_levels) + 1
    }
    #
    current_levels <- current_levels[current_levels != new_level]
    current_levels <- append(current_levels, new_level, after = position - 1)
  }

  #
  factor_vec <- factor(factor_vec, levels = current_levels)

  return(factor_vec)
}
#' @title Replace Parts of Factor Levels Based on a Pattern
#' @description Replaces parts of the factor levels that match a specified pattern with a new string.
#' @param factor_vec A factor vector to be modified.
#' @param pattern A regular expression pattern to match.
#' @param replacement A string to replace the matched parts.
#' @param replace_all Logical. If \code{TRUE} (default), all occurrences of the pattern are replaced. If \code{FALSE}, only the first occurrence is replaced.
#' @return A factor vector with levels modified.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple_pie', 'banana_bread', 'cherry_cake'))
#'
#' # Replace '_pie', '_bread', '_cake' with '_dessert' (all occurrences)
#' fct_replace_pattern(factor_vec, pattern = '_.*', replacement = '_dessert')
#'
#' # Replace only the first occurrence of '_' with '-'
#' fct_replace_pattern(factor_vec, pattern = '_', replacement = '-', replace_all = FALSE)
#' @export
#' @author Kai Guo
fct_replace_pattern <- function(factor_vec, pattern, replacement, replace_all = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("The 'pattern' parameter must be a single character string.")
  }
  if (!is.character(replacement) || length(replacement) != 1) {
    stop("The 'replacement' parameter must be a single character string.")
  }
  if (!is.logical(replace_all) || length(replace_all) != 1) {
    stop("The 'replace_all' parameter must be a single logical value (TRUE or FALSE).")
  }

  # Choose the appropriate replacement function based on replace_all
  if (replace_all) {
    new_levels <- gsub(pattern, replacement, levels(factor_vec))
  } else {
    new_levels <- sub(pattern, replacement, levels(factor_vec))
  }

  # Update factor levels
  factor_vec <- factor(factor_vec, levels = levels(factor_vec), labels = new_levels)

  return(factor_vec)
}

###
#' @title Merge Two Factors and Retain Unique Levels
#' @description Merges two factor vectors into one, retaining unique levels from both factors.
#' @param factor_vec1 The first factor vector.
#' @param factor_vec2 The second factor vector.
#' @param level_order A character vector specifying the desired order of levels. If NULL, levels are ordered by their first appearance.
#' @return A factor vector containing the combined data from both factors with unique levels.
#' @examples
#' # Example factor vectors
#' factor_vec1 <- factor(c('apple', 'banana', 'cherry'))
#' factor_vec2 <- factor(c('banana', 'date', 'fig', 'grape'))
#'
#' # Merge factors and retain unique levels
#' fct_merge(factor_vec1, factor_vec2)
#' @export
#' @author Kai Guo
fct_merge <- function(factor_vec1, factor_vec2, level_order = NULL) {
  #
  if(!is.factor(factor_vec1) || !is.factor(factor_vec2)){
    factor_vec1 <- as.factor(factor_vec1)
    factor_vec2 <- as.factor(factor_vec2)

  }
  if (!is.null(level_order) && !is.character(level_order)) {
    stop("The 'level_order' parameter must be a character vector or NULL.")
  }

  #
  combined_data <- c(as.character(factor_vec1), as.character(factor_vec2))

  #
  unique_levels <- unique(c(levels(factor_vec1), levels(factor_vec2)))

  #
  if (!is.null(level_order)) {
    #
    missing_levels <- setdiff(unique_levels, level_order)
    if (length(missing_levels) > 0) {
      stop("The 'level_order' must include all unique levels from both factors.")
    }
    unique_levels <- level_order
  }

  #
  new_factor <- factor(combined_data, levels = unique_levels)

  return(new_factor)
}

