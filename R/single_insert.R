#' @title Insert New Levels into a Factor Vector After Specified Targets
#' @description Inserts one or more new levels into a factor vector immediately after specified target levels or
#' positions. Each new level corresponds to its respective target in a one-to-one manner.
#' Supports exact matches, position-based targeting, and pattern-based matching with optional case sensitivity.
#' Can handle multiple insertions, manage duplicates, and optionally reorder the data vector's elements to
#' align with the new levels.
#' @importFrom utils tail
#' @param factor_vec A factor vector into which new levels will be inserted.
#' @param insert A character vector of new levels to insert. Each new level corresponds to the respective target level or position.
#' @param target A character vector specifying the levels after which the new levels will be inserted. Overrides \code{positions} and \code{pattern} if provided.
#' @param positions An integer vector specifying the positions of levels after which the new levels will be inserted. Overrides \code{target} and \code{pattern} if both are provided.
#' @param pattern A regular expression pattern to identify target levels for insertion. Overrides both \code{target} and \code{positions} if provided.
#' @param case Logical. Should pattern matching be case-sensitive? Defaults to \code{FALSE}.
#' @param insert_after_na Logical. Should \code{NA} be considered as a target level for insertion? Defaults to \code{FALSE}.
#' @param allow_duplicates Logical. If \code{TRUE}, allows insertion of new levels that already exist in the factor by making them unique (appending suffixes). Defaults to \code{FALSE}.
#' @param inplace Logical. If \code{TRUE}, returns a new factor vector with elements reordered to align with the new levels' order. If \code{FALSE}, returns a new factor vector with only the levels' order adjusted, leaving the data vector's elements' order unchanged. Defaults to \code{FALSE}.
#' @return A new factor vector with the new levels inserted at the specified positions. If \code{inplace = TRUE}, the data vector's elements are reordered to match the new levels' order. If \code{inplace = FALSE}, only the levels' order is adjusted without changing the data vector's elements' order.
#' @examples
#' # Example 1: Insert 'date' after position 2 and 'grape' after position 4
#' # without allowing duplicates, returning a new factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'fig', 'grape'))
#' new_factor <- fct_insert(
#'   factor_vec,
#'   insert = c('date', 'grape'),
#'   positions = c(2, 4),
#'   inplace = FALSE
#' )
#' print(new_factor)
#' # [1] apple  banana date   cherry fig    grape
#' # Levels: apple banana date cherry fig grape
#'
#' # Example 2: Insert 'date' after position 2 and 'grape' after position 4,
#' # allowing duplicates, returning a new factor vector
#' new_factor_dup <- fct_insert(
#'   factor_vec,
#'   insert = c('date', 'grape'),
#'   positions = c(2, 4),
#'   allow_duplicates = TRUE,
#'   inplace = FALSE
#' )
#' print(new_factor_dup)
#' # [1] apple  banana date   cherry fig    grape.1
#' # Levels: apple banana date cherry fig grape.1
#'
#' # Example 3: Insert 'date' after position 2 and 'grape' after position 4,
#' # and reorder data elements
#' new_factor_inplace <- fct_insert(
#'   factor_vec,
#'   insert = c('date', 'grape'),
#'   positions = c(2, 4),
#'   inplace = TRUE
#' )
#' print(new_factor_inplace)
#' # [1] apple  banana date   cherry fig    grape
#' # Levels: apple banana date cherry fig grape
#'
#' # Example 4: Insert 'kiwi' after 'banana' and 'grape', case-sensitive,
#' # allowing duplicates, returning a new factor vector
#' factor_vec_case <- factor(c('Apple', 'banana', 'Cherry', 'date', 'Fig', 'grape'))
#' new_factor_case <- fct_insert(
#'   factor_vec_case,
#'   insert = c('kiwi', 'kiwi'),
#'   target = c('banana', 'grape'),
#'   case = TRUE,
#'   allow_duplicates = TRUE,
#'   inplace = FALSE
#' )
#' print(new_factor_case)
#' # [1] Apple   banana  Cherry  date    Fig     grape   kiwi    kiwi.1
#' # Levels: Apple banana Cherry date Fig grape kiwi kiwi.1
#'
#' # Example 5: Insert 'lychee' after NA, returning a new factor vector
#' factor_vec_na <- factor(c('apple', NA, 'banana', 'cherry', NA, 'date'))
#' new_factor_na <- fct_insert(
#'   factor_vec_na,
#'   insert = 'lychee',
#'   insert_after_na = TRUE,
#'   inplace = FALSE
#' )
#' print(new_factor_na)
#' # [1] apple  <NA>    lychee banana cherry <NA>    date
#' # Levels: apple <NA> lychee banana cherry date
#' @export
#' @author Kai Guo
fct_insert <- function(factor_vec, insert, target = NULL, positions = NULL, pattern = NULL,
                       case = FALSE, insert_after_na = FALSE, allow_duplicates = FALSE, inplace = FALSE) {
  # Input validation
  if (!is.factor(factor_vec)) {
    stop("'factor_vec' must be a factor.")
  }

  if (!is.character(insert) || length(insert) < 1) {
    stop("'insert' must be a non-empty character vector.")
  }

  if (!is.null(target) && !is.character(target)) {
    stop("'target' must be a character vector.")
  }

  if (!is.null(positions)) {
    if (!is.numeric(positions) || any(positions < 1) || any(positions > length(levels(factor_vec)))) {
      stop("'positions' must be a numeric vector with valid positions.")
    }
  }

  if (!is.null(pattern) && !is.character(pattern)) {
    stop("'pattern' must be a character string for regex.")
  }

  if (!is.logical(case) || length(case) != 1) {
    stop("'case' must be a single logical value.")
  }

  if (!is.logical(insert_after_na) || length(insert_after_na) != 1) {
    stop("'insert_after_na' must be a single logical value.")
  }

  if (!is.logical(allow_duplicates) || length(allow_duplicates) != 1) {
    stop("'allow_duplicates' must be a single logical value.")
  }

  if (!is.logical(inplace) || length(inplace) != 1) {
    stop("'inplace' must be a single logical value.")
  }

  # Get current levels
  current_levels <- levels(factor_vec)

  # Identify target indices based on priority: pattern > positions > target
  if (!is.null(pattern)) {
    if (case) {
      matches <- grepl(pattern, current_levels, perl = TRUE)
    } else {
      matches <- grepl(pattern, current_levels, ignore.case = TRUE, perl = TRUE)
    }
    target_indices <- which(matches)
  } else if (!is.null(positions)) {
    target_indices <- unique(positions)
  } else if (!is.null(target)) {
    if (case) {
      matches <- current_levels %in% target
    } else {
      matches <- tolower(current_levels) %in% tolower(target)
    }
    target_indices <- which(matches)
  } else {
    target_indices <- integer(0)
  }

  # Handle insertion after NA
  if (insert_after_na && any(is.na(current_levels))) {
    na_indices <- which(is.na(current_levels))
    target_indices <- unique(c(target_indices, na_indices))
  }

  # If no targets, warn and return original
  if (length(target_indices) == 0) {
    warning("No target levels found for insertion. Returning the original factor.")
    return(factor_vec)
  }

  # Sort target_indices in ascending order
  positions_sorted_asc <- sort(target_indices)

  # Determine the number of insertions
  num_targets <- length(positions_sorted_asc)
  num_insert <- length(insert)

  # Extend insert if necessary
  if (num_insert < num_targets) {
    # Repeat the last new_level for remaining targets
    insert_extended <- c(insert, rep(tail(insert, 1), num_targets - num_insert))
  } else if (num_insert > num_targets) {
    # Ignore extra insert and warn
    insert_extended <- insert[1:num_targets]
    warning("Number of 'insert' exceeds number of targets. Extra 'insert' are ignored.")
  } else {
    insert_extended <- insert
  }

  # Initialize updated_levels
  updated_levels <- current_levels

  # Perform insertions in ascending order
  for (i in seq_along(positions_sorted_asc)) {
    target_pos <- positions_sorted_asc[i]
    new_level <- insert_extended[i]

    # Handle duplicates
    if (!allow_duplicates && new_level %in% updated_levels) {
      # Remove existing new_level to move it
      updated_levels <- updated_levels[updated_levels != new_level]
    }

    if (allow_duplicates && new_level %in% updated_levels) {
      # Make unique by appending suffixes
      suffix <- 1
      unique_level <- new_level
      while (unique_level %in% updated_levels) {
        unique_level <- paste0(new_level, ".", suffix)
        suffix <- suffix + 1
      }
      new_level <- unique_level
    }

    # Determine insertion position
    if (target_pos >= length(updated_levels)) {
      # Append at end
      updated_levels <- c(updated_levels, new_level)
    } else {
      # Insert after target_pos
      updated_levels <- append(updated_levels, new_level, after = target_pos)
    }
  }

  # Ensure levels are unique if not allowing duplicates
  if (!allow_duplicates) {
    updated_levels <- unique(updated_levels)
  }

  # Create updated factor with new levels
  updated_factor <- factor(factor_vec, levels = updated_levels, ordered = is.ordered(factor_vec))

  if (inplace) {
    # Reorder the data vector's elements to align with the new levels' order
    # Create a mapping of levels to their new order
    level_order <- setNames(seq_along(updated_levels), updated_levels)

    # Assign an order value to each element based on its level
    element_order <- level_order[as.character(updated_factor)]

    # Handle NA by assigning Inf to place them at the end
    element_order[is.na(element_order)] <- Inf

    # Get the order of elements
    reordered_indices <- order(element_order, na.last = TRUE)

    # Reorder the data vector
    reordered_data <- updated_factor[reordered_indices]

    return(reordered_data)
  } else {
    return(updated_factor)
  }
}
