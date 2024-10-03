#' @title Insert New Levels into a Factor Vector at Specified Positions
#' @description Inserts one or more new levels into a factor vector immediately before or after specified target levels or
#' positions. Each new level corresponds to its respective target level or position. If \code{positions} or \code{target}
#' has a single element, it will be repeated to match the length of \code{insert}.
#' Supports exact matches, position-based targeting, and pattern-based matching with optional case sensitivity.
#' Can handle multiple insertions, manage duplicates, and optionally reorder the data vector's elements to
#' align with the new levels.
#'
#' If any position in \code{positions} exceeds the number of levels in \code{factor_vec}, the new level(s) will be
#' appended at the end.
#' @importFrom utils tail
#' @param factor_vec A factor vector into which new levels will be inserted.
#' @param insert A character vector of new levels to insert. Each new level corresponds to the respective target level or position.
#' @param target A character vector specifying the levels before or after which the new levels will be inserted. If length 1 and \code{length(insert) > 1}, it will be repeated to match the length of \code{insert}. Overrides \code{positions} and \code{pattern} if provided.
#' @param positions An integer vector specifying the positions of levels before or after which the new levels will be inserted. If length 1 and \code{length(insert) > 1}, it will be repeated to match the length of \code{insert}. If any position exceeds the number of levels, the new level(s) will be appended at the end. Overrides \code{target} and \code{pattern} if both are provided.
#' @param pattern A regular expression pattern to identify target levels for insertion. Overrides both \code{target} and \code{positions} if provided.
#' @param case Logical. Should pattern matching be case-sensitive? Defaults to \code{FALSE}.
#' @param insert_after_na Logical. Should \code{NA} be considered as a target level for insertion? Defaults to \code{FALSE}.
#' @param allow_duplicates Logical. If \code{TRUE}, allows insertion of new levels that already exist in the factor by making them unique (appending suffixes). Defaults to \code{FALSE}.
#' @param position Character. Where to insert the new levels relative to the target: \code{"after"} or \code{"before"}. Defaults to \code{"after"}.
#' @param inplace Logical. If \code{TRUE}, returns a new factor vector with elements reordered to align with the new levels' order. If \code{FALSE}, returns a new factor vector with only the levels' order adjusted, leaving the data vector's elements' order unchanged. Defaults to \code{FALSE}.
#' @return A new factor vector with the new levels inserted at the specified positions. If \code{inplace = TRUE}, the data vector's elements are reordered to match the new levels' order. If \code{inplace = FALSE}, only the levels' order is adjusted without changing the data vector's elements' order.
#' @examples
#' # Example 1: Insert 'date' after position 2 and 'grape' after position 4
#' # without allowing duplicates, returning a new factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'fig', 'grape'))
#' new_factor <- ft_insert(
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
#' new_factor_dup <- ft_insert(
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
#' new_factor_inplace <- ft_insert(
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
#' new_factor_case <- ft_insert(
#'   factor_vec_case,
#'   insert = c('kiwi', 'kiwi'),
#'   target = c('banana', 'grape'),
#'   case = TRUE,
#'   allow_duplicates = TRUE,
#'   inplace = FALSE
#' )
#' print(new_factor_case)
#' # [1] Apple   banana  Cherry  date    Fig     grape   kiwi    kiwi.1
#'
#' # Example 5: Insert 'date' and 'elderberry' after position 2
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'fig', 'grape'))
#' new_factor <- ft_insert(
#'   factor_vec,
#'   insert = c('date', 'elderberry'),
#'   positions = 2,
#'   position = "after",
#'   inplace = FALSE
#' )
#' print(levels(new_factor))
#' # [1] "apple"      "banana"     "date"       "elderberry" "cherry"     "fig"        "grape"
#'
#' # Example 6: Insert 'kiwi' at position exceeding the number of levels
#' new_factor_exceed <- ft_insert(
#'   factor_vec,
#'   insert = 'kiwi',
#'   positions = 10,  # Position exceeds number of levels
#'   position = "after",
#'   inplace = FALSE
#' )
#' print(levels(new_factor_exceed))
#' # [1] "apple" "banana" "cherry" "fig" "grape" "kiwi"
#'
#' # Example 7: Insert multiple levels with positions exceeding the number of levels
#' new_factor_multi_exceed <- ft_insert(
#'   factor_vec,
#'   insert = c('lemon', 'mango'),
#'   positions = c(5, 10),  # Second position exceeds number of levels
#'   position = "after",
#'   inplace = FALSE
#' )
#' print(levels(new_factor_multi_exceed))
#' # [1] "apple" "banana" "cherry" "fig" "grape" "lemon" "mango"
#' # Example 8: Insert multiple levels after a single position (positions repeated)
#' new_factor_repeat <- ft_insert(
#'   factor_vec,
#'   insert = c('kiwi', 'lemon', 'mango'),
#'   positions = 2,
#'   position = "after",
#'   inplace = FALSE
#' )
#' print(levels(new_factor_repeat))
#' # [1] "apple" "banana" "kiwi" "lemon" "mango" "cherry" "fig" "grape"
#'
#' # Example 9: Insert multiple levels before a single target (target repeated)
#' new_factor_target_repeat <- ft_insert(
#'   factor_vec,
#'   insert = c('kiwi', 'lemon', 'mango'),
#'   target = 'cherry',
#'   position = "before",
#'   inplace = FALSE
#' )
#' print(levels(new_factor_target_repeat))
#' # [1] "apple" "banana" "kiwi" "lemon" "mango" "cherry" "fig" "grape"
#' @export
#' @author Kai Guo
ft_insert <- function(factor_vec, insert, target = NULL, positions = NULL, pattern = NULL,
                      case = FALSE, insert_after_na = FALSE, allow_duplicates = FALSE,
                      position = "after", inplace = FALSE) {
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
    if (!is.numeric(positions) || any(positions < 1)) {
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

  if (!position %in% c("after", "before")) {
    stop("'position' must be either 'after' or 'before'.")
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
    target_indices <- positions
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

  # If positions or target has length 1 and insert has length >1, repeat positions or target
  if (length(target_indices) == 1 && length(insert) > 1) {
    target_indices <- rep(target_indices, length(insert))
  }

  # Sort target_indices in ascending order
  target_indices <- sort(target_indices)

  # Determine the number of insertions
  num_targets <- length(target_indices)
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

  # Adjust insertion positions based on 'position' argument
  insertion_offsets <- if (position == "after") rep(0, length(target_indices)) else rep(-1, length(target_indices))

  # Adjust target positions exceeding the number of levels
  max_level_pos <- length(updated_levels)
  target_indices[target_indices > max_level_pos] <- max_level_pos

  # Perform insertions
  for (i in seq_along(target_indices)) {
    target_pos <- target_indices[i] + insertion_offsets[i]
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

    # Ensure target_pos is within valid range
    target_pos <- max(0, min(target_pos, length(updated_levels)))

    # Insert new level at the correct position
    updated_levels <- append(updated_levels, new_level, after = target_pos)
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
