#' @title Filter Factor Levels by Frequency and Recalculate Character Frequencies
#' @description Filters out factor levels that occur less than a specified frequency threshold and recalculates character frequencies excluding the removed levels. Offers options to handle NA values and returns additional information.
#' @param factor_vec A factor vector to be filtered.
#' @param min_freq A positive integer specifying the minimum frequency threshold. Factor levels occurring less than this number will be dropped.
#' @param na.rm Logical. Should NA values be removed before filtering and frequency calculation? Default is \code{FALSE}.
#' @param case Logical. Should the character frequency count be case-sensitive? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering of levels be decreasing by total character frequency? Default is \code{TRUE}.
#' @param return_info Logical. Should the function return additional information such as removed levels and character frequencies? Default is \code{FALSE}.
#' @return If \code{return_info} is \code{FALSE}, returns a factor vector with levels filtered by the specified frequency threshold and reordered based on recalculated total character frequency. If \code{return_info} is \code{TRUE}, returns a list containing the filtered factor vector, removed levels, and character frequency table.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'banana', 'apple', 'fig', NA))
#'
#' # Filter levels occurring less than 2 times and reorder by character frequency
#' fct_filter_freq(factor_vec, min_freq = 2)
#'
#' # Filter levels, remove NA values, and return additional information
#' result <- fct_filter_freq(factor_vec, min_freq = 2, na.rm = TRUE, return_info = TRUE)
#' result$filtered_factor
#' result$removed_levels
#' result$char_freq_table
#' @export
#' @author Kai Guo
fct_filter_freq <- function(factor_vec, min_freq = 1, na.rm = FALSE, case = FALSE, decreasing = TRUE, return_info = FALSE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.numeric(min_freq) || min_freq <= 0 || min_freq != as.integer(min_freq)) {
    stop("The 'min_freq' parameter must be a positive integer.")
  }
  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("The 'na.rm' parameter must be a single logical value.")
  }
  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }
  if (!is.logical(return_info) || length(return_info) != 1) {
    stop("The 'return_info' parameter must be a single logical value.")
  }

  # Handle NA values
  if (na.rm) {
    factor_vec <- factor_vec[!is.na(factor_vec)]
  }

  # Calculate frequency of each level
  level_counts <- table(factor_vec, useNA = "no")

  # Identify levels to keep
  levels_to_keep <- names(level_counts[level_counts >= min_freq])

  # Identify levels that were removed
  levels_removed <- setdiff(levels(factor_vec), levels_to_keep)

  # Filter the factor vector
  filtered_factor_vec <- factor_vec[factor_vec %in% levels_to_keep]

  # Drop unused levels
  filtered_factor_vec <- droplevels(filtered_factor_vec)

  # Proceed to calculate character frequencies using fct_freq logic
  factor_chars <- as.character(filtered_factor_vec)

  # Handle case sensitivity
  if (!case) {
    factor_chars <- tolower(factor_chars)
  }

  # Concatenate all strings and split into characters
  all_chars <- unlist(strsplit(factor_chars, split = ""))

  # Count frequency of each character
  char_freq_table <- table(all_chars)

  # For each level, sum the frequencies of its characters
  levels_vec <- levels(filtered_factor_vec)
  levels_vec_processed <- if (case) levels_vec else tolower(levels_vec)

  level_char_freq <- sapply(levels_vec_processed, function(level_str) {
    chars <- unlist(strsplit(level_str, split = ""))
    sum(char_freq_table[chars], na.rm = TRUE)
  })

  # Create a data frame to sort levels
  df_levels <- data.frame(
    level = levels(filtered_factor_vec),
    freq = level_char_freq,
    stringsAsFactors = FALSE
  )

  # Order the levels based on frequency
  df_levels <- df_levels[order(df_levels$freq, decreasing = decreasing), ]

  # Return factor with reordered levels
  final_factor_vec <- factor(filtered_factor_vec, levels = df_levels$level)

  if (return_info) {
    return(list(
      filtered_factor = final_factor_vec,
      removed_levels = levels_removed,
      char_freq_table = char_freq_table
    ))
  } else {
    return(final_factor_vec)
  }
}
#####
#' @title Remove Factor Levels with Specific Characters at Specified Positions
#' @description Removes factor levels where a specified character appears at specified positions within the levels.
#' @param factor_vec A factor vector from which levels will be removed.
#' @param positions A vector of positive integers indicating the character positions to check.
#' @param char A single character string specifying the character to look for.
#' @param case Logical. Should the character matching be case-sensitive? Default is \code{FALSE}.
#' @param remove_na remove NA from the output? Default is \code{TRUE}.
#' @param invert logical. If TRUE return indices or values for elements that do not match.
#' @param .return logical. If TRUE return TRUE or FALSE instead of element.
#' @return A factor vector with levels removed where the specified character appears at the specified positions.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'apricot', 'cherry', 'date', 'fig', 'grape'))
#'
#' # Remove levels where 'a' appears at position 1
#' fct_filter_pos(factor_vec, positions = 1, char = 'a')
#'
#' # Remove levels where 'e' appears at positions 2 or 3
#' fct_filter_pos(factor_vec, positions = c(2, 3), char = 'e')
#'
#' # Case-sensitive removal
#' factor_vec_case <- factor(c('Apple', 'banana', 'Apricot', 'Cherry', 'Date', 'Fig', 'grape'))
#' fct_filter_pos(factor_vec_case, positions = 1, char = 'A', case = TRUE)
#' @export
#' @author Kai Guo
fct_filter_pos <- function(factor_vec, positions = NULL, char, case = FALSE, remove_na = TRUE, invert = FALSE, .return = FALSE) {
  # Input validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }

  if (!is.null(positions)) {
    if (!is.numeric(positions) || any(positions <= 0) || any(positions != as.integer(positions))) {
      stop("The 'positions' parameter must be a vector of positive integers.")
    }
  }

  if (!is.character(char) || nchar(char) != 1) {
    stop("The 'char' parameter must be a single character string.")
  }

  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }

  if (!is.logical(remove_na) || length(remove_na) != 1) {
    stop("The 'remove_na' parameter must be a single logical value.")
  }

  # Get levels of the factor
  levels_vec <- levels(factor_vec)

  # Function to check if the character matches at the specified positions
  check_char_at_positions <- function(level_str) {
    str_length <- nchar(level_str)

    # If positions is NULL, check all positions
    if (is.null(positions)) {
      pos_list <- 1:str_length
    } else {
      pos_list <- positions[positions <= str_length]
      if (length(pos_list) == 0) {
        return(TRUE)
      }
    }

    for (pos in pos_list) {
      level_char <- substr(level_str, pos, pos)

      if (!case) {
        level_char <- tolower(level_char)
        target_char <- tolower(char)
      } else {
        target_char <- char
      }

      if (level_char == target_char) {
        return(FALSE)
      }
    }

    return(TRUE)
  }

  # Determine which levels to keep
  levels_to_keep_logical <- sapply(levels_vec, check_char_at_positions)
  if(isTRUE(invert)){
    levels_to_keep_logical <- - levels_to_keep_logical
  }
  levels_to_keep <- levels_vec[levels_to_keep_logical]
  # Create a new factor with the filtered levels
  filtered_factor_vec <- factor(factor_vec, levels = levels_to_keep)

  # Drop unused levels
  filtered_factor_vec <- droplevels(filtered_factor_vec)

  # Remove NA entries if remove_na is TRUE
  if (remove_na) {
    filtered_factor_vec <- filtered_factor_vec[!is.na(filtered_factor_vec)]
  }
  if(isTRUE(.return)){
    filtered_factor_vec <- levels_to_keep_logical
  }
  return(filtered_factor_vec)

}

####
#' @title Remove Specified Levels from a Factor
#' @description Removes specified levels from a factor vector, keeping the remaining levels and their order unchanged.
#' @param factor_vec A factor vector from which levels will be removed.
#' @param levels_to_remove A character vector of levels to be removed from the factor.
#' @param remove_na remove NA from the output? Default is \code{TRUE}.
#' @return A factor vector with specified levels removed and remaining levels unchanged.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'fig', 'grape'))
#'
#' # Remove levels 'banana' and 'date'
#' fct_remove_levels(factor_vec, levels_to_remove = c('banana', 'date'))
#' @export
#' @author Kai Guo
fct_remove_levels <- function(factor_vec, levels_to_remove, remove_na = TRUE) {
  #
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.character(levels_to_remove)) {
    stop("The 'levels_to_remove' parameter must be a character vector.")
  }

  #
  current_levels <- levels(factor_vec)

  #
  missing_levels <- setdiff(levels_to_remove, current_levels)
  if (length(missing_levels) > 0) {
    warning("The following levels are not present in the factor and will be ignored: ",
            paste(missing_levels, collapse = ", "))
  }

  #
  levels_to_keep <- setdiff(current_levels, levels_to_remove)

  #
  factor_vec <- factor(factor_vec, levels = levels_to_keep)

  #
  factor_vec <- droplevels(factor_vec)
  if (remove_na) {
    factor_vec <- factor_vec[!is.na(factor_vec)]
  }
  return(factor_vec)
}

