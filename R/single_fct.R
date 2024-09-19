#' @title Reorder Factor Levels Based on Characters at Specific Positions
#' @description Reorders the levels of a factor vector based on the characters at specified positions within the factor levels.
#' @importFrom stringr str_length str_sub str_to_lower
#' @param factor_vec A factor vector whose levels are to be reordered.
#' @param positions A vector of positive integers specifying the character positions to use for reordering. Positions can be non-consecutive.
#' @param case Logical. Should the character comparison be case-sensitive? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering be decreasing? Default is \code{FALSE}.
#' @return A factor vector with levels reordered based on the specified character positions.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('Apple', 'banana', 'Cherry', 'date', 'Fig', 'grape'))
#'
#' # Reorder based on positions 1 and 3, case-insensitive
#' fct_pos(factor_vec, positions = c(1, 3))
#'
#' # Reorder in decreasing order, case-sensitive
#' fct_pos(factor_vec, positions = 1:2, case = TRUE, decreasing = TRUE)
#' @export
#' @author Kai Guo
fct_pos <- function(factor_vec, positions, case = FALSE, decreasing = FALSE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.numeric(positions) || any(positions <= 0) || any(positions != as.integer(positions))) {
    stop("The 'positions' parameter must be a vector of positive integers.")
  }

  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }

  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Get factor levels
  levels_vec <- levels(factor_vec)

  # Extract characters at specified positions
  extract_chars <- function(x, positions) {
    chars <- sapply(positions, function(pos) {
      if (str_length(x) >= pos) {
        str_sub(x, pos, pos)
      } else {
        NA  # Return NA if string is too short
      }
    })
    # Combine extracted characters into a single string
    paste(chars, collapse = "")
  }

  # Apply extraction to each level
  chars_at_positions <- sapply(levels_vec, extract_chars, positions = positions)

  # Handle case sensitivity
  if (!case) {
    chars_at_positions <- str_to_lower(chars_at_positions)
  }

  # Original order for stable sorting
  original_order <- seq_along(levels_vec)

  # Order levels
  levels_ordered <- levels_vec[order(chars_at_positions, original_order, decreasing = decreasing, na.last = TRUE)]

  # Return factor with reordered levels
  factor(factor_vec, levels = levels_ordered)
}
###
#' @title Reorder Factor Levels by Level Count
#' @description Reorders the levels of a factor vector based on the count of each level in the data.
#' @param factor_vec A factor vector whose levels are to be reordered.
#' @param decreasing Logical. Should the ordering be decreasing by count? Default is \code{TRUE}.
#' @return A factor vector with levels reordered based on their count.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'apple', 'cherry', 'banana', 'banana', 'date'))
#'
#' # Reorder levels by decreasing count
#' fct_count(factor_vec)
#'
#' # Reorder levels by increasing count
#' fct_count(factor_vec, decreasing = FALSE)
#' @export
#' @author Kai Guo
fct_count <- function(factor_vec, decreasing = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Calculate count of each level
  count_table <- sort(table(factor_vec), decreasing = decreasing)

  # Get ordered levels
  levels_ordered <- names(count_table)

  # Return factor with reordered levels
  factor(factor_vec, levels = levels_ordered)
}
####
#' @title Reorder Factor Levels Based on Substrings
#' @description Reorders the levels of a factor vector based on substrings extracted from the factor levels.
#' @param factor_vec A factor vector whose levels are to be reordered.
#' @param start_pos Positive integer. The starting position of the substring. If \code{NULL}, starts from the beginning.
#' @param end_pos Positive integer. The ending position of the substring. If \code{NULL}, goes to the end of the string.
#' @param case Logical. Should the substring comparison be case-sensitive? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering be decreasing? Default is \code{FALSE}.
#' @return A factor vector with levels reordered based on the specified substring.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('Apple', 'banana', 'Cherry', 'date', 'Fig', 'grape'))
#'
#' # Reorder based on substring from position 2 to 4
#' fct_sub(factor_vec, start_pos = 2, end_pos = 4)
#'
#' # Reorder from position 3 to end, case-sensitive
#' fct_sub(factor_vec, start_pos = 3, case = TRUE)
#' @export
#' @author Kai Guo
fct_sub <- function(factor_vec, start_pos = NULL, end_pos = NULL, case = FALSE, decreasing = FALSE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.null(start_pos) && (!is.numeric(start_pos) || start_pos <= 0 || start_pos != as.integer(start_pos))) {
    stop("The 'start_pos' parameter must be a positive integer.")
  }

  if (!is.null(end_pos) && (!is.numeric(end_pos) || end_pos <= 0 || end_pos != as.integer(end_pos))) {
    stop("The 'end_pos' parameter must be a positive integer.")
  }

  if (is.null(start_pos) && is.null(end_pos)) {
    stop("At least one of 'start_pos' or 'end_pos' must be provided.")
  }

  if (!is.null(start_pos) && !is.null(end_pos) && start_pos > end_pos) {
    stop("The 'start_pos' must be less than or equal to 'end_pos'.")
  }

  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }

  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Get factor levels
  levels_vec <- levels(factor_vec)

  # Extract substrings
  extract_substring <- function(x) {
    str_length <- str_length(x)

    # Determine actual start and end positions
    actual_start <- ifelse(is.null(start_pos), 1, start_pos)
    actual_end <- ifelse(is.null(end_pos), str_length, end_pos)

    if (str_length >= actual_start) {
      substr <- str_sub(x, actual_start, min(actual_end, str_length))
      substr
    } else {
      NA  # Return NA if string is too short
    }
  }

  # Apply extraction to each level
  substrings <- sapply(levels_vec, extract_substring)

  # Handle case sensitivity
  if (!case) {
    substrings <- str_to_lower(substrings)
  }

  # Original order for stable sorting
  original_order <- seq_along(levels_vec)

  # Order levels
  levels_ordered <- levels_vec[order(substrings, original_order, decreasing = decreasing, na.last = TRUE)]

  # Return factor with reordered levels
  factor(factor_vec, levels = levels_ordered)
}
####
#' @title Reorder Factor Levels Based on Total Character Frequency
#' @description Reorders the levels of a factor vector based on the total frequency of characters appearing in the vector.
#' @param factor_vec A factor vector whose levels are to be reordered.
#' @param case Logical. Should the character frequency count be case-sensitive? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering be decreasing by total character frequency? Default is \code{TRUE}.
#' @return A factor vector with levels reordered based on the total frequency of their constituent characters in the data.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'banana', 'apple', 'fig'))
#'
#' # Reorder levels based on total character frequency
#' fct_freq(factor_vec)
#'
#' # Reorder levels, case-sensitive
#' factor_vec_case <- factor(c('Apple', 'banana', 'Cherry', 'date', 'banana', 'apple', 'Fig'))
#' fct_freq(factor_vec_case, case = TRUE)
#' @export
#' @author Kai Guo
fct_freq <- function(factor_vec, case = FALSE, decreasing = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Convert factor levels to characters
  factor_chars <- as.character(factor_vec)

  # Handle case sensitivity
  if (!case) {
    factor_chars <- tolower(factor_chars)
  }

  # Concatenate all strings and split into characters
  all_chars <- unlist(strsplit(factor_chars, split = ""))

  # Count frequency of each character
  char_freq_table <- table(all_chars)

  # For each level, sum the frequencies of its characters
  levels_vec <- levels(factor_vec)
  levels_vec_processed <- if (case) levels_vec else tolower(levels_vec)

  level_char_freq <- sapply(levels_vec_processed, function(level_str) {
    chars <- unlist(strsplit(level_str, split = ""))
    sum(char_freq_table[chars], na.rm = TRUE)
  })

  # Create a data frame to sort levels
  df_levels <- data.frame(
    level = levels(factor_vec),
    freq = level_char_freq,
    stringsAsFactors = FALSE
  )

  # Order the levels based on frequency
  df_levels <- df_levels[order(df_levels$freq, decreasing = decreasing), ]

  # Return factor with reordered levels
  factor(factor_vec, levels = df_levels$level)
}

####
#' @title Reorder Factor Levels Based on Character Frequency at Positions
#' @description Reorders the levels of a factor vector based on the frequency of characters at specified positions within the data.
#' @importFrom stringr str_length str_sub str_to_lower
#' @param factor_vec A factor vector whose levels are to be reordered.
#' @param positions A vector of positive integers specifying the character positions to consider.
#' @param case Logical. Should the character comparison be case-sensitive? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering be decreasing by frequency? Default is \code{TRUE}.
#' @return A factor vector with levels reordered based on the frequency of characters at specified positions.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'apricot', 'cherry', 'banana', 'banana', 'date'))
#'
#' # Reorder based on characters at positions 1 and 2
#' fct_char_freq(factor_vec, positions = 1:2)
#'
#' # Reorder, case-sensitive, decreasing order
#' fct_char_freq(factor_vec, positions = c(1, 3), case = TRUE)
#' @export
#' @author Kai Guo
fct_char_freq <- function(factor_vec, positions, case = FALSE, decreasing = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.numeric(positions) || any(positions <= 0) || any(positions != as.integer(positions))) {
    stop("The 'positions' parameter must be a vector of positive integers.")
  }
  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Extract characters at specified positions and combine
  extract_chars <- function(x) {
    chars <- sapply(positions, function(pos) {
      if (str_length(x) >= pos) {
        str_sub(x, pos, pos)
      } else {
        NA
      }
    })
    paste(chars, collapse = "")
  }
  chars_at_positions <- sapply(as.character(factor_vec), extract_chars)

  # Handle case sensitivity
  if (!case) {
    chars_at_positions <- str_to_lower(chars_at_positions)
  }

  # Calculate frequency of character combinations
  char_freq_table <- table(chars_at_positions, useNA = "no")

  # Extract for levels
  level_chars <- sapply(levels(factor_vec), extract_chars)
  if (!case) {
    level_chars <- str_to_lower(level_chars)
  }

  # Get frequencies for levels
  level_char_freq <- sapply(level_chars, function(char) {
    if (!is.na(char)) {
      if (char %in% names(char_freq_table)) {
        char_freq_table[char]
      } else {
        0
      }
    } else {
      0
    }
  })

  # Create dataframe and order
  df_levels <- data.frame(
    level = levels(factor_vec),
    chars = level_chars,
    freq = as.numeric(level_char_freq),
    stringsAsFactors = FALSE
  )

  df_levels <- df_levels[order(df_levels$freq, df_levels$level, decreasing = decreasing), ]

  # Return factor with reordered levels
  factor(factor_vec, levels = df_levels$level)
}
###
#' @title Reorder Factor Levels Based on Substring Frequency
#' @description Reorders the levels of a factor vector based on the frequency of substrings extracted from the data.
#' @importFrom stringr str_length str_sub str_to_lower
#' @param factor_vec A factor vector whose levels are to be reordered.
#' @param start_pos Positive integer. The starting position of the substring. If \code{NULL}, starts from the beginning.
#' @param end_pos Positive integer. The ending position of the substring. If \code{NULL}, goes to the end of the string.
#' @param case Logical. Should the substring comparison be case-sensitive? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering be decreasing by frequency? Default is \code{TRUE}.
#' @return A factor vector with levels reordered based on the frequency of substrings.
#' @examples
#' # Example factor vector with multi-byte characters
#' factor_vec <- factor(c('苹果', '香蕉', '苹果', '樱桃', '香蕉', '香蕉', '日期'))
#'
#' # Reorder based on substrings from position 1 to 1
#' fct_substr_freq(factor_vec, start_pos = 1, end_pos = 1)
#'
#' # Reorder from position 2 to end
#' fct_substr_freq(factor_vec, start_pos = 2)
#' factor_vec <- factor(c('apple', 'banana', 'apricot', 'cherry', 'banana', 'banana', 'date'))
#' fct_substr_freq(factor_vec, start_pos = 2, end_pos=3)
#' @export
#' @author Kai Guo
fct_substr_freq <- function(factor_vec, start_pos = NULL, end_pos = NULL, case = FALSE, decreasing = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (is.null(start_pos) && is.null(end_pos)) {
    stop("At least one of 'start_pos' or 'end_pos' must be provided.")
  }
  if (!is.null(start_pos) && (!is.numeric(start_pos) || start_pos <= 0 || start_pos != as.integer(start_pos))) {
    stop("The 'start_pos' parameter must be a positive integer.")
  }
  if (!is.null(end_pos) && (!is.numeric(end_pos) || end_pos <= 0 || end_pos != as.integer(end_pos))) {
    stop("The 'end_pos' parameter must be a positive integer.")
  }
  if (!is.null(start_pos) && !is.null(end_pos) && start_pos > end_pos) {
    stop("The 'start_pos' must be less than or equal to 'end_pos'.")
  }
  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Extract substrings
  extract_substring <- function(x) {
    str_length <- str_length(x)

    actual_start <- ifelse(is.null(start_pos), 1, start_pos)
    actual_end <- ifelse(is.null(end_pos), str_length, end_pos)

    if (str_length >= actual_start) {
      substr <- str_sub(x, actual_start, actual_end)
      substr
    } else {
      NA
    }
  }
  factor_chars <- as.character(factor_vec)
  substrings <- sapply(factor_chars, extract_substring)

  # Handle case sensitivity
  if (!case) {
    substrings <- str_to_lower(substrings)
  }

  # Calculate frequency
  substr_freq_table <- table(substrings, useNA = "no")

  # Extract for levels
  levels_vec <- levels(factor_vec)
  level_substrings <- sapply(levels_vec, extract_substring)
  if (!case) {
    level_substrings <- str_to_lower(level_substrings)
  }

  level_substr_freq <- sapply(level_substrings, function(substr) {
    if (!is.na(substr)) {
      if (substr %in% names(substr_freq_table)) {
        substr_freq_table[substr]
      } else {
        0
      }
    } else {
      0
    }
  })

  # Create dataframe and order
  df_levels <- data.frame(
    level = levels_vec,
    substring = level_substrings,
    freq = as.numeric(level_substr_freq),
    stringsAsFactors = FALSE
  )

  df_levels <- df_levels[order(df_levels$freq, df_levels$level, decreasing = decreasing), ]

  # Return factor with reordered levels
  factor(factor_vec, levels = df_levels$level)
}
####
#' @title Reorder Factor Levels Based on Regex Pattern Frequency
#' @description Reorders the levels of a factor vector based on the frequency of substrings matching a regular expression.
#' @importFrom stringr regex str_extract
#' @param factor_vec A factor vector whose levels are to be reordered.
#' @param pattern A string representing the regular expression pattern to match.
#' @param case Logical. Should the pattern matching be case-sensitive? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering be decreasing by frequency? Default is \code{TRUE}.
#' @return A factor vector with levels reordered based on the frequency of matched substrings.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'apricot', 'cherry', 'blueberry', 'blackberry', 'date'))
#'
#' # Reorder based on pattern matching 'a'
#' fct_regex_freq(factor_vec, pattern = 'a')
#'
#' # Reorder with case-sensitive matching
#' fct_regex_freq(factor_vec, pattern = '^[A-Z]', case = TRUE)
#' @export
#' @author Kai Guo
fct_regex_freq <- function(factor_vec, pattern, case = FALSE, decreasing = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("The 'pattern' parameter must be a single string representing a regular expression.")
  }
  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Prepare pattern
  regex_pattern <- if (case) {
    pattern
  } else {
    regex(pattern, ignore_case = TRUE)
  }

  # Extract substrings matching pattern
  factor_chars <- as.character(factor_vec)
  substrings <- str_extract(factor_chars, regex_pattern)

  # Calculate frequency
  substr_freq_table <- table(substrings, useNA = "no")

  # Extract for levels
  levels_vec <- levels(factor_vec)
  level_substrings <- str_extract(levels_vec, regex_pattern)

  level_substr_freq <- sapply(level_substrings, function(substr) {
    if (!is.na(substr)) {
      if (substr %in% names(substr_freq_table)) {
        substr_freq_table[substr]
      } else {
        0
      }
    } else {
      0
    }
  })

  # Create dataframe and order
  df_levels <- data.frame(
    level = levels_vec,
    substring = level_substrings,
    freq = as.numeric(level_substr_freq),
    stringsAsFactors = FALSE
  )

  df_levels <- df_levels[order(df_levels$freq, df_levels$level, decreasing = decreasing), ]

  # Return factor with reordered levels
  factor(factor_vec, levels = df_levels$level)
}
####
#' @title Handle NA Values in Factor Vectors
#' @description Handles NA values in a factor vector by either keeping NA as a level or removing levels and characters corresponding to NA values.
#' @param factor_vec A factor vector to be processed.
#' @param keep_na Logical. Should NA values be kept as a level in the factor? Default is \code{TRUE}.
#' @return A factor vector with NA values handled as specified.
#' @examples
#' # Example factor vector with NA values
#' factor_vec <- factor(c('apple', NA, 'banana', 'cherry', NA, 'date'))
#'
#' # Keep NA as a level
#' fct_na(factor_vec, keep_na = TRUE)
#'
#' # Remove NA values
#' fct_na(factor_vec, keep_na = FALSE)
#' @export
#' @author Kai Guo
fct_na <- function(factor_vec, keep_na = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.logical(keep_na) || length(keep_na) != 1) {
    stop("The 'keep_na' parameter must be a single logical value.")
  }

  if (keep_na) {
    # Ensure NA is a level
    if (!any(is.na(levels(factor_vec)))) {
      levels(factor_vec) <- c(levels(factor_vec), NA)
    }
  } else {
    # Remove NA values
    factor_vec <- factor_vec[!is.na(factor_vec)]
    factor_vec <- droplevels(factor_vec)
  }

  return(factor_vec)
}
####
#' @title Count Character Frequencies in Factor Levels (Including NA Handling)
#' @description Counts the frequency of each character appearing in the levels of a factor vector, optionally including NA values, and returns a table or vector.
#' @param factor_vec A factor vector whose levels will be analyzed.
#' @param case Logical. Should the character count be case-sensitive? Default is \code{FALSE}.
#' @param include_na Logical. Should NA levels be included in the character count? Default is \code{FALSE}.
#' @param as_table Logical. Should the result be returned as a table? If \code{FALSE}, a named vector is returned. Default is \code{TRUE}.
#' @return A table or named vector of character frequencies.
#' @examples
#' # Example factor vector with NA levels
#' factor_vec <- factor(c('apple', 'banana', NA, 'cherry', 'date', NA, 'fig', 'grape'), exclude = NULL)
#'
#' # Get character frequencies (case-insensitive), excluding NA levels
#' fct_table(factor_vec)
#'
#' # Include NA levels in the character frequencies
#' fct_table(factor_vec, include_na = TRUE)
#' @export
#' @author Kai Guo
fct_table <- function(factor_vec, case = FALSE, include_na = FALSE, as_table = TRUE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.logical(case) || length(case) != 1) {
    stop("The 'case' parameter must be a single logical value.")
  }
  if (!is.logical(include_na) || length(include_na) != 1) {
    stop("The 'include_na' parameter must be a single logical value.")
  }
  if (!is.logical(as_table) || length(as_table) != 1) {
    stop("The 'as_table' parameter must be a single logical value.")
  }

  # Get factor levels
  levels_vec <- levels(factor_vec)

  # Handle NA levels
  if (include_na && any(is.na(levels_vec))) {
    levels_vec[is.na(levels_vec)] <- "NA"
  } else {
    levels_vec <- levels_vec[!is.na(levels_vec)]
  }

  # Handle case sensitivity
  if (!case) {
    levels_vec <- tolower(levels_vec)
  }

  # Split levels into characters and combine
  all_chars <- unlist(strsplit(levels_vec, split = ""))

  # Count character frequencies
  char_freq <- table(all_chars)

  # Return as table or vector
  if (as_table) {
    return(char_freq)
  } else {
    freq_vector <- as.vector(char_freq)
    names(freq_vector) <- names(char_freq)
    return(freq_vector)
  }
}
#' @title Group Factor Levels by Common Prefix
#' @description Groups factor levels by a common prefix of specified length.
#' @param factor_vec A factor vector to be grouped.
#' @param prefix_length An integer specifying the number of characters in the prefix.
#' @return A factor vector with levels grouped by the common prefix.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple_red', 'apple_green', 'banana_yellow', 'banana_green', 'cherry_red'))
#'
#' # Group by first 5 characters (common prefix)
#' fct_group_by_prefix(factor_vec, prefix_length = 5)
#' @export
#' @author Kai Guo
fct_group_by_prefix <- function(factor_vec, prefix_length) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.numeric(prefix_length) || prefix_length <= 0 || prefix_length != as.integer(prefix_length)) {
    stop("The 'prefix_length' must be a positive integer.")
  }

  # Extract prefixes
  prefixes <- substr(levels(factor_vec), 1, prefix_length)

  # Create mapping of levels to prefixes
  mapping <- data.frame(
    original = levels(factor_vec),
    group = prefixes,
    stringsAsFactors = FALSE
  )

  # Update levels to be the prefix groups
  new_levels <- mapping$group
  factor_vec_grouped <- factor(factor_vec, levels = mapping$original, labels = new_levels)

  return(factor_vec_grouped)
}

#' @title Split Factor Levels into Multiple Factors
#' @description Splits the levels of a factor vector into multiple factors based on a specified delimiter.
#' @param factor_vec A factor vector to split.
#' @param delimiter A character string used to split the factor levels.
#' @param names A character vector specifying names for the resulting factors. Default is \code{NULL}, in which case factors are named 'Factor1', 'Factor2', etc.
#' @return A data frame containing the resulting factors.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('red_large', 'blue_small', 'green_medium'))
#'
#' # Split levels into two factors
#' fct_split_levels(factor_vec, delimiter = '_')
#' @export
#' @author Kai Guo
fct_split_levels <- function(factor_vec, delimiter, names = NULL) {
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.character(delimiter) || length(delimiter) != 1) {
    stop("The 'delimiter' must be a single character string.")
  }

  # Split levels
  split_data <- strsplit(as.character(factor_vec), split = delimiter)
  max_parts <- max(sapply(split_data, length))

  # Create data frame
  split_matrix <- do.call(rbind, lapply(split_data, function(x) {
    length(x) <- max_parts
    return(x)
  }))

  # Create factor columns
  result_df <- as.data.frame(split_matrix, stringsAsFactors = TRUE)

  # Assign names
  if (is.null(names)) {
    colnames(result_df) <- paste0('Factor', seq_len(ncol(result_df)))
  } else {
    if (length(names) != ncol(result_df)) {
      stop("The length of 'names' must match the number of resulting factors.")
    }
    colnames(result_df) <- names
  }

  return(result_df)
}

