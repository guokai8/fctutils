#' @title Get Intersection of Factor Levels from Multiple Vectors
#' @description Combines multiple factor vectors and returns a factor vector containing only the levels common to all.
#' @param ... Factor vectors to be intersected.
#' @return A factor vector containing the intersection of levels from all provided factors.
#' @examples
#' # Example factor vectors
#' factor_vec1 <- factor(c('apple', 'banana', 'cherry'))
#' factor_vec2 <- factor(c('banana', 'date', 'cherry'))
#' factor_vec3 <- factor(c('banana', 'cherry', 'fig'))
#'
#' # Get intersection of levels
#' fct_intersect(factor_vec1, factor_vec2, factor_vec3)
#' @export
#' @author Kai Guo
fct_intersect <- function(...) {
  factors <- list(...)
  # Parameter validation
  if (length(factors) < 2) {
    stop("At least two factor vectors must be provided.")
  }
  if (!all(sapply(factors, is.factor))) {
    stop("All inputs must be factor vectors.")
  }

  # Get intersection of levels
  common_levels <- Reduce(intersect, lapply(factors, levels))

  # Combine data
  combined_data <- unlist(lapply(factors, as.character))

  # Filter combined data to include only common levels
  filtered_data <- combined_data[combined_data %in% common_levels]

  # Create factor with common levels
  result_factor <- factor(filtered_data, levels = common_levels)

  return(result_factor)
}

#' @title Get Union of Factor Levels from Multiple Vectors
#' @description Combines multiple factor vectors and returns a factor vector containing all unique levels.
#' @param ... Factor vectors to be united.
#' @return A factor vector containing all unique levels from all provided factors.
#' @examples
#' # Example factor vectors
#' factor_vec1 <- factor(c('apple', 'banana'))
#' factor_vec2 <- factor(c('banana', 'cherry'))
#' factor_vec3 <- factor(c('date', 'fig'))
#'
#' # Get union of levels
#' fct_union(factor_vec1, factor_vec2, factor_vec3)
#' @export
#' @author Kai Guo
fct_union <- function(...) {
  factors <- list(...)
  if (length(factors) < 2) {
    stop("At least two factor vectors must be provided.")
  }
  if (!all(sapply(factors, is.factor))) {
    stop("All inputs must be factor vectors.")
  }

  # Get union of levels
  all_levels <- unique(unlist(lapply(factors, levels)))

  # Combine data
  combined_data <- unlist(lapply(factors, as.character))

  # Create factor with all levels
  result_factor <- factor(combined_data, levels = all_levels)

  return(result_factor)
}

#' @title Reorder Factor Levels Within Groups
#' @description Reorders the levels of a factor vector within groups defined by another factor vector.
#' @param factor_vec A factor vector to be reordered.
#' @param group_vec A factor vector defining the groups.
#' @param by A numeric vector to order by.
#' @param fun A function to summarize within groups (e.g., mean, median).
#' @param decreasing Logical. Should the ordering be decreasing? Default is \code{FALSE}.
#' @return A factor vector with levels reordered within groups.
#' @examples
#' # Example data
#' data <- data.frame(
#'   item = factor(c('A', 'B', 'C', 'D', 'E', 'F')),
#'   group = factor(c('G1', 'G1', 'G1', 'G2', 'G2', 'G2')),
#'   value = c(10, 15, 5, 20, 25, 15)
#' )
#'
#' # Reorder 'item' within 'group' by 'value'
#' data$item <- fct_reorder_within(data$item, data$group, data$value, mean)
#' @export
#' @author Kai Guo
fct_reorder_within <- function(factor_vec, group_vec, by, fun = mean, decreasing = FALSE) {
  # Parameter validation
  if (!is.factor(factor_vec) || !is.factor(group_vec)) {
    stop("Both 'factor_vec' and 'group_vec' must be factor vectors.")
  }
  if (length(factor_vec) != length(group_vec) || length(factor_vec) != length(by)) {
    stop("All input vectors must be of the same length.")
  }
  if (!is.function(fun)) {
    stop("The 'fun' parameter must be a function.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Compute summary statistics within groups
  stats <- tapply(by, list(factor_vec, group_vec), FUN = fun, simplify = TRUE)

  # Flatten the stats table and create a data frame
  stats_df <- as.data.frame(as.table(stats), stringsAsFactors = FALSE)
  colnames(stats_df) <- c("factor_level", "group_level", "stat")

  # Order within groups
  stats_df <- stats_df[order(stats_df$group_level, stats_df$stat, decreasing = decreasing), ]

  # Create a new levels vector
  new_levels <- unique(stats_df$factor_level)

  # Reorder factor levels
  factor_vec_reordered <- factor(factor_vec, levels = new_levels)

  return(factor_vec_reordered)
}


#' @title Reverse Factor Levels
#' @description Reverses the order of the levels in a factor vector.
#' @param factor_vec A factor vector whose levels will be reversed.
#' @return A factor vector with levels in reversed order.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('low', 'medium', 'high'))
#'
#' # Reverse the levels
#' fct_reverse(factor_vec)
#' @export
#' @author Kai Guo
fct_reverse <- function(factor_vec) {
  # Parameter validation
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }

  # Reverse levels
  reversed_levels <- rev(levels(factor_vec))

  # Create new factor with reversed levels
  factor_vec_reversed <- factor(factor_vec, levels = reversed_levels)

  return(factor_vec_reversed)
}

#' @title Sort Factor Levels Based on Their Length
#' @description Reorders the levels of a factor vector based on the character length of each level.
#' @param factor_vec A factor vector to be sorted.
#' @param decreasing Logical. Should the ordering be decreasing by length? Default is \code{FALSE}.
#' @return A factor vector with levels reordered based on their length.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date'))
#'
#' # Sort levels by length
#' fct_len(factor_vec)
#' @export
#' @author Kai Guo
fct_len <- function(factor_vec, decreasing = FALSE) {
  # Parameter validation
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Get levels and their lengths
  levels_vec <- levels(factor_vec)
  levels_lengths <- nchar(levels_vec)

  # Order levels based on length
  ordered_levels <- levels_vec[order(levels_lengths, decreasing = decreasing)]

  # Reorder factor levels
  factor_vec_ordered <- factor(factor_vec, levels = ordered_levels)

  return(factor_vec_ordered)
}

#' @title Combine Two Vectors of Unequal Lengths and Sort Based on Specified Levels
#' @description Combines two vectors, which may be of unequal lengths, into a factor vector and sorts based on the levels of either the first or second vector.
#' @param vector1 The first vector to combine.
#' @param vector2 The second vector to combine.
#' @param sort_by An integer (1 or 2) indicating which vector's levels to use for sorting. Default is \code{1}.
#' @param decreasing Logical. Should the sorting be in decreasing order? Default is \code{FALSE}.
#' @return A factor vector combining both vectors and sorted based on specified levels.
#' @examples
#' # Example vectors of unequal lengths
#' vector1 <- c('apple', 'banana', 'cherry')
#' vector2 <- c('date', 'fig', 'grape', 'honeydew')
#'
#' # Combine and sort based on vector1 levels
#' fct_combine(vector1, vector2, sort_by = 1)
#'
#' # Combine and sort based on vector2 levels
#' fct_combine(vector1, vector2, sort_by = 2)
#' @export
#' @author Kai Guo
fct_combine <- function(vector1, vector2, sort_by = 1, decreasing = FALSE) {
  # Parameter validation
  if (!is.numeric(sort_by) || !(sort_by %in% c(1, 2))) {
    stop("The 'sort_by' parameter must be 1 or 2.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }

  # Combine vectors
  combined_vector <- c(vector1, vector2)

  # Determine levels for sorting
  if (sort_by == 1) {
    levels_vec <- unique(vector1)
  } else {
    levels_vec <- unique(vector2)
  }

  # Include levels from both vectors to avoid missing levels
  levels_vec <- unique(c(levels_vec, combined_vector))

  # Sort levels
  levels_vec <- sort(levels_vec, decreasing = decreasing)

  # Create factor
  combined_factor <- factor(combined_vector, levels = levels_vec)

  return(combined_factor)
}


#' @title Encode Factor Levels into Numeric Codes
#' @description Converts the levels of a factor vector into numeric codes, optionally using a provided mapping.
#' @importFrom stats setNames
#' @param factor_vec A factor vector to encode.
#' @param mapping An optional named vector providing the numeric code for each level.
#' @return A numeric vector with encoded values.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('low', 'medium', 'high', 'medium'))
#'
#' # Encode without mapping
#' fct_encode(factor_vec)
#'
#' # Encode with custom mapping
#' custom_mapping <- c('low' = 1, 'medium' = 2, 'high' = 3)
#' fct_encode(factor_vec, mapping = custom_mapping)
#' @export
#' @author Kai Guo
fct_encode <- function(factor_vec, mapping = NULL) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }

  levels_vec <- levels(factor_vec)

  if (is.null(mapping)) {
    # Default encoding: integer codes
    mapping <- setNames(seq_along(levels_vec), levels_vec)
  } else {
    # Ensure mapping includes all levels
    if (!all(levels_vec %in% names(mapping))) {
      stop("The mapping must include all levels of the factor.")
    }
  }

  encoded_values <- mapping[as.character(factor_vec)]
  return(as.numeric(encoded_values))
}
#' @title Decode Numeric Codes into Factor Levels
#' @description Converts numeric codes back into factor levels using a provided mapping.
#' @importFrom stats setNames
#' @param codes A numeric vector of codes to decode.
#' @param mapping A named vector where names are levels and values are codes.
#' @return A factor vector with decoded levels.
#' @examples
#' # Numeric codes
#' codes <- c(1, 2, 3, 2)
#'
#' # Mapping from levels to codes
#' mapping <- c('low' = 1, 'medium' = 2, 'high' = 3)
#'
#' # Decode codes into factor levels
#' fct_decode(codes, mapping = mapping)
#' @export
#' @author Kai Guo
fct_decode <- function(codes, mapping) {
  if (!is.numeric(codes)) {
    stop("The 'codes' must be a numeric vector.")
  }
  if (!is.numeric(mapping) || is.null(names(mapping))) {
    stop("The 'mapping' must be a named numeric vector.")
  }

  inverse_mapping <- setNames(names(mapping), mapping)
  decoded_levels <- inverse_mapping[as.character(codes)]

  if (any(is.na(decoded_levels))) {
    warning("Some codes did not match any level in the mapping.")
  }

  factor_vec <- factor(decoded_levels, levels = names(mapping))
  return(factor_vec)
}
#' @title Aggregate Factor Levels Based on Grouping
#' @description Aggregates the levels of a factor vector based on another grouping vector.
#' @importFrom stats setNames
#' @param factor_vec A factor vector to aggregate.
#' @param groups A vector of the same length as \code{factor_vec} indicating group assignments.
#' @return A factor vector with aggregated levels.
#' @examples
#' # Example factor vector and groups
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'fig'))
#' groups <- c('fruit', 'fruit', 'fruit', 'dry fruit', 'dry fruit')
#'
#' # Aggregate levels based on groups
#' fct_rollup(factor_vec, groups)
#' @export
#' @author Kai Guo
fct_rollup <- function(factor_vec, groups) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (length(factor_vec) != length(groups)) {
    stop("The 'factor_vec' and 'groups' must be of the same length.")
  }

  new_levels <- unique(groups)
  mapping <- setNames(groups, as.character(factor_vec))

  aggregated_levels <- mapping[as.character(factor_vec)]
  factor_vec_aggregated <- factor(aggregated_levels, levels = new_levels)

  return(factor_vec_aggregated)
}
#' @title Replace Patterns in Factor Levels
#' @description Replaces substrings in factor levels that match a pattern with a replacement string.
#' @param factor_vec A factor vector to modify.
#' @param pattern A regular expression pattern to match.
#' @param replacement A string to replace the matched patterns.
#' @return A factor vector with modified levels.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('user_123', 'admin_456', 'guest_789'))
#'
#' # Replace numeric IDs with 'ID'
#' fct_pattern_replace(factor_vec, pattern = '[0-9]+', replacement = 'ID')
#' @export
#' @author Kai Guo
fct_pattern_replace <- function(factor_vec, pattern, replacement) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("The 'pattern' must be a single string representing a regular expression.")
  }
  if (!is.character(replacement) || length(replacement) != 1) {
    stop("The 'replacement' must be a single string.")
  }

  new_levels <- gsub(pattern, replacement, levels(factor_vec))
  factor_vec_modified <- factor(factor_vec, levels = levels(factor_vec), labels = new_levels)

  return(factor_vec_modified)
}
###
#' @title Calculate Statistics for Each Factor Level
#' @description Computes statistical summaries for each level of a factor vector based on associated numeric data.
#' @param factor_vec A factor vector.
#' @param numeric_vec A numeric vector of the same length as \code{factor_vec}.
#' @param stat_func A function to compute the statistic (e.g., mean, median).
#' @return A data frame with factor levels and their corresponding statistics.
#' @examples
#' # Example data
#' factor_vec <- factor(c('A', 'B', 'A', 'B', 'C'))
#' numeric_vec <- c(10, 20, 15, 25, 30)
#'
#' # Calculate mean for each level
#' fct_level_stats(factor_vec, numeric_vec, stat_func = mean)
#' @export
#' @author Kai Guo
fct_level_stats <- function(factor_vec, numeric_vec, stat_func) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.numeric(numeric_vec)) {
    stop("The 'numeric_vec' must be a numeric vector.")
  }
  if (length(factor_vec) != length(numeric_vec)) {
    stop("The 'factor_vec' and 'numeric_vec' must be of the same length.")
  }
  if (!is.function(stat_func)) {
    stop("The 'stat_func' must be a function.")
  }

  stats <- tapply(numeric_vec, factor_vec, stat_func)
  result_df <- data.frame(
    Level = names(stats),
    Statistic = as.numeric(stats),
    stringsAsFactors = FALSE
  )

  return(result_df)
}
#' @title Apply a Function to Factor Levels
#' @description Transforms factor levels by applying a function to each level.
#' @param factor_vec A factor vector to transform.
#' @param apply_func A function to apply to each level.
#' @return A factor vector with transformed levels.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry'))
#'
#' # Append '_fruit' to each level
#' fct_apply(factor_vec, function(x) paste0(x, '_fruit'))
#' @export
#' @author Kai Guo
fct_apply <- function(factor_vec, apply_func) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.function(apply_func)) {
    stop("The 'apply_func' must be a function.")
  }

  new_levels <- sapply(levels(factor_vec), apply_func)
  factor_vec_transformed <- factor(factor_vec, levels = levels(factor_vec), labels = new_levels)

  return(factor_vec_transformed)
}
#' @title Sample Levels from a Factor Vector
#' @description Randomly selects a specified number of levels from a factor vector.
#' @param factor_vec A factor vector.
#' @param size An integer specifying the number of levels to sample.
#' @param seed An optional integer for setting the random seed.
#' @return A factor vector containing only the sampled levels.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(letters[1:10])
#'
#' # Sample 5 levels
#' fct_sample_levels(factor_vec, size = 5, seed = 123)
#' @export
#' @author Kai Guo
fct_sample_levels <- function(factor_vec, size, seed = NULL) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.numeric(size) || size <= 0 || size != as.integer(size)) {
    stop("The 'size' must be a positive integer.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  levels_vec <- levels(factor_vec)
  if (size > length(levels_vec)) {
    stop("The 'size' cannot be greater than the number of levels in the factor.")
  }

  sampled_levels <- sample(levels_vec, size)
  factor_vec_sampled <- factor(factor_vec, levels = sampled_levels)
  factor_vec_sampled <- droplevels(factor_vec_sampled)

  return(factor_vec_sampled)
}
#' @title Pad Factor Levels to Uniform Length
#' @description Pads the levels of a factor vector with leading characters to achieve a specified width.
#' @param factor_vec A factor vector.
#' @param width An integer specifying the desired width.
#' @param pad_char A character to use for padding. Default is \code{'0'}.
#' @return A factor vector with padded levels.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('1', '12', '123'))
#'
#' # Pad levels to width 4
#' fct_pad_levels(factor_vec, width = 4)
#' @export
#' @author Kai Guo
fct_pad_levels <- function(factor_vec, width, pad_char = '0') {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.numeric(width) || width <= 0 || width != as.integer(width)) {
    stop("The 'width' must be a positive integer.")
  }
  if (!is.character(pad_char) || nchar(pad_char) != 1) {
    stop("The 'pad_char' must be a single character.")
  }

  new_levels <- sprintf(paste0("%", pad_char, width, "s"), levels(factor_vec))
  factor_vec_padded <- factor(factor_vec, levels = levels(factor_vec), labels = new_levels)

  return(factor_vec_padded)
}

#' @title Replace NA Values in Factor Vector
#' @description Replaces \code{NA} values in a factor vector with a specified level.
#' @param factor_vec A factor vector.
#' @param replacement_level A string specifying the level to replace \code{NA} values with.
#' @return A factor vector with \code{NA} values replaced.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', NA, 'banana', 'cherry', NA))
#'
#' # Replace NAs with 'Unknown'
#' fct_replace_na(factor_vec, replacement_level = 'Unknown')
#' @export
#' @author Kai Guo
fct_replace_na <- function(factor_vec, replacement_level) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.character(replacement_level) || length(replacement_level) != 1) {
    stop("The 'replacement_level' must be a single character string.")
  }

  # Add the replacement level if not already present
  if (!replacement_level %in% levels(factor_vec)) {
    levels(factor_vec) <- c(levels(factor_vec), replacement_level)
  }

  # Replace NAs
  factor_vec[is.na(factor_vec)] <- replacement_level

  return(factor_vec)
}

#' @title Get Order of Factor Levels in Data
#' @description Returns a vector indicating the order in which factor levels appear in the data.
#' @param factor_vec A factor vector.
#' @return A numeric vector representing the order of levels.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('banana', 'apple', 'cherry', 'apple', 'banana'))
#'
#' # Get level order
#' fct_level_order(factor_vec)
#' @export
#' @author Kai Guo
fct_level_order <- function(factor_vec) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }

  unique_levels <- unique(as.character(factor_vec))
  level_order <- match(levels(factor_vec), unique_levels)
  names(level_order) <- levels(factor_vec)

  return(level_order)
}
#' @title Create Dummy Variables from Factor Levels
#' @description Generates a data frame of dummy variables (one-hot encoded) from a factor vector.
#' @importFrom stats model.matrix
#' @param factor_vec A factor vector.
#' @return A data frame where each column represents a level of the factor, containing 1s and 0s.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'apple', 'cherry'))
#'
#' # Create dummy variables
#' fct_dummy(factor_vec)
#' @export
#' @author Kai Guo
fct_dummy <- function(factor_vec) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }

  # Create dummy variables
  dummy_df <- model.matrix(~ factor_vec - 1)
  colnames(dummy_df) <- levels(factor_vec)
  dummy_df <- as.data.frame(dummy_df)

  return(dummy_df)
}
#' @title Get Character Lengths of Factor Levels
#' @description Calculates the number of characters in each level of a factor vector.
#' @param factor_vec A factor vector.
#' @return A named numeric vector with the length of each level.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry'))
#'
#' # Get level lengths
#' fct_level_lengths(factor_vec)
#' @export
#' @author Kai Guo
fct_level_lengths <- function(factor_vec) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }

  level_lengths <- nchar(levels(factor_vec))
  names(level_lengths) <- levels(factor_vec)

  return(level_lengths)
}

#' @title Flag Duplicate Factor Levels
#' @description Identifies duplicate levels in a factor vector and returns a logical vector indicating which elements are duplicates.
#' @param factor_vec A factor vector.
#' @return A logical vector where \code{TRUE} indicates a duplicate level.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'apple', 'cherry', 'banana'))
#'
#' # Flag duplicates
#' fct_duplicates(factor_vec)
#' @export
#' @author Kai Guo
fct_duplicates <- function(factor_vec) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }

  duplicates <- duplicated(as.character(factor_vec))
  return(duplicates)
}

#' @title Collapse Factor Levels Based on Grouping
#' @description Collapses specified levels of a factor into new levels based on a grouping list.
#' @param factor_vec A factor vector to modify.
#' @param groups A named list where each element contains levels to be collapsed into a new level named after the list element's name.
#' @return A factor vector with collapsed levels.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'elderberry'))
#'
#' # Define groups
#' groups <- list(
#'   'Group1' = c('apple', 'banana'),
#'   'Group2' = c('cherry', 'date')
#' )
#'
#' # Collapse levels
#' fct_collapse_lev(factor_vec, groups)
#' @export
#' @author Kai Guo
fct_collapse_lev <- function(factor_vec, groups) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.list(groups) || is.null(names(groups))) {
    stop("The 'groups' must be a named list.")
  }

  # Create mapping
  mapping <- setNames(rep(names(groups), lengths(groups)), unlist(groups))

  # Recode levels
  new_levels <- ifelse(levels(factor_vec) %in% names(mapping), mapping[levels(factor_vec)], levels(factor_vec))

  # Update factor levels
  factor_vec_collapsed <- factor(factor_vec, levels = levels(factor_vec), labels = new_levels)

  return(factor_vec_collapsed)
}

#' @title Extract Substrings from Factor Levels
#' @description Extracts substrings from the levels of a factor vector based on a regular expression pattern and creates a new factor.
#' @param factor_vec A factor vector from which substrings will be extracted.
#' @param pattern A regular expression pattern to match.
#' @param capture_group An integer specifying which capture group to extract if using capturing groups in the pattern. Default is \code{0}, which extracts the entire match.
#' @return A new factor vector containing the extracted substrings.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('item123', 'item456', 'item789'))
#'
#' # Extract numeric part
#' fct_extract(factor_vec, pattern = '\\d+')
#'
#' # Extract with capturing group
#' factor_vec <- factor(c('apple: red', 'banana: yellow', 'cherry: red'))
#' fct_extract(factor_vec, pattern = '^(\\w+):', capture_group = 1)
#' @export
#' @author Kai Guo
fct_extract <- function(factor_vec, pattern, capture_group = 0) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("The 'pattern' must be a single string representing a regular expression.")
  }
  if (!is.numeric(capture_group) || length(capture_group) != 1 || capture_group < 0) {
    stop("The 'capture_group' must be a non-negative integer.")
  }

  # Extract substrings
  matches <- regexec(pattern, levels(factor_vec))
  captures <- regmatches(levels(factor_vec), matches)

  # Handle cases where no match is found
  extracted <- sapply(captures, function(x) {
    if (length(x) > 0) {
      if (capture_group == 0) {
        return(x[1])
      } else if (length(x) >= capture_group + 1) {
        return(x[capture_group + 1])
      } else {
        return(NA)
      }
    } else {
      return(NA)
    }
  })

  # Create new factor
  new_factor_vec <- factor(factor_vec, levels = levels(factor_vec), labels = extracted)

  return(new_factor_vec)
}
#' @title Map Factor Levels Using a Function
#' @description Transforms factor levels by applying a function that can include complex logic.
#' @param factor_vec A factor vector to map.
#' @param map_func A function that takes a character vector of levels and returns a character vector of new levels.
#' @return A factor vector with levels mapped according to the function.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry'))
#'
#' # Map levels to uppercase if they start with 'a'
#' fct_map_func(factor_vec, function(x) {
#'   ifelse(grepl('^a', x), toupper(x), x)
#' })
#' @export
#' @author Kai Guo
fct_map_func <- function(factor_vec, map_func) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.function(map_func)) {
    stop("The 'map_func' must be a function.")
  }

  levels_vec <- levels(factor_vec)
  new_levels <- map_func(levels_vec)

  if (!is.character(new_levels) || length(new_levels) != length(levels_vec)) {
    stop("The 'map_func' must return a character vector of the same length as the input levels.")
  }

  factor_vec_mapped <- factor(factor_vec, levels = levels_vec, labels = new_levels)

  return(factor_vec_mapped)
}
###
#' @title Factorize Character Vectors with Consistent Levels
#' @description Converts one or more character vectors into factors, ensuring that they share the same levels.
#' @param ... Character vectors to factorize.
#' @param levels An optional character vector specifying the levels. If \code{NULL}, levels are determined from the combined unique values of all vectors.
#' @return A list of factor vectors with consistent levels.
#' @examples
#' # Example character vectors
#' vec1 <- c('apple', 'banana', 'cherry')
#' vec2 <- c('banana', 'date', 'apple')
#'
#' # Factorize with consistent levels
#' factors <- fct_factorize(vec1, vec2)
#' levels(factors[[1]])
#' levels(factors[[2]])
#' @export
#' @author Kai Guo
fct_factorize <- function(..., levels = NULL) {
  vectors <- list(...)

  if (!all(sapply(vectors, is.character))) {
    stop("All inputs must be character vectors.")
  }

  if (is.null(levels)) {
    levels <- unique(unlist(vectors))
  }

  factors <- lapply(vectors, function(x) factor(x, levels = levels))
  return(factors)
}
#' @title Filter Factor Levels Using a Function
#' @description Removes levels from a factor vector based on a user-defined  function.
#' @param factor_vec A factor vector to filter.
#' @param func A function that takes a character vector of levels and returns a logical vector.
#' @return A factor vector with levels filtered according to the function.
#' @examples
#' # Example factor vector
#' factor_vec <- factor(c('apple', 'banana', 'cherry', 'date'))
#'
#' # Remove levels that start with 'b'
#' fct_filter_func(factor_vec, function(x) !grepl('^b', x))
#' @export
#' @author Kai Guo
fct_filter_func <- function(factor_vec, func) {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }
  if (!is.function(func)) {
    stop("The 'func' must be a function.")
  }

  levels_vec <- levels(factor_vec)
  keep_levels <- levels_vec[func(levels_vec)]

  factor_vec_filtered <- factor(factor_vec, levels = keep_levels)

  return(factor_vec_filtered)
}
#' @title Impute Missing Values in Factor Vector
#' @description Replaces \code{NA} values in a factor vector using specified imputation methods.
#' @param factor_vec A factor vector with potential \code{NA} values.
#' @param method The imputation method: \code{'mode'}, \code{'random'}, or a user-defined function.
#' @return A factor vector with \code{NA} values imputed.
#' @examples
#' # Example factor vector with NAs
#' factor_vec <- factor(c('apple', NA, 'banana', 'apple', NA))
#'
#' # Impute using mode
#' fct_impute(factor_vec, method = 'mode')
#'
#' # Impute using random selection
#' fct_impute(factor_vec, method = 'random')
#' @export
#' @author Kai Guo
fct_impute <- function(factor_vec, method = 'mode') {
  if (!is.factor(factor_vec)) {
    stop("The 'factor_vec' must be a factor vector.")
  }

  na_indices <- which(is.na(factor_vec))
  if (length(na_indices) == 0) {
    return(factor_vec)
  }

  if (method == 'mode') {
    mode_level <- names(sort(table(factor_vec), decreasing = TRUE))[1]
    factor_vec[na_indices] <- mode_level
  } else if (method == 'random') {
    non_na_levels <- as.character(factor_vec[!is.na(factor_vec)])
    factor_vec[na_indices] <- sample(non_na_levels, length(na_indices), replace = TRUE)
  } else if (is.function(method)) {
    imputed_values <- method(factor_vec)
    if (length(imputed_values) != length(factor_vec)) {
      stop("Custom imputation function must return a vector of the same length.")
    }
    factor_vec <- imputed_values
  } else {
    stop("Invalid 'method' specified. Use 'mode', 'random', or a custom function.")
  }

  return(factor_vec)
}

#' @title Create Factor of Unique Combinations from Multiple Factors
#' @description Generates a new factor where each level represents a unique combination of levels from the input factors.
#' @param ... Factor vectors to combine.
#' @param sep A string to separate levels in the combined factor. Default is \code{'_'}.
#' @return A factor vector representing unique combinations.
#' @examples
#' # Example factors
#' factor_vec1 <- factor(c('A', 'A', 'B', 'B'))
#' factor_vec2 <- factor(c('X', 'Y', 'X', 'Y'))
#'
#' # Create unique combinations
#' combined_factor <- fct_unique_comb(factor_vec1, factor_vec2)
#' levels(combined_factor)
#' @export
#' @author Kai Guo
fct_unique_comb <- function(..., sep = '_') {
  factors <- list(...)

  if (!all(sapply(factors, is.factor))) {
    stop("All inputs must be factor vectors.")
  }
  if (!is.character(sep) || length(sep) != 1) {
    stop("The 'sep' must be a single character string.")
  }

  combined_levels <- do.call(paste, c(lapply(factors, as.character), sep = sep))
  combined_factor <- factor(combined_levels)

  return(combined_factor)
}

