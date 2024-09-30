#' @title Split Factor Levels and Reorder Based on Specified Criteria
#' @description Splits the levels of a factor vector using specified patterns or positions and reorders based on specified parts or criteria. Optionally reorders the data vector's elements to align with the new levels' order.
#' @param factor_vec A factor vector to be processed.
#' @param split_pattern A character vector specifying the pattern(s) or position(s) to use for splitting. Can be regular expressions or integer positions.
#' @param use_pattern An integer specifying which pattern to use if multiple patterns are provided. Default is \code{NULL} by using all patterns.
#' @param part An integer or integer vector specifying which part(s) to use after splitting (e.g., 1 for the first part). Can be a range or specific indices.
#' @param position An integer or integer vector specifying the character positions within the part(s) to consider.
#' @param char_freq Logical. Should the sorting be based on character frequencies within the specified part(s)? Default is \code{FALSE}.
#' @param decreasing Logical. Should the ordering be decreasing? Default is \code{FALSE}.
#' @param inplace Logical. If \code{TRUE}, returns a new factor vector with elements reordered to align with the new levels' order. If \code{FALSE}, returns a new factor vector with only the levels' order adjusted, leaving the data vector's elements' order unchanged. Defaults to \code{FALSE}.
#' @return A factor vector with levels reordered based on the specified conditions. Depending on the \code{inplace} parameter, the data vector's elements may also be reordered.
#' @examples
#' # Example 1: Split by patterns '-', '_', or '|' and reorder based on the
#' # first part without reordering data elements
#' factor_vec <- factor(c('item1-sub1', 'item2_sub2', 'item3|sub3', 'item1-sub4'))
#' ft_split(factor_vec, split_pattern = c('-', '_', '\\|'), part = 1, inplace = FALSE)
#'
#' # Example 2: Use the second pattern '_' for splitting and reorder
#' # data elements
#' ft_split(factor_vec, split_pattern = c('-', '_', '\\|'), use_pattern = 2, part = 2, inplace = TRUE)
#'
#' # Example 3: Reorder based on character frequencies in the specified part
#' # without reordering data elements
#' ft_split(factor_vec, split_pattern = '-', part = 2, char_freq = TRUE, inplace = FALSE)
#'
#' # Example 4: Split by pattern '-' and reorder both levels and data
#' # elements based on the first part
#' ft_split(factor_vec, split_pattern = '-', part = 1, inplace = TRUE)
#' @export
#' @author Kai Guo
ft_split <- function(factor_vec, split_pattern, use_pattern = NULL, part = 1,
                      position = NULL, char_freq = FALSE, decreasing = FALSE,
                      inplace = FALSE) {
  # Parameter validation
  if(!is.factor(factor_vec)){
    factor_vec <- as.factor(factor_vec)
  }
  if (!is.character(split_pattern) || length(split_pattern) < 1) {
    stop("The 'split_pattern' must be a character vector with at least one pattern.")
  }
  if(is.null(use_pattern)){
    use_pattern <- length(split_pattern)
  }
  if (!is.numeric(use_pattern) || use_pattern <= 0 || use_pattern > length(split_pattern) || use_pattern != as.integer(use_pattern)) {
    stop("The 'use_pattern' must be a positive integer within the range of 'split_pattern' length.")
  }
  if (!is.numeric(part) || any(part <= 0) || any(part != as.integer(part))) {
    stop("The 'part' parameter must be a positive integer or a vector of positive integers.")
  }
  if (!is.null(position) && (!is.numeric(position) || any(position <= 0) || any(position != as.integer(position)))) {
    stop("The 'position' parameter must be NULL or a positive integer or a vector of positive integers.")
  }
  if (!is.logical(char_freq) || length(char_freq) != 1) {
    stop("The 'char_freq' parameter must be a single logical value.")
  }
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("The 'decreasing' parameter must be a single logical value.")
  }
  if (!is.logical(inplace) || length(inplace) != 1) {
    stop("The 'inplace' parameter must be a single logical value.")
  }

  # Select the pattern to use for splitting
  if(use_pattern == length(split_pattern)){
    pattern_to_use <- split_pattern
  } else {
    pattern_to_use <- split_pattern[use_pattern]
  }

  # Split levels using the selected pattern
  split_levels <- strsplit(as.character(factor_vec), split = pattern_to_use, perl = TRUE)

  # Extract specified part(s)
  extracted_parts <- sapply(split_levels, function(x) {
    selected_parts <- x[part]
    # Handle cases where parts are missing
    selected_parts[is.na(selected_parts)] <- ""
    paste(selected_parts, collapse = "")
  })

  # Further process based on position or character frequency
  if (!is.null(position)) {
    # Extract characters at specified positions
    extracted_chars <- sapply(extracted_parts, function(part_str) {
      chars <- unlist(strsplit(part_str, split = ""))
      if (length(chars) >= max(position)) {
        return(paste(chars[position], collapse = ""))
      } else {
        return("")
      }
    })
    sort_key <- extracted_chars
  } else if (char_freq) {
    # Calculate character frequencies within the parts
    all_chars <- unlist(strsplit(extracted_parts, split = ""))
    char_freq_table <- table(all_chars)
    part_freqs <- sapply(extracted_parts, function(part_str) {
      chars <- unlist(strsplit(part_str, split = ""))
      sum(char_freq_table[chars], na.rm = TRUE)
    })
    sort_key <- part_freqs
  } else {
    # Use the extracted parts directly for sorting
    sort_key <- extracted_parts
  }

  # Create data frame for ordering
  df_levels <- data.frame(
    original = as.character(factor_vec),
    sort_key = sort_key,
    stringsAsFactors = FALSE
  )

  # Order the levels based on sort_key
  df_levels <- df_levels[order(df_levels$sort_key, decreasing = decreasing), ]

  # Reorder factor levels
  factor_vec_ordered <- factor(factor_vec, levels = unique(df_levels$original))

  if (inplace) {
    # Reorder the data vector's elements to align with the new levels' order
    # Create a mapping of levels to their new order
    level_order <- setNames(seq_along(levels(factor_vec_ordered)), levels(factor_vec_ordered))

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
