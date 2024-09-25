#' @title Generate Pairwise Comparisons Between Elements in factor
#' @description Creates all unique pairwise combinations between factor_vec of a vector, with options for references, symmetry, NA handling, custom filtering, and output formats. Automatically handles factors by converting them to vectors and removes extra spaces from factor_vec before processing.
#' @param factor_vec A vector containing the factor_vec to compare. Can be of any type (character, numeric, factor, etc.).
#' @param ref Optional. A vector containing the reference factor_vec. If NULL (default), comparisons are made within the \code{factor_vec} vector.
#' @param symmetric Logical. If \code{TRUE} (default), unique unordered pairs are returned. If \code{FALSE}, all ordered pairs are returned.
#' @param include_na Logical. If \code{FALSE} (default), \code{NA} values are excluded from comparisons. If \code{TRUE}, \code{NA} values are included.
#' @param include_self Logical. If \code{FALSE} (default), pairs where \code{Var1 == Var2} are excluded. If \code{TRUE}, they are included.
#' @param filter_fn Optional. A custom function to filter the pairs. Should accept a data frame and return a logical vector.
#' @param pre_fn Optional. A function to preprocess the factor_vec before comparison (e.g., \code{tolower}, \code{trimws}). Default is \code{trimws}.
#' @param sort_by Character string specifying how to sort the output. Options are \code{"Var1"}, \code{"Var2"}, \code{"both"}, or \code{"none"} (default).
#' @param output_format Character string specifying the output format. Options are \code{"data.frame"} (default), \code{"list"}, or \code{"matrix"}.
#' @return A data frame, list, matrix, or tibble containing pairwise comparisons between factor_vec.
#' @examples
#' # Example vector with extra spaces and mixed case
#' vec <- c(' A', 'b ', ' C ', 'D')
#'
#' # Generate pairwise comparisons within vec
#' fct_pairs(vec)
#'
#' # Use a custom preprocessing function to convert factor_vec to lower case
#' fct_pairs(vec, pre_fn = function(x) tolower(trimws(x)))
#' fct_paris(vec, ref = c("A","C"))
#' @export
fct_pairs <- function(factor_vec, ref = NULL, symmetric = TRUE,
                                    include_na = FALSE, include_self = FALSE, filter_fn = NULL,
                                    pre_fn = trimws, sort_by = "none", output_format = "data.frame") {
  # Convert factors to vectors
  if (is.factor(factor_vec)) {
    factor_vec <- as.character(factor_vec)
  }
  if (!is.null(ref) && is.factor(ref)) {
    ref <- as.character(ref)
  }

  # Ensure factor_vec and ref are vectors
  if (!is.vector(factor_vec)) {
    stop("'factor_vec' must be a vector.")
  }
  if (!is.null(ref) && !is.vector(ref)) {
    stop("'ref' must be a vector.")
  }

  # Remove duplicates from factor_vec and ref
  factor_vec <- unique(factor_vec)
  if (!is.null(ref)) {
    ref <- unique(ref)
  }

  # Preprocess factor_vec using the provided function
  if (!is.null(pre_fn)) {
    factor_vec <- pre_fn(factor_vec)
    if (!is.null(ref)) {
      ref <- pre_fn(ref)
    }
  }

  # Include or exclude NA values
  if (!include_na) {
    factor_vec <- factor_vec[!is.na(factor_vec)]
    if (!is.null(ref)) {
      ref <- ref[!is.na(ref)]
    }
  }

  # If ref is NULL, use factor_vec as ref
  if (is.null(ref)) {
    ref <- factor_vec
  }

  # Generate all combinations
  combinations <- expand.grid(Var1 = factor_vec, Var2 = ref, stringsAsFactors = FALSE)

  # Include or exclude self-pairs
  if (!include_self) {
    combinations <- combinations[combinations$Var1 != combinations$Var2, ]
  }

  # Remove duplicate pairs if symmetric
  if (symmetric) {
    combinations_sorted <- t(apply(combinations, 1, function(x) sort(as.character(x))))
    combinations <- combinations[!duplicated(combinations_sorted), ]
  }

  # Apply custom filter function if provided
  if (!is.null(filter_fn)) {
    if (!is.function(filter_fn)) {
      stop("'filter_fn' must be a function that accepts a data frame and returns a logical vector.")
    }
    filter_result <- filter_fn(combinations)
    if (!is.logical(filter_result) || length(filter_result) != nrow(combinations)) {
      stop("'filter_fn' must return a logical vector of the same length as the number of combinations.")
    }
    combinations <- combinations[filter_result, , drop = FALSE]
  }

  # Sorting
  if (sort_by == "Var1") {
    combinations <- combinations[order(combinations$Var1), ]
  } else if (sort_by == "Var2") {
    combinations <- combinations[order(combinations$Var2), ]
  } else if (sort_by == "both") {
    combinations <- combinations[order(combinations$Var1, combinations$Var2), ]
  } else if (sort_by != "none") {
    stop("Invalid value for 'sort_by'. Options are 'Var1', 'Var2', 'both', or 'none'.")
  }

  # Reset row names
  rownames(combinations) <- NULL

  # Output format
  if (output_format == "list") {
    result <- split(combinations, seq(nrow(combinations)))
  } else if (output_format == "matrix") {
    result <- as.matrix(combinations)
  } else if (output_format == "data.frame") {
    result <- combinations
  } else {
    stop("Invalid 'output_format'. Options are 'data.frame', 'list',  or 'matrix'.")
  }

  return(result)
}
