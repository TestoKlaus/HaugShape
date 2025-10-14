# Utility functions for HaugShape package
# Internal helper functions for validation and common operations

#' Validate Data Frame Input
#'
#' Internal function to validate data frame inputs across HaugShape functions.
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param numeric_cols Character vector of columns that must be numeric
#' @param function_name Character string with calling function name for
#'   error messages
#'
#' @return Nothing. Stops execution with informative error if validation fails.
#' @keywords internal
validate_data_frame <- function(data, required_cols = NULL, numeric_cols = NULL,
                                function_name = "function") {
  # Check if data is provided and is a data frame
  if (missing(data)) {
    stop("Argument 'data' is required in ", function_name, call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame in ", function_name, call. = FALSE)
  }

  if (nrow(data) == 0) {
    stop("'data' cannot be empty in ", function_name, call. = FALSE)
  }

  # Check required columns exist
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
      stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
           " (in ", function_name, ")", call. = FALSE)
    }
  }

  # Check numeric columns are actually numeric
  if (!is.null(numeric_cols)) {
    for (col in numeric_cols) {
      if (col %in% colnames(data) && !is.numeric(data[[col]])) {
        stop("Column '", col, "' must be numeric in ", function_name,
             call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

#' Validate Plot Style Parameter
#'
#' Internal function to validate plot_style parameter.
#'
#' @param plot_style Character string to validate
#' @param function_name Character string with calling function name
#'
#' @return Nothing. Stops execution if invalid.
#' @keywords internal
validate_plot_style <- function(plot_style, function_name = "function") {
  valid_styles <- c("Haug", "inverted_Haug", "publication")
  if (!is.character(plot_style) || length(plot_style) != 1) {
    stop("'plot_style' must be a single character string in ", function_name,
         call. = FALSE)
  }

  if (!plot_style %in% valid_styles) {
    stop("'plot_style' must be one of: ", paste(valid_styles, collapse = ", "),
         " (in ", function_name, ")", call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Numeric Parameter
#'
#' Internal function to validate numeric parameters with optional range
#' checking.
#'
#' @param value Numeric value to validate
#' @param param_name Character string with parameter name
#' @param min_val Optional minimum value
#' @param max_val Optional maximum value
#' @param integer_only Logical indicating if value must be integer
#' @param function_name Character string with calling function name
#'
#' @return Nothing. Stops execution if invalid.
#' @keywords internal
validate_numeric_param <- function(value, param_name, min_val = NULL,
                                   max_val = NULL, integer_only = FALSE,
                                   function_name = "function") {
  if (!is.numeric(value) || length(value) != 1) {
    stop("'", param_name, "' must be a single numeric value in ",
         function_name, call. = FALSE)
  }

  if (!is.finite(value)) {
    stop("'", param_name, "' must be finite in ", function_name,
         call. = FALSE)
  }

  if (integer_only && value != round(value)) {
    stop("'", param_name, "' must be an integer in ", function_name,
         call. = FALSE)
  }

  if (!is.null(min_val) && value < min_val) {
    stop("'", param_name, "' must be >= ", min_val, " in ", function_name,
         call. = FALSE)
  }

  if (!is.null(max_val) && value > max_val) {
    stop("'", param_name, "' must be <= ", max_val, " in ", function_name,
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Color Parameter
#'
#' Internal function to validate color specifications.
#'
#' @param colors Character vector of color specifications
#' @param param_name Character string with parameter name
#' @param allow_null Logical indicating if NULL is allowed
#' @param function_name Character string with calling function name
#'
#' @return Nothing. Stops execution if invalid.
#' @keywords internal
validate_colors <- function(colors, param_name, allow_null = TRUE,
                            function_name = "function") {
  if (is.null(colors) && allow_null) {
    return(invisible(TRUE))
  }

  if (!is.character(colors) || length(colors) == 0) {
    stop("'", param_name, "' must be a character vector of colors in ",
         function_name, call. = FALSE)
  }

  # Check for empty strings
  if (any(nchar(trimws(colors)) == 0)) {
    stop("'", param_name, "' cannot contain empty color strings in ",
         function_name, call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Group Values Parameter
#'
#' Internal function to validate group_vals parameter against available groups.
#'
#' @param group_vals Vector of group values to validate
#' @param available_groups Vector of available group values in data
#' @param param_name Character string with parameter name
#' @param function_name Character string with calling function name
#'
#' @return Nothing. Stops execution if invalid.
#' @keywords internal
validate_group_vals <- function(group_vals, available_groups,
                                param_name = "group_vals",
                                function_name = "function") {
  if (is.null(group_vals)) {
    return(invisible(TRUE))
  }

  invalid_groups <- setdiff(group_vals, available_groups)
  if (length(invalid_groups) > 0) {
    stop("Values in '", param_name, "' not found in data: ",
         paste(invalid_groups, collapse = ", "), " (in ", function_name, ")",
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Check Required Package
#'
#' Internal function to check if a required package is available.
#'
#' @param package_name Character string with package name
#' @param function_name Character string with calling function name
#'
#' @return Nothing. Stops execution if package not available.
#' @keywords internal
check_required_package <- function(package_name, function_name = "function") {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop("Package '", package_name, "' is required but not installed.\n",
         "Install it with: install.packages('", package_name, "')\n",
         "(required by ", function_name, ")", call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate File Path
#'
#' Internal function to validate file paths and create directories if needed.
#'
#' @param file_path Character string with file path
#' @param must_exist Logical indicating if file must already exist
#' @param create_dir Logical indicating if directory should be created
#' @param param_name Character string with parameter name
#' @param function_name Character string with calling function name
#'
#' @return Character string with normalized path.
#' @keywords internal
validate_file_path <- function(file_path, must_exist = FALSE, create_dir = FALSE,
                               param_name = "file_path",
                               function_name = "function") {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("'", param_name, "' must be a single character string in ",
         function_name, call. = FALSE)
  }

  if (nchar(trimws(file_path)) == 0) {
    stop("'", param_name, "' cannot be empty in ", function_name,
         call. = FALSE)
  }

  if (must_exist && !file.exists(file_path)) {
    stop("File not found: '", file_path, "' (", param_name, " in ",
         function_name, ")", call. = FALSE)
  }
  if (create_dir) {
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path)) {
      tryCatch({
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }, error = function(e) {
        stop("Cannot create directory for '", param_name, "': ", dir_path,
             " (in ", function_name, ")", call. = FALSE)
      })
    }
  }

  return(normalizePath(file_path, mustWork = FALSE))
}

#' Clean Data for Analysis
#'
#' Internal function to remove NA and non-finite values from data.
#'
#' @param data Data frame to clean
#' @param columns Character vector of columns to check for valid values
#' @param verbose Logical indicating whether to print cleaning messages
#'
#' @return Cleaned data frame.
#' @keywords internal
clean_data <- function(data, columns, verbose = TRUE) {
  original_rows <- nrow(data)

  # Create logical vector for valid rows
  valid_rows <- rep(TRUE, original_rows)

  for (col in columns) {
    if (col %in% colnames(data)) {
      valid_rows <- valid_rows & !is.na(data[[col]]) & is.finite(data[[col]])
    }
  }

  cleaned_data <- data[valid_rows, , drop = FALSE]

  if (verbose && original_rows > nrow(cleaned_data)) {
    rows_removed <- original_rows - nrow(cleaned_data)
    message("Removed ", rows_removed, " rows with missing or non-finite values")
  }

  return(cleaned_data)
}

#' Generate Default Colors
#'
#' Internal function to generate default color palettes for groups.
#'
#' @param n_groups Integer number of groups
#' @param palette Character string specifying color palette type
#'
#' @return Character vector of colors.
#' @keywords internal
generate_default_colors <- function(n_groups, palette = "Set1") {
  if (n_groups <= 0) {
    return(character(0))
  }

  if (n_groups == 1) {
    return("#1f77b4")  # Single blue color
  }

  # Use RColorBrewer if available, otherwise use basic colors
  if (requireNamespace("RColorBrewer", quietly = TRUE) && n_groups <= 9) {
    colors <- RColorBrewer::brewer.pal(max(3, n_groups), palette)
    return(colors[seq_len(n_groups)])
  } else {
    # Fallback to built-in colors
    basic_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    if (n_groups <= length(basic_colors)) {
      return(basic_colors[seq_len(n_groups)])
    } else {
      # Generate more colors using rainbow
      return(rainbow(n_groups))
    }
  }
}
