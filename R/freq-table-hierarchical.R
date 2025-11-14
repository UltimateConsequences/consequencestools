globalVariables(c("n_2", "pct", "n", "is_first_in_group",
                  "variable1_display", "n_display", "pct_display"))
#' Insert rows into a grouped table from an additional dataframe
#'
#' @description
#' Insert rows from `additional_df` into `main_df` by matching groups defined
#' by `variable1`. For each unique value of `variable1` in `main_df`, if a
#' corresponding row exists in `additional_df` it will be appended to that
#' group's rows. Any `variable1` values that appear only in `additional_df`
#' will be appended as new groups at the end of the returned data frame.
#'
#' This function uses tidy-eval for `variable1` and `variable2` (unquoted
#' column names).
#'
#' @param main_df A data.frame. The primary table containing the main groups.
#' @param additional_df A data.frame. Rows to be inserted into `main_df` by
#'   matching on `variable1`.
#' @param variable1 Column name (unquoted) used as the grouping key (tidy-eval).
#' @param variable2 Column name (unquoted) indicating the second-level variable
#'   to be inserted (tidy-eval).
#'
#' @return A data.frame containing rows from `main_df` with rows from
#'   `additional_df` inserted by group. Rows from `additional_df` whose
#'   `variable1` values do not exist in `main_df` will appear as new groups.
#'
#' @examples
#' df_main <- data.frame(group = c("A", "A", "B"),
#'              sub = c("x", "y", "z"), n = 1:3, stringsAsFactors = FALSE)
#' df_add  <- data.frame(group = c("A", "C"),
#'              sub = c("other", "other"), n_2 = c(10, 5), stringsAsFactors = FALSE)
#' insert_rows_by_group_vars(df_main, df_add, group, sub)
#'
#' @seealso two_layer_frequency_table, two_layer_frequency_kable
#' @export
insert_rows_by_group_vars <- function(main_df, additional_df, variable1, variable2) {
  result <- data.frame()
  var1_name <- rlang::as_name(enquo(variable1))
  var2_name <- rlang::as_name(enquo(variable2))

  # Get unique variable1 values from the main dataframe
  unique_var1_values <- unique(main_df[[var1_name]])

  # Process each group
  for (var1_value in unique_var1_values) {
    # Get rows for this var1_value from main dataframe
    main_rows <- main_df[main_df[, var1_name] == var1_value, ]

    # Check if there are additional rows for this var1_value
    if (var1_value %in% additional_df[[var1_name]]) {
      # Get the other row that corresponds to this var1_value
      other_row <- additional_df[additional_df[, var1_name] == var1_value, ]

      # Append the rows together
      group_result <- bind_rows(main_rows, other_row)
    } else {
      group_result <- main_rows
    }

    # Append to the result
    result <- bind_rows(result, group_result)
  }

  # Check if there are any values in additional_df that aren't in main_df
  unique_other_var1_values <- setdiff(unique(additional_df[[var1_name]]), unique_var1_values)

  # Add these as new groups
  if (length(unique_other_var1_values) > 0) {
    for (var1_value in unique_other_var1_values) {
      other_row <- additional_df[additional_df[, var1_name] == var1_value, ]
      result <- bind_rows(result, other_row)
    }
  }

  # Return the combined result
  return(result)
}


#' Two-layer frequency table (data)
#'
#' @description
#' Compute a two-layer frequency table (unformatted data) that contains primary
#' counts and percentages for `variable1` and the secondary breakdown by
#' `variable2`. This function returns the combined data.frame (ft.combined)
#' which can be further processed or passed to a kable/printing function.
#'
#' The `variable1` and `variable2` arguments use tidy-eval (unquoted column
#' names).
#'
#' @param dataset A data.frame. The input dataset.
#' @param variable1 Column name (unquoted) for the primary grouping variable
#'   (tidy-eval).
#' @param variable2 Column name (unquoted) for the secondary grouping variable
#'   (tidy-eval).
#' @param sort Logical. If TRUE, sort groups by descending counts. Default:
#'   TRUE.
#' @param threshold Integer. Minimum count threshold for including a secondary
#'   category. Secondary categories with counts below `threshold` will be
#'   combined into an "Other" row. Default: 1.
#'
#' @return A data.frame (ft.combined) with columns for the primary variable,
#'   counts (n), percentage (pct), secondary variable, and secondary counts
#'   (n_2).
#'
#' @examples
#' library(dplyr)
#' mtcars2 <- mtcars %>% mutate(cyl = as.factor(cyl), gear = as.factor(gear))
#' two_layer_frequency_table(mtcars2, cyl, gear, sort = TRUE, threshold = 1)
#'
#' @export
two_layer_frequency_table <- function(dataset,
                                      variable1,
                                      variable2,
                                      sort=TRUE,
                                      threshold = 1) {
  # clear out NA values in the variables (warn if present)
  if(n_filter(dataset, is.na({{variable1}}) > 0)) {
    warning(paste("The dataset contains", n_filter(dataset, is.na({{variable1}})),
                  "rows with NA values in", rlang::as_name(enquo(variable1)),
                  "which will be excluded from the analysis."))
  }
  if(n_filter(dataset, is.na({{variable2}}) > 0)) {
    warning(paste("The dataset contains", n_filter(dataset, is.na({{variable2}})),
                  "rows with NA values in", rlang::as_name(enquo(variable2)),
                  "which will be excluded from the analysis."))
  }

  # Create primary table
  ft.primary <- dataset %>%
    janitor::tabyl({{variable1}}, show_na = FALSE) %>%
    janitor::adorn_pct_formatting() %>%
    rename("pct" = "percent") %>%
    filter(n >= threshold)

  if (sort) {
    ft.primary <- ft.primary %>% arrange(desc(n))
  }

  # Create secondary counts
  ft.secondary <- dataset %>%
    count({{variable1}}, {{variable2}}) %>%
    rename("n_2" = "n")

  # Join tables
  ft.combined <- left_join(ft.primary, ft.secondary, by = rlang::as_name(enquo(variable1)))

  if (sort) {
    ft.combined <- ft.combined %>% arrange(desc(n), desc(n_2))
  }

  if (threshold > 1) {
    ft.other <- ft.combined %>%
      filter((n_2 < threshold)) %>%
      group_by({{variable1}}) %>%
      summarize(n_2 = sum(n_2)) %>%
      mutate({{variable2}} := "Other",
             n = ft.primary$n[match({{variable1}}, ft.primary[,1])],
             pct = ft.primary$pct[match({{variable1}}, ft.primary[,1])]
      ) %>%
      relocate(n, pct, {{variable2}}, .after = {{variable1}})

    # Filter out rows where n_2 is less than the threshold
    ft.combined <- ft.combined %>% filter(n_2 >= threshold)

    # Combine the filtered rows with the "Other" rows
    ft.combined_final <- insert_rows_by_group_vars(ft.combined, ft.other, {{variable1}}, {{variable2}})
    ft.combined <- ft.combined_final
  }

  # Return the combined (unformatted) table
  return(ft.combined)
}


#' Two-layer frequency table (formatted kable)
#'
#' @description
#' Create a formatted HTML kable from the two-layer frequency data. This
#' function calls two_layer_frequency_table() to compute the raw combined
#' table and then applies labeling, translation via transcats, and kableExtra
#' styling to produce a display-ready table.
#'
#' @param dataset A data.frame. The input dataset.
#' @param variable1 Column name (unquoted) for the primary grouping variable
#'   (tidy-eval).
#' @param variable2 Column name (unquoted) for the secondary grouping variable
#'   (tidy-eval).
#' @param sort Logical. If TRUE, sort groups by descending counts. Default:
#'   TRUE.
#' @param threshold Integer. Minimum count threshold for including a secondary
#'   category. Secondary categories with counts below `threshold` will be
#'   combined into an "Other" row. Default: 1.
#' @param unit_is_deaths Logical. If TRUE, use "Deaths"/"Muertes" translation
#'   for the secondary count label; otherwise use a generic "Numero"/"Count".
#'   Default: TRUE.
#'
#' @return A kableExtra kable object (HTML) styled for reporting.
#'
#' @examples
#' library(dplyr)
#' mtcars2 <- mtcars %>% mutate(cyl = as.factor(cyl), gear = as.factor(gear))
#' two_layer_frequency_kable(mtcars2, cyl, gear, sort = TRUE, threshold = 1)
#'
#' @export
two_layer_frequency_kable <- function(dataset,
                                      variable1,
                                      variable2,
                                      sort=TRUE,
                                      threshold = 1,
                                      unit_is_deaths = TRUE) {
  # Compute the combined data frame
  ft.combined <- two_layer_frequency_table(dataset = dataset,
                                           variable1 = {{variable1}},
                                           variable2 = {{variable2}},
                                           sort = sort,
                                           threshold = threshold)

  # Recompute primary table (needed for total row) using same logic as the data function
  ft.primary <- dataset %>%
    janitor::tabyl({{variable1}}, show_na = FALSE) %>%
    janitor::adorn_pct_formatting() %>%
    rename("pct" = "percent") %>%
    filter(n >= threshold)

  # Set knitr NA display option
  options(knitr.kable.NA = '')

  # Manage translations via transcats
  old_lang <- transcats::set_title_lang("en")
  on.exit(transcats::set_title_lang(old_lang))

  if (unit_is_deaths) {
    n_secondary_trans_table <- tribble(
      ~pct, ~n_2, ~language,
      "% of Total", "Deaths", "en",
      "%", "Muertes", "es",
      "pct", "n_2", "r_variable"
    )
  } else {
    n_secondary_trans_table <- tribble(
      ~pct, ~n_,  ~n_2, ~language,
      "% of Total", "Count", "Count", "en",
      "%", "N\u00FAmero", "N\u00FAmero", "es",
      "pct", "n_", "n_2", "r_variable"
    )
  }

  uc_var_table_ext_2 <- transcats::append_to_var_name_table(n_secondary_trans_table,
                                                            transcats::uc_var_table_ext,
                                                            overwrite = TRUE)
  transcats::set_var_name_table(uc_var_table_ext_2)


  col.names = c(rlang::as_name(enquo(variable1)), "n_", "pct",
                rlang::as_name(enquo(variable2)), "n_2")
  n_2_total <- sum(ft.combined$n_2)

  output_kable <- ft.combined %>%
    # Create a version of the dataset that has blank values for repeating cells
    mutate(
      # Create new columns for display that will have blanks for repeated values
      variable1_display = as.character({{variable1}}),
      n_display = n,
      pct_display = pct
    ) %>%
    # Use group_by and row_number to identify repeats in the first column
    group_by({{variable1}}) %>%
    mutate(
      # Only keep the first instance of each group, make others blank
      variable1_display = ifelse(row_number() > 1, "", variable1_display),
      n_display = ifelse(row_number() > 1, NA, n_display),
      pct_display = ifelse(row_number() > 1, NA, pct_display),
      # Create a flag for the first row of each group to add horizontal line
      is_first_in_group = row_number() == 1
    ) %>%
    mutate(
      # Only keep the first instance of each group, make others blank
      variable1_display = ifelse(is_first_in_group, variable1_display, "")
    ) %>%
    ungroup() %>%
    # Create the table using the display columns
    select(variable1_display, n_display, pct_display, {{variable2}}, n_2) %>%
    add_row(
      variable1_display := "Total",
      n_display = sum(ft.primary$n),
      pct_display = "100%",
      {{variable2}} := "",
      n_2 = n_2_total) %>%
    #   kbl(col.names = c("perp_affiliation", "n", "pct", "dec_affiliation", "n_2")) %>%
    kableExtra::kbl(col.names = sapply(col.names, transcats::variable_name_from_string)) %>%
    # Apply styling
    kableExtra::kable_classic(full_width = F, html_font = "Minion Pro") %>%
    # Add horizontal lines above each new group
    kableExtra::row_spec(which(ft.combined %>%
                                 group_by({{variable1}}) %>%
                                 mutate(is_first_in_group = row_number() == 1) %>%
                                 ungroup() %>%
                                 pull(is_first_in_group)) %>%
                           c(nrow(ft.combined) + 1),
                         extra_css = "border-top: 1px solid black;") %>%
    kableExtra::row_spec(c(0, nrow(ft.combined) + 1), bold = TRUE)

  return(output_kable)
}
