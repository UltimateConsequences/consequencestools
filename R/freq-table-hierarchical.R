


insert_rows_by_group_vars <- function(main_df, additional_df, variable1, variable2) {
  result <- data.frame()
  var1_name <- rlang::as_name(enquo(variable1))
  var2_name <- rlang::as_name(enquo(variable2))

  # print("Varible is:")
  # print(var1_name)
  #
  # print("var1 column:")
  # print(main_df[[var1_name]])

  # Get unique variable1 values from the main dataframe
  unique_var1_values <- unique(main_df[[var1_name]])
  # print("Unique variable1 values in main_df:")
  # print(unique_var1_values)

  # Process each perp_affiliation group
  for (var1_value in unique_var1_values) {
    # Get rows for this var1_value from main dataframe
    main_rows <- main_df[main_df[, var1_name] == var1_value, ]

    # Check if there are additional rows for this var1_value
    if (var1_value %in% additional_df[[var1_name]]) {
      # Get the other row that corresponds to this var1_value
      other_row <- additional_df[additional_df[, var1_name] == var1_value, ]

      # Append the rows together
      group_result <- rbind(main_rows, other_row)
    } else {
      group_result <- main_rows
    }

    # Append to the result
    result <- rbind(result, group_result)
  }

  # Check if there are any perp_affiliations in additional_df that aren't in main_df
  unique_other_var1_values <- setdiff(unique(additional_df[[var1_name]]), unique_var1_values)

  # Add these as new groups
  if (length(unique_other_var1_values) > 0) {
    for (var1_value in unique_other_var1_values) {
      other_row <- additional_df[additional_df[, var1_name] == var1_value, ]
      result <- rbind(result, other_row)
    }
  }

  # Return the combined result
  return(result)
}


#' Two-layer frequency table (hierarchical)
#'
#' @description
#' Construct a two-layer frequency table with counts and percentages for
#' `variable1` and a secondary breakdown by `variable2`. The function returns
#' a formatted kable (HTML) object suitable for reporting.
#'
#' The function performs the following:
#' - Removes rows with NA in either `variable1` or `variable2`.
#' - Produces a primary frequency table for `variable1` (counts and percent).
#' - Produces a secondary count table by `variable1` and `variable2`.
#' - Optionally groups small secondary categories (below `threshold`) into
#'   an "Other" category.
#' - Optionally sorts the result.
#'
#' The `variable1` and `variable2` arguments use tidy-eval (unquoted column
#' names).
#'
#' @param dataset A data.frame. The input dataset. Defaults to `de` in the
#'   surrounding environment if not supplied.
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
#' @return A kable/kableExtra HTML table (invisibly returned for knit/HTML
#'   output) showing the hierarchical frequency table with totals.
#'
#' @export
#'
#' @examples
#' # Using mtcars as an example (treating numeric values as factors for grouping)
#' library(dplyr)
#' mtcars2 <- mtcars %>% mutate(cyl = as.factor(cyl), gear = as.factor(gear))
#' two_layer_frequency_table(mtcars2, cyl, gear, sort = TRUE, threshold = 1)
#' deaths_aug24 %>%
#' two_layer_frequency_table(perp_affiliation, dec_affiliation, sort=TRUE, threshold=3)
#'
two_layer_frequency_table <- function(dataset=de,
                                      variable1,
                                      variable2,
                                      sort=TRUE,
                                      threshold = 1,
                                      unit_is_deaths=TRUE) {
  # clear out NA values in the variables
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

  # This filtering is handled automatically by the show_na in tabyl():
  # dataset <- dataset %>%
  #   filter(!is.na({{variable1}})) %>%
  #   filter(!is.na({{variable2}}))

  # Create table from full dataset
  ft.primary <- dataset %>%
    janitor::tabyl({{variable1}}, show_na=FALSE) %>%
    #    janitor::adorn_totals("row") %>%
    janitor::adorn_pct_formatting() %>%
    rename(
      "pct" = "percent"
    ) %>%
    filter(n >= threshold)

  if (sort){
    ft.primary <- ft.primary %>%
      arrange(desc(n))
  }

  # Create table from filtered dataset
  ft.secondary <- dataset %>%
    count({{variable1}}, {{variable2}}) %>%
    #    janitor::adorn_totals("row") %>%
    rename(
      "n_2" = "n",
    )

  # Join tables and rename the variable column
  ft.combined <- left_join(ft.primary, ft.secondary, by=rlang::as_name(enquo(variable1)))

  if(sort) {
    # Recombine the sorted rows with the total row
    ft.combined <- ft.combined %>% arrange(desc(n), desc(n_2))
  }
  if(threshold > 1) {
    ft.other <- ft.combined %>% filter((n_2 < threshold)) %>%
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

  # # ft.combined <- add_row(ft.combined,
  #                        {{variable1}} := "Total",
  #                        n = sum(ft.combined$n),
  #                        pct = "100%",
  #                        {{variable2}} := "",
  #                        n_2 = NA)
  # Set NA display option and return formatted table
  options(knitr.kable.NA = '')

  # return(ft.combined %>%
  #          kbl() %>%
  #          kable_classic(full_width = F, html_font = "Minion Pro"))

  old_lang <- transcats::set_title_lang("en")
  on.exit(transcats::set_title_lang(old_lang))

  if(unit_is_deaths){
    n_secondary_trans_table <- tribble(
      ~pct, ~n_2, ~language,
      #--|--|----
      "% of Total", "Deaths", "en",
      "%", "Muertes", "es",
      "pct", "n_2", "r_variable",
    )
  } else {
    n_secondary_trans_table <- tribble(
      ~pct, ~n_,  ~n_2, ~language,
      #--|--|----
      "% of Total", "Count", "Count", "en",
      "%", "Número", "Número", "es",
      "pct", "n_", "n_2", "r_variable",
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
               c(nrow(ft.combined)+1),
             extra_css = "border-top: 1px solid black;") %>%
    # bold the final total line
    kableExtra::row_spec(c(0,nrow(ft.combined)+1), bold = TRUE)

  return(output_kable)
}

