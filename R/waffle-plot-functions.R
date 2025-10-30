# Helper function to create counts dataframe for waffle charts
#'
#' @param dataframe The input dataframe containing the data.
#' @param x_var The variable to facet the waffle chart by (e.g., year).
#' @param fill_var The variable to color the waffle chart blocks by (e.g.,
#'   state_responsibility).
#' @param fill_var_description A list containing the title, levels, and colors
#'  for the fill variable. Can be taken from our level description list, lev.
#'
#' @return A dataframe with counts of occurrences for each combination of x_var
#'   and fill_var.
#'
#' @export
#' @examples
#' waffle_counts(dataframe = assign_state_responsibility_levels(deaths_aug24, simplify = TRUE),
#'  x_var = year, fill_var = state_responsibility,
#'               fill_var_description = lev$state_responsibility)
waffle_counts <- function(dataframe, x_var,
                          fill_var, fill_var_description) {
  # Ensure the fill variable is properly factored with the correct levels
  dataframe <- dataframe %>%
    mutate(across({{ fill_var }},
                  ~factor(., levels = fill_var_description$levels)))

  # Create the counts dataframe
  counts_df <- dataframe %>%
    filter(!is.na(year)) %>%
    filter(!is.na({{ x_var }})) %>%
    filter({{ x_var }} != "Unknown") %>%
    filter(!is.na({{ fill_var }})) %>%
    dplyr::count({{ x_var }}, {{ fill_var }})

  return(counts_df)
}

#' New helper function to add null/missing x values
#'
#' @param counts_df The counts dataframe from waffle_counts
#' @param x_var The variable to facet the waffle chart by (e.g., year).
#' @param fill_var The variable to color the waffle chart blocks by (e.g.,
#'   state_responsibility).
#' @param all_levels Optional: All possible levels for the x variable (needed
#'   for pres_admin)
#' @param .verbose Logical indicating whether to print debug information
#' @return The counts dataframe with missing x values added
#'
#' @examples
#' deaths <- assign_levels(deaths_aug24, "standard", .simplify=TRUE)  %>%
#'   dplyr::filter(year != 2012)  # Remove 2012
#' waffle_counts_df <- waffle_counts(deaths,
#'                                  x_var = year,
#'                                   fill_var = protest_domain,
#'                                   fill_var_description = lev$protest_domain)
#' waffle_counts_df_completed <- complete_x_values(waffle_counts_df,
#'                                                 x_var = year,
#'                                                 fill_var = protest_domain,
#'                                                 all_levels = NULL,
#'                                                 .verbose = FALSE)
complete_x_values <- function(counts_df, x_var, fill_var,
                              all_levels = NULL, .verbose = FALSE) {
  x_var_name <- quo_name(enquo(x_var))
  existing_values <- unique(counts_df[[x_var_name]])

  null_x_values <- c()
  range_of_x_levels <- c()

  # Handle year variable
  if (x_var_name == "year") {
    min_val <- min(existing_values)
    max_val <- max(existing_values)
    range_of_x_levels <- min_val:max_val
    null_x_values <- setdiff(range_of_x_levels, existing_values)

    if (.verbose) {
      print(paste("Null values: ", paste(null_x_values, collapse = ", ")))
    }
  }

  # Handle pres_admin variable (requires all_levels)
  if (x_var_name == "pres_admin") {
    if (is.null(all_levels)) {
      stop("all_levels must be provided for pres_admin variable")
    }

    min_idx <- min(match(existing_values, all_levels))
    max_idx <- max(match(existing_values, all_levels))
    range_of_x_levels <- all_levels[min_idx:max_idx]
    null_x_values <- setdiff(range_of_x_levels, existing_values)
  }

  # Add null rows if any missing values found
  if (length(null_x_values) > 0) {
    null_rows <- tibble(
      {{ x_var }} := null_x_values,
      {{ fill_var }} := " ",
      n = 1
    )
    counts_df <- bind_rows(counts_df, null_rows)
  }

  # Re-factor x variable with complete range of levels
  if (length(range_of_x_levels) > 0) {
    counts_df <- counts_df %>%
      mutate(across(1, ~factor(., levels = range_of_x_levels)))
  }

  if (.verbose) {
    print(str(counts_df))
  }

  return(counts_df)
}

#' Create a waffle chart with facets
#'
#' Creates a waffle chart from an overall dataset, both calculating
#' the relevant counts and formatting the chart.
#'
#' @param dataframe The input dataframe containing the data.
#' @param x_var The variable to facet the waffle chart by (e.g., year
#'  or pres_admin). This will name the facets.
#' @param fill_var The variable to color the waffle chart blocks by (e.g.,
#'   state_responsibility).
#' @param fill_var_description A list containing the title, levels, and colors
#'  for the fill variable. Can be taken from our level description list, lev.
#' @param n_columns Number of columns for the facet wrap.
#' @param waffle_width Number of rows in each waffle chart (default 10).
#' @param complete_x Logical indicating whether to complete missing x values
#'   with a single null block. Only implemented for year and pres_admin.
#' @param .verbose Logical indicating whether to print debug information.
#'
#' @return A ggplot object representing the waffle chart.
#' @export
#'
#' @examples
#' deaths <- assign_levels(deaths_aug24, "standard", .simplify = TRUE)
#' make_waffle_chart(deaths,
#'   x_var = pres_admin,
#'   fill_var = state_responsibility,
#'   fill_var_description = state_resp,
#'   complete_x = TRUE, n_columns = 7)
make_waffle_chart <- function(dataframe, x_var, fill_var,
                              fill_var_description,
                              n_columns = 5,
                              waffle_width = 10,
                              complete_x = FALSE,
                              lang = "en",
                              .verbose = FALSE) {
  # Get the color palette from the corresponding description variable
  fill_colors <- fill_var_description$colors
  fill_legend <- fill_colors
  if (lang=="es" & "colors_es" %in% names(fill_var_description)){
    fill_legend <- fill_var_description$colors_es
    fill_var_description$title <- fill_var_description$title_es
  }

  x_var_name <- quo_name(enquo(x_var))
  range_of_x_levels <- list()
  if(is.factor(dataframe[[x_var_name]])){
    all_levels <- levels(dataframe[[x_var_name]])
    range_of_x_levels <- all_levels
  }

  counts_df <- waffle_counts(dataframe, {{x_var}}, {{fill_var}},
                             {{fill_var_description}})

  # Complete with null blocks if requested
  if (complete_x) {
    # code only implemented for these two variables for now
    if (x_var_name %in% c("year", "pres_admin")){
      counts_df <- complete_x_values(counts_df, {{x_var}}, {{fill_var}},
                                     all_levels = range_of_x_levels,
                                     .verbose = .verbose)

      # Add null to the color set but not to the legend
      fill_colors <- c(fill_colors, ' ' = "white")
    }
  }

  # Create the plot
  ggplot(counts_df, aes(fill = {{ fill_var }}, values = n)) +
    waffle::geom_waffle(color = "white", size = .25, n_rows = waffle_width,
                flip = TRUE, na.rm = TRUE) +
    facet_wrap(ggplot2::vars({{ x_var }}), ncol = n_columns,
               labeller = ggplot2::label_wrap_gen(20),
               strip.position = "bottom") +
    scale_x_discrete() +
    scale_y_continuous(breaks = c(0.5, 5.5, 10.5),
                       labels = function(x) (x-0.5) * waffle_width, # make this multiplier the same as n_rows
                       expand = c(0,0)) +
    coord_equal() +
    scale_fill_manual(name = fill_var_description$title,
                      values = fill_colors,
                      limits = names(fill_colors),
                      labels = names(fill_legend),  # Language specific
                      breaks = names(fill_colors)) +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          strip.text.x = element_text(size = 12)) +
    ggplot2::guides(fill = guide_legend(reverse = TRUE))
}

#' Create a tall waffle chart with facets
#'
#' Creates a tall-oriented waffle chart from an overall dataset, both calculating
#' the relevant counts and formatting the chart. Facets are arranged vertically.
#'
#' @param dataframe The input dataframe containing the data.
#' @param x_var The variable to facet the waffle chart by (e.g., year
#'  or pres_admin). This will name the facets.
#' @param fill_var The variable to color the waffle chart blocks by (e.g.,
#'   state_responsibility).
#' @param fill_var_description A list containing the title, levels, and colors
#'  for the fill variable. Can be taken from our level description list, lev.
#' @param n_columns Number of rows for the facet wrap (displayed vertically).
#' @param complete_x Logical indicating whether to complete missing x values
#'   with a single null block. Only implemented for year and pres_admin.
#' @param .verbose Logical indicating whether to print debug information.
#'
#' @return A ggplot object representing the tall waffle chart.
#' @export
#'
#' @examples
#' deaths <- assign_levels(deaths_aug24, "standard", .simplify = TRUE)
#' make_waffle_chart_tall(deaths,
#'   x_var = pres_admin,
#'   fill_var = state_responsibility,
#'   fill_var_description = lev$state_responsibility,
#'   complete_x = TRUE, n_columns = 7)
#'  make_waffle_chart_tall(deaths,
#'    x_var = pres_admin, fill_var = state_responsibility,
#'    fill_var_description = lev$state_responsibility,
#'    complete_x = FALSE, n_columns = 6)
#'  make_waffle_chart_tall(deaths,
#'    x_var = pres_admin, fill_var = state_responsibility,
#'    fill_var_description = lev$state_responsibility,
#'    complete_x = FALSE, n_columns = 6, lang="es')
make_waffle_chart_tall <- function(dataframe, x_var, fill_var, fill_var_description,
                                   n_columns = 5,
                                   complete_x = FALSE,
                                   lang = "en",
                                   .verbose = FALSE) {
  # Get the color palette from the corresponding description variable
  fill_colors <- fill_var_description$colors
  fill_legend <- fill_colors
  if (lang=="es" & "colors_es" %in% names(fill_var_description)){
    fill_legend <- fill_var_description$colors_es
    fill_var_description$title <- fill_var_description$title_es
  }

  x_var_name <- quo_name(enquo(x_var))
  range_of_x_levels <- list()
  if(is.factor(dataframe[[x_var_name]])){
    all_levels <- levels(dataframe[[x_var_name]])
    range_of_x_levels <- all_levels
  }

  counts_df <- waffle_counts(dataframe, {{x_var}}, {{fill_var}},
                             {{fill_var_description}})

  # Complete with null blocks if requested
  if (complete_x) {
    # code only implemented for these two variables for now
    if (x_var_name %in% c("year", "pres_admin")){
      counts_df <- complete_x_values(counts_df, {{x_var}}, {{fill_var}},
                                     all_levels = range_of_x_levels,
                                     .verbose = .verbose)

      # Add null to the color set but not to the legend
      fill_colors <- c(fill_colors, ' ' = "white")
    }
  }

  # Define waffle chart parameters
  waffle_width <- 5

  legend_orientation <- "horizontal"
  if ((nrow(counts_df) / n_columns) <= 1) {
    legend_orientation <- "vertical"
  }

  # Create the plot
  ggplot(counts_df, aes(fill = {{ fill_var }}, values = n)) +
    # Remove flip=TRUE to make bars build left to right
    geom_waffle(color = "white", size = .25, n_rows = waffle_width, na.rm = TRUE) +
    # Change strip.position to "left" and use nrow instead of ncol
    facet_wrap(ggplot2::vars({{ x_var }}), nrow = n_columns,
               labeller = ggplot2::label_wrap_gen(20),
               strip.position = "left",
               dir = "v") +
    scale_x_continuous(breaks = c(0.5, 5.5, 10.5, 15.5, 20.5, 25.5),
                       labels = function(x) (x-0.5) * waffle_width,
                       expand = c(0,0)) +
    # Use scale_y_discrete() instead of scale_x_discrete()
    scale_y_discrete() +
    coord_equal() +
    scale_fill_manual(name = fill_var_description$title,
                      values = fill_colors,
                      breaks = names(fill_colors),
                      labels = names(fill_legend),  # Language specific
                      guide = guide_legend(reverse = FALSE)) +
    theme_minimal(base_family = "Roboto Condensed") +
    # Change axis.ticks.y to axis.ticks.x
    theme(panel.grid = element_blank(),
          axis.ticks.x = element_line(),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.position = "top",
          legend.direction = legend_orientation,
          # Change strip.text.x to strip.text.y
          strip.text.y.left = element_text(size = 14,
                                           angle = 0, hjust = 0),
          strip.placement = "outside",
          plot.margin = ggplot2::margin(5, 20, 5, 35)) +
    # add a 1pt solid line at x=0.5 and 1pt grey line at x =20.5
    geom_vline(xintercept = 0.5, color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 20.5, color = "darkgrey", linewidth = 0.25)
}
