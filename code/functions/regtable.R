# Created by Thom Covert and edited by Yixin Sun in November 2018
# modified a bunch since then
# Goal is to create a function outputting a latex table from a regression object
# Requirements:
  # user can call regtable(list(m_1, m_2, m_3), ...) for arbitrary objects
    # m_i that support coef(summary(m_i))
  # OR: ask user to specifically submit a recalculated matrix or tibble of
    # coefficient estimates + standard errors to the user's liking
  # should be able to rename coefficients, like in stargazer or texreg
  # should be able to add rows at the bottom

# PARAMETERS FOR REGTABLE
# ms list with one or more model objects that are compatible with the coeftest()
#    function
# est a character vector of the covariates to output
# est_names a character vector of labels for est
# mnames a character vector of labels for each model object in ms
# se_fun covariance functions to be passed to coeftest. This can be a single
  # function or a list with length(ms)
# extra_rows list of additional information to display for
#    each model, such as fixed effects or controls. This must be in the format
#    list(Name1 = c("Thing1", "Thing2"), Name2 = c("Stuff1", "Stuff2"))
# stats a character vector specifying which model statistics should be
#    kept in the output. This should follow the same format as the output from
#    glance() in the broom package
# stats_names a character vector of labels for each object in stats
# n_obs a logical indicating if the table should include the number of
#    observations used in the models
# output_format A string passed to kable() that specifies the format of
#    the table output. The options are latex, html, markdown, pandoc, and rst.
#    The default is latex
# header named vector passed into the add_header_above function from kabelExtra
#     to print multi-row headers
# sig_stars logical indicating whether or not significant stars should
#    be added to the coefficients
# note A character string if a footnote is to be added to the end of the
#    table.
# ... additional parameters that can be passed into se_fun

library(modelr)
library(broom)
library(kableExtra)

# function for creating significant stars
significance <- function(x){
  symp <- symnum(x, corr = FALSE, na = FALSE,
                 cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                 symbols = c("$^{***}$", "$^{**}$", "$^{*}$", "$^{}$"))
  return(as.character(c(symp)))
}


# Extracting coefficients and standard errors -------------------------------
get_tau <- function(m, colname, se_fun, decimals, est, est_names,
                    sig_stars, ...) {
  if(is.null(est_names)) est_names <- est
  est_names_map <- tibble(term = est, term_name = est_names)

  if(is.null(decimals)) decimals <- 3

  colname1 <- quo_name(enquo(colname))

  # extract coefficients and standard errors
  output <- coeftest(m, se_fun(m, ...))

  # add significant stars if sig_stars == TRUE
  output <-
    output %>%
    tidy() %>%
    filter(term %in% est) %>%
    mutate(sigs = sig_stars,
           stars = if_else(sigs, significance(`p.value`), ""),
           estimate = trimws(format(round(estimate, decimals),
                                    nsmall = decimals)),
           estimate = paste0(estimate, stars))

  # tack on dummy rows for values of est that aren't in term
  missing_est <- setdiff(est, with(output, term))
  if(length(missing_est) > 0) {
    output_extra <- tibble(term = missing_est,
                           estimate = NA_character_,
                           std.error = NA_real_,
                           statistic = NA_real_,
                           p.value = NA_real_)
    output <- bind_rows(output, output_extra)
  }
  
  # format values such that it's rounded to 3 decimal places
  # format value so output has SE and coefficients lined up and centered
  output %>%
    select(term, estimate, se = `std.error`) %>%
    mutate(se = trimws(format(round(se, decimals), nsmall = decimals))) %>%
    inner_join(est_names_map, by = "term") %>%
    select(-term) %>%
    rename(term = term_name) %>%
    mutate(index = row_number()) %>%
    gather(type, value, -term, -index) %>%
    group_by(index) %>%
    arrange(index, type) %>%
    ungroup %>%
    select(-index) %>%
    mutate(sigs = sig_stars,
           value = if_else(type == "se", paste0("(", value, ")"),
                           as.character(value)),
           value = format(value, justify = "centre")) %>%
    rename(!!colname1 := value) %>%
    select(-sigs)

}


# Extract summary statistics from model --------------------------------------
get_nobs <- function(m) {
  if(class(m) == "felm") {
    return(summary(m)$N)
  } else if(class(m) == "coxph") {
    return(pluck(m, "n"))
  }
  else {
    return(nobs(m))
  }
}

get_stats <- function(m, stat1, stats_name, mnames, n_obs){
  stats_out <- glance(m)

  if(!is.na(stat1)){
    stats_out <-
      select(stats_out, one_of(stat1)) %>%
      t() %>%
      round(., 3) %>%
      format(., nsmall = 3, big.mark = ",") %>%
      str_replace_all(., ".000$", "") %>%
      trimws %>%
      str_replace_all(., "NA", "")
  } else {
    stats_out <- NULL
  }
  if(is.na(stats_name)) stats_name <- colnames(stats_out)

  if(n_obs){
    stats_out <- c(format(get_nobs(m), big.mark = ","), stats_out)
    stats_out <- bind_cols(!!mnames := stats_out, term = c("N", stats_name))
  } else{
    stats_out <- bind_cols(!!mnames := stats_out, term = stats_name)
  }

  return(as_tibble(stats_out))
}


# Main regtable function ------------------------------------------------------
#' @export
#' @rdname regtable
regtable <- function(ms, est, mnames = NULL, est_names = NULL,
                     extra_rows = NULL, se_fun = vcov,
                     stats = c("r.squared", "adj.r.squared"),
                     stats_names = c("$R^2$", "Proj. $R^2$"),
                     n_obs = TRUE,
                     output_format = "latex",
                     header = NULL,
                     sig_stars = FALSE,
                     decimals = NULL,
                     note = NULL, ...) {

  temp_names <- paste("(", seq(1:length(ms)), ")")
  if(is.null(mnames)) mnames <- temp_names
  if(is.null(decimals)) decimals <- rep(3, length(ms))
  if(is.null(est_names)) est_names <- est

  # create section of table housing stats such as N, R^2 and Projected R^2
  if(!is.list(stats)){ stats <- list(stats)}
  if(!is.list(n_obs)){n_obs <- list(n_obs)}
  if(!is.list(stats_names)){ stats_names <- list(stats_names)}

  statstable <-
    pmap(list(ms, stats, stats_names, temp_names, n_obs), get_stats) %>%
    reduce(full_join, by = "term") %>%
    select(term, everything()) %>%
    mutate(type = "")

  # create section of table housing coefficients and standard error
  if(!is.list(se_fun)) se_fun <- list(se_fun)
  if(!is.list(est)) est <- list(est)
  if(!is.list(est_names)) est_names <- list(est_names)

  coef_table <-
    pmap(list(ms, temp_names, se_fun, decimals, est, est_names),
         get_tau, sig_stars, ...) %>%
    reduce(full_join, by = c("term", "type"))

  # fix row order in coef_table
  est_row_order <- 
    tibble(term = est_names[[1]]) %>%
    mutate(row_order = row_number())

  coef_table <- 
    left_join(coef_table, est_row_order, by = "term") %>%
    arrange(row_order, type) %>%
    select(-row_order)

  # replace NA and (NA) with spaces in Latex Tables
  if(output_format == "latex") {
    coef_table <-
      coef_table %>%
      mutate_at(vars(temp_names),
                ~ str_replace(., regex("NA|\\(NA\\)"), "\\phantom{X}"))
  }

  # add in extra rows
  if(!is.null(extra_rows)) {
    extras <-
      extra_rows %>%
      unlist() %>%
      matrix(ncol = length(extra_rows)) %>%
      t() %>%
      as_tibble() %>%
      rename_all(~ temp_names) %>%
      mutate(type = "",
             term = names(extra_rows))
    row_spec_no <- length(extra_rows) + 2 * max(map_dbl(est, length))

  } else {
    extras <- NULL
    row_spec_no <- NA_integer_
  }

  # bind together the regression coefficients, stats, and extra lines
 if(output_format == "df"){
    final_table <-
      mutate(coef_table, part = "coef") %>%
      bind_rows(mutate(statstable, part = "stats"))

    if(!is.null(extras)){
      final_table <-
        final_table %>%
        bind_rows(mutate(extras, part = "extra"))
    }
    return(list(output = final_table, model_names = mnames))
  }

  final_table <-
    bind_rows(coef_table, extras, statstable) %>%
    mutate_all(~ if_else(is.na(.), "", .)) %>%
    select(-type) %>%
    rename(` ` = term)  %>%
    kable(format = output_format,
          booktabs = TRUE,
          col.names = c("", mnames),
          linesep = "",
          escape = FALSE,
          align = c('l', rep('c', length(mnames)))) %>%
    add_footnote(note)

  if(!is.null(header)) final_table <- final_table %>% add_header_above(header)

  if(output_format == "latex"){
    final_table <-
      final_table %>%
      row_spec(2 * max(map_dbl(est, length)),
        extra_latex_after = "\\midrule") %>%
      collapse_rows(columns = 1, latex_hline = "none")

    if(is.na(unlist(stats))[[1]] | is.na(unlist(n_obs))[[1]]){
      final_table <-
        final_table %>%
        row_spec(row_spec_no, extra_latex_after = "\\midrule")
    }

    # ridiculous hack to get latex \ back in phantoms
    final_table <-
      final_table %>%
      str_replace_all(fixed("phantom{X}"), "\\phantom{X}")
  }

  attr(final_table, "format") <- output_format
  return(final_table)
}




# regtable stack ------------------------------------------------------------
# function that takes several regtable outputs (in dataframe format) and
# stacks them together
# n_bottom is a logical indicating if N should be at the bottom of the table
# or section-specific
regtable_stack <- function(final_tables, table_names = NULL,
  output_format = "latex", note = NULL, header = NULL, n_bottom = TRUE){

  if(!is.null(table_names)){
    final_df <-
      map2_df(final_tables, table_names,
        function(x, y) mutate(x$output, table_name = y))
  } else{
    final_df <-
      map_df(final_tables, function(x) c(x$output)) %>%
      mutate(table_name = NA)
  }

  mnames <-
    map(final_tables, function(x) c(x$model_names)) %>%
    unique %>%
    unlist

  # rename part zN so later when we sort by the part column, N comes last
  final_df <- mutate(final_df, part = if_else(term == "N", "zN", part))

  coef <-
    final_df %>%
    filter(part == "coef" | part == "stats")  %>%
    mutate(term = if_else(part == "coef" & !is.na(table_name),
      paste(term, "-", table_name), term))

  extra <-
    final_df %>%
    filter(part == "extra") %>%
    select(-table_name) %>%
    arrange(as.factor(part)) %>%
    distinct()

  n_row <- filter(final_df, part == "zN")

  if(n_bottom){
    extra <- bind_rows(extra, n_row[1,])
  } else{
    coef <-
      bind_rows(coef, n_row) %>%
      arrange(table_name, part)
  }

  output <-
    bind_rows(coef, extra) %>%
    select(-type, -table_name, -part) %>%
    rename(` ` = term)

  output <-
    output %>%
    kable(format = output_format,
          booktabs = TRUE,
          col.names = colnames(.),
          linesep = "",
          escape = FALSE,
          align = c('l', rep('c', length(mnames)))) %>%
    add_footnote(note)

  if(!is.null(header)) final_table <- final_table %>% add_header_above(header)

  if(output_format == "latex"){
    break_end <- nrow(coef)
    break_start <- break_end / length(final_tables)
    breaks <- seq(break_start, break_end, break_start)

    output <-
      output %>%
      row_spec(breaks, extra_latex_after = "\\midrule") %>%
      collapse_rows(columns = 1, latex_hline = "none")

    output <-
      output %>%
      str_replace_all(fixed("phantom{X}"), "\\phantom{X}")
  }

  return(output)

}
