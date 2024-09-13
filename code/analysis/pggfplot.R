# set default pggf options
.pggf_defaults <- structure(
  list(
    out_dir = '.',
    preview_plot = TRUE,
    print_code = TRUE,
    scales = 'fixed',
    space = 'fixed',
    enlarge_x = c(0.05, 0.05),
    enlarge_y = c(0.05, 0.05),
    points_below = 10,
    samples = 256,
    trim = TRUE
  ),
  class = 'pggf_options'
)

pggf_defaults <- .pggf_defaults

pggf_config <- function(..., reset_all = FALSE) {
  if (reset_all) {
    pggf_defaults <<- .pggf_defaults
    return(invisible())
  }
  
  arg_list <- list(...)
  
  for (i in seq_along(arg_list)) {
    key = names(arg_list[i])
    value = arg_list[[i]]
    
    if (is.null(key) || key == '') { # `key` not specified
      if (is.null(value)) { # `value` is `NULL` --> ignore
        next
      } else {
        cli::cli_abort(c('Invalid option {.field {value}}', i = 'Options must be specified as {.arg key = value} pairs'))
      }
    } else if (! key %in% names(.pggf_defaults)) { # `key` not a potential option
      cli::cli_abort('Unknown option {.field {key}}') 
    } else {
      fallback <- .pggf_defaults[[key]]
      if (length(value) > length(fallback)) {
          cli::cli_abort(c('Too many values for option {.field {key}}', i = 'Must be a vector of length {length(fallback)}'))
      } else if (is.null(value) || all(is.na(value))) { # `value` is `NULL` or `NA` --> restore default to factory setting
        pggf_defaults[[key]] <<- fallback
      } else if (! all(class(value) == class(fallback))) { # invalid `value`
        cli::cli_abort(c('Invalid value {.field {value}} for option {.field {key}}', i = '{.field {key}} must be of class {.cls {class(fallback)}}'))
      } else { # set option
        pggf_defaults[[key]] <<- value
      }
    }
  }
}

print.pggf_options <- function(options) {
  for (i in seq_along(options)) {
    cat(writeLines(paste0(names(options[i]), ':\t', options[[i]])))
  }
}


# set up a pggfplot
pggfplot <- function(data, filename, x = NULL, y = NULL, facet_col = NULL, facet_row = NULL, facet_wrap = NULL,
                     scales = pggf_defaults$scales, space = pggf_defaults$space, enlarge_x = pggf_defaults$enlarge_x,
                     enlarge_y = pggf_defaults$enlarge_y, out_dir = pggf_defaults$out_dir, 
                     preview_plot = pggf_defaults$preview_plot, preview_theme = ggplot2::theme_bw(),
                     print_code = pggf_defaults$print_code, xmin = NULL, xmax = NULL,
                     ymin = NULL, ymax = NULL) {
  
  # check arguments
  if (! is.character(filename) || length(filename) != 1) {
    cli::cli_abort('{.arg filename} must be a {.cls character} vector of length 1')
  }
  if (! scales %in% c('fixed', 'free_x', 'free_y', 'free', 'square')) {
    cli::cli_abort('{.arg scales} must be one of "fixed", "free_x", "free_y", "free", or "square"')
  }
  if (! is.numeric(enlarge_x) || ! dplyr::between(length(enlarge_x), 1, 2)) {
    cli::cli_abort('{.arg enlarge_x} must be a {.cls numeric} vector of length 1 or 2')
  } else if (length(enlarge_x) == 1) {
    enlarge_x <- rep(enlarge_x, 2)
  }
  if (! is.numeric(enlarge_y) || ! dplyr::between(length(enlarge_y), 1, 2)) {
    cli::cli_abort('{.arg enlarge_y} must be a {.cls numeric} vector of length 1 or 2')
  } else if (length(enlarge_y) == 1) {
    enlarge_y <- rep(enlarge_y, 2)
  }
  if (! space %in% c('fixed', 'free_x', 'free_y', 'free')) {
    cli::cli_abort('{.arg space} must be one of "fixed", "free_x", "free_y", or "free"')
  }
  
  # convert `x` and `y` to strings
  x_str <- if (is.symbol(substitute(x))) {deparse(substitute(x))} else if (is.null(x)) {x_str <- NULL} else {as.character(x)}
  y_str <- if (is.symbol(substitute(y))) {deparse(substitute(y))} else if (is.null(y)) {y_str <- NULL} else {as.character(y)}
  
  # check if at least one of `x` or `y` is given
  if (is.null(x_str) && is.null(y_str)) {
    cli::cli_abort('At least one of {.arg x} or {.arg y} must be given')
  }
  
  # prepare data
  data <- data |>
    tibble::as_tibble() |>
    dplyr::ungroup()
  
  # get facet info
  if (
    (missing(facet_col) || pggf_is_null(substitute(facet_col))) && 
    (missing(facet_row) || pggf_is_null(substitute(facet_row)))
  ) { # neither `facet_col` nor `facet_row` given
    if (! missing(facet_wrap) && ! pggf_is_null(substitute(facet_wrap))) { # `facet_wrap` given
      wrap <- data |> dplyr::pull({{facet_wrap}}) |> unique() |> sort()
      facets <- tibble::tibble(wrap = wrap) |> tibble::rowid_to_column('facet_id')
      data <- data |> dplyr::inner_join(facets, dplyr::join_by({{facet_wrap}} == wrap))
    } else { # all three missing
      wrap <- NULL
      facets <- NULL
    }
    cols <- NULL
    rows <- NULL
  } else { # `facet_col` and/or `facet_row` given
    if (! missing(facet_wrap) && ! pggf_is_null(substitute(facet_wrap))) { # `facet_wrap` also given
      cli::cli_abort('{.arg facet_wrap} cannot be specified together with {.arg facet_col} or {.arg facet_row}')
    } else { # `facet_wrap` not given
      wrap <- NULL
      if (! missing(facet_col) && ! pggf_is_null(substitute(facet_col))) {
        cols <- data |> dplyr::pull({{facet_col}}) |> unique() |> sort()
      } else {
        cols <- NULL
      }
      if (! missing(facet_row) && ! pggf_is_null(substitute(facet_row))) {
        rows <- data |> dplyr::pull({{facet_row}}) |> unique() |> sort()
      } else {
        rows <- NULL
      }
      facets <- tidyr::expand_grid('row' = rows, 'col' = cols) |> tibble::rowid_to_column('facet_id')
      if (is.null(rows)) { # only `facet_col` given
        data <- data |> dplyr::inner_join(facets, dplyr::join_by({{facet_col}} == col))
      } else if (is.null(cols)) { # only `facet_row` given
        data <- data |> dplyr::inner_join(facets, dplyr::join_by({{facet_row}} == row))
      } else { # both given
        data <- data |> dplyr::inner_join(facets, dplyr::join_by({{facet_col}} == col, {{facet_row}} == row))
      }
    }
  }
  
  # store supplied min/max values
  if (! is.null(xmin)) {
    if (is.numeric(xmin) && length(xmin) %in% c(1, length(cols))) {
      xmin <- tibble::tibble('col' = cols, 'x' = xmin)
    } else {
      cli::cli_abort(
        paste0(
          '{.arg xmin} must be a {.cls numeric} vector of length 1',
          ifelse(is.null(cols), '', ' or {length(cols)} (number of facet columns)')
        )
      )
    }
  }
  if (! is.null(xmax)) {
    if (is.numeric(xmax) && length(xmax) %in% c(1, length(cols))) {
      xmax <- tibble::tibble('col' = cols, 'x' = xmax)
    } else {
      cli::cli_abort(
        paste0(
          '{.arg xmax} must be a {.cls numeric} vector of length 1',
          ifelse(is.null(cols), '', ' or {length(cols)} (number of facet columns)')
        )
      )
    }
  }
  extra_x <- dplyr::bind_rows(xmin, xmax)
  if (nrow(extra_x) == 0) {extra_x <- NULL}
  
  if (! is.null(ymin)) {
    if (is.numeric(ymin) && length(ymin) %in% c(1, length(rows))) {
      ymin <- tibble::tibble('row' = rows, 'y' = ymin)
    } else {
      cli::cli_abort(
        paste0(
          '{.arg ymin} must be a {.cls numeric} vector of length 1',
          ifelse(is.null(rows), '', ' or {length(rows)} (number of facet rows)')
        )
      )
    }
  }
  if (! is.null(ymax)) {
    if (is.numeric(ymax) && length(ymax) %in% c(1, length(rows))) {
      ymax <- tibble::tibble('row' = rows, 'y' = ymax)
    } else {
      cli::cli_abort(
        paste0(
          '{.arg ymax} must be a {.cls numeric} vector of length 1',
          ifelse(is.null(rows), '', ' or {length(rows)} (number of facet rows)')
        )
      )
    }
  }
  extra_y <- dplyr::bind_rows(ymin, ymax)
  if (nrow(extra_y) == 0) {extra_y <- NULL}
  
  if (is.null(facets)) {
    extra_coords <- dplyr::bind_cols(extra_x, extra_y)
    if (nrow(extra_coords) == 0) {extra_coords <- NULL}
  } else {
    if (is.null(extra_x)) {
      extra_coords <- facets
    } else if (is.null(cols)) {
      extra_coords <- facets |>
        dplyr::cross_join(extra_x)
    } else {
      extra_coords <- facets |>
        dplyr::inner_join(extra_x, by = 'col', relationship = 'many-to-many')
    }
    if (! is.null(extra_y)) {
      if (is.null(rows) || is.null(extra_y)) {
        extra_coords <- extra_coords |>
          dplyr::cross_join(extra_y)
      } else {
        extra_coords <- extra_coords |>
          dplyr::inner_join(extra_y, by = 'row', relationship = 'many-to-many')
      }
    }
    if ('col' %in% names(extra_coords)) {
      extra_coords <- extra_coords |>
        dplyr::rename({{facet_col}} := col)
    }
    if ('row' %in% names(extra_coords)) {
      extra_coords <- extra_coords |>
        dplyr::rename({{facet_row}} := row)
    }
  }
 
  # create a preview plot
  if (preview_plot) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(.data[[x_str]], .data[[y_str]])) + preview_theme
    if (! is.null(facets)) {
      if (! is.null(wrap)) {
        plot_facet_wrap <- pggf_sym_or_str(substitute(facet_wrap))
        plot <- plot + ggplot2::facet_wrap(ggplot2::vars({{plot_facet_wrap}}))
      } else {
        plot_facet_col <- pggf_sym_or_str(substitute(facet_col))
        plot_facet_row <- pggf_sym_or_str(substitute(facet_row))
        if (scales == 'square') {
          plot_scales <- 'fixed'
        } else {
          plot_scales <- scales
        }
        plot <- plot +
          ggplot2::facet_grid(
            cols = ggplot2::vars({{plot_facet_col}}),
            rows = ggplot2::vars({{plot_facet_row}}),
            scales = plot_scales,
            space = space
          )
      }
    }
  } else {
    plot <- NULL
  }
  
  # rename `x` and `y` columns
  data <- data |>
    dplyr::rename('x' = {{x}}, 'y' = {{y}})
  
  # convert non-numerical `x` and `y` data
  if (! 'x' %in% names(data) || is.numeric(data$x)) {
    numeric_x <- TRUE
  } else {
    if (scales == 'square') {
      cli::cli_abort('Option {.arg scales = "square"} is not compatible with non-numeric data')
    }
    
    numeric_x <- FALSE
    
    data$x <- as.ordered(data$x)
    
    if (is.null(facets) || ! is.null(wrap) || scales %in% c('fixed', 'free_y') || is.null(cols)) {
      data <- data |>
        dplyr::mutate(
          xticklabels = as.character(x),
          x = as.numeric(droplevels(x))
        )
    } else {
      data <- data |>
        dplyr::group_by({{facet_col}}) |>
        dplyr::mutate(
          xticklabels = as.character(x),
          x = as.numeric(droplevels(x))
        ) |>
        dplyr::ungroup()
    }
  }
  if (! 'y' %in% names(data) || is.numeric(data$y)) {
    numeric_y <- TRUE
  } else {
    if (scales == 'square') {
      cli::cli_abort('Option {.arg scales = "square"} is not compatible with non-numeric data')
    }
    
    numeric_y <- FALSE
    
    data$y <- as.ordered(data$y)
    
    if (is.null(facets) || ! is.null(wrap) || scales %in% c('fixed', 'free_x') || is.null(rows)) {
      data <- data |>
        dplyr::mutate(
          yticklabels = as.character(y),
          y = as.numeric(droplevels(y))
        )
    } else {
      data <- data |>
        dplyr::group_by({{facet_row}}) |>
        dplyr::mutate(
          yticklabels = as.character(y),
          y = as.numeric(droplevels(y))
        ) |>
        dplyr::ungroup()
    }
  }
  
  # generate LaTeX code
  if (print_code) {
    if (! is.null(wrap)) {
      code_wrap <- '\twrap cols = 3, % adjust to change number of columns'
    } else {
      code_wrap <- NULL
    }
    code <- c('\\begin{pggfplot}[%', code_wrap, paste0('\t% <pggfplot options>\n]{', filename, '}\n'), '\\end{pggfplot}')
  } else {
    code <- NULL
  }
  
  # set up `pggf` object
  pggf <- structure(
    list(
      data = data,
      facets = facets,
      facet_col = rlang::enquo(facet_col),
      facet_row = rlang::enquo(facet_row),
      facet_wrap = rlang::enquo(facet_wrap),
      scales = scales,
      space = space,
      enlarge_x = enlarge_x,
      enlarge_y = enlarge_y,
      error = list('pggf_x_err_minus' = 0, 'pggf_x_err_plus' = 0, 'pggf_y_err_minus' = 0, 'pggf_y_err_plus' = 0),
      extra_coords = extra_coords,
      numeric_x = numeric_x,
      numeric_y = numeric_y,
      x_str = x_str,
      y_str = y_str,
      plot = plot,
      code = code,
      filename = filename,
      out_dir = out_dir,
      files = NULL,
      stats = NULL,
      axes_extra = NULL
    ),
    class = 'pggf'
  )
  
  # call `pggf` object
  pggf
}


# "print" `pggf` objects
print.pggf <- function(pggf) {
  # check if there are any plot files
  if (is.null(pggf$files)) {
    cli::cli_abort('No plots added to {.fn pggfplot} yet.')
  } else {
    # export plot files
    for (file in names(pggf$files)) {
      readr::write_file(pggf$files[file], file.path(pggf$out_dir, file))
    }
  }
  
  # export stats
  if (! is.null(pggf$stats)) {
    readr::write_file(pggf$stats, file.path(pggf$out_dir, paste0(pggf$filename, '_stats.tsv')))
  }
  
  # export facet info
  if (! is.null(pggf$facets)) {
    readr::write_tsv(pggf$facets, file.path(pggf$out_dir, paste0(pggf$filename, '_facets.tsv')))
  }
  
  # calculate and export axis limits
  axis_limits <- pggf_get_axes(pggf)
  
  readr::write_tsv(axis_limits, file.path(pggf$out_dir, paste0(pggf$filename, '_axes.tsv')))
  
  # show preview plot
  if (! is.null(pggf$plot)) {
    # updata preview plot axis limits
    if (pggf$numeric_x) {
      if (pggf$scales %in% c('free_x', 'free') && nrow(axis_limits > 1) && 'col' %in% names(pggf$facets)) {
        axis_limits <- axis_limits |>
          tidyr::separate_longer_delim(facet_id, ',') |>
          dplyr::mutate(facet_id = as.numeric(facet_id)) |>
          dplyr::inner_join(pggf$facets, by = 'facet_id') |>
          dplyr::rename(!! pggf$facet_col := col)
      }
      pggf$plot <- pggf$plot + 
        ggplot2::expand_limits(axis_limits |> dplyr::select(-ymin, -ymax)) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = pggf$enlarge_x))
    }
    if (pggf$numeric_y) {
      if (pggf$scales %in% c('free_y', 'free') && nrow(axis_limits > 1) && 'row' %in% names(pggf$facets)) {
        if (! 'row' %in% names(axis_limits)) {
          axis_limits <- axis_limits |>
            tidyr::separate_longer_delim(facet_id, ',') |>
            dplyr::mutate(facet_id = as.numeric(facet_id)) |>
            dplyr::inner_join(pggf$facets, by = 'facet_id')
        }
        axis_limits <- axis_limits |>
          dplyr::rename(!! pggf$facet_row := row)
      }
      pggf$plot <- pggf$plot +
        ggplot2::expand_limits(axis_limits |> dplyr::select(-xmin, -xmax)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = pggf$enlarge_y))
    }
    print(pggf$plot)
  }
  
  # print LaTeX code
  if (! is.null(pggf$code)) {
    # add x and y labels
    pggf$code <- append(pggf$code, paste0(c('\txlabel = ', '\tylabel = '), c(pggf$x_str, pggf$y_str), ','), after = 1)
    cat(writeLines(pggf$code))
  }
  
  # return `pggf` object invisibly
  invisible(pggf)
}


# calculate axis limits
pggf_get_axes <- function(pggf) {
  # prepare data
  axis_limits <- pggf$data |>
    dplyr::bind_cols(pggf$error) |>
    dplyr::bind_rows(pggf$extra_coords) |>
    dplyr::mutate(
      dplyr::across(dplyr::matches('pggf_._err_.*'), ~ tidyr::replace_na(.x, 0))
    )
  
  # add all possible facets
  if (! is.null(pggf$facets) && ! 'wrap' %in% names(pggf$facets)) {
    if (! 'row' %in% names(pggf$facets)) { # only `facet_col` given
      has_cols <- TRUE
      has_rows <- FALSE
      axis_limits <- axis_limits |> dplyr::full_join(pggf$facets, dplyr::join_by(facet_id, !! pggf$facet_col == col))
    } else if (! 'col' %in% names(pggf$facets)) { # only `facet_row` given
      has_cols <- FALSE
      has_rows <- TRUE
      axis_limits <- axis_limits |> dplyr::full_join(pggf$facets, dplyr::join_by(facet_id, !! pggf$facet_row == row))
    } else { # both given
      has_cols <- TRUE
      has_rows <- TRUE
      axis_limits <- axis_limits |> dplyr::full_join(pggf$facets, dplyr::join_by(facet_id, !! pggf$facet_col == col, !! pggf$facet_row == row))
    }
  } else {
    has_cols <- FALSE
    has_rows <- FALSE
  }
  
  # get x limits (and labels for non-numeric data)
  if (! is.null(pggf$facets) && has_cols && pggf$scales %in% c('free_x', 'free')) {
    axis_limits <- axis_limits |>
      dplyr::group_by(!! pggf$facet_col)
  }
  
  axis_limits <- axis_limits |>
    dplyr::mutate(
      dplyr::across(x, list('min' = ~ min(.x - pggf_x_err_minus, na.rm = TRUE), 'max' = ~ max(.x + pggf_x_err_plus, na.rm = TRUE)), .names = '{.col}{.fn}'),
      dplyr::across(dplyr::any_of('xticklabels'), ~ paste0(unique(.x[order(x)]), collapse = ','))
    ) |>
    dplyr::ungroup()
  
  # get y limits (and labels for non-numeric data)
  if (! is.null(pggf$facets) && has_rows && pggf$scales %in% c('free_y', 'free')) {
    axis_limits <- axis_limits |>
      dplyr::group_by(!! pggf$facet_row)
  }
  
  axis_limits <- axis_limits |>
    dplyr::mutate(
      dplyr::across(y, list('min' = ~ min(.x - pggf_y_err_minus, na.rm = TRUE), 'max' = ~ max(.x + pggf_y_err_plus, na.rm = TRUE)), .names = '{.col}{.fn}'),
      dplyr::across(dplyr::any_of('yticklabels'), ~ paste0(unique(.x[order(y)]), collapse = ','))
    ) |>
    dplyr::ungroup()
  
  # select distinct rows
  axis_limits <- axis_limits |>
    dplyr::distinct(xmin, xmax, ymin, ymax, dplyr::pick(dplyr::any_of(c('xticklabels', 'yticklabels', 'facet_id'))))
  
  # get x and y ticks (only for non-numeric data)
  if (! pggf$numeric_x) {
    axis_limits <- axis_limits |>
      dplyr::rowwise() |>
      dplyr::mutate(xtick = paste0(seq(xmin, xmax), collapse = ',')) |>
      dplyr::ungroup()
  }
  if (! pggf$numeric_y) {
    axis_limits <- axis_limits |>
      dplyr::rowwise() |>
      dplyr::mutate(ytick = paste0(seq(ymin, ymax), collapse = ',')) |>
      dplyr::ungroup()
  }
  
  # add extra columns to axis limits
  if (! is.null(pggf$axes_extra)) {
    if ('facet_id' %in% names(axis_limits)) {
      axis_limits <- axis_limits |>
        dplyr::inner_join(
          pggf$axes_extra,
          by = 'facet_id'
        )
    } else {
      axis_limits <- axis_limits |>
        dplyr::bind_cols(pggf$axes_extra)
    }
  }
  
  # enforce equal x and y axis limits for `scales = "square"`
  if (pggf$scales == 'square') {
    axis_limits <- axis_limits |>
      dplyr::mutate(
        xmin = min(xmin, ymin),
        xmax = max(xmax, ymax),
        ymin = xmin,
        ymax = xmax,
      )
  }
  
  # enlarge axis limits
  if (pggf$numeric_x) {
    axis_limits <- axis_limits |>
      dplyr::mutate(
        xdiff = xmax - xmin,
        xmin = xmin - pggf$enlarge_x[1] * xdiff,
        xmax = xmax + pggf$enlarge_x[2] * xdiff
      )
  } else {
    axis_limits <- axis_limits |>
      dplyr::mutate(
        xmin = xmin - 0.5,
        xmax = xmax + 0.5,
      )
  }
  if (pggf$numeric_y) {
    axis_limits <- axis_limits |>
      dplyr::mutate(
        ydiff = ymax - ymin,
        ymin = ymin - pggf$enlarge_y[1] * ydiff,
        ymax = ymax + pggf$enlarge_y[2] * ydiff
      )
  } else {
    axis_limits <- axis_limits |>
      dplyr::mutate(
        ymin = ymin - 0.5,
        ymax = ymax + 0.5,
      )
  }
  
  axis_limits <- axis_limits |>
    dplyr::select(-dplyr::ends_with('diff'))
  
  # scale axes relative to their limits
  if (has_cols && pggf$space %in% c('free_x', 'free') && pggf$scales %in% c('free_x', 'free')) {
    axis_limits <- axis_limits |>
      dplyr::mutate(
        `scale facet width` = (xmax - xmin) / sum(xmax - xmin) * dplyr::n()
      )
  }
  if (has_rows && pggf$space %in% c('free_y', 'free') && pggf$scales %in% c('free_y', 'free')) {
    axis_limits <- axis_limits |>
      dplyr::mutate(
        `scale facet height` = (ymax - ymin) / sum(ymax - ymin) * dplyr::n()
      )
  }
  
  # collapse identical rows
  axis_limits <- axis_limits |>
    dplyr::group_by(dplyr::pick(-dplyr::any_of('facet_id'))) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of('facet_id'), ~ paste0(sort(unique(.x)), collapse = ',')),
      .groups = 'drop'
    )
  
  if (nrow(axis_limits) == 1 && 'facet_id' %in% names(axis_limits)) {
    axis_limits <- axis_limits |> dplyr::select(-facet_id)
  } else {
    axis_limits <- axis_limits |> dplyr::arrange(dplyr::pick(dplyr::any_of('facet_id')))
  }
  
  # return axis limits
  return(axis_limits)
}


# test if an object is of class `pggf`
assert_pggf <- function(pggf) {
  if (! inherits(pggf, 'pggf')) {
    cli::cli_abort('{.arg pggf} must be a {.cls pggf} object')
  }
}


# helper to use quoted or unquoted arguments to select columns for ggplot
pggf_sym_or_str <- function(x) {
  if (is.symbol(x)) {
    return(x)
  } else if (is.null(x)) {
    return(NULL)
  } else if (rlang::is_quosure(x)) {
    x <- rlang::as_name(x)
    return(rlang::quo(.data[[x]]))
  } else {
    return(rlang::quo(.data[[x]]))
  }
}


# helper to test if argument is null
pggf_is_null <- function(x) {
  return(! is.symbol(substitute(x)) && is.null(x) || (rlang::is_quosure(x) && rlang::quo_is_null(x)))
}


# stats
pggf_stats <- function(pggf, fns) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # test if `pggf_stats` has already been call before
  if (! is.null(pggf$stats)) {
    cli::cli_abort(
      c('{.fn pggf_stats} can only be used once per {.fn pggfplot}'),
      i = 'Multiple stats can be combined in {.arg fns}; e.g. "c(!! pggf_stat_fns$correlation, !! pggf_stat_fns$trendline, \'sum\' = ~ sum(x))"')
  }
  
  # test if `fns` is given
  if (missing(fns) || pggf_is_null(substitute(fns))) {
    cli::cli_abort('argument {.arg fns} is missing, with no default')
  }
  
  # calculate stats
  fns <- rlang::enquo(fns)
  
  stats <- pggf$data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'))) |>
    dplyr::summarise(
      dplyr::across(.cols = 1, .fns = !! fns, .names = '{.fn}'),
      .groups = 'drop'
    )
    
  # save stats
  pggf$stats <- readr::format_tsv(stats)
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    plot_stats <- stats |>
      tidyr::pivot_longer(
        -dplyr::any_of('facet_id')
      ) |>
      dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'))) |>
      dplyr::summarise(
        label = paste(paste0(' ', name), round(value, 2), sep = ' = ', collapse = '\n'),
        .groups = 'drop'
      )
    
    if (! is.null(pggf$facets)) {
      plot_stats <- plot_stats |>
        dplyr::inner_join(pggf$facets, by = 'facet_id')
      
      if ('wrap' %in% names(plot_stats)) {
        plot_stats <- plot_stats |> dplyr::rename(!! pggf$facet_wrap := wrap)
      } else {
        if ('col' %in% names(plot_stats)) {plot_stats <- plot_stats |> dplyr::rename(!! pggf$facet_col := col)}
        if ('row' %in% names(plot_stats)) {plot_stats <- plot_stats |> dplyr::rename(!! pggf$facet_row := row)}
      }
    }
    
    pggf$plot <- pggf$plot + ggplot2::geom_text(data = plot_stats, mapping = ggplot2::aes(label = label), x = -Inf, y = Inf, hjust = 0, vjust = 1.05)
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    stat_cols <- stats |>
      dplyr::select(-dplyr::any_of('facet_id')) |>
      names() |>
      paste0(collapse = ',')
    
    pggf$code <- append(pggf$code, paste0('\t\\pggf_stats(stats = {', stat_cols, '})\n'), after = length(pggf$code) - 1)
  }
  
  # call the new `pggf` object
  pggf
}

# pre-defined stat functions
pggf_stat_fns <- list(
  'n' = rlang::quo(c('n' = ~ dplyr::n())),
  'correlation' = rlang::quo(
    c('n' = ~ dplyr::n(), 'pearson' = ~ cor(x, y), 'rsquare' = ~ cor(x, y)^2, 'spearman' = ~ cor(x, y, method = 'spearman'))
  ),
  'trendline' = rlang::quo(
    c('slope' = ~ coef(lm(y ~ x))[2], 'intercept' = ~ coef(lm(y ~ x))[1], 'goodness of fit' = ~ summary(lm(y ~ x))$r.squared)
  )
)


# scatter plot
pggf_scatter <- function(pggf, color = NULL, shuffle = TRUE, split = NULL, extra_cols = NULL, y_error = NULL, x_error = NULL) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # prepare `data`
  data <- pggf$data |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::any_of('facet_id'), x, y, {{color}}, {{split}}, {{extra_cols}}, {{y_error}}, {{x_error}})
  
  # save error data for axis limit calculations
  if (! missing(y_error) && ! pggf_is_null(substitute(y_error))) {
    y_error_data <- data |>
      dplyr::select({{y_error}})
    
    if (ncol(y_error_data) %in% c(1, 2)) {
      pggf$error[c(3, 4)] <- y_error_data
    } else {
      cli::cli_abort('{.arg y_error} must be of length 1 or 2')
    }
  }
  if (! missing(x_error) && ! pggf_is_null(substitute(x_error))) {
    x_error_data <- data |>
      dplyr::select({{x_error}})
    
    if (ncol(x_error_data) %in% c(1, 2)) {
      pggf$error[c(1, 2)] <- x_error_data
    } else {
      cli::cli_abort('{.arg x_error} must be of length 1 or 2')
    }
  }
  
  # shuffle data
  if (shuffle) {
    data <- data |>
      dplyr::slice_sample(prop = 1)
  }
  
  # split (optional) and export data
  files <- data |>
    dplyr::arrange(dplyr::pick(dplyr::any_of('facet_id'))) |>
    dplyr::mutate('.file_basename' = pggf$filename, '.file_suffix' = 'scatter.tsv') |>
    tidyr::unite('file', .file_basename, {{split}}, .file_suffix, remove = FALSE) |>
    dplyr::select(-c(.file_basename, .file_suffix)) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file(s) to pggf object
  pggf$files <- c(pggf$files, files)
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (! (missing(color) || pggf_is_null(substitute(color))) && ! (missing(split) || pggf_is_null(substitute(split)))) {
      plot_color <- pggf_sym_or_str(substitute(color))
      plot_split <- pggf_sym_or_str(substitute(split))
      pggf$plot <- pggf$plot + ggplot2::geom_point(mapping = ggplot2::aes(color = {{plot_color}}, shape = {{plot_split}}))
    } else if (! missing(color) && ! pggf_is_null(substitute(color))) {
      plot_color <- pggf_sym_or_str(substitute(color))
      pggf$plot <- pggf$plot + ggplot2::geom_point(mapping = ggplot2::aes(color = {{plot_color}}))
    } else if (! missing(split) && ! pggf_is_null(substitute(split))) {
      plot_split <- pggf_sym_or_str(substitute(split))
      pggf$plot <- pggf$plot + ggplot2::geom_point(mapping = ggplot2::aes(shape = {{plot_split}}))
    } else {
      pggf$plot <- pggf$plot + ggplot2::geom_point()
    }
    if (! missing(y_error) && ! pggf_is_null(substitute(y_error))) {
      plot_y_err <- names(y_error_data)
      if (length(plot_y_err) == 1) {
        plot_y_err[2] <- plot_y_err
      }
      pggf$plot <- pggf$plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[pggf$y_str]] - .data[[plot_y_err[1]]], ymax = .data[[pggf$y_str]] + .data[[plot_y_err[2]]]), width = 0.1)
    }
    if (! missing(x_error) && ! pggf_is_null(substitute(x_error))) {
      plot_x_err <- names(x_error_data)
      if (length(plot_x_err) == 1) {
        plot_x_err[2] <- plot_x_err
      }
      pggf$plot <- pggf$plot + ggplot2::geom_errorbar(ggplot2::aes(xmin = .data[[pggf$x_str]] - .data[[plot_x_err[1]]], xmax = .data[[pggf$x_str]] + .data[[plot_x_err[2]]]), width = 0.1)
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    if (! missing(y_error) && ! pggf_is_null(substitute(y_error))) {
      if (length(names(y_error_data)) == 1) {
        code_y_err <- paste0('y error = {', names(y_error_data), '}')
      } else {
        code_y_err <- paste0('y error* = {', names(y_error_data)[1], '}{', names(y_error_data)[2], '}')
      }
    } else {
      code_y_err <- NULL
    }
    if (! missing(x_error) && ! pggf_is_null(substitute(x_error))) {
      if (length(names(x_error_data)) == 1) {
        code_x_err <- paste0('x error = {', names(x_error_data), '}')
      } else {
        code_x_err <- paste0('x error* = {', names(x_error_data)[1], '}{', names(x_error_data)[2], '}')
      }
    } else {
      code_x_err <- NULL
    }
    
    if (! missing(color) && ! pggf_is_null(substitute(color))) {
      colors <- data |> dplyr::distinct(dplyr::pick({{color}})) |> dplyr::pull() |> sort() |> paste0(collapse = ',')
      color_str <- if (is.symbol(substitute(color))) {deparse(substitute(color))} else {as.character(color)}
      code_color <- paste0('style from column = ', color_str)
      pggf$code <- append(
        pggf$code,
        paste0('\tscatter styles = {', colors, '},'),
        after = 1
      )
    } else {
      code_color <- NULL
    }
    
    code_extras <- paste0(c(code_x_err, code_y_err, code_color), collapse = ', ')
    
    if (! missing(split) && ! pggf_is_null(substitute(split))) {
      if (code_extras != '') {
        code_extras <- paste0(', ', code_extras)
      }
      code_split <- paste0('data = ', gsub('_scatter.tsv', '', names(files)), code_extras)
      pggf$code <- append(
        pggf$code,
        c(paste0('\t\\pggf_scatter(', code_split, ')'), '\t'),
        after = length(pggf$code) - 1
      )
    } else {
      pggf$code <- append(
        pggf$code,
        paste0('\t\\pggf_scatter(', code_extras, ')\n'),
        after = length(pggf$code) - 1
      )
    }
    
    if (! is.null(code_color)) {
      pggf$code <- append(pggf$code, paste0('\t\\pggf_annotate(scatter legend)\n'), after = length(pggf$code) - 1)
    }
  }
  
  # call the new `pggf` object
  pggf
}


# box plots
pggf_boxplot <- function(pggf, group = NULL, width = 'fixed', orientation = 'x',
                         extra_cols = NULL, points_below = pggf_defaults$points_below,
                         type = 'box', trim = pggf_defaults$trim, scale = 'area',
                         samples = pggf_defaults$samples) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # get orientation
  if (orientation %in% c('x', 'v', 'vertical')) {
    orientation <- 'x'
    if (! pggf$numeric_y) {
      cli::cli_abort('{.arg y} must be numeric for {.arg orientation = "x"}')
    }
    sample_cols <- c('x', 'xticklabels')
    value_col <- 'y'
  } else if (orientation %in% c('y', 'h', 'horizontal')) {
    orientation <- 'y'
    if (! pggf$numeric_x) {
      cli::cli_abort('{.arg x} must be numeric for {.arg orientation = "y"}')
    }
    sample_cols <- c('y', 'yticklabels')
    value_col <- 'x'
  } else {
    cli::cli_abort(c('{.arg orientation} must be "x" or "y"', i = 'You can also use "v" or "vertical" for "x" and "h" or "horizontal" for "y"'))
  }
  
  # prepare `data`
  box_data <- pggf$data |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across({{group}}, as.ordered)
    )
  
  # check if individual points should be drawn
  show_points <- box_data |>
    dplyr::count(dplyr::pick(dplyr::any_of(c('facet_id', sample_cols)), {{group}})) |>
    dplyr::summarise(any(n < points_below), .groups = 'drop') |>
    dplyr::pull()
  
  # scale box width relative to the group size
  if (! missing(group) && ! pggf_is_null(substitute(group))) {
    if (width == 'fixed') {
      box_data <- box_data |>
        dplyr::mutate(
          group_n = dplyr::n_distinct(pick({{group}}))
        )
    } else if (width == 'free') {
      box_data <- box_data |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(c('facet_id', sample_cols)))) |>
        dplyr::mutate(
          group_n = dplyr::n_distinct(pick({{group}}))
        ) |>
        dplyr::ungroup()
    } else {
      cli::cli_abort('{.arg width} must be "fixed" or "free"')
    }
    
    box_data <- box_data |>
      dplyr::mutate(
        box_sep = dplyr::if_else(group_n == 1, 0, 0.1 * sqrt(group_n - 1) / (group_n - 1)),
        box_width = (1 - (group_n - 1) * box_sep) / group_n,
        across({{group}}, ~ 0.5 * box_width + (as.numeric(.x) - 1) * (box_width + box_sep) - 0.5, .names = 'box_shift')
      )
  } else {
    box_data <- box_data |>
      dplyr::mutate(
        box_width = 1,
        box_shift = 0
      )
  }
  
  # calculate box plot metrics
  box_metrics <- box_data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of(c('facet_id', sample_cols)), {{group}}, box_width, box_shift)) |>
    dplyr::summarise(
      n = dplyr::n(),
      med = median(.data[[value_col]], na.rm = TRUE),
      av = mean(.data[[value_col]], na.rm = TRUE),
      lq = quantile(.data[[value_col]], 0.25, na.rm = TRUE),
      uq = quantile(.data[[value_col]], 0.75, na.rm = TRUE),
      iqr = 1.5 * IQR(.data[[value_col]], na.rm = TRUE),
      lw = max(lq - iqr, min(.data[[value_col]][pggf_outliers(.data[[value_col]]) == 0])),
      uw = min(uq + iqr, max(.data[[value_col]][pggf_outliers(.data[[value_col]]) == 0])),
      min = min(.data[[value_col]]),
      max = max(.data[[value_col]]),
      outliers = sum(pggf_outliers(.data[[value_col]]) != 0),
      dplyr::across(
        {{extra_cols}},
        ~ if (length(unique(.x)) == 1) {
          unique(.x)
        } else {
          cli::cli_warn('Non-unique values in {.arg extra_cols}')
          paste0(.x, collapse = ',')
        }
      ),
      .groups = 'drop'
    ) |>
    dplyr::select(-c(iqr)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::pick(dplyr::any_of(c('facet_id', orientation)), {{group}})) |>
    dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'))) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(orientation), dplyr::n_distinct, .names = 'n_samples'),
      sample_id = seq_len(dplyr::n())
    ) |>
    dplyr::ungroup()
  
  if (show_points) {
    box_metrics$outliers <- -1
  }
  
  # export data
  box_file <- box_metrics |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_boxplot.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()

  # add file to pggf object
  pggf$files <- c(pggf$files, box_file)
  
  # create files for violin or for points/outliers
  if (type == 'violin') {
    # prepare `data` for violin plots
    sample_ids <- box_metrics |>
      dplyr::select(dplyr::any_of(c('sample_id', 'facet_id', orientation)), {{group}})
    
    violin_data <- pggf$data |>
      dplyr::mutate(
        dplyr::across({{group}}, as.ordered)
      ) |>
      inner_join(sample_ids, by = names(sample_ids)[-1])
    
    # calculate density
    violin_data <- violin_data |>
      ungroup() |>
      dplyr::nest_by(dplyr::pick(dplyr::any_of(c('facet_id', 'sample_id')), {{group}})) |>
      dplyr::reframe(
        density = dplyr::if_else(
          trim,
          list(density(data[[value_col]], from = min(data[[value_col]]), to = max(data[[value_col]]), n = {{samples}})),
          list(density(data[[value_col]], n = {{samples}}))
        ),
        x = density[[value_col]],
        y = density[[orientation]],
        n = length(data$y)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-density)
    
    # scale width
    if (scale == 'area') {
      violin_data <- violin_data |>
        dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'))) |>
        dplyr::mutate(
          {{orientation}} := 0.5 * .data[[orientation]] / max(.data[[orientation]])
        ) |>
        dplyr::ungroup()
    } else if (scale == 'width') {
      violin_data <- violin_data |>
        dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id')), sample_id, {{group}}) |>
        dplyr::mutate(
          {{orientation}} := 0.5 * .data[[orientation]] / max(.data[[orientation]])
        ) |>
        dplyr::ungroup()
    } else if (scale == 'count') {
      violin_data <- violin_data |>
        dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'))) |>
        dplyr::mutate(
          {{orientation}} := 0.5 * (.data[[orientation]] * n) / (max(.data[[orientation]] * n))
        ) |>
        dplyr::ungroup()
    } else {
      cli::cli_abort('{.arg scale} must be one of "area", "width", or "count"')
    }
    
    violin_data <- violin_data  |>
      dplyr::select(-n) |>
      dplyr::arrange(dplyr::pick(dplyr::any_of('facet_id')), sample_id, {{group}})
    
    # export data
    violin_file <- violin_data |>
      dplyr::mutate(
        file = paste0(pggf$filename, '_violin.tsv')
      ) |>
      dplyr::nest_by(file) |>
      dplyr::summarise(
        content = readr::format_tsv(data),
        .groups = 'drop'
      ) |>
      tibble::deframe()
    
    # add file to pggf object
    pggf$files <- c(pggf$files, violin_file)
  } else if (show_points || any(box_metrics$outliers > 0)) {
    # create file for points or outliers
    outliers <- box_data |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::any_of(c('facet_id', sample_cols, value_col)), box_shift, {{group}}, {{extra_cols}})
    
    if (! show_points) {
      outliers <- outliers |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(c('facet_id', sample_cols)), {{group}})) |>
        dplyr::filter(pggf_outliers(.data[[value_col]]) != 0) |>
        dplyr::ungroup()
    }
  
    if (missing(group) || pggf_is_null(substitute(group))) {
      if ('facet_id' %in% names(box_metrics)) {
        outliers <- outliers |>
          dplyr::inner_join(
            box_metrics |> dplyr::select(dplyr::any_of(c('facet_id', orientation)), sample_id),
            dplyr::join_by('facet_id', {{orientation}})
          )
      } else {
        outliers <- outliers |>
          dplyr::inner_join(
            box_metrics |> dplyr::select(dplyr::any_of(c('facet_id', orientation)), sample_id),
            dplyr::join_by({{orientation}})
          ) 
      }
    } else {
      if ('facet_id' %in% names(box_metrics)) {
        outliers <- outliers |>
          dplyr::inner_join(
            box_metrics |> dplyr::select(dplyr::any_of(c('facet_id', orientation)), {{group}}, sample_id),
            dplyr::join_by('facet_id', {{orientation}}, {{group}})
          )
      } else {
        outliers <- outliers |>
          dplyr::inner_join(
            box_metrics |> dplyr::select(dplyr::any_of(c('facet_id', orientation)), {{group}}, sample_id),
            dplyr::join_by({{orientation}}, {{group}})
          ) 
      }
    }
    
    outliers <- outliers |>
      dplyr::mutate(
        file = paste0(pggf$filename, '_outlier.tsv')
      ) |>
      dplyr::nest_by(file) |>
      dplyr::summarise(
        content = readr::format_tsv(data),
        .groups = 'drop'
      ) |>
      tibble::deframe()

    pggf$files <- c(pggf$files, outliers)
  }
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (type == 'violin') {
      plot_width <- 0.1
    } else {
      plot_width <- 0.75
    }
    
    if (missing(group) || pggf_is_null(substitute(group))) {
      if (type == 'violin') {
        pggf$plot <- pggf$plot +
          ggplot2::geom_violin(
            mapping = ggplot2::aes(group = .data[[pggf[[paste0(orientation, '_str')]]]]),
            orientation = orientation,
            scale = scale,
            trim = trim
          )
      }
      pggf$plot <- pggf$plot +
        ggplot2::geom_boxplot(
          mapping = ggplot2::aes(group = .data[[pggf[[paste0(orientation, '_str')]]]]),
          orientation = orientation,
          outlier.shape = ifelse(show_points || type == 'violin', NA, 19),
          width = plot_width
        ) +
        stat_summary(
          fun.data = pggf_sample_size,
          geom = 'text'
        )
    } else {
      plot_group <- pggf_sym_or_str(substitute(group))
      if (type == 'violin') {
        pggf$plot <- pggf$plot +
          ggplot2::geom_violin(
            mapping = ggplot2::aes(group = paste0(.data[[pggf[[paste0(orientation, '_str')]]]], {{plot_group}}), fill = as.ordered({{plot_group}})),
            orientation = orientation,
            scale = scale,
            trim = trim
          )
      }
      pggf$plot <- pggf$plot +
        ggplot2::geom_boxplot(
          mapping = ggplot2::aes(group = paste0(.data[[pggf[[paste0(orientation, '_str')]]]], {{plot_group}}), fill = as.ordered({{plot_group}})),
          orientation = orientation,
          outlier.shape = ifelse(show_points || type == 'violin', NA, 19),
          width = plot_width,
          position = ggplot2::position_dodge(0.9)
        ) +
        stat_summary(
          fun.data = pggf_sample_size,
          ggplot2::aes(color = as.ordered({{plot_group}})),
          geom = 'text',
          position = ggplot2::position_dodge(0.9)
        )
    }
    if (show_points) {
      pggf$plot <- pggf$plot + ggplot2::geom_jitter(width = 0.3)
    }
  }

  # add to code
  if (! is.null(pggf$code)) {
    pggf$code <- append(
      pggf$code,
      paste0('\tenlarge ', ifelse(orientation == 'x', 'y', 'x'), ' limits = {lower, value = 0.1}, % increase axis limits to fit sample sizes'),
      after = 1
    )
    pggf$code <- append(pggf$code, paste0('\t\\pggf_', if_else(type == 'violin', 'violin', 'boxplot'), '()\n'), after = length(pggf$code) - 1)
  }
  
  # call the new `pggf` object
  pggf
}


# find outliers
pggf_outliers <- function(data) {
  dplyr::case_when(
    data > quantile(data, 0.75, na.rm = TRUE) + 1.5 * IQR(data, na.rm = TRUE) ~ 1,
    data < quantile(data, 0.25, na.rm = TRUE) - 1.5 * IQR(data, na.rm = TRUE) ~ -1,
    TRUE ~ 0
  )
}

# display sample size on ggplot
pggf_sample_size <- function(x, y = -Inf, vjust = -1, size = 8/.pt){
  return(c(y = y, vjust = vjust, size = size, label = length(x)))
}


# violin plots
pggf_violin <- function(pggf, group = NULL, orientation = 'x', extra_cols = NULL,
                        width = 'fixed', trim = pggf_defaults$trim, scale = 'area',
                        samples = pggf_defaults$samples) {
  # check arguments
  if (! is.logical(trim) || length(trim) != 1) {
    cli::cli_abort('{.arg trim} must be a single logical value.')
  }
  if (! is.numeric(samples) || length(samples) != 1) {
    cli::cli_abort('{.arg samples} must be a single number.')
  }
  
  # call pggf_boxplot
  pggf <- rlang::inject(
    pggf_boxplot(
      pggf,
      group = {{group}},
      orientation = orientation,
      extra_cols = {{extra_cols}},
      width = width,
      type = 'violin',
      points_below = 0,
      trim = trim,
      scale = scale,
      samples = samples
    )
  )
  
  # call the new `pggf` object
  pggf
}


# horizontal lines
pggf_hline <- function(pggf, fn, group = NULL) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # test if `fn` is given
  if (missing(fn) || pggf_is_null(substitute(fn))) {
    cli::cli_abort('argument {.arg fn} is missing, with no default')
  }
  
  # calculate intercepts
  intercepts <- pggf$data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'), !! pggf$facet_col, !! pggf$facet_row, !! pggf$facet_wrap, {{group}})) |>
    dplyr::summarise(
      intercept = {{fn}},
      .groups = 'drop'
    ) |>
    tidyr::unnest_longer(intercept)
  
  # export data
  intercept_file <- intercepts |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_hline.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file to pggf object
  pggf$files <- c(pggf$files, intercept_file)
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$plot <- pggf$plot + ggplot2::geom_hline(data = intercepts, mapping = ggplot2::aes(yintercept = intercept))
    } else {
      plot_group <- pggf_sym_or_str(substitute(group))
      pggf$plot <- pggf$plot + ggplot2::geom_hline(data = intercepts, mapping = ggplot2::aes(yintercept = intercept, color = {{plot_group}}))
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$code <- append(pggf$code, '\t\\pggf_hline()\n', after = length(pggf$code) - 1)
    } else {
      group_str <- if (is.symbol(substitute(group))) {deparse(substitute(group))} else {as.character(group)}
      pggf$code <- append(pggf$code, paste0('\t\\pggf_hline(style from column = ', group_str, ')\n'), after = length(pggf$code) - 1) 
    }
  }
  
  # call the new `pggf` object
  pggf
}


# vertical lines
pggf_vline <- function(pggf, fn, group = NULL) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # test if `fn` is given
  if (missing(fn) || pggf_is_null(substitute(fn))) {
    cli::cli_abort('argument {.arg fn} is missing, with no default')
  }
  
  # calculate intercepts
  intercepts <- pggf$data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'), !! pggf$facet_col, !! pggf$facet_row, !! pggf$facet_wrap, {{group}})) |>
    dplyr::summarise(
      intercept = {{fn}},
      .groups = 'drop'
    ) |>
    tidyr::unnest_longer(intercept)
  
  # export data
  intercept_file <- intercepts |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_vline.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file to pggf object
  pggf$files <- c(pggf$files, intercept_file)
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$plot <- pggf$plot + ggplot2::geom_vline(data = intercepts, mapping = ggplot2::aes(xintercept = intercept))
    } else {
      plot_group <- pggf_sym_or_str(substitute(group))
      pggf$plot <- pggf$plot + ggplot2::geom_vline(data = intercepts, mapping = ggplot2::aes(xintercept = intercept, color = {{plot_group}}))
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$code <- append(pggf$code, '\t\\pggf_vline()\n', after = length(pggf$code) - 1)
    } else {
      group_str <- if (is.symbol(substitute(group))) {deparse(substitute(group))} else {as.character(group)}
      pggf$code <- append(pggf$code, paste0('\t\\pggf_vline(style from column = ', group_str, ')\n'), after = length(pggf$code) - 1) 
    }
  }
  
  # call the new `pggf` object
  pggf
}


# linear regression line
pggf_trendline <- function(pggf, group = NULL) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # calculate slope and intercept
  trendline <- pggf$data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'), !! pggf$facet_col, !! pggf$facet_row, !! pggf$facet_wrap, {{group}})) |>
    dplyr::summarise(
      slope = coef(lm(y ~ x))[2],
      intercept = coef(lm(y ~ x))[1],
      .groups = 'drop'
    )
  
  # export data
  trendline_file <- trendline |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_trendline.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file to pggf object
  pggf$files <- c(pggf$files, trendline_file)
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$plot <- pggf$plot + ggplot2::geom_smooth(method = 'lm', formula = 'y ~ x', se = FALSE)
    } else {
      plot_group <- pggf_sym_or_str(substitute(group))
      pggf$plot <- pggf$plot + ggplot2::geom_smooth(mapping = ggplot2::aes(color = {{plot_group}}), method = 'lm', formula = 'y ~ x', se = FALSE)
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$code <- append(pggf$code, '\t\\pggf_trendline()\n', after = length(pggf$code) - 1)
    } else {
      group_str <- if (is.symbol(substitute(group))) {deparse(substitute(group))} else {as.character(group)}
      pggf$code <- append(pggf$code, paste0('\t\\pggf_trendline(style from column = ', group_str, ')\n'), after = length(pggf$code) - 1) 
    }
  }
  
  # call the new `pggf` object
  pggf
}


# sloped lines
pggf_abline <- function(pggf, slope = 1, intercept = 0, group = NULL) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # calculate slope and/or intercept
  data_abline <- pggf$data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'), !! pggf$facet_col, !! pggf$facet_row, !! pggf$facet_wrap, {{group}})) |>
    dplyr::summarise(
      slope = {{slope}},
      intercept = {{intercept}},
      .groups = 'drop'
    )
  
  # export data
  abline_file <- data_abline |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_abline.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file to pggf object
  pggf$files <- c(pggf$files, abline_file)
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$plot <- pggf$plot + ggplot2::geom_abline(data = data_abline, mapping = ggplot2::aes(slope = slope, intercept = intercept))
    } else {
      plot_group <- pggf_sym_or_str(substitute(group))
      pggf$plot <- pggf$plot + ggplot2::geom_abline(data = data_abline, mapping = ggplot2::aes(slope = slope, intercept = intercept, color = {{plot_group}}))
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    if (missing(group) || pggf_is_null(substitute(group))) {
      pggf$code <- append(pggf$code, '\t\\pggf_abline()\n', after = length(pggf$code) - 1)
    } else {
      group_str <- if (is.symbol(substitute(group))) {deparse(substitute(group))} else {as.character(group)}
      pggf$code <- append(pggf$code, paste0('\t\\pggf_abline(style from column = ', group_str, ')\n'), after = length(pggf$code) - 1) 
    }
  }
  
  # call the new `pggf` object
  pggf
}


# quiver plot
pggf_quiver <- function(pggf, u = NULL, v = NULL, split = NULL, extra_cols = NULL) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # at least one of `u` and `v` is required
  if ((missing(u) || pggf_is_null(substitute(u))) && (missing(v) || pggf_is_null(substitute(v)))) {
    cli::cli_abort('At least one of {.arg u} or {.arg v} must be given')
  }
  
  # save `u`/`v` for axis limit calculations
  if (! missing(u) && ! pggf_is_null(substitute(u))) {
    u_data <- pggf$data |>
      dplyr::select({{u}})
    
    if (ncol(u_data) == 1) {
      pggf$error[1] <- -u_data
      pggf$error[2] <- u_data
    } else {
      cli::cli_abort('{.arg u} must be of length 1')
    }
  }
  if (! missing(v) && ! pggf_is_null(substitute(v))) {
    v_data <- data |>
      dplyr::select({{v}})
    
    if (ncol(v_data)  == 1) {
      pggf$error[3] <- -v_data
      pggf$error[4] <- v_data
    } else {
      cli::cli_abort('{.arg v} must be of length 1')
    }
  }
  
  # prepare `data`
  quiver_data <- pggf$data |>
    dplyr::select(dplyr::any_of('facet_id'), x, y, 'u' = {{u}}, 'v' = {{v}}, {{split}}, {{extra_cols}})
  
  if (missing(u) || pggf_is_null(substitute(u))) {
    quiver_data <- quiver_data |>
      dplyr::mutate(
        u = 0
      )
  }
  if (missing(v) || pggf_is_null(substitute(v))) {
    quiver_data <- quiver_data |>
      dplyr::mutate(
        v = 0
      )
  }

  # split (optional) and export data
  files <- quiver_data |>
    dplyr::arrange(dplyr::pick(dplyr::any_of('facet_id'))) |>
    dplyr::mutate('.file_basename' = pggf$filename, '.file_suffix' = 'quiver.tsv') |>
    tidyr::unite('file', .file_basename, {{split}}, .file_suffix, remove = FALSE) |>
    dplyr::select(-c(.file_basename, .file_suffix)) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file(s) to pggf object
  pggf$files <- c(pggf$files, files)
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (missing(u) || pggf_is_null(substitute(u))) {
      plot_u <- 0
    } else {
      plot_u <- pggf_sym_or_str(substitute(u))
    }
    if (missing(v) || pggf_is_null(substitute(v))) {
      plot_v <- 0
    } else {
      plot_v <- pggf_sym_or_str(substitute(v))
    }
    
    if (missing(split) || pggf_is_null(substitute(split))) {
      pggf$plot <- pggf$plot +
        ggplot2::geom_segment(
          ggplot2::aes(xend = .data[[pggf$x_str]] + {{plot_u}}, yend = .data[[pggf$y_str]] + {{plot_v}}),
          arrow = ggplot2::arrow(length = ggplot2::unit(0.15, 'cm'))
        )
    } else {
      plot_split <- pggf_sym_or_str(substitute(split))
      pggf$plot <- pggf$plot +
        ggplot2::geom_segment(
          ggplot2::aes(xend = .data[[pggf$x_str]] + {{plot_u}}, yend = .data[[pggf$y_str]] + {{plot_v}}, color = {{plot_split}}),
          arrow = ggplot2::arrow(length = ggplot2::unit(0.15, 'cm'))
        )
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    if (missing(split) || pggf_is_null(substitute(split))) {
      pggf$code <- append(
        pggf$code,
        paste0('\t\\pggf_quiver()\n'),
        after = length(pggf$code) - 1
      )
    } else {
      code_data <- paste0('data = ', gsub('_quiver.tsv', '', names(files)))
      pggf$code <- append(
        pggf$code,
        c(paste0('\t\\pggf_scatter(', code_data, ')'), '\t'),
        after = length(pggf$code) - 1
      )
    }
  }
  
  # call the new `pggf` object
  pggf
}

# hexbin plot
pggf_hexbin <- function(pggf, bins = 50, count = 'log2', colorbar = 'fixed') {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # helper for count functions
  count_fns <- c('raw' = identity, 'log2' = log2, 'log10' = log10)
  
  # check arguments
  if (! is.numeric(bins) || length(bins) != 1) {
    cli::cli_abort('{.arg bins} must be a {.cls numeric} vector of length 1')
  }
  if (! count %in% names(count_fns)) {
    cli::cli_abort('{.arg count} must be one of "raw", "log2", or "log10"')
  } else {
    count_fn <- count_fns[[count]]
  }
  if (! colorbar %in% c('fixed', 'free')) {
    cli::cli_abort('{.arg colorbar} must be "fixed" or "free"')
  }
  
  # get facet info
  if (! is.null(pggf$facets)) {
    has_cols <- ('col' %in% names(pggf$facets))
    has_rows <- ('row' %in% names(pggf$facets))
  } else {
    has_cols <- FALSE
    has_rows <- FALSE
  }
  
  # prepare `data`
  hexbin_data <- pggf$data |>
    dplyr::ungroup()
  
  # get x and y bounds
  if (pggf$scales == 'square') {
    hexbin_data <- hexbin_data |>
      dplyr::mutate(
        x_bounds = list(range(x, y)),
        y_bounds = x_bounds
      )
  } else {
    if (pggf$scales %in% c('free', 'free_x') && has_cols) {
      hexbin_data <- hexbin_data |>
        dplyr::group_by(!! pggf$facet_col) |>
        dplyr::mutate(
          x_bounds = list(range(x))
        ) |>
        dplyr::ungroup()
    } else {
      hexbin_data <- hexbin_data |>
        dplyr::mutate(
          x_bounds = list(range(x))
        )
    }
    if (pggf$scales %in% c('free', 'free_y') && has_rows) {
      hexbin_data <- hexbin_data |>
        dplyr::group_by(!! pggf$facet_row) |>
        dplyr::mutate(
          y_bounds = list(range(y))
        ) |>
        dplyr::ungroup()
    } else {
      hexbin_data <- hexbin_data |>
        dplyr::mutate(
          y_bounds = list(range(y))
        )
    }
  }
    
  # get hexbin coordinates
  hexbin_data <- hexbin_data |>
    dplyr::nest_by(dplyr::pick(dplyr::any_of('facet_id'), !! pggf$facet_col, !! pggf$facet_row)) |>
    dplyr::reframe(
      hexbin = list(
        hexbin::hexbin(
          x = data$x,
          y = data$y,
          xbins = bins,
          xbnds = range(data$x_bounds),
          ybnds = range(data$y_bounds)
        )
      ),
      tibble::as_tibble(hexbin::hcell2xy(hexbin)),
      count = slot(hexbin, 'count'),
      count = count_fn(count)
    ) |>
    dplyr::select(-hexbin)
  
  # export data
  hexbin_file <- hexbin_data |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_hexbin.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file to pggf object
  pggf$files <- c(pggf$files, hexbin_file)
  
  # export colorbar limits
  if (colorbar == 'fixed') {
    colorbar_limits <- hexbin_data |>
      dplyr::summarise(
        dplyr::across(dplyr::any_of('facet_id'), ~ paste0(unique(.x), collapse = ',')),
        dplyr::across(count, list('min' = min, 'max' = max), .names = 'point meta {.fn}'),
        .groups = 'drop'
      )
  } else {
    colorbar_limits <- hexbin_data |>
      dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'))) |>
      dplyr::summarise(
        dplyr::across(count, list('min' = min, 'max' = max), .names = 'point meta {.fn}'),
        .groups = 'drop'
      ) |>
      dplyr::ungroup()
  }
  
  if ('facet_id' %in% names(colorbar_limits)) {
    colorbar_limits <- colorbar_limits |>
      tidyr::separate_longer_delim(
        facet_id,
        delim = ','
      ) |>
      dplyr::mutate(
        facet_id = as.integer(facet_id)
      )
  }
  
  pggf$axes_extra <- colorbar_limits
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    pggf$plot <- pggf$plot +
      ggplot2::geom_hex(
        bins = bins
      ) +
      ggplot2::scale_fill_viridis_c(
        trans = ifelse(count == 'raw', 'identity', count)
      )
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    pggf$code <- append(
      pggf$code,
      paste0('\t', ifelse(colorbar == 'fixed', 'facet 1', 'every facet'), '/.append style = {pggf colorbar = {title = count, ', count,'}},'),
      after = 1
    )
    pggf$code <- append(
      pggf$code,
      paste0('\t\\pggf_hexbin(bins = ', bins, ')\n'),
      after = length(pggf$code) - 1
    )
  }
  
  # call the new `pggf` object
  pggf
}

# bar plot
pggf_bar <- function(pggf, group = NULL, error = NULL, extra_cols = NULL, orientation = 'x', stack = FALSE) {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # throw an error if `error` is used together with `stack = TRUE`
  if (stack && (! missing(error) && ! pggf_is_null(substitute(error)))) {
    cli::cli_abort('Error bars cannot be added to stack bars')
  }
  
  # get orientation
  if (orientation %in% c('x', 'v', 'vertical')) {
    orientation <- 'x'
    if (! pggf$numeric_y) {
      cli::cli_abort('{.arg y} must be numeric for {.arg orientation = "x"}')
    }
    sample_cols <- c('x', 'xticklabels')
    value_col <- 'y'
  } else if (orientation %in% c('y', 'h', 'horizontal')) {
    orientation <- 'y'
    if (! pggf$numeric_x) {
      cli::cli_abort('{.arg x} must be numeric for {.arg orientation = "y"}')
    }
    sample_cols <- c('y', 'yticklabels')
    value_col <- 'x'
  } else {
    cli::cli_abort(c('{.arg orientation} must be "x" or "y"', i = 'You can also use "v" or "vertical" for "x" and "h" or "horizontal" for "y"'))
  }
  
  # save error data for axis limit calculations
  if (! missing(error) && ! pggf_is_null(substitute(error))) {
    error_data <- pggf$data |>
      dplyr::select({{error}})
    
    if (ncol(error_data) %in% c(1, 2)) {
      if (orientation == 'x') {
        pggf$error[c(3, 4)] <- error_data
      } else {
        pggf$error[c(1, 2)] <- error_data
      }
    } else {
      cli::cli_abort('{.arg error} must be of length 1 or 2')
    }
  }
  
  # save stack height as error for axis limit calculations
  if (stack) {
    error_data <- pggf$data |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(value_col), ~ sum(.x) - .x, .names = 'error'),
        .by = dplyr::any_of(c('facet_id', orientation))
      ) |>
      dplyr::select(error)
    
    if (orientation == 'x') {
      pggf$error[c(3, 4)] <- error_data
    } else {
      pggf$error[c(1, 2)] <- error_data
    }
  }
  
  # prepare `data`
  bar_data <- pggf$data |>
    dplyr::select(dplyr::any_of(c('facet_id', sample_cols, value_col)), 'group' = {{group}}, {{error}}, {{extra_cols}}) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of('group'), as.ordered)
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::any_of(c('facet_id', 'group', orientation))))
  
  # calculate bar width and shift
  shift_col <- paste0(orientation, '_shift')
  
  if (stack) {
    bar_data <- bar_data |>
      dplyr::mutate(
        bar_width = 1,
        {{shift_col}} := 0
      )
  } else {
    bar_data <- bar_data |>
      dplyr::mutate(
        group_n = 1,
        dplyr::across(dplyr::any_of('group'), ~ max(as.numeric(.x)), .names = 'group_n'),
        bar_width = 1 / group_n,
        {{shift_col}} := ((seq_len(unique(group_n)) - 0.5) / group_n) - 0.5
      ) |>
      dplyr::select(-group_n)
  }

  # export data
  bar_file <- bar_data |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_bar.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file to pggf object
  pggf$files <- c(pggf$files, bar_file)

  # add to ggplot
  if (! is.null(pggf$plot)) {
    if (! missing(group) && ! pggf_is_null(substitute(group))) {
      plot_group <- pggf_sym_or_str(substitute(group))
      pggf$plot <- pggf$plot +
        ggplot2::geom_col(
          mapping = ggplot2::aes(fill = as.ordered({{plot_group}})),
          position = ifelse(stack, 'stack', 'dodge'),
          orientation = orientation
        )
    } else {
      pggf$plot <- pggf$plot +
        ggplot2::geom_col(
          position = ifelse(stack, 'stack', 'dodge'),
          orientation = orientation
        )
    }
    if (! missing(error) && ! pggf_is_null(substitute(error))) {
      plot_err <- names(error_data)
      if (length(plot_err) == 1) {
        plot_err[2] <- plot_err
      }
      if (orientation == 'x') {
        if (! missing(group) && ! pggf_is_null(substitute(group))) {
          pggf$plot <- pggf$plot +
            ggplot2::geom_errorbar(
              aes(ymin = .data[[pggf$y_str]] - .data[[plot_err[1]]], ymax = .data[[pggf$y_str]] + .data[[plot_err[2]]], color = as.ordered({{plot_group}})),
              position = ggplot2::position_dodge(0.9),
              width = 0.1
            )
        } else {
          pggf$plot <- pggf$plot +
            ggplot2::geom_errorbar(
              aes(ymin = .data[[pggf$y_str]] - .data[[plot_err[1]]], ymax = .data[[pggf$y_str]] + .data[[plot_err[2]]]),
              width = 0.1
            )
        }
      } else {
        if (! missing(group) && ! pggf_is_null(substitute(group))) {
          pggf$plot <- pggf$plot +
            ggplot2::geom_errorbar(
              aes(xmin = .data[[pggf$x_str]] - .data[[plot_err[1]]], xmax = .data[[pggf$x_str]] + .data[[plot_err[2]]], color = as.ordered({{plot_group}})),
              position = ggplot2::position_dodge(0.9),
              width = 0.1
            )
        } else {
          pggf$plot <- pggf$plot +
            ggplot2::geom_errorbar(
              aes(xmin = .data[[pggf$x_str]] - .data[[plot_err[1]]], xmax = .data[[pggf$x_str]] + .data[[plot_err[2]]]),
              width = 0.1
            )
        }
      }
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    if (! missing(error) && ! pggf_is_null(substitute(error))) {
      if (length(names(error_data)) == 1) {
        code_err <- paste0(', ', value_col, ' error = {', names(error_data), '}')
      } else {
        code_err <- paste0(', ', value_col, ' error* = {', names(error_data)[1], '}{', names(error_data)[2], '}')
      }
    } else {
      code_err <- NULL
    }

    pggf$code <- append(
      pggf$code,
      paste0('\t\\pggf_bar(', value_col, 'bar', ifelse(stack, ' stacked', ''), code_err, ')\n'),
      after = length(pggf$code) - 1
    )
  }
  
  # call the new `pggf` object
  pggf
}


# heatmap
pggf_heatmap <- function(pggf, color, extra_cols = NULL, colorbar = 'fixed') {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # check arguments
  if (missing(color)) {
    cli::cli_abort('Required argument {.arg color} is missing')
  }
  if (! colorbar %in% c('fixed', 'free')) {
    cli::cli_abort('{.arg colorbar} must be "fixed" or "free"')
  }
  
  # get facet info
  if (! is.null(pggf$facets)) {
    has_cols <- ('col' %in% names(pggf$facets))
    has_rows <- ('row' %in% names(pggf$facets))
  } else {
    has_cols <- FALSE
    has_rows <- FALSE
  }
  
  # prepare `data`
  heatmap_data <- pggf$data |>
    dplyr::ungroup()
  
  # add all possible x/y combinations
  if (is.null(pggf$facets)) {
    all_combis <- heatmap_data |>
      tidyr::expand(x, y)
    heatmap_data <- all_combis |>
      dplyr::left_join(
        heatmap_data |>
          dplyr::select(x, y, 'score' = {{color}}, {{extra_cols}}),
        join_by(x, y)
      ) |>
      dplyr::arrange(x, y)
  } else {
    if (has_cols && pggf$scales %in% c('free', 'free_x')) {
      all_combis <- heatmap_data |>
        dplyr::distinct(!! pggf$facet_col, x) |>
        dplyr::full_join(
          pggf$facets,
          dplyr::join_by(!! pggf$facet_col == col),
          relationship = 'many-to-many'
        )
    } else {
      all_combis <- heatmap_data |>
        dplyr::distinct(x) |>
        tidyr::expand_grid(pggf$facets)
    }
    if (has_rows && pggf$scales %in% c('free', 'free_y')) {
      all_combis <- heatmap_data |>
        dplyr::distinct(!! pggf$facet_row, y) |>
        dplyr::full_join(
          all_combis,
          dplyr::join_by(!! pggf$facet_row == row),
          relationship = 'many-to-many'
        )
    } else {
      all_combis <- heatmap_data |>
        dplyr::distinct(y) |>
        tidyr::expand_grid(all_combis)
    }
    heatmap_data <- all_combis |>
      dplyr::select(facet_id, x, y) |>
      dplyr::left_join(
        heatmap_data |>
          dplyr::select(facet_id, x, y, 'score' = {{color}}, {{extra_cols}}),
        join_by(facet_id, x, y)
      ) |>
      dplyr::arrange(facet_id, x, y)
  }

  # export data
  heatmap_file <- heatmap_data |>
    dplyr::mutate(
      file = paste0(pggf$filename, '_heatmap.tsv')
    ) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data, na = 'NaN'),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file to pggf object
  pggf$files <- c(pggf$files, heatmap_file)
  
  # export column and row numbers and colorbar limits
  heatmap_axis <- heatmap_data |>
    dplyr::group_by(dplyr::pick(dplyr::any_of('facet_id'))) |>
    dplyr::summarise(
      `mesh/cols` = dplyr::n_distinct(x),
      `mesh/rows` = dplyr::n_distinct(y),
      dplyr::across(score, list('min' = ~ min(.x, na.rm = TRUE), 'max' = ~ max(.x, na.rm = TRUE)), .names = 'point meta {.fn}'),
      .groups = 'drop'
    ) |>
    dplyr::ungroup()
    
  if (colorbar == 'fixed') {
    heatmap_axis <- heatmap_axis|>
      dplyr::mutate(
        `point meta min` = min(`point meta min`),
        `point meta max` = max(`point meta max`)
      )
  }
  
  pggf$axes_extra <- heatmap_axis
  
  # add to ggplot
  if (! is.null(pggf$plot)) {
    plot_fill <- pggf_sym_or_str(substitute(color))
    pggf$plot <- pggf$plot +
      ggplot2::geom_tile(
        aes(fill = {{plot_fill}})
      )
    if (pggf$numeric_y) {
      pggf$plot <- pggf$plot +
        ggplot2::scale_y_continuous(
          limits = rev
        )
    } else {
      pggf$plot <- pggf$plot +
        ggplot2::scale_y_discrete(
          limits = rev
        )
    }
  }
  
  # add to code
  if (! is.null(pggf$code)) {
    code_title <- if (is.symbol(substitute(color))) {deparse(substitute(color))} else {as.character(color)}
    pggf$code <- append(
      pggf$code,
      paste0('\t', ifelse(colorbar == 'fixed', 'facet 1', 'every facet'), '/.append style = {pggf colorbar = {title = ', code_title, '}},'),
      after = 1
    )
    pggf$code <- append(
      pggf$code,
      paste0('\t\\pggf_heatmap()\n'),
      after = length(pggf$code) - 1
    )
  }
  
  # call the new `pggf` object
  pggf
}


# line plot
pggf_line <- function(pggf, group = NULL, split = NULL, extra_cols = NULL, error = NULL, orientation = 'x') {
  # make sure `pggf` is a `pggf` class object
  assert_pggf(pggf)
  
  # get orientation
  if (orientation %in% c('x', 'v', 'vertical')) {
    orientation <- 'x'
    if (! pggf$numeric_y) {
      cli::cli_abort('{.arg y} must be numeric for {.arg orientation = "x"}')
    }
    value_col <- 'y'
  } else if (orientation %in% c('y', 'h', 'horizontal')) {
    orientation <- 'y'
    if (! pggf$numeric_x) {
      cli::cli_abort('{.arg x} must be numeric for {.arg orientation = "y"}')
    }
    value_col <- 'x'
  } else {
    cli::cli_abort(c('{.arg orientation} must be "x" or "y"', i = 'You can also use "v" or "vertical" for "x" and "h" or "horizontal" for "y"'))
  }
  
  # prepare `data`
  data <- pggf$data |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::any_of('facet_id'), x, y, {{group}}, {{split}}, {{extra_cols}}, {{error}})
  
  # save error data for axis limit calculations and calculate error coordinates
  if (! missing(error) && ! pggf_is_null(substitute(error))) {
    error_data <- data |>
      dplyr::select({{error}})
    
    if (ncol(error_data) %in% c(1, 2)) {
      if (orientation == 'x') {
        pggf$error[c(3, 4)] <- error_data
      } else {
        pggf$error[c(1, 2)] <- error_data
      }
    } else {
      cli::cli_abort('{.arg error} must be of length 1 or 2')
    }
    
    error_names <- names(error_data)
    if (length(error_names) == 1) {
      error_names[2] <- error_names
    }
    
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(value_col),
          list('min' = ~ .x - .data[[error_names[1]]], 'max' = ~ .x + .data[[error_names[2]]]),
          .names = 'error_{value_col}_{fn}'
        )
      )
    
    if (length(error_names) == 1) {
      data <- data |>
        dplyr::rename(
          !! paste0('error_', value_col) := .data[[error_names[1]]]
        )
    } else {
      data <- data |>
        dplyr::rename(
          !! paste0('error_', value_col, '_1') := .data[[error_names[1]]],
          !! paste0('error_', value_col, '_2') := .data[[error_names[2]]],
        )
    }
  }
  
  # order data
  data <- data |>
    dplyr::arrange(dplyr::pick(dplyr::any_of('facet_id'), {{group}}, {{split}}, dplyr::any_of(orientation)))
  
  # split (optional) and export data
  files <- data |>
    dplyr::mutate('.file_basename' = pggf$filename, '.file_suffix' = 'line.tsv') |>
    tidyr::unite('file', .file_basename, {{split}}, .file_suffix, remove = FALSE) |>
    dplyr::select(-c(.file_basename, .file_suffix)) |>
    dplyr::nest_by(file) |>
    dplyr::summarise(
      content = readr::format_tsv(data),
      .groups = 'drop'
    ) |>
    tibble::deframe()
  
  # add file(s) to pggf object
  pggf$files <- c(pggf$files, files)

  # add to ggplot
  if (! is.null(pggf$plot)) {
    plot_group <- pggf_sym_or_str(substitute(group))
    pggf$plot <- pggf$plot + ggplot2::geom_line(mapping = ggplot2::aes(color = {{plot_group}}, group = {{plot_group}}), orientation = orientation)
    if (! missing(error) && ! pggf_is_null(substitute(error))) {
      if (orientation == 'x') {
        pggf$plot <- pggf$plot + ggplot2::geom_line(ggplot2::aes(y = .data[[pggf$y_str]] - .data[[error_names[1]]], color = {{plot_group}}, group = {{plot_group}}), orientation = orientation, linetype = 'dashed')
        pggf$plot <- pggf$plot + ggplot2::geom_line(ggplot2::aes(y = .data[[pggf$y_str]] + .data[[error_names[2]]], color = {{plot_group}}, group = {{plot_group}}), orientation = orientation, linetype = 'dashed')
      } else {
        pggf$plot <- pggf$plot + ggplot2::geom_line(ggplot2::aes(x = .data[[pggf$x_str]] - .data[[error_names[1]]], color = {{plot_group}}, group = {{plot_group}}), orientation = orientation, linetype = 'dashed')
        pggf$plot <- pggf$plot + ggplot2::geom_line(ggplot2::aes(x = .data[[pggf$x_str]] + .data[[error_names[2]]], color = {{plot_group}}, group = {{plot_group}}), orientation = orientation, linetype = 'dashed')
      }
    }
  }

  # add to code
  if (! is.null(pggf$code)) {
    if (! missing(error) && ! pggf_is_null(substitute(error))) {
      code_error <- paste0('error = lines')
    } else {
      code_error <- NULL
    }
    
    if (! missing(group) && ! pggf_is_null(substitute(group))) {
      groups <- data |> dplyr::distinct(dplyr::pick({{group}})) |> dplyr::pull() |> sort() |> paste0(collapse = ',')
      group_str <- if (is.symbol(substitute(group))) {deparse(substitute(group))} else {as.character(group)}
      code_group <- paste0('group column = ', group_str)
      pggf$code <- append(
        pggf$code,
        paste0('\tline styles = {', groups, '},'),
        after = 1
      )
    } else {
      code_group <- NULL
    }

    code_extras <- paste0(c(code_error, code_group), collapse = ', ')

    if (! missing(split) && ! pggf_is_null(substitute(split))) {
      if (code_extras != '') {
        code_extras <- paste0(', ', code_extras)
      }
      code_split <- paste0('data = ', gsub('_line.tsv', '', names(files)), code_extras)
      pggf$code <- append(
        pggf$code,
        c(paste0('\t\\pggf_line(', code_split, ')'), '\t'),
        after = length(pggf$code) - 1
      )
    } else {
      pggf$code <- append(
        pggf$code,
        paste0('\t\\pggf_line(', code_extras, ')\n'),
        after = length(pggf$code) - 1
      )
    }

    if (! is.null(code_group)) {
      pggf$code <- append(pggf$code, paste0('\t\\pggf_annotate(line legend)\n'), after = length(pggf$code) - 1)
    }
  }
  
  # call the new `pggf` object
  pggf
}
