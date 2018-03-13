#' Plot out text strings as individual items
#'
#' To support interactive text, this function generates a plot where each character
#' has a known position

#' @export
gf_characters <- function(object = NULL, gformula = row ~ col, textdf) {
  nlines <- length(attr(textdf, "orig_string"))
  P <- gf_raster(object = object, gformula = gformula, data = textdf,
                 fill = ~ background_color, alpha = ~ background_alpha, show.legend = FALSE ) %>%
    gf_text(gformula = gformula, data = textdf, label = ~ letter, alpha = ~ char_alpha, show.legend = FALSE)

  if (is.null(object)) {
    aspect_ratio <- 2
    P <- P %>% gf_refine(coord_fixed(ratio = aspect_ratio,
                                     xlim = c(0, 50),
                                     ylim = -c(0, (1 + nlines) ))) %>%
      gf_theme(legend.position = "none") %>%
      gf_theme(no_background)
  }

  P
}

no_background <- ggplot2::theme_bw() + ggplot2::theme(
  panel.grid.major = ggplot2::element_blank()
  ,panel.grid.minor = ggplot2::element_blank()
  ,panel.border = ggplot2::element_blank()
  ,axis.line = ggplot2::element_line(color = NA)
  ,axis.title.y=ggplot2::element_blank()
  ,axis.text.y=ggplot2::element_blank()
  ,axis.ticks.y=ggplot2::element_blank()
  ,axis.title.x=ggplot2::element_blank()
  ,axis.text.x=ggplot2::element_blank()
  ,axis.ticks.x=ggplot2::element_blank()
)


#' @export
str_to_df <- function(str, seps = unlist(strsplit(".,?\\/\"\' ![](){}\n", split = ""))) {
  res <- as.list(character(length(str))) # pre-allocate space
  token_start = 1
  for (k in 1:length(str)) {
    individuals <- c(unlist(strsplit(str[k], split = "")), "\n")
    dividing_chars <- individuals %in% seps
    token_nums <- 2*cumsum(dividing_chars)
    token_nums[dividing_chars] <- token_nums[dividing_chars] - 1 # separators
    token_nums <- token_nums + 1
    # get the corresponding token
    tokens <- list()
    tokens[unique(token_nums)] <- lapply(as.list(unique(token_nums)), FUN =
                       function(n) { paste(individuals[token_nums == n], collapse = "")})
    token_vals <- unlist(tokens[token_nums])
    token_nums <- token_nums + token_start # put them in sequence with previous lines
    token_start <- max(token_nums) + 1
    res[[k]] <- data.frame(letter = individuals, token_num = token_nums, token = token_vals,
                           col = 1:length(individuals), row = -k,
                           color = "black", font_size = 12, char_alpha = 1.0,
                           background_color = "white", background_alpha = 0.0, font = "helvetica",
                           stringsAsFactors = FALSE)
  }
  res <- bind_rows(res)

  attr(res, "orig_string") <- str
  res
}

#' @export
row_from_xy <- function(textdf, x, y) {
  which(with(textdf, row %in% get_range(y) & col %in% get_range(x)))
}

#' @export
get_range <- function(x) {
  if (length(x) == 2) do.call(seq, as.list(round(x)))
  else round(x)
}
