library(flextable)
library(officer)

custom_theme <- function(x) {
  x <- set_table_properties(x, layout = "fixed")
  x <- border_remove(x)
  std_border <- fp_border(width = 1, color = "black")
  x <- border_outer(x, part="all", border = std_border)
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
  x <- align(x, align = "center", part = "all")
  x <- bold(x, bold = TRUE, part = "header")
  x <- bold(x, bold = TRUE, j=1)
  border2 <- fp_border(width = 2, color = "black")
  x <- border(x, i=1, border.bottom=border2, part="header")
  x <- border(x, j=1, border.right=border2, part="all")
  width(x, width=2, unit="cm")
}

custom_theme2headers <- function(x, header_row) {
  x <- set_table_properties(x, layout = "fixed")
  x <- border_remove(x)
  x <- add_header_row(x, values=header_row)
  std_border <- fp_border(width = 1, color = "black")

  x <- border_outer(x, part="all", border = std_border)
  x <- border_inner_h(x, border = std_border, part="body")
  x <- border_inner_v(x, border = std_border, part="all")

  x <- align(x, align = "center", part = "all")
  x <- bold(x, bold = TRUE, part = "header")
  x <- bold(x, bold = TRUE, j=1)
  border2 <- fp_border(width = 2, color = "black")

  x <- border(x, i=2, border.bottom=border2, part="header")
  x <- border(x, j=1, border.right=border2, part="all")
  width(x, width=2, unit="cm")
}

index_replace <- function(x) {
  col_names <- x$col_keys
  new <- stringi::stri_replace_all_regex(col_names, c("\\d+"), c(""))
  sup <- stringi::stri_extract_all_regex(col_names, c("\\^\\{[^\\{]{1,}\\}"))
  sup <- unlist(sup)
  sup <- stringi::stri_replace_all_regex(sup, c("\\^\\{"), c(""))
  sup <- stringi::stri_replace_all_regex(sup, c("\\}"), c(""))

  sub <- stringi::stri_extract_all_regex(col_names, c("_\\{[^\\{]{1,}\\}"))
  sub <- unlist(sub)
  sub <- stringi::stri_replace_all_regex(sub, c("_\\{"), c(""))
  sub <- stringi::stri_replace_all_regex(sub, c("\\}"), c(""))

  new <- stringi::stri_replace_all_regex(new, c("\\^\\{[^\\{]{1,}\\}"), c(""))
  new <- stringi::stri_replace_all_regex(new, c("_\\{[^\\{]{1,}\\}"), c(""))
  #x <- compose(x, part="header", j=col_names, value = as_paragraph(new))
  x <- compose(x, part="header", j=col_names, value = as_paragraph(new, as_sup(sup), as_sub(sub)))
  return(x)
}
# onec <- data.frame(a = c("-9", "A7", "A6", "C5", "A4", "C3", "C2"),
#                    b = c(0, 0, 0, 0, 0, 0, 1),
#                    c = c(0, 0, 0, 0, 0, 0, 1),
#                    d = c(0, 0, 0, 0, 0, 0, 1),
#                    e = c(0, 0, 0, 0, 0, 0, 1),
#                    f = c(0, 0, 0, 0, 0, 0, 1),
#                    g = c(0, 0, 0, 0, 0, 0, 1),
#                    h = c(0, 0, 0, 0, 0, 0, 1))
# names(onec) <- c("M_{xbla}^{ybl}",  "-", "A1",   "C2", "C3",  "A4",   "A5",   "G6")
# onec <- flextable(onec)
# custom_theme(onec)
# index_replace(onec)



