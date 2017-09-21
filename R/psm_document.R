
#' psm_document
#' 
#' Document template for prettysimplemd.
#' 
#' @export
psm_document <- function() {
  template_path <- system.file(
    "rmarkdown", "templates", "psm_document", package = "prettysimplemd"
  )
  ## set locations of css and doc pre/suf fixes
  custom <- file.path(
    template_path, "resources", "custom.css"
  )
  css_file <- file.path(
    template_path, "resources", "prettysimplemd.css"
  )
  html_prefix <- file.path(
    template_path, "resources", "prettysimplemd_prefix.html"
  )
  html_suffix <- file.path(
    template_path, "resources", "prettysimplemd_suffix.html"
  )
  ## create css file
  con <- file(custom)
  css <- readLines(con)
  close(con)
  css <- gsub("LOGOS", add_logos(), css)
  css <- add_css(style = FALSE)
  cat(paste0(css, collapse = "\n"), file = css_file, fill = TRUE)
  ## call the base html_document function
  rmarkdown::html_document(
    toc = FALSE,
    fig_width = 7,
    fig_height = 5,
    theme = "default",
    css = css_file,
    highlight = "kate",
    includes = rmarkdown::includes(
      before_body = html_prefix,
      after_body = html_suffix
    )
  )
}

