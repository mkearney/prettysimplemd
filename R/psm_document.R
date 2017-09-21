
#' psm_document
#' 
#' Document template for prettysimplemd.
#' 
#' @export
psm_document <- function() {
  ## set locations of css and doc pre/suf fixes
  css <- file.path(
    "~", "Dropbox", "Public", "prettysimplemd.css"
  )
  html_prefix <- file.path(
    "~", "Dropbox", "Public", "prettysimplemd_prefix.html"
  )
  html_suffix <- file.path(
    "~", "Dropbox", "Public", "prettysimplemd_suffix.html"
  )
  timestamp <- meta_block()
  ## call the base html_document function
  rmarkdown::html_document(
    toc = FALSE,
    fig_width = 7,
    fig_height = 5,
    theme = "default",
    css = css,
    highlight = "kate",
    includes = rmarkdown::includes(
      before_body = html_prefix,
      after_body = html_suffix
    )
  )
}

