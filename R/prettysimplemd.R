#' prettysimplemd
#'
#' Generates pretty simple markdown files
#'
#' @param file Name of markdown file.
#' @param open Logical indicating whether to open output in browser. Defaults to open the
#'   first time the function is called in a session.
#' @param dots Passed to rmarkdown::render.
#' @return Converts markdown file as pretty simple html file of same root name.
#' @export
prettysimplemd <- function(file, open = NULL, ...) {
  file <- path.expand(file)
  con <- file(file)
  md <- readLines(con)
  close(con)
  md[length(md) + 1] <- add_footer()
  md[length(md) + 1] <- add_css()
  tmp <- tempfile(fileext = "md")
  cat(paste(md, collapse = "\n"), file = tmp, fill = TRUE)
  outfile <- gsub("\\.md", ".html", file)
  rmarkdown::render(tmp, output_file = outfile, ...)
  if (is.null(open)) {
    if (any(
      is.null(getOption("prettysimplemd.open")),
      isTRUE(getOption("prettysimplemd.open"))
    )) {
      browseURL(outfile)
      options(prettysimplemd.open = FALSE)
    }
  } else if (open) {
    browseURL(outfile)
  }
}

add_css <- function() {
  "
<style>
html {
background: rgba(0,15,60,.125);
background-image: url(\"/Users/mwk/r/linen8.png\");
opacity: .9;
padding: 30px 0;
}
body {
border: 1px solid rgba(0,5,20,.35);
border-radius: 10px;
max-width: 600px;
margin: 2px auto;
line-height: 1.5;
font-family: Roboto, sans-serif;
font-weight: 400;
color: #222;
padding: 0 60px;
background: #ffffff;
font-size: 16px;
}
p {
padding: 2px 0;
font-size: 17px;
text-align: justify;
line-height:
1.4;
}
strong { font-weight: bold; }
h1, h2, h3, h4 { font-weight: bold; }
h2, h3, h4 { color: rgba(0,25,100,.85); }
h1 {
text-align: center;
color: rgba(150,25,25.85);
}
h2 { font-size: 26px; }
h3 { font-size: 24px; }
h4 { font-size: 22px; }
p.footer { padding: 30px 0; color: #666; font-size: 14px; text-align: center; }
</style>
"
}

add_footer <- function() {
  "
<p class=\"footer\">Michael W. Kearney &copy; 2017</p>
"
}
