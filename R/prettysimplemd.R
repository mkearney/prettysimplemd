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
padding: 10px 0;
color: #111;
}
body {
background-image: url(\"/Users/mwk/Downloads/screen/MU_UnitSig_MissouriSchoolofJournalism_rgb_std_horiz.png\");
border: 1px solid rgba(0,5,20,.35);
background-size: 350px auto;
background-repeat: no-repeat;
background-position: right top;
border-radius: 10px;
max-width: 1000px;
min-width: 400px;
width: 90%;
margin: 2px auto;
line-height: 1.5;
font-family: 'GothamBook', sans-serif;
font-weight: 400;
padding: 10px;
background-color: #fff;
font-size: 16px;
}
p {
padding: 2px 0;
font-size: 16px;
text-align: justify;
line-height:
1.4;
}
strong { font-family: 'GothamBold', sans-serif; }
h1, h2, h3, h4 { font-family: 'GothamBold', sans-serif; }
h2, h3, h4 { color: #000; }
h1 {
padding-top: 50px;
font-size: 40px;
text-align: center;
color: #f1b82d;
}
h2 { font-size: 22px; }
h3 { font-size: 20px; }
h4 { font-size: 18px; }
p.footer {
padding-top:80px;
padding-bottom: 30px;
color: #666;
font-size: 14px;
text-align: center; }
s,
strike {
  text-decoration: none;
  position: relative;
  display: inline-block;
}
s:after,
strike:after {
  content: \"\";
  position: absolute;
  bottom: 0;
  left: -2%;
  border-radius:0 10px 10px 0;
  border-top: 3px solid rgba(204, 0, 0, .75);
  border-top-right-radius: 1px;
  border-top-left-radius: 1px;
  width: 104%;
  height: 53%;
}
</style>
"
}

add_footer <- function() {
  "
<p class=\"footer\">Michael W. Kearney &copy; 2017</p>
"
}
