#' prettysimplemd
#'
#' Generates pretty simple markdown files
#'
#' @param file Name of markdown file.
#' @param description Optional, short hand description of document contents.
#' @param date Date to be timestamped on top left of document. Defaults to 
#'   current year-month-day.
#' @param open Logical indicating whether to open output in browser. Defaults to
#'   open the first time the function is called in a session.
#' @param ... Passed to rmarkdown::render.
#' @return Converts markdown file as pretty simple html file of same root name.
#' @export
prettysimplemd <- function(file, description = NULL, date = NULL, open = NULL, ...) {
  file <- path.expand(file)
  con <- file(file)
  md <- readLines(con, warn = FALSE)
  close(con)
  md[length(md) + 1] <- add_footer()
  md[length(md) + 1] <- add_css()
  if (grepl("\\.md$", file)) {
    ext <- ".md"
  } else {
    ext <- ".Rmd"
  }
  tmp <- tempfile(fileext = ext)
  md <- c(
    meta_block(description = description, date = date), 
    md
  )
  cat(paste(md, collapse = "\n"), file = tmp, fill = TRUE)
  outfile <- gsub(paste0(ext, "$"), ".html", file)
  rmarkdown::render(
    tmp, 
    output_format = "html_document",
    output_file = basename(outfile), 
    output_dir = dirname(outfile), 
    ...
  )
  if (is.null(open)) {
    actives <- ls(
      envir = .GlobalEnv, 
      pattern = "^\\.active", 
      all.names = TRUE
    )
    dot_obj <- paste0(
      ".active_", 
      gsub("\\s{1,}|-", "", basename(file))
    )
    if (dot_obj %in% actives) {
      return(invisible())
    }
    ## save .active_file in Global environment
    assign(dot_obj, dot_obj, envir = .GlobalEnv)
    ## open file
    browseURL(outfile)
  } else if (open) {
    browseURL(outfile)
  }
}

meta_block <- function(description = NULL, date = NULL) {
  if (is.null(date)) {
    date <- Sys.time()
  }
  meta <- paste0("<p class=\"note\">", date, "</p>")
  if (!is.null(description)) {
    description <- paste0("<p class=\"note\">", description, "</p>")
    meta <- paste0(meta, "\n", description, "\n")
  }
  meta
}



add_css <- function() {
  logo1 <- Sys.getenv("INSTITUTION_LOGO1")
  logo2 <- Sys.getenv("INSTITUTION_LOGO2")
  logos <- paste0(
    "background-image: url(\"", 
    logo1, "\"), ",
    "url(\"", 
    logo2, "\");"
  )
  paste0(
    "
<style>
    html {
      color: #24292e;
    }
    body {
      max-width: 940px;
      min-width: 400px;
      width: 90%;
      margin: 2px auto;
      line-height: 1.8;
      font-family: 'Helvetica Neue', Helvetica, sans-serif;
      font-weight: 400;
      padding: 40px 10px;
      background-color: #fff;
      font-size: 18px;
    }
    div.container-fluid.main-container { 
      max-width: 800px;
      ", logos, "
      background-size: auto 60px, auto 60px;
      background-repeat: no-repeat, no-repeat;
      background-position: top left, top right;
    }
    p {
      padding: 2px 0;
      font-size: 18px;
      text-align: justify;
      line-height: 1.6;
    }
    strong { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-weight: 700; }
    h1, h2, h3, h4 { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-weight: 700; }
    h1 {
      padding-top: 40px;
      font-size: 50px;
      text-align: center;
      color: #f1b82d;
      -webkit-text-stroke: 1px black;
    }
    h2 { font-size: 22px; }
    h3 { font-size: 20px; }
    h4 { font-size: 18px; }
    p.footer {
      color: #666;
      font-size: 14px;
      text-align: center; 
    }
    s, strike {
      text-decoration: none;
      position: relative;
      display: inline-block;
    }
    s:after, strike:after {
      content: \"\";
      position: absolute;
      bottom: 0;
      left: -2%;
      border-radius:0 10px 10px 0;
      border-top: 2px solid rgba(204, 0, 0, .75);
      border-top-right-radius: 1px;
      border-top-left-radius: 1px;
      width: 104%;
      height: 53%;
    }
    p.note {
      font-family: Monaco, monospace;
      font-size: 11px;
      color: #999;
      text-align: left;
      line-height: 1.3;
      padding-top: 55px;
      padding-bottom: 0;
      margin-top: 0;
      margin-bottom: 0;
  }
    </style>
    "
  )
}

add_footer <- function() {
  "
<!-- <p class=\"footer\">Michael W. Kearney &copy; 2017</p> -->
"
}

#' renderPSM
#' 
#' Renders simple_markdown
#' 
#' @param input Name of file
#' @param description Optional, short hand description of document contents.
#' @param date Date to be timestamped on top left of document. Defaults to 
#'   current year-month-day.
#' @param open Logical indicating whether to open output in browser. Defaults to
#'   open the first time the function is called in a session.
#' @param ... Passed to \code{\link{prettysimplemd}}
#' @return Output saved as html file.
#' @export
renderPSM <- function(input, description = NULL, date = NULL, open = NULL, ...) {
  prettysimplemd(
    file = input, 
    description = description, 
    date = date, 
    open = open, 
    ...
  )
}

