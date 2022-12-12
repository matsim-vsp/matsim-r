# Hello, MATSim world!
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
# -----------------------------------------------------------


#' Generate html docs from roxygen2 docs
#'
#' from
#' https://stackoverflow.com/questions/30897301/export-r-package-documentation-to-a-web-page
#'
#' not exported, for internal use only
generate_html_docs = function(pkg, links = tools::findHTMLlinks()) {
  wd <- getwd()

  helpdir <- "./docs"
  dir.create(helpdir)
  setwd(helpdir)
  message("Generated help files will be placed in ", helpdir)

  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg),
                                        'help', pkg))
  force(links); topics = names(pkgRdDB)
  for (p in topics) {
    tools::Rd2HTML(pkgRdDB[[p]],
                   paste(p, 'docs', sep = '.'),
                   package = pkg,
                   Links = links,
                   no_links = is.null(links))
  }
  setwd(wd) # Get back to the current working directory
}
