
render_project <- function(){
  files <- list.files(path = paste0( getwd(), "/docs/"),
                      pattern = "md$",
                      full.names=T)
  files
  for (f in files) rmarkdown::render(f)
}