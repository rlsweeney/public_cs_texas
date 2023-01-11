# the ... here are fields passed to formatC
# I believe they are necessary
latex_number <- function(value, name, ...) {
  value %>%
    formatC(...) %>%
    cat(file = file.path(edir, paste(name, ".tex", sep = "")))
}
