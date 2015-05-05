tidy_baselearner_names <- function(model, which = NULL) {
  txt <- gsub('\"', "", extract(model, "bnames", which = which))
  txt <- gsub(".*%X%.*", "\\\\text{xkm} \\\\cdot \\\\text{ykm} \\\\cdot \\\\text{sTime}", txt)
  txt <- gsub("bols\\((.*), by = (.*), intercept = FALSE\\)", "\\\\text{\\1} \\\\cdot \\\\text{\\2}", txt)
  txt <- gsub("bols\\((.*), intercept = FALSE\\)", "\\\\text{\\1}", txt)
  txt <- gsub("bbs\\((.*), df = df, center = center\\)", "f(\\\\text{\\1})", txt)
  txt <- gsub("bspatial\\((.*), by = (.*), center = center, df = df\\)",
              "f(\\\\text{\\1}) \\\\cdot \\\\text{\\2}", txt)
  txt <- gsub("bspatial\\((.*), center = center, df = df\\)", "f(\\\\text{\\1})", txt)
  txt
}

tidy_baselearner_names_plot <- function(model, which = NULL) {
  txt <- gsub('\"', "", extract(model, "bnames", which = which))
  txt <- gsub(".*%X%.*", "xkm * ykm * sTime", txt)
  txt <- gsub("bols\\((.*), by = (.*), intercept = FALSE\\)", "\\1 * \\2", txt)
  txt <- gsub("bols\\((.*), intercept = FALSE\\)", "\\1", txt)
  txt <- gsub("bbs\\((.*), df = df, center = center\\)", "f(\\1)", txt)
  txt <- gsub("bspatial\\((.*), by = (.*), center = center, df = df\\)",
              "f(\\1) * \\2", txt)
  txt <- gsub("bspatial\\((.*), center = center, df = df\\)", "f(\\1)", txt)
  txt
}
