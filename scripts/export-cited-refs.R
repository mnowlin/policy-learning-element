#!/usr/bin/env Rscript

# Pre-render build step.
# Scans the book's chapter sources for citation keys and writes a trimmed,
# project-local `references.bib` containing only the cited entries, drawn
# from the central master bib (the source of truth). This keeps citeproc
# from having to parse the entire 24MB master bibliography on every render.

master_bib <- "/Users/matthewnowlin/Library/CloudStorage/OneDrive-UTArlington/01-RESEARCH/Manuscript-Files/refs.bib"

out_bib <- "references.bib"

# --- 1. Collect citation keys from every chapter source ---------------------
src <- list.files(".", pattern = "^(chap[0-9]+|index|intro|summary|references)\\.qmd$")
text <- paste(unlist(lapply(src, readLines, warn = FALSE, encoding = "UTF-8")),
              collapse = "\n")

# Pandoc citation keys: @key, where key starts alphanumeric and may contain
# internal _ : . + - . Crossref keys (@fig-*, @tbl-*, ...) are harmlessly
# included here but dropped later because they are not in the bib.
cite_pat <- "@[A-Za-z0-9][A-Za-z0-9_:.+-]*"
keys <- regmatches(text, gregexpr(cite_pat, text, perl = TRUE))[[1]]
keys <- unique(sub("^@", "", keys))
keys <- sub("[.:+-]+$", "", keys)  # trim trailing punctuation not part of key

# --- 2. Split the master bib into entries ----------------------------------
bib <- readLines(master_bib, warn = FALSE, encoding = "UTF-8")
starts <- grep("^[[:space:]]*@", bib)
ends   <- c(starts[-1] - 1L, length(bib))
entry_keys <- trimws(sub("^[[:space:]]*@[^{(]+[{(]([^,]+),?.*$", "\\1", bib[starts]))

# --- 3. Select cited entries and write the local bib -----------------------
sel <- which(entry_keys %in% keys)
out_lines <- unlist(lapply(sel, function(i) c(bib[starts[i]:ends[i]], "")))
if (is.null(out_lines)) out_lines <- character(0)
writeLines(out_lines, out_bib, useBytes = TRUE)

# --- 4. Report ---------------------------------------------------------------
missing <- setdiff(keys[grepl("[0-9]", keys)], entry_keys)  # likely-real keys
message(sprintf("export-cited-refs: %d/%d cited keys matched -> %s",
                length(sel), length(entry_keys[entry_keys %in% keys]), out_bib))
if (length(missing)) {
  message("export-cited-refs: cited keys NOT found in master bib:\n  ",
          paste(sort(missing), collapse = "\n  "))
}
