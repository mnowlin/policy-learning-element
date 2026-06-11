# trackdown-sync.R
# ---------------------------------------------------------------------------
# Round-trip the book's chapters between local .qmd files and Google Docs,
# so a coauthor can edit the prose in Google Docs / Word while the R code,
# citation keys (@key), and cross-references (@fig-, @tbl-) stay protected.
#
# Requires: trackdown >= 1.5.0 (Quarto/.qmd support) and a Google account.
#   install.packages("trackdown")
#
# Run this script with the working directory set to compete-book/
# (it operates on the chapter files in this folder).
#
# WORKFLOW
#   1. upload_chapters()    once, to create one Google Doc per chapter
#   2. coauthor edits the prose in Google Docs (code chunks are hidden)
#   3. download_chapters()  to pull their edits back into the local .qmd files
#   4. ...you edit locally, render, commit as usual...
#   5. update_chapters()    to push your local changes back up to Google Docs
#
# CAUTION
#   - update_chapters() OVERWRITES the Google Doc with your local version
#     (their un-downloaded edits would be lost). Always download_chapters()
#     and reconcile BEFORE you update_chapters().
#   - download_chapters() OVERWRITES your local .qmd with the Google Doc
#     version. Commit your local work first so git can recover it.
# ---------------------------------------------------------------------------

library(trackdown)

# First run opens a browser to authorize Google Drive access; the token is
# cached so later runs are non-interactive. To force a specific account:
# options(gargle_oauth_email = "you@example.com")

# Chapters to collaborate on (references.qmd is auto-generated; skip it).
# Names become the Google Doc titles -> keep them human-readable & ordered.
chapters <- c(
  "index.qmd" = "00 - Abstract",
  "chap1.qmd" = "01 - Introduction",
  "chap2.qmd" = "02",
  "chap3.qmd" = "03",
  "chap4.qmd" = "04 - Nuclear Application",
  "chap5.qmd" = "05",
  "chap6.qmd" = "06"
)

# All docs live in this Drive folder (created if absent), keeping them tidy.
gpath <- "trackdown/policy-learning-element"

# --- Step 1: initial upload (run ONCE per chapter) -------------------------
upload_chapters <- function(files = names(chapters)) {
  for (f in files) {
    message("Uploading ", f)
    trackdown::upload_file(
      file      = f,
      gfile     = unname(chapters[f]),
      gpath     = gpath,
      hide_code = TRUE        # hide code chunks + YAML from the coauthor
    )
  }
}

# --- Step 3: pull coauthor edits back into local .qmd ----------------------
download_chapters <- function(files = names(chapters)) {
  for (f in files) {
    message("Downloading ", f)
    trackdown::download_file(file = f, gfile = unname(chapters[f]), gpath = gpath)
  }
}

# --- Step 5: push your local edits up (OVERWRITES the Google Doc) ----------
update_chapters <- function(files = names(chapters)) {
  for (f in files) {
    message("Updating ", f)
    trackdown::update_file(
      file      = f,
      gfile     = unname(chapters[f]),
      gpath     = gpath,
      hide_code = TRUE
    )
  }
}

# --- Optional: render locally and upload the output beside each Doc --------
# Gives the coauthor a formatted reference (PDF) while they edit the markdown.
# render_file() both renders the .qmd and uploads it with its output attached.
render_chapters <- function(files = names(chapters)) {
  for (f in files) {
    message("Rendering + uploading ", f)
    trackdown::render_file(
      file      = f,
      gfile     = unname(chapters[f]),
      gpath     = gpath,
      hide_code = TRUE
    )
  }
}

# Single chapter? Just call the underlying functions directly, e.g.:
#   trackdown::upload_file("chap3.qmd", gfile = "03", gpath = gpath, hide_code = TRUE)
#   trackdown::download_file("chap3.qmd", gfile = "03", gpath = gpath)
