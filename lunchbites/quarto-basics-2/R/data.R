# Entry point: load all inputs from one allowed snapshot, then validate them.
allowed_snapshots <- c("snapshot_2026-10-01", "snapshot_2026-10-15")

assert_data <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
}

read_typed_csv <- function(path, types, date_columns = character()) {
  assert_data(file.exists(path), paste("Missing file:", path))
  x <- read.csv(path, colClasses = types, na.strings = "", check.names = FALSE,
                stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  assert_data(identical(names(x), names(types)), paste("Unexpected schema:", path))
  for (field in date_columns) {
    raw <- x[[field]]
    parsed <- as.Date(raw, format = "%Y-%m-%d")
    good <- is.na(raw) | (!is.na(parsed) & grepl("^\\d{4}-\\d{2}-\\d{2}$", raw) &
                          format(parsed, "%Y-%m-%d") == raw)
    assert_data(all(good), paste("Invalid ISO date in", path, field))
    x[[field]] <- parsed
  }
  x
}

load_study_data <- function(root = ".", snapshot = NULL) {
  if (is.null(snapshot)) {
    pointer <- file.path(root, "data", "current_snapshot.txt")
    assert_data(file.exists(pointer), "Missing data/current_snapshot.txt")
    snapshot <- readLines(pointer, warn = FALSE, encoding = "UTF-8")
  }
  assert_data(length(snapshot) == 1L && !is.na(snapshot) &&
                snapshot %in% allowed_snapshots, "Snapshot must be one of the two allowed folder names")
  folder <- file.path(root, "data", snapshot)
  metadata <- read_typed_csv(file.path(folder, "metadata.csv"),
    c(STUDYID = "character", DATA_CUT = "character", STUDY_START = "character",
      RECRUITMENT_TARGET = "integer", DATA_LABEL = "character"), c("DATA_CUT", "STUDY_START"))
  subjects <- read_typed_csv(file.path(folder, "subjects.csv"),
    c(USUBJID = "character", SITEID = "character", TRT01P = "character", RANDDT = "character",
      FIRSTDOSEDT = "character", STATUS = "character", STATUSDT = "character",
      DISCREASON = "character"), c("RANDDT", "FIRSTDOSEDT", "STATUSDT"))
  adverse_events <- read_typed_csv(file.path(folder, "adverse_events.csv"),
    c(AEID = "character", USUBJID = "character", AETERM = "character", AESTDT = "character",
      AEENDT = "character", SEVERITY = "character", SERIOUS = "character", RELATED = "character"),
    c("AESTDT", "AEENDT"))
  sites <- read_typed_csv(file.path(folder, "sites.csv"),
    c(SITEID = "character", COUNTRY = "character", ACTIVATIONDT = "character", TARGET = "integer"),
    "ACTIVATIONDT")
  result <- list(metadata = metadata, subjects = subjects, adverse_events = adverse_events,
                 sites = sites, snapshot = snapshot)
  validate_study_data(result)
  result
}

validate_study_data <- function(data) {
  m <- data$metadata; s <- data$subjects; a <- data$adverse_events; z <- data$sites
  assert_data(nrow(m) == 1L && !anyNA(m), "Metadata must contain one complete row")
  cut <- m$DATA_CUT
  assert_data(paste0("snapshot_", cut) == data$snapshot, "Metadata cut disagrees with folder")
  assert_data(m$STUDYID == "ABC-001" && m$STUDY_START == as.Date("2026-05-01") &&
                m$RECRUITMENT_TARGET == 300L && grepl("synthetic", m$DATA_LABEL, ignore.case = TRUE),
              "Unexpected study metadata")
  for (key in list(s$USUBJID, a$AEID, z$SITEID)) {
    assert_data(!anyNA(key) && all(nzchar(key)) && !anyDuplicated(key), "Missing or duplicate primary key")
  }
  assert_data(nrow(z) == 12L && setequal(z$SITEID, as.character(101:112)) &&
                !anyNA(z) && all(z$TARGET == 25L) &&
                all(z$COUNTRY %in% c("UK", "USA", "Germany", "Spain")), "Invalid site master")
  assert_data(all(s$SITEID %in% z$SITEID) && all(a$USUBJID %in% s$USUBJID), "Broken site/patient reference")
  assert_data(!anyNA(s[c("USUBJID", "SITEID", "TRT01P", "RANDDT", "STATUS", "STATUSDT")]),
              "Missing required patient value")
  activation <- z$ACTIVATIONDT[match(s$SITEID, z$SITEID)]
  assert_data(all(s$RANDDT >= activation & s$RANDDT >= m$STUDY_START & s$RANDDT <= cut),
              "Recruitment before activation/study start or after cut")
  assert_data(all(is.na(s$FIRSTDOSEDT) | (s$FIRSTDOSEDT >= s$RANDDT & s$FIRSTDOSEDT <= cut)),
              "Dose before randomisation or after cut")
  assert_data(all(s$STATUS %in% c("Active", "Completed", "Discontinued")) &&
                all(s$TRT01P %in% c("Drug A", "Placebo")), "Invalid status or treatment")
  assert_data(all(s$STATUSDT >= s$RANDDT & s$STATUSDT <= cut) &&
                all(s$STATUSDT[s$STATUS == "Active"] == s$RANDDT[s$STATUS == "Active"]),
              "Invalid status date")
  stopped <- s$STATUS == "Discontinued"
  assert_data(all(!is.na(s$DISCREASON[stopped])) && all(is.na(s$DISCREASON[!stopped])) &&
                all(s$DISCREASON[stopped] %in% c("Adverse event", "Patient decision")),
              "Invalid discontinuation reason")
  assert_data(!anyNA(a[c("USUBJID", "AETERM", "AESTDT", "SEVERITY", "SERIOUS", "RELATED")]) &&
                all(nzchar(a$AETERM)), "Missing required AE value")
  rand <- s$RANDDT[match(a$USUBJID, s$USUBJID)]
  assert_data(all(a$AESTDT > rand & a$AESTDT <= cut) &&
                all(is.na(a$AEENDT) | (a$AEENDT >= a$AESTDT & a$AEENDT <= cut)),
              "Invalid AE date or post-cut history")
  assert_data(all(a$SEVERITY %in% c("Mild", "Moderate", "Severe")) &&
                all(a$SERIOUS %in% c("Y", "N")) && all(a$RELATED %in% c("Y", "N")),
              "Invalid AE severity or flag")
  invisible(TRUE)
}
