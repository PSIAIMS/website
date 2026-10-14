# Optional regeneration: one fixed-seed master history, censored at each cut.
source("R/data.R")
source("R/summaries.R")
set.seed(260501)
cuts <- as.Date(c("2026-10-01", "2026-10-15"))
sites <- data.frame(SITEID = as.character(101:112),
  COUNTRY = rep(c("UK", "USA", "Germany", "Spain"), each = 3),
  ACTIVATIONDT = c(as.Date("2026-05-01") + 0:9, as.Date(c("2026-10-05", "2026-10-08"))), TARGET = 25L)
rand <- c(sort(sample(seq(as.Date("2026-05-10"), as.Date("2026-09-28"), by = "day"), 186, replace = TRUE)),
          as.Date("2026-10-01") + c(1:9, 9:14))
subjects <- data.frame(USUBJID = sprintf("ABC-001-%03d", 1:201),
  SITEID = c(sample(as.character(101:110), 186, replace = TRUE),
             c("101", "102", "103", "111", "104", "105", "112", "106", "107", "111", "108", "112", "109", "110", "111")),
  TRT01P = sample(rep(c("Drug A", "Placebo"), length.out = 201)), RANDDT = rand,
  FIRSTDOSEDT = rand + 3, STATUS = "Active", STATUSDT = rand, DISCREASON = NA_character_)
subjects$FIRSTDOSEDT[181:186] <- as.Date("2026-10-03") + 0:5
subjects$FIRSTDOSEDT[187:195] <- rand[187:195] + 2
subjects$FIRSTDOSEDT[196:201] <- as.Date(NA)
subjects$STATUS[1:22] <- "Completed"
subjects$STATUSDT[1:22] <- rand[1:22] + 60
subjects$STATUS[23:30] <- "Discontinued"
subjects$STATUSDT[23:30] <- rand[23:30] + 45
subjects$DISCREASON[23:25] <- "Adverse event"
subjects$DISCREASON[26:30] <- "Patient decision"
subjects$STATUS[31:34] <- "Completed"
subjects$STATUSDT[31:34] <- as.Date("2026-10-04") + 0:3
subjects$STATUS[35:36] <- "Discontinued"
subjects$STATUSDT[35:36] <- as.Date(c("2026-10-10", "2026-10-12"))
subjects$DISCREASON[35:36] <- c("Adverse event", "Patient decision")

# 90 affected patients, plus repeated events. Seven serious events in six people.
# Later: 14 newly affected people, three new serious events in two new people.
event_subject <- c(1:90, 1:30, 170:174, 91:100, 187:190, 1)
onset <- c(rand[1:90] + 8, rand[1:30] + 12, rand[170:174] + 1,
           rep(as.Date("2026-10-09"), 10), subjects$FIRSTDOSEDT[187:190] + 1, as.Date("2026-10-11"))
events <- data.frame(AEID = sprintf("AE-%04d", seq_along(event_subject)),
  USUBJID = subjects$USUBJID[event_subject],
  AETERM = sample(c("Headache", "Nausea", "Fatigue", "Dizziness", "Rash", "Abdominal pain"),
                  length(event_subject), replace = TRUE),
  AESTDT = onset, AEENDT = onset + sample(5:200, length(onset), replace = TRUE),
  SEVERITY = sample(c("Mild", "Moderate", "Severe"), length(onset), replace = TRUE,
                    prob = c(0.55, 0.35, 0.10)), SERIOUS = "N",
  RELATED = sample(c("Y", "N"), length(onset), replace = TRUE))
events$SERIOUS[c(1:6, 91, 126, 136, 140)] <- "Y"
# Deliberate independent attributes: moderate serious and severe nonserious events.
events$SEVERITY[1] <- "Moderate"
events$SEVERITY[10] <- "Severe"

make_cut <- function(cut) {
  s <- subjects[subjects$RANDDT <= cut, ]
  future_status <- s$STATUSDT > cut
  s$STATUS[future_status] <- "Active"
  s$STATUSDT[future_status] <- s$RANDDT[future_status]
  s$DISCREASON[future_status] <- NA_character_
  s$FIRSTDOSEDT[!is.na(s$FIRSTDOSEDT) & s$FIRSTDOSEDT > cut] <- as.Date(NA)
  a <- events[events$AESTDT <= cut, ]
  a$AEENDT[a$AEENDT > cut & !is.na(a$AEENDT)] <- as.Date(NA)
  m <- data.frame(STUDYID = "ABC-001", DATA_CUT = cut, STUDY_START = as.Date("2026-05-01"),
                  RECRUITMENT_TARGET = 300L, DATA_LABEL = "Synthetic demonstration data")
  list(metadata = m, subjects = s, adverse_events = a, sites = sites, snapshot = paste0("snapshot_", cut))
}

expected <- read.csv("demo/expected_metrics.csv", stringsAsFactors = FALSE)
prepared <- lapply(cuts, make_cut)
# Validate every cut and expected metric BEFORE writing any CSV.
for (data in prepared) {
  validate_study_data(data)
  actual <- summarise_study(data)$metrics
  rows <- expected[expected$data_cut == as.character(data$metadata$DATA_CUT), ]
  for (i in seq_len(nrow(rows))) {
    assert_data(identical(as.integer(actual[[rows$metric[i]]]), as.integer(rows$expected[i])),
                paste("Unexpected generated metric:", rows$metric[i]))
  }
}
for (data in prepared) {
  folder <- file.path("data", data$snapshot)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  for (name in c("metadata", "subjects", "adverse_events", "sites")) {
    write.csv(data[[name]], file.path(folder, paste0(name, ".csv")), row.names = FALSE,
              na = "", fileEncoding = "UTF-8")
  }
}
cat("Regenerated both validated cuts with seed 260501. Pointer unchanged.\n")
