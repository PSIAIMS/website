# Entry point: one shared summary feeds every headline, table, plot and sentence.
incidence_label <- function(n, denominator) {
  if (!length(n)) return(character())
  rate <- if (denominator == 0L) rep("N/A", length(n)) else sprintf("%.1f%%", 100 * n / denominator)
  paste0(n, "/", denominator, " (", rate, ")")
}

summarise_study <- function(data) {
  m <- data$metadata; cut <- m$DATA_CUT
  s <- data$subjects[data$subjects$RANDDT <= cut, , drop = FALSE]
  safety <- s[!is.na(s$FIRSTDOSEDT) & s$FIRSTDOSEDT <= cut, , drop = FALSE]
  # Teaching reveal: event rows and distinct affected patients are different.
  teae <- dplyr::inner_join(data$adverse_events, safety[c("USUBJID", "FIRSTDOSEDT")], by = "USUBJID")
  teae <- teae[teae$AESTDT >= teae$FIRSTDOSEDT & teae$AESTDT <= cut, , drop = FALSE]
  serious <- teae[teae$SERIOUS == "Y", , drop = FALSE]
  metrics <- list(n_randomised = nrow(s), n_active_sites = sum(data$sites$ACTIVATIONDT <= cut),
    n_active = sum(s$STATUS == "Active"), n_completed = sum(s$STATUS == "Completed"),
    n_discontinued = sum(s$STATUS == "Discontinued"), n_safety = nrow(safety),
    n_teae = length(unique(teae$USUBJID)), n_serious = length(unique(serious$USUBJID)),
    n_serious_events = nrow(serious),
    n_ae_discontinued = sum(s$STATUS == "Discontinued" & s$DISCREASON == "Adverse event", na.rm = TRUE))
  metrics$teae_label <- incidence_label(metrics$n_teae, metrics$n_safety)
  metrics$serious_label <- incidence_label(metrics$n_serious, metrics$n_safety)

  counts <- dplyr::summarise(dplyr::group_by(s, SITEID), randomised = dplyr::n(),
    active = sum(STATUS == "Active"), completed = sum(STATUS == "Completed"),
    discontinued = sum(STATUS == "Discontinued"),
    teae = sum(USUBJID %in% teae$USUBJID), serious_teae = sum(USUBJID %in% serious$USUBJID), .groups = "drop")
  # All planned sites retained, including zero-recruitment sites.
  sites <- dplyr::left_join(data$sites, counts, by = "SITEID")
  for (field in c("randomised", "active", "completed", "discontinued", "teae", "serious_teae")) {
    sites[[field]][is.na(sites[[field]])] <- 0L
  }
  sites$activation_status <- ifelse(sites$ACTIVATIONDT <= cut, "Active", "Planned")

  days <- seq(m$STUDY_START, cut, by = "day")
  recruitment <- data.frame(date = days, randomised = cumsum(tabulate(match(s$RANDDT, days), length(days))))
  planned_end <- as.Date("2026-12-31")
  recruitment$plan <- pmin(m$RECRUITMENT_TARGET,
    m$RECRUITMENT_TARGET * as.numeric(days - m$STUDY_START) / as.numeric(planned_end - m$STUDY_START))
  terms <- dplyr::summarise(dplyr::group_by(teae, AETERM), subjects = dplyr::n_distinct(USUBJID), .groups = "drop")
  terms <- as.data.frame(terms[order(-terms$subjects, terms$AETERM), ])
  terms$incidence <- incidence_label(terms$subjects, metrics$n_safety)
  context <- paste0(m$STUDYID, " | Data cut: ", format(cut, "%d %B %Y"),
                    " | Fictional study | Synthetic data | Demonstration only")
  narrative <- sprintf("%d of %d target patients are randomised across %d active sites. Patients with TEAE: %s of the safety population.",
    metrics$n_randomised, m$RECRUITMENT_TARGET, metrics$n_active_sites, metrics$teae_label)
  safety_table <- data.frame(Measure = c("Safety population (first dose)", "Patients with TEAE / safety N",
    "Patients with serious TEAE / safety N", "Serious TEAE events", "Patients discontinued due to AE"),
    Value = c(metrics$n_safety, metrics$teae_label, metrics$serious_label,
              metrics$n_serious_events, metrics$n_ae_discontinued), check.names = FALSE)
  list(metadata = m, metrics = metrics, recruitment = recruitment, sites = sites,
       terms = terms, teae = teae, context = context, narrative = narrative,
       safety_table = safety_table, planned_end = planned_end)
}
