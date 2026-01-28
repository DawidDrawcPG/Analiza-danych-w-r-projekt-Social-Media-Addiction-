# ============================================================
# Analiza: Social Media Addiction - studenci - skrypt .R
# CSV musi znajdować się w tym samym katalogu co skrypt / raport.
# ============================================================

# 1. Pakiety --------------------------------------------------
pkgs <- c("tidyverse", "broom", "corrplot", "scales", "forcats", "car", "pROC")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(broom)
library(corrplot)
library(scales)
library(forcats)
library(car)
library(pROC)

# 2. Wczytanie danych ----------------------------------------
data_file_name <- "Students Social Media Addiction.csv"

get_base_dir <- function() {
  # 1) RStudio: katalog aktywnego pliku
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(p))
  }
  # 2) Knit: katalog Rmd
  if (requireNamespace("knitr", quietly = TRUE)) {
    p <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) "")
    if (nzchar(p)) return(dirname(p))
  }
  # 3) fallback
  getwd()
}

base_dir0 <- get_base_dir()
candidates <- unique(c(base_dir0, getwd(), dirname(base_dir0), dirname(getwd())))

data_file <- NA_character_
for (dd in candidates) {
  if (!is.na(dd) && nzchar(dd)) {
    fp <- file.path(dd, data_file_name)
    if (file.exists(fp)) { data_file <- fp; break }
  }
}



d <- readr::read_csv(data_file, show_col_types = FALSE)

# folder na wykresy: ZAWSZE obok CSV
base_dir <- dirname(data_file)
fig_dir  <- file.path(base_dir, "fig")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

cat("\nCSV -> ", data_file, "\n", sep = "")
cat("PNG -> ", normalizePath(fig_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")

# szybki test zapisu (jak to nie przejdzie, to nic nie zapisze)
test_fp <- file.path(fig_dir, "_test_write.txt")
ok_write <- tryCatch({ writeLines("ok", test_fp); TRUE }, error = function(e) FALSE)
if (!ok_write) stop("Nie mogę zapisywać do folderu fig: ", fig_dir)

# helper do zapisu wykresów
save_plot <- function(filename, plot, width = 8, height = 5, dpi = 300) {
  fp <- file.path(fig_dir, filename)
  ggplot2::ggsave(fp, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  if (file.exists(fp)) {
    cat("OK: ", fp, "\n", sep = "")
  } else {
    warning("Nie zapisalo sie: ", fp)
  }
}

# 3. Czyszczenie danych --------------------------------------
cat_cols <- c(
  "Gender", "Academic_Level", "Country", "Most_Used_Platform",
  "Affects_Academic_Performance", "Relationship_Status"
)
cat_cols <- intersect(cat_cols, names(d))

d <- d %>%
  mutate(across(all_of(cat_cols), as.factor))

if ("Affects_Academic_Performance" %in% names(d)) {
  d <- d %>% mutate(
    Affects_Academic_Performance = fct_relevel(Affects_Academic_Performance, "No", "Yes")
  )
}

if ("Country" %in% names(d)) {
  d <- d %>% mutate(Country = fct_lump_n(Country, n = 10, other_level = "Other"))
}

# 4. Kontrola danych -----------------------------------------
cat("\n=== 4. Kontrola danych ===\n")
glimpse(d)
cat("Wymiary:", paste(dim(d), collapse = " x "), "\n")

cat("\nBraki (NA):\n")
print(colSums(is.na(d)))

cat("\nDuplikaty Student_ID:\n")
if ("Student_ID" %in% names(d)) {
  print(d %>% count(Student_ID) %>% filter(n > 1))
} else {
  cat("Brak kolumny Student_ID.\n")
}

# 4.1 Zakresy
cat("\nZakresy:\n")
check_range <- function(df, var, lo, hi) {
  if (!var %in% names(df)) {
    cat(sprintf("- %s: brak kolumny\n", var))
    return(invisible(NULL))
  }
  x <- df[[var]]
  if (!is.numeric(x)) {
    cat(sprintf("- %s: nie jest numeric\n", var))
    return(invisible(NULL))
  }
  bad <- which(x < lo | x > hi)
  cat(sprintf("- %s: poza [%s, %s] -> %d wierszy\n", var, lo, hi, length(bad)))
}

check_range(d, "Age", 15, 60)
check_range(d, "Avg_Daily_Usage_Hours", 0, 24)
check_range(d, "Sleep_Hours_Per_Night", 0, 24)
check_range(d, "Mental_Health_Score", 0, 10)
check_range(d, "Conflicts_Over_Social_Media", 0, 10)
check_range(d, "Addicted_Score", 0, 10)

# 5. Statystyki opisowe --------------------------------------
cat("\n=== 5. Statystyki opisowe ===\n")

num_cols <- d %>% select(where(is.numeric)) %>% select(-any_of("Student_ID"))

if (ncol(num_cols) > 0) {
  stats_tbl <- num_cols %>%
    summarise(across(
      everything(),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        min  = ~min(.x, na.rm = TRUE),
        q25  = ~quantile(.x, 0.25, na.rm = TRUE),
        med  = ~median(.x, na.rm = TRUE),
        q75  = ~quantile(.x, 0.75, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )) %>%
    pivot_longer(
      everything(),
      names_to = c("zmienna", "miara"),
      names_sep = "__",
      values_to = "wartosc"
    ) %>%
    pivot_wider(names_from = miara, values_from = wartosc) %>%
    arrange(zmienna)
  
  print(stats_tbl)
} else {
  cat("Brak kolumn liczbowych do statystyk.\n")
}

desc_cat <- function(x) {
  tibble(wartosc = as.character(x)) %>%
    count(wartosc, sort = TRUE) %>%
    mutate(pct = round(100 * n / sum(n), 1))
}

cat_vars <- c("Gender","Academic_Level","Most_Used_Platform",
              "Affects_Academic_Performance","Relationship_Status","Country")

for (v in cat_vars) {
  if (v %in% names(d)) {
    cat("\n", v, "\n", sep = "")
    print(desc_cat(d[[v]]))
  }
}

# 6. Testy ----------------------------------------------------
cat("\n=== 6. Testy ===\n")

if (!("Affects_Academic_Performance" %in% names(d))) {
  cat("Brak Affects_Academic_Performance, pomijam testy Yes/No.\n")
} else {
  
  need_vars <- c("Avg_Daily_Usage_Hours","Sleep_Hours_Per_Night","Addicted_Score",
                 "Mental_Health_Score","Conflicts_Over_Social_Media")
  miss_vars <- setdiff(need_vars, names(d))
  if (length(miss_vars) > 0) {
    cat("Brakuje kolumn do testow:", paste(miss_vars, collapse = ", "), "\n")
  }
  
  group_means <- d %>%
    group_by(Affects_Academic_Performance) %>%
    summarise(
      n = n(),
      usage_mean = if ("Avg_Daily_Usage_Hours" %in% names(d)) mean(Avg_Daily_Usage_Hours, na.rm = TRUE) else NA_real_,
      sleep_mean = if ("Sleep_Hours_Per_Night" %in% names(d)) mean(Sleep_Hours_Per_Night, na.rm = TRUE) else NA_real_,
      addicted_mean = if ("Addicted_Score" %in% names(d)) mean(Addicted_Score, na.rm = TRUE) else NA_real_,
      mental_mean = if ("Mental_Health_Score" %in% names(d)) mean(Mental_Health_Score, na.rm = TRUE) else NA_real_,
      conflicts_mean = if ("Conflicts_Over_Social_Media" %in% names(d)) mean(Conflicts_Over_Social_Media, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
  print(group_means)
  
  # 6.1 Nieparametryczne + Cohen d
  cohen_d_yes_no <- function(x, g, ref = "No") {
    g <- droplevels(as.factor(g))
    if (length(levels(g)) != 2) return(NA_real_)
    g <- relevel(g, ref = ref)
    
    x0 <- x[g == levels(g)[1]]
    x1 <- x[g == levels(g)[2]]
    
    n0 <- sum(!is.na(x0)); n1 <- sum(!is.na(x1))
    s0 <- sd(x0, na.rm = TRUE); s1 <- sd(x1, na.rm = TRUE)
    if (is.na(s0) || is.na(s1) || n0 < 2 || n1 < 2) return(NA_real_)
    
    sp <- sqrt(((n0 - 1) * s0^2 + (n1 - 1) * s1^2) / (n0 + n1 - 2))
    if (sp == 0 || is.na(sp)) return(NA_real_)
    
    m0 <- mean(x0, na.rm = TRUE); m1 <- mean(x1, na.rm = TRUE)
    (m1 - m0) / sp
  }
  
  wilcox_safe <- function(formula, data) {
    suppressWarnings(wilcox.test(formula, data = data, exact = FALSE))
  }
  
  num_yesno <- c("Avg_Daily_Usage_Hours","Sleep_Hours_Per_Night","Addicted_Score",
                 "Mental_Health_Score","Conflicts_Over_Social_Media","Age")
  num_yesno <- intersect(num_yesno, names(d))
  
  tests_yesno <- purrr::map_dfr(num_yesno, function(v) {
    f <- as.formula(paste0(v, " ~ Affects_Academic_Performance"))
    tt <- t.test(f, data = d)
    ww <- wilcox_safe(f, data = d)
    d_eff <- cohen_d_yes_no(d[[v]], d$Affects_Academic_Performance, ref = "No")
    
    est <- unname(tt$estimate) # mean(No), mean(Yes)
    diff_yes_minus_no <- as.numeric(est[2] - est[1])
    
    tibble(
      zmienna = v,
      diff_mean_yes_minus_no = diff_yes_minus_no,
      t_p = tt$p.value,
      wilcox_p = ww$p.value,
      cohen_d = d_eff
    )
  }) %>% mutate(across(where(is.numeric), ~round(.x, 4)))
  
  cat("\nYes vs No (t + Wilcox + Cohen d):\n")
  print(tests_yesno)
  
  # 6.2 Chi^2
  cramers_v <- function(tab) {
    cs <- suppressWarnings(chisq.test(tab, correct = FALSE))
    chi2 <- as.numeric(cs$statistic)
    n <- sum(tab)
    r <- nrow(tab); k <- ncol(tab)
    sqrt(chi2 / (n * (min(r - 1, k - 1))))
  }
  
  cat_tests <- c("Gender","Academic_Level","Most_Used_Platform","Relationship_Status")
  cat_tests <- intersect(cat_tests, names(d))
  
  chi_tbl <- purrr::map_dfr(cat_tests, function(v) {
    tab <- table(d$Affects_Academic_Performance, d[[v]])
    cs <- suppressWarnings(chisq.test(tab))
    tibble(
      zmienna = v,
      chi2_p = cs$p.value,
      cramers_v = cramers_v(tab)
    )
  }) %>% mutate(across(where(is.numeric), ~round(.x, 4)))
  
  cat("\nChi^2 (Affects_Academic_Performance vs kategorie):\n")
  print(chi_tbl)
  
  # 6.3 Kruskal-Wallis
  cat("\nKruskal-Wallis: Addicted_Score ~ Most_Used_Platform (top 6)\n")
  if ("Most_Used_Platform" %in% names(d) && "Addicted_Score" %in% names(d)) {
    d_pl <- d %>% mutate(Most_Used_Platform = fct_lump_n(Most_Used_Platform, n = 6, other_level = "Other"))
    print(kruskal.test(Addicted_Score ~ Most_Used_Platform, data = d_pl))
    
    cat("\nPost-hoc (pairwise Wilcoxon, BH):\n")
    pw <- suppressWarnings(pairwise.wilcox.test(
      d_pl$Addicted_Score, d_pl$Most_Used_Platform,
      p.adjust.method = "BH", exact = FALSE
    ))
    print(pw)
  }
  
  cat("\nKruskal-Wallis: Addicted_Score ~ Academic_Level\n")
  if ("Academic_Level" %in% names(d) && "Addicted_Score" %in% names(d)) {
    print(kruskal.test(Addicted_Score ~ Academic_Level, data = d))
    
    cat("\nPost-hoc (pairwise Wilcoxon, BH):\n")
    pw2 <- suppressWarnings(pairwise.wilcox.test(
      d$Addicted_Score, d$Academic_Level,
      p.adjust.method = "BH", exact = FALSE
    ))
    print(pw2)
  }
}

# 7. Korelacje ------------------------------------------------
cat("\n=== 7. Korelacje ===\n")
if (ncol(num_cols) >= 2) {
  corr <- cor(num_cols, use = "pairwise.complete.obs")
  print(round(corr, 2))
} else {
  corr <- NULL
  cat("Za malo kolumn liczbowych do korelacji.\n")
}

corr_pairs <- list(
  c("Avg_Daily_Usage_Hours","Addicted_Score"),
  c("Sleep_Hours_Per_Night","Addicted_Score"),
  c("Mental_Health_Score","Addicted_Score"),
  c("Conflicts_Over_Social_Media","Addicted_Score"),
  c("Age","Addicted_Score")
)

corr_tbl <- purrr::map_dfr(corr_pairs, function(p) {
  x <- p[1]; y <- p[2]
  if (!(x %in% names(d) && y %in% names(d))) return(NULL)
  ct <- cor.test(d[[x]], d[[y]], method = "spearman", exact = FALSE)
  tibble(x = x, y = y, rho = as.numeric(ct$estimate), p = ct$p.value)
}) %>% mutate(across(where(is.numeric), ~round(.x, 4)))

cat("\nSpearman (wybrane pary):\n")
print(corr_tbl)

# 8. Modele ---------------------------------------------------
cat("\n=== 8. Modele ===\n")

# 8.1 Model liniowy
if (all(c("Addicted_Score","Avg_Daily_Usage_Hours","Sleep_Hours_Per_Night","Mental_Health_Score",
          "Conflicts_Over_Social_Media","Affects_Academic_Performance","Gender","Most_Used_Platform") %in% names(d))) {
  
  d_m1 <- d %>%
    mutate(Most_Used_Platform = fct_lump_n(Most_Used_Platform, n = 6, other_level = "Other"))
  
  m1 <- lm(
    Addicted_Score ~ Avg_Daily_Usage_Hours + Sleep_Hours_Per_Night + Mental_Health_Score +
      Conflicts_Over_Social_Media + Affects_Academic_Performance + Gender + Most_Used_Platform,
    data = d_m1
  )
  print(summary(m1))
  
  cat("\nVIF:\n")
  print(car::vif(m1))
  
  cat("\nCook's distance (top 10):\n")
  cd <- cooks.distance(m1)
  top_cd <- order(cd, decreasing = TRUE)[1:min(10, length(cd))]
  print(tibble(
    row = top_cd,
    Student_ID = if ("Student_ID" %in% names(d_m1)) d_m1$Student_ID[top_cd] else NA,
    cooks_d = round(cd[top_cd], 4)
  ))
  
} else {
  m1 <- NULL
  cat("Brakuje kolumn do modelu liniowego, pomijam.\n")
}

# 8.2 Model logistyczny
cat("\nModel logistyczny\n")
roc_obj <- NULL
auc_val <- NA_real_
best_thr <- NA_real_

if (all(c("Affects_Academic_Performance","Avg_Daily_Usage_Hours","Sleep_Hours_Per_Night","Age","Gender","Academic_Level") %in% names(d))) {
  d2 <- d %>%
    mutate(Affects_Academic_Performance = fct_relevel(Affects_Academic_Performance, "No", "Yes"))
  
  m_logit <- glm(
    Affects_Academic_Performance ~ Avg_Daily_Usage_Hours + Sleep_Hours_Per_Night +
      Age + Gender + Academic_Level,
    data = d2,
    family = binomial()
  )
  
  or_tbl <- broom::tidy(m_logit, exponentiate = TRUE, conf.int = TRUE) %>%
    transmute(
      term = term,
      OR = estimate,
      OR_low = conf.low,
      OR_high = conf.high,
      p.value = p.value
    ) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))
  
  print(or_tbl)
  
  pr <- predict(m_logit, type = "response")
  pred05 <- ifelse(pr >= 0.5, "Yes", "No")
  tab05 <- table(Pred = pred05, True = d2$Affects_Academic_Performance)
  print(tab05)
  cat("Accuracy (cutoff 0.5): ", round(mean(pred05 == d2$Affects_Academic_Performance), 3), "\n", sep = "")
  
  roc_obj <- pROC::roc(d2$Affects_Academic_Performance, pr,
                       levels = c("No", "Yes"), direction = "<")
  auc_val <- as.numeric(pROC::auc(roc_obj))
  cat("AUC: ", round(auc_val, 3), "\n", sep = "")
  
  best <- pROC::coords(roc_obj, "best", best.method = "youden", transpose = FALSE)
  best_thr <- as.numeric(best["threshold"])
  cat("Prog (Youden): ", round(best_thr, 3), "\n", sep = "")
  
  pred_best <- ifelse(pr >= best_thr, "Yes", "No")
  tab_best <- table(Pred = pred_best, True = d2$Affects_Academic_Performance)
  print(tab_best)
  cat("Accuracy (best threshold): ", round(mean(pred_best == d2$Affects_Academic_Performance), 3), "\n", sep = "")
  
} else {
  cat("Brakuje kolumn do logita, pomijam.\n")
}

# 8.3 Wnioski
cat("\nWnioski (krotko):\n")
if (exists("group_means")) {
  y_u <- group_means$usage_mean[group_means$Affects_Academic_Performance == "Yes"] -
    group_means$usage_mean[group_means$Affects_Academic_Performance == "No"]
  y_s <- group_means$sleep_mean[group_means$Affects_Academic_Performance == "No"] -
    group_means$sleep_mean[group_means$Affects_Academic_Performance == "Yes"]
  y_a <- group_means$addicted_mean[group_means$Affects_Academic_Performance == "Yes"] -
    group_means$addicted_mean[group_means$Affects_Academic_Performance == "No"]
  
  cat(
    "W danych osoby deklarujace wplyw social mediow na nauke (Yes) maja srednio:\n",
    "- wiecej czasu w social mediach o ~", round(y_u, 2), " h dziennie,\n",
    "- mniej snu o ~", round(y_s, 2), " h na dobe,\n",
    "- wyzszy Addicted_Score o ~", round(y_a, 2), " pkt.\n",
    "To sa zaleznosci korelacyjne (nie dowod przyczyny).\n",
    sep = ""
  )
} else {
  cat("Brak group_means (testy Yes/No byly pominiete).\n")
}

# 9. Wykresy --------------------------------------------------
cat("\n=== 9. Wykresy ===\n")

# Wykresy robimy tylko jesli sa kolumny
if ("Most_Used_Platform" %in% names(d)) {
  p1 <- d %>%
    count(Most_Used_Platform, sort = TRUE) %>%
    ggplot(aes(x = reorder(Most_Used_Platform, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "Platforma", y = "Liczba osob", title = "Najczesciej uzywana platforma") +
    theme_minimal()
  save_plot("01_platformy.png", p1)
}

if ("Avg_Daily_Usage_Hours" %in% names(d)) {
  p2 <- ggplot(d, aes(x = Avg_Daily_Usage_Hours)) +
    geom_histogram(bins = 25) +
    labs(x = "Godziny dziennie", y = "Liczba osob", title = "Rozklad czasu uzycia") +
    theme_minimal()
  save_plot("02_hist_uzycie.png", p2)
  
  if ("Addicted_Score" %in% names(d)) {
    p3 <- ggplot(d, aes(x = Avg_Daily_Usage_Hours, y = Addicted_Score)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
      labs(x = "Godziny dziennie", y = "Addicted_Score", title = "Czas uzycia a uzaleznienie") +
      theme_minimal()
    save_plot("03_usage_vs_addicted.png", p3)
  }
}

if (all(c("Sleep_Hours_Per_Night","Addicted_Score") %in% names(d))) {
  p4 <- ggplot(d, aes(x = Sleep_Hours_Per_Night, y = Addicted_Score)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
    labs(x = "Sen (h/noc)", y = "Addicted_Score", title = "Sen a uzaleznienie") +
    theme_minimal()
  save_plot("04_sleep_vs_addicted.png", p4)
}

if (all(c("Affects_Academic_Performance","Avg_Daily_Usage_Hours") %in% names(d))) {
  p5 <- ggplot(d, aes(x = Affects_Academic_Performance, y = Avg_Daily_Usage_Hours)) +
    geom_boxplot() +
    labs(x = "Wplywa na nauke?", y = "Godziny dziennie", title = "Czas uzycia: Yes vs No") +
    theme_minimal()
  save_plot("05_box_yesno_usage.png", p5)
}

if ("Addicted_Score" %in% names(d)) {
  p6 <- ggplot(d, aes(x = Addicted_Score)) +
    geom_histogram(binwidth = 1, boundary = 0) +
    labs(x = "Addicted_Score", y = "Liczba osob", title = "Rozklad Addicted_Score") +
    theme_minimal()
  save_plot("06_hist_addicted.png", p6)
}

if (all(c("Affects_Academic_Performance","Addicted_Score") %in% names(d))) {
  p7 <- ggplot(d, aes(x = Affects_Academic_Performance, y = Addicted_Score)) +
    geom_boxplot() +
    labs(x = "Wplywa na nauke?", y = "Addicted_Score", title = "Uzaleznienie: Yes vs No") +
    theme_minimal()
  save_plot("07_box_yesno_addicted.png", p7)
}

if (all(c("Most_Used_Platform","Addicted_Score") %in% names(d))) {
  p8 <- d %>%
    mutate(Most_Used_Platform = fct_lump_n(Most_Used_Platform, n = 6, other_level = "Other")) %>%
    ggplot(aes(x = Most_Used_Platform, y = Addicted_Score)) +
    geom_boxplot() +
    coord_flip() +
    labs(x = "Platforma (top 6 + Other)", y = "Addicted_Score", title = "Uzaleznienie a platforma") +
    theme_minimal()
  save_plot("08_box_platform_addicted.png", p8)
}

if (all(c("Age","Addicted_Score") %in% names(d))) {
  p9 <- ggplot(d, aes(x = Age, y = Addicted_Score)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
    labs(x = "Wiek", y = "Addicted_Score", title = "Wiek a uzaleznienie") +
    theme_minimal()
  save_plot("09_age_vs_addicted.png", p9)
}

# corrplot do PNG
if (!is.null(corr)) {
  png(file.path(fig_dir, "12_corrplot.png"), width = 900, height = 700)
  corrplot::corrplot(corr, method = "color", type = "upper", tl.cex = 0.8)
  dev.off()
  if (file.exists(file.path(fig_dir, "12_corrplot.png"))) cat("OK: 12_corrplot.png\n")
}

# Diagnostyka lm do PNG
if (!is.null(m1)) {
  png(file.path(fig_dir, "10_lm_resid_fitted.png"), width = 900, height = 700)
  plot(m1$fitted.values, resid(m1),
       xlab = "Wartosci dopasowane", ylab = "Reszty",
       main = "Reszty vs dopasowanie")
  abline(h = 0, lty = 2)
  dev.off()
  if (file.exists(file.path(fig_dir, "10_lm_resid_fitted.png"))) cat("OK: 10_lm_resid_fitted.png\n")
  
  png(file.path(fig_dir, "11_lm_qq.png"), width = 900, height = 700)
  qqnorm(resid(m1)); qqline(resid(m1))
  dev.off()
  if (file.exists(file.path(fig_dir, "11_lm_qq.png"))) cat("OK: 11_lm_qq.png\n")
}

# ROC do PNG
if (!is.null(roc_obj)) {
  png(file.path(fig_dir, "13_roc_logit.png"), width = 900, height = 700)
  plot(roc_obj, main = paste0("ROC logit (AUC=", round(auc_val, 3), ")"))
  abline(a = 0, b = 1, lty = 2)
  dev.off()
  if (file.exists(file.path(fig_dir, "13_roc_logit.png"))) cat("OK: 13_roc_logit.png\n")
}

cat("\nPliki w fig:\n")
print(list.files(fig_dir))

# 10. Info o sesji -------------------------------------------
cat("\n=== 10. Info o sesji ===\n")
print(sessionInfo())

