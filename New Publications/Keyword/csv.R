if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, igraph)

wos_file    <- "WoS.csv"
scopus_file <- "Scopus.csv"
output_csv  <- "Clean_Keyword_Matrix.csv"

# Loading data
wos <- read.csv(wos_file, stringsAsFactors = FALSE) %>%
  rename(
    title          = `Article.Title`,
    author_kw      = `Author.Keywords`,
    fallback_kw    = `Keywords.Plus`,
    year           = `Publication.Year`
  ) %>%
  mutate(source = "WoS") %>%
  select(title, author_kw, fallback_kw, year, source)

scopus <- read.csv(scopus_file, stringsAsFactors = FALSE) %>%
  rename(
    title          = `Title`,
    author_kw      = `Author.Keywords`,
    fallback_kw    = `Index.Keywords`,
    year           = `Year`
  ) %>%
  mutate(source = "Scopus") %>%
  select(title, author_kw, fallback_kw, year, source)

all_records <- bind_rows(wos, scopus)

# Removing the dupes articles
all_records <- all_records %>%
  mutate(title_key = str_squish(str_to_lower(title))) %>%
  # Keep the WoS record when there is a clash cuz it has more keywords
  arrange(title_key, factor(source, levels = c("WoS", "Scopus"))) %>%
  distinct(title_key, .keep_all = TRUE) %>%
  select(-title_key) %>%
  mutate(article_id = row_number())

# Selecting keyword field
all_records <- all_records %>%
  mutate(
    kw_raw = case_when(
      !is.na(author_kw) & str_trim(author_kw) != "" ~ author_kw,
      !is.na(fallback_kw) & str_trim(fallback_kw) != "" ~ fallback_kw,
      TRUE ~ NA_character_
    ),
    kw_source = case_when(
      !is.na(author_kw) & str_trim(author_kw) != "" ~ "author",
      !is.na(fallback_kw) & str_trim(fallback_kw) != "" ~ "fallback",
      TRUE ~ "none"
    )
  )

print(table(all_records$kw_source))

# Mapping
synonyms <- c(
  "brain[- ]computer[- ]interface|brain[- ]machine[- ]interface|\\bbci\\b|\\bbmi\\b|neural[- ]interface[- ]system" = "BCI",
  
  "electroencephalograph|electroencephalogram|\\beeg\\b" = "EEG",
  "functional[- ]magnetic[- ]resonance|\\bfmri\\b" = "fMRI",
  "near[- ]infrared[- ]spectroscopy|\\bfnirs\\b" = "fNIRS",
  "electrocorticograph|\\becog\\b" = "ECoG",
  "electromyograph|\\bemg\\b" = "EMG",
  "\\bp300\\b" = "P300",
  "\\bssvep\\b|steady[- ]state[- ]visual" = "SSVEP",
  
  "motor[- ]imagery|\\bmi\\b(?!.*magnet)" = "motor_imagery",   
  "motor[- ]cortex" = "motor_cortex",
  "motor[- ]recover|motor[- ]function|motor[- ]impair|motor[- ]control|motor[- ]learning|movement[- ]disorder" = "motor_function_recovery",
  
  "spinal[- ]cord[- ]injur|\\bsci\\b" = "spinal_cord_injury",
  "stroke[- ]rehab" = "stroke_rehabilitation",
  "\\bstroke\\b" = "stroke",
  "parkinson" = "parkinsons",
  "multiple[- ]sclerosis" = "multiple_sclerosis",
  "phantom[- ]limb" = "phantom_limb",
  
  "neuropathic[- ]pain" = "neuropathic_pain",
  "chronic[- ]pain|acute[- ]pain|nociceptive|pain[- ]relat" = "pain_outcomes",
  "\\bpain\\b" = "pain",
  
  "virtual[- ]reality|\\bvr\\b" = "virtual_reality",
  "augmented[- ]reality|\\bar\\b" = "augmented_reality",
  "functional[- ]electrical[- ]stimulation|\\bfes\\b" = "FES",
  "deep[- ]brain[- ]stimulation|\\bdbs\\b" = "DBS",
  "transcranial[- ]magnetic[- ]stimulation|\\btms\\b" = "TMS",
  "transcranial[- ]direct[- ]current|\\btdcs\\b" = "tDCS",
  "neurofeedback|nf[- ]training" = "neurofeedback",
  "neuroprostheti" = "neuroprosthetics",
  "neurorehabilit" = "neurorehabilitation",
  "\\brehabilitation\\b" = "rehabilitation",
  
  "common[- ]spatial[- ]pattern|\\bcsp\\b" = "CSP",
  "machine[- ]learning|\\bml\\b(?![a-z])" = "machine_learning",
  "deep[- ]learning" = "deep_learning",
  "neural[- ]network|\\bann\\b|\\bdnn\\b" = "neural_network",
  "convolutional[- ]neural|\\bcnn\\b" = "CNN",
  "long[- ]short[- ]term[- ]memory|\\blstm\\b" = "LSTM",
  "feature[- ]extract" = "feature_extraction",
  "signal[- ]process" = "signal_processing",
  
  "neuroplastic|neural[- ]plastic|cortical[- ]reorgani|use[- ]dependent[- ]plastic" = "neuroplasticity",
  
  "age|aging" = "age_demographic",
  "pediatric|child|children|adolescent|infant" = "pediatric_demographic",
  "gender|sex|female|male|women|men" = "gender_sex",
  "female|women|woman" = "female",
  "male|man|men" = "male",
  "race|ethnic|diversity|minority|representation|demographic" = "demographic_representation",
  
  "\\bhumans?\\b" = "human"
)

# Apply synonym mapping to a single keyword string
map_keyword <- function(kw) {
  kw_lc <- str_to_lower(kw)
  for (pattern in names(synonyms)) {
    if (str_detect(kw_lc, pattern)) {
      return(synonyms[[pattern]])
    }
  }
  # No match means clean up the raw keyword
  kw %>%
    str_to_lower() %>%
    str_squish() %>%
    str_replace_all("[\\s\\-]+", "_") %>%   # spaces / hyphens to underscore
    str_replace_all("[^a-z0-9_]", "")        # strip remaining punctuation
}

# Split, map, and deduplicate keywords per article 
split_keywords <- function(text) {
  if (is.na(text) || str_trim(text) == "") return(character(0))
  raw <- str_split(text, ";")[[1]]
  raw <- str_trim(raw)
  raw <- raw[nchar(raw) > 0]
  raw
}

processed <- all_records %>%
  filter(kw_source != "none") %>%
  mutate(
    kw_list = map(kw_raw, function(txt) {
      raw_kws  <- split_keywords(txt)
      mapped   <- map_chr(raw_kws, map_keyword)
      # Remove empty strings, then deduplicate WITHIN an article
      mapped   <- mapped[nchar(mapped) > 0]
      unique(mapped)
    })
  ) %>%
  select(article_id, year, kw_list) %>%
  unnest(kw_list) %>%
  rename(keyword = kw_list)

# Frequency filter to keep only keywords appearing in ≥ 2 articles
kw_freq <- processed %>%
  group_by(keyword) %>%
  summarise(n_articles = n_distinct(article_id), .groups = "drop") %>%
  arrange(desc(n_articles))

print(head(kw_freq, 20))

MIN_ARTICLES <- 2   # adjust here if needed
keep_kws <- kw_freq %>% filter(n_articles >= MIN_ARTICLES) %>% pull(keyword)

# Checking numbers of ≥2 articles
cat(sprintf("Keywords retained (≥%d articles): %d\n\n", MIN_ARTICLES, length(keep_kws)))

processed_filtered <- processed %>%
  filter(keyword %in% keep_kws)

# Remove generic/uninformative keywords (noise stoplist) if needed

#noise_words <- c(
#  "humans", "human", "male", "female", "adult", "adults",
#  "aged", "animals", "article", "review", "study",
#  "patient", "patients", "case_report", "clinical_trial",
#  "controlled_study", "randomized_controlled_trial",
#  "major_clinical_study", "priority_journal", "letter",
#  "note", "erratum"
#)

#processed_filtered <- processed_filtered %>%
#  filter(!keyword %in% noise_words)

# Build the co-occurrence matrix


# Incidence matrix: articles × keywords
incidence <- table(processed_filtered$article_id, processed_filtered$keyword)

# Co-occurrence: keyword × keyword  (symmetric)
co_matrix <- t(incidence) %*% incidence

# Sanity checks
stopifnot(isSymmetric(unclass(co_matrix)))          # must be symmetric
stopifnot(all(diag(co_matrix) == kw_freq$n_articles[match(rownames(co_matrix), kw_freq$keyword)]))

print(sort(diag(co_matrix), decreasing = TRUE)[1:5])

# Export

write.csv(as.matrix(co_matrix), output_csv)

# Optional: also export the keyword frequency table
write.csv(kw_freq, "Keyword_Frequencies.csv", row.names = FALSE)
