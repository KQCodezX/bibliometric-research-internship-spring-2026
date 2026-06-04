# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr)

# 1. Load your original 100 articles file
raw_data <- read.csv("100articles.csv", stringsAsFactors = FALSE)

# 2. Define a Robust Cleaning and Merging Function
clean_and_merge <- function(text) {
  if (is.na(text) || text == "") return(NA)
  
  # Split by semicolon and clean whitespace
  kws <- unlist(strsplit(text, ";")) %>% str_trim() %>% str_to_lower()
  
  # Grouping Logic (Collapsing Synonyms)
  kws <- map_chr(kws, function(x) {
    case_when(
      # Group BCI / BMI / Brain-Machine
      str_detect(x, "brain.computer.interface|brain.machine.interface|bci|bmi|neural.interface.system") ~ "BCI",
      
      # Group EEG
      str_detect(x, "electroencephalography|electroencephalogram|^eeg$") ~ "EEG",
      
      # Group fMRI
      str_detect(x, "functional.magnetic.resonance|fmri") ~ "fMRI",
      
      # Group fNIRS
      str_detect(x, "near.infrared.spectroscopy|fnirs") ~ "fNIRS",
      
      # Group Motor Imagery
      str_detect(x, "^mi$|motor.imagery") ~ "motor_imagery",
      
      # Group Spinal Cord Injury
      str_detect(x, "spinal.cord.injury|^sci$") ~ "spinal_cord_injury",
      
      # Group Phantom Limb
      str_detect(x, "phantom.limb") ~ "phantom_limb_pain",
      
      # Group Electrocorticography
      str_detect(x, "electrocorticograph|ecog") ~ "ECoG",
      
      # Group Stroke
      str_detect(x, "stroke") ~ "stroke",
      
      # Group Neuropathic Pain
      str_detect(x, "neuropathic.pain") ~ "neuropathic_pain",
      
      # Group Virtual Reality
      str_detect(x, "virtual.reality|^vr$") ~ "virtual_reality",
      
      # Group P300
      str_detect(x, "p300") ~ "P300",
      
      TRUE ~ x
    )
  })
  
  # Formatting Logic (Snake Case + Specific Acronym Casing)
  kws <- kws %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("-", "_") %>%
    str_replace_all("[^a-zA-Z0-9_]", "")
  
  # Manual Acronym Fixes (Ensuring specific casing)
  kws <- str_replace_all(kws, "eeg", "EEG")
  kws <- str_replace_all(kws, "bci", "BCI")
  kws <- str_replace_all(kws, "fmri", "fMRI")
  kws <- str_replace_all(kws, "ecog", "ECoG")
  kws <- str_replace_all(kws, "fnirs", "fNIRS")
  kws <- str_replace_all(kws, "csp", "CSP")
  kws <- str_replace_all(kws, "ssvep", "SSVEP")
  kws <- str_replace_all(kws, "ersp", "ERSP")
  kws <- str_replace_all(kws, "fes", "FES")
  kws <- str_replace_all(kws, "cnn", "CNN")
  kws <- str_replace_all(kws, "lstm", "LSTM")
  
  return(unique(kws))
}

# 3. Process the Data
processed_data <- raw_data %>%
  mutate(article_id = row_number()) %>%
  mutate(clean_kws = map(Author.Keywords, clean_and_merge)) %>%
  unnest(clean_kws) %>%
  filter(!is.na(clean_kws))

# 4. Create the Co-occurrence Matrix
incidence_matrix <- table(processed_data$article_id, processed_data$clean_kws)
co_matrix <- t(incidence_matrix) %*% incidence_matrix

# 5. Export to CSV
write.csv(as.matrix(co_matrix), "Clean_Keyword_Matrix.csv")