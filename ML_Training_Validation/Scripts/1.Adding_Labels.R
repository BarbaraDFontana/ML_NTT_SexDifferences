library(readr)
library(dplyr)
library(purrr)

# =========================================================
# ALL FILES NEED TO BE A NUMBER TO MATCH ID GIVEN IN TABLE
# =========================================================

# =========================================================
# PATHS
# =========================================================

data_path <- "C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Data/CSVs"
label_path <- "C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Consensus_Labels.csv"

# =========================================================
# LOAD LABEL DATA
# =========================================================

labels <- read_csv(label_path)

# ---------------------------------------------------------
# CLEAN LABEL NAMES
# ---------------------------------------------------------

labels$Behavior.label <- trimws(labels$Behavior.label)

# Merge BS/TR into TR
labels$Behavior.label[
  labels$Behavior.label == "BS/TR"
] <- "TR"

# =========================================================
# GET ALL CSV FILES
# =========================================================

files <- list.files(
  path = data_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

# =========================================================
# FUNCTION TO LABEL ONE FILE
# =========================================================

label_file <- function(file_path, labels_df) {
  
  # -------------------------------------------------------
  # LOAD TRACKING DATA
  # -------------------------------------------------------
  
  df <- read_csv(
    file_path,
    show_col_types = FALSE
  )
  
  # Force consistent type
  df$animal_ID <- as.character(df$animal_ID)
  
  # -------------------------------------------------------
  # USE ROW NUMBER AS FRAME
  # -------------------------------------------------------
  
  df$frame_row <- 0:(nrow(df) - 1)
  
  # -------------------------------------------------------
  # EXTRACT ID FROM FILE NAME
  # -------------------------------------------------------
  
  file_id <- basename(file_path) |>
    tools::file_path_sans_ext() |>
    as.numeric()
  
  # -------------------------------------------------------
  # FILTER LABELS FOR THIS ID
  # -------------------------------------------------------
  
  id_labels <- labels_df %>%
    dplyr::filter(as.numeric(ID) == file_id)
  
  # -------------------------------------------------------
  # CREATE NEW COLUMNS
  # -------------------------------------------------------
  
  df$Behavior.label <- NA_character_
  df$Left_Total <- NA
  
  # -------------------------------------------------------
  # APPLY LABELS
  # -------------------------------------------------------
  
  if (nrow(id_labels) > 0) {
    
    for (i in 1:nrow(id_labels)) {
      
      start_f <- id_labels$Frame_Initial[i]
      end_f <- id_labels$Frame_Final[i]
      
      behavior_value <- id_labels$Behavior.label[i]
      left_value <- id_labels$Left_Total[i]
      
      rows_to_fill <- df$frame_row >= start_f &
        df$frame_row <= end_f
      
      df$Behavior.label[rows_to_fill] <- behavior_value
      df$Left_Total[rows_to_fill] <- left_value
    }
  }
  
  return(df)
}

# =========================================================
# CREATE LIST OF LABELED DATASETS
# =========================================================

labeled_list <- map(
  files,
  ~label_file(.x, labels)
)

# =========================================================
# NAME LIST USING FILE NAMES
# =========================================================

names(labeled_list) <- tools::file_path_sans_ext(
  basename(files)
)

# =========================================================
# CHECK RESULTS
# =========================================================

table(
  bind_rows(labeled_list)$Behavior.label,
  useNA = "ifany"
)

# =========================================================
# SAVE ALL FILES
# =========================================================

output_dir <- file.path(
  data_path,
  "labeled_output"
)

dir.create(
  output_dir,
  showWarnings = FALSE
)

walk2(
  labeled_list,
  names(labeled_list),
  ~write_csv(
    .x,
    file.path(
      output_dir,
      paste0(.y, "_labeled.csv")
    )
  )
)

cat("\nDone! Labels successfully assigned.\n")

# =========================================================
# SAVE COMPLETE LIST AS RDS
# =========================================================

saveRDS(
  labeled_list,
  file.path(
    data_path,
    "labeled_list.rds"
  )
)

cat("\nRDS list saved successfully.\n")