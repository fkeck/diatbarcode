# This script provides functions and demonstration for 
# processing Diat.barcode Excel database into DADA2-compatible FASTA files.

# It is intended to be run manually by the user and is
# not executed automatically when loading the diatbarcode package.

# Note that earlier DB versions had the xlsx in another format
#     and the algorithm for their processing into fasta was different (not available).

# Here we start by manually importing the sheets and locating the alignment column

# Using the functions, we find boundaries of the 263 bp metabarcoding fragment within the alignment,
#     based on dashes in the Rbcl-263bp aligned sequences
# Then we trim all rbcl sequences (both long and short) at these boundaries,
#     and retain only unique sequences without gaps.
# DADA2-compatible fasta files are exported after finding parent taxa for the species

# Note that some non-unique sequences have multiple species assignments
#     We export a report of these ambiguities, with their full taxonomy.
#     Fasta are exported either with or without these sequences.
#     This needs to be addressed further. 
#     sequence might be acceptable for DADA2 assignment even if it has "Fragilaria sp.__OR__Fragilaria crotonensis"

# When using the fasta files in https://github.com/fkeck/DADA2_diatoms_pipeline
#      simply specify their path in the tax_fas or spe_fas variables
#      and use modify the taxLevels argument in assignTaxonomy as follows:
#      taxLevels = c("Empire", "Kingdom", "Subkingdom", "Subphylum", "Class", "Subclass", "Order", "Family", "Genus", "Species"),

# Required packages:
library(readxl)
library(dplyr)
library(stringr)
library(Biostrings)
library(purrr)
library(tibble)
library(tidyr)


# IMPORT ####

## SPECIFY PATH TO YOUR DB DOWNLOAD HERE: ####
dbfile <- "path/to/your/downloaded/2025-09-04-Diat.barcode_release-version 15.2.xlsx"

seq_info  <- read_excel(dbfile, sheet = "sequences_info")
taxo_RCM  <- read_excel(dbfile, sheet = "taxo_RCM") %>%
  mutate(rank = str_extract(tolower(rank), "^[a-z]+")) # remove notes in the rank column and unify as lower-case
taxo_Adl  <- read_excel(dbfile, sheet = "taxo_Adl") %>%
  mutate(rank = str_extract(tolower(rank), "^[a-z]+"))

# add missing family Radialiplicataceae in taxo_Adl sheet
taxo_Adl[nrow(taxo_Adl)+1,] = taxo_RCM[taxo_RCM$`taxon name` == "Radialiplicataceae", 1:3]


# FUNCTIONS ####

# We are going to use aligned sequences in the column `Sequence aligned - rbcl only - aligned 04/09/2025` of db v15.2. 
# The function below facilitates correct column selection in future db versions where the date might change.
find_alignment_column <- function(df) {
  candidates <- names(df)[
    str_detect(tolower(names(df)), "sequence") &
      str_detect(tolower(names(df)), "rbcl") &
      str_detect(tolower(names(df)), "aligned")
  ]
  
  if (length(candidates) == 0) {
    stop("No aligned rbcL sequence column found.")
  }
  
  if (length(candidates) > 1) {
    warning(
      "Multiple candidate sequence columns found. Using first:\n",
      paste(candidates, collapse = ", ")
    )
  }
  
  candidates[1]
}


# function to find bounds of the 263 bp fragment in the alignment
find_amplicon_bounds <- function(
    seqs,
    expected_length = 263,
    csv_path = "amplicon_bounds_diagnostics.csv"
) {
  
  n <- length(seqs)
  
  start_pos <- sapply(seqs, function(s) {
    if (is.na(s)) return(NA_integer_)
    m <- regexpr("[ACGT]", s)
    if (is.na(m[1]) || m[1] == -1) NA_integer_ else m[1]
  })
  
  end_pos <- sapply(seqs, function(s) {
    if (is.na(s)) return(NA_integer_)
    m <- regexpr("[ACGT](?=[^ACGT]*$)", s, perl = TRUE)
    if (is.na(m[1]) || m[1] == -1) NA_integer_ else m[1]
  })
  
  df <- data.frame(
    seq_index = seq_len(n),
    start = start_pos,
    end   = end_pos,
    stringsAsFactors = FALSE
  )
  
  df$length <- with(df, end - start + 1)
  
  df <- df[!is.na(df$start) & !is.na(df$end), ]
  
  if (nrow(df) == 0) {
    stop("No valid amplicon boundaries detected in any sequence")
  }
  
  # ---- _summarize boundary pairs ----
  
  summary <- aggregate(
    seq_index ~ start + end + length,
    data = df,
    FUN = function(x) list(x)
  )
  
  summary$count <- sapply(summary$seq_index, length)
  
  # ---- _prefer correct length ----
  
  preferred <- summary[summary$length == expected_length, ]
  
  if (nrow(preferred) == 0) {
    warning(
      paste0(
        "No amplicon boundaries with expected length (",
        expected_length,
        " bp) found. Using most frequent boundary pair instead.\n",
        "Please inspect diagnostic CSV: ", csv_path
      ),
      call. = FALSE
    )
    chosen <- summary[which.max(summary$count), ]
  } else if (nrow(preferred) > 1) {
    warning(
      paste0(
        "Multiple boundary pairs with expected length (",
        expected_length,
        " bp) found. Using the most frequent one.\n",
        "Please inspect diagnostic CSV: ", csv_path
      ),
      call. = FALSE
    )
    chosen <- preferred[which.max(preferred$count), ]
  } else {
    chosen <- preferred
  }
  
  # ---- _prepare diagnostic table ----
  
  diag <- summary
  
  diag$seq_indices_list <- diag$seq_index
  
  diag$seq_indices <- vapply(
    diag$seq_index,
    function(x) paste(unlist(x), collapse = ";"),
    character(1)
  )
  
  diag$example_sequence <- vapply(
    seq_len(nrow(diag)),
    function(i) {
      idx <- diag$seq_indices_list[[i]][1]
      s <- seqs[[idx]]
      if (is.na(s)) return(NA_character_)
      substr(s, diag$start[i], diag$end[i])
    },
    character(1)
  )
  
  diag$seq_index <- NULL
  
  diag_export = diag
  diag_export$seq_indices_list = NULL
  
  write.csv2(diag_export, csv_path, row.names = FALSE)
  
  # ---- _return chosen bounds ----
  
  list(
    start  = chosen$start,
    end    = chosen$end,
    length = chosen$length,
    count  = chosen$count
  )
}

# function for IUPAC-aware reverse complement
revcomp_iupac <- function(seq) {
  
  comp <- c(
    A = "T", T = "A", C = "G", G = "C",
    R = "Y", Y = "R", W = "W", S = "S",
    K = "M", M = "K",
    B = "V", V = "B", D = "H", H = "D",
    N = "N"
  )
  
  bases <- strsplit(seq, "")[[1]]
  rc <- rev(comp[bases])
  
  paste(rc, collapse = "")
}

# function for regex on degenerate bases
iupac_to_regex <- function(seq) {
  map <- c(
    R = "[AG]", Y = "[CT]", W = "[AT]", S = "[GC]",
    K = "[GT]", M = "[AC]", B = "[CGT]",
    D = "[AGT]", H = "[ACT]", V = "[ACG]",
    N = "[ACGT]"
  )
  
  for (i in names(map)) {
    seq <- gsub(i, map[[i]], seq)
  }
  seq
}


# function for masking primers from the alignment
mask_primers <- function(seq, fwd_primer, rev_primer) {
  
  if (is.na(seq)) return(NA_character_)
  
  fwd_regex <- iupac_to_regex(fwd_primer)
  rev_rc    <- revcomp_iupac(rev_primer)
  rev_regex <- iupac_to_regex(rev_rc)
  
  m_fwd <- regexpr(fwd_regex, seq)
  if (!is.na(m_fwd[1]) && m_fwd[1] != -1) {
    len <- attr(m_fwd, "match.length")
    substr(seq, m_fwd[1], m_fwd[1] + len - 1) <- strrep("-", len)
  }
  
  m_rev <- regexpr(rev_regex, seq)
  if (!is.na(m_rev[1]) && m_rev[1] != -1) {
    len <- attr(m_rev, "match.length")
    substr(seq, m_rev[1], m_rev[1] + len - 1) <- strrep("-", len)
  }
  
  seq
}

# function to evaluate gaps in sequences (number of gap characters "-" and how many are at the start, end and inside the sequence)
gap_metrics <- function(seq) {
  
  if (is.na(seq)) {
    return(data.frame(
      n_gaps = NA_integer_,
      pct_gaps = NA_real_,
      gap_at_start = NA_integer_,
      gap_at_end = NA_integer_,
      gaps_internal = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }
  
  chars <- strsplit(seq, "")[[1]]
  n <- length(chars)
  
  gap_idx <- which(chars == "-")
  n_gaps <- length(gap_idx)
  
  gap_at_start <- if (startsWith(seq, "-")) {
    rle(chars)$lengths[1]
  } else {
    0L
  }
  
  gap_at_end <- if (endsWith(seq, "-")) {
    tail(rle(chars)$lengths, 1)
  } else {
    0L
  }
  
  gaps_internal <- n_gaps - gap_at_start - gap_at_end
  
  data.frame(
    n_gaps = n_gaps,
    pct_gaps = round(100 * n_gaps / n, 2),
    gap_at_start = gap_at_start,
    gap_at_end = gap_at_end,
    gaps_internal = gaps_internal,
    stringsAsFactors = FALSE
  )
}


# 3 functions for searching parent taxon in the taxo_RCM or taxo_Adl sheets:
# - to save logs of failed searches into taxo_log list
log_na <- function(level, name) { 
  taxo_log[[level]] <<- unique(c(taxo_log[[level]], name))
}

# - to search parent taxon
get_parent <- function(taxon, taxo_df, txrank) {
  res <- taxo_df %>%
    filter(
      tolower(`taxon name`) == tolower(taxon),
      rank == tolower(txrank)
    )
  
  if (nrow(res) == 0) {
    log_na(rank, taxon)
    return(NA)
  }
  
  res$`parent taxon name`[1]
}

# - to apply the parent search on ambiguous taxa (when identical sequences appear for multiple taxa)
get_parent_ambiguous_taxa <- function(
    ambtaxon, taxo_table, target_rank, sep = "__OR__") {
  
  if (is.na(ambtaxon) || ambtaxon == "") {return(NA_character_)}
  
  taxon_vec <- trimws(strsplit(ambtaxon, sep)[[1]])  # split ambiguous "txn1__OR__txn2" into c("txn1", "txn2")
  
  parents <- vapply(  taxon_vec,
    function(txn) get_parent(txn, taxo_table, target_rank),
    FUN.VALUE = character(1)
  ) # apply get_parent for each txn
  
  parents <- parents[!is.na(parents) & parents != ""] # remove NAs
  if (length(parents) == 0) {return(NA_character_)}
  
  # sort: start with most frequent, and in case of tie use first occurrence (important for cases when the ambiguities persist on higher taxonomic levels, e.g. to output family of the most frequent genus first)
  freq = table(parents)
  frst = match(names(freq), parents)
  parents = names( freq[ 
    order( -as.integer(freq), frst)
  ])
  
  paste(parents, collapse = sep)
}


## USE THE FUNCTIONS ####

seq_col <- find_alignment_column(seq_info)

rbcl263 <- seq_info %>%
  filter(`Amplified region` == "Rbcl-263bp")

F_PRIMER <- "AGGTGAARYWAAAGGTTCWTAYTTAAA"
R_PRIMER <- "CCTTCTAATTTACCWACWACTG"

seqs_masked <- vapply(
  rbcl263[[seq_col]],
  mask_primers,
  character(1),
  fwd_primer = F_PRIMER,
  rev_primer = R_PRIMER
)

bounds <- find_amplicon_bounds(seqs_masked)

# Now extract the 263 bp fragment from all rbcl sequences

seqs_trimmed <- seq_info %>%
  filter(`Amplified region` %in% c("rbcl", "Rbcl-263bp")) %>%
  mutate(
    seq_raw = .data[[seq_col]], 
    seq_trim = ifelse(
      nchar(seq_raw) >= bounds$end,
      substr(seq_raw, bounds$start, bounds$end),
      NA
    )
  ) %>%
  filter(!is.na(seq_trim)) %>%
  select(Species, seq_trim)


### Remove sequences with gaps ####
# and check which species and genus names are lost in this step

seqs_trimmed$Genus <- sub(" .*", "", seqs_trimmed$Species)

with_gaps <- grepl("-", seqs_trimmed$seq_trim)

seqs_complete   <- seqs_trimmed[!with_gaps, ]
seqs_withgaps <- seqs_trimmed[ with_gaps, ]

metrics <- do.call(rbind, lapply(seqs_withgaps$seq_trim, gap_metrics))
seqs_withgaps <- cbind(seqs_withgaps, metrics)

species_before <- unique(seqs_trimmed$Species)
genus_before   <- unique(seqs_trimmed$Genus)

species_after <- unique(seqs_complete$Species)
genus_after   <- unique(seqs_complete$Genus)

species_dropped <- setdiff(species_before, species_after)
genus_dropped <- setdiff(genus_before, genus_after)

species_report <- seqs_withgaps[seqs_withgaps$Species %in% species_dropped, ]
species_report <- species_report[order(species_report$Species), ]
species_report$Species_unique <- species_report$Species
dup <- duplicated(species_report$Species)
species_report$Species_unique[dup] <- ""

genus_report <- seqs_withgaps[seqs_withgaps$Genus %in% genus_dropped, ]
genus_report <- genus_report[order(genus_report$Genus), ]
genus_report$Genus_unique <- genus_report$Genus
dupg <- duplicated(genus_report$Genus)
genus_report$Genus_unique[dupg] <- ""


write.csv2( species_report[, c("Species_unique","Species","n_gaps","pct_gaps","gap_at_start","gap_at_end","gaps_internal","seq_trim"
  )], "report_dropped_species_due_to_gaps.csv",  row.names = FALSE)
write.csv2( genus_report[, c("Genus_unique","Genus","Species","n_gaps","pct_gaps","gap_at_start","gap_at_end","gaps_internal","seq_trim"
  )], "report_dropped_genera_due_to_gaps.csv",  row.names = FALSE)

# another report:

genus_retention <- data.frame(
  Genus = genus_before,
  n_species_before = sapply(
    genus_before,
    function(g) sum(seqs_trimmed$Genus == g & seqs_trimmed$Species %in% species_before)
  ),
  n_species_after = sapply(
    genus_before,
    function(g) sum(seqs_complete$Genus == g & seqs_complete$Species %in% species_after)
  ),
  stringsAsFactors = FALSE
)

genus_retention$genus_fully_dropped <- genus_retention$n_species_after == 0

write.csv2( genus_retention, "report_genus_retention_after_gap_filtering.csv", row.names = FALSE)


if (length(species_dropped) > 0) {
  warning(
    paste0(
      "Removed all sequences for ", length(species_dropped),
      " species due to incomplete (gapped) sequences.\n",
      "Entire genera lost: ", length(genus_dropped), "\n",
      "See reports:\n",
      " - report_dropped_species_due_to_gaps.csv\n",
      " - report_dropped_genera_due_to_gaps.csv\n",
      " - report_genus_retention_after_gap_filtering.csv"
    ),
    call. = FALSE
  )
}


### Dereplicate seqs ####
# i.e. keep only unique sequences, but first check if the identical seqs have identical Species name as well

conflicts <- aggregate(
  Species ~ seq_trim,
  data = seqs_complete,
  FUN = function(x) length(unique(x))
)

conflicts <- conflicts[conflicts$Species > 1, ]

if (nrow(conflicts) > 0) {
  
  conflict_details <- aggregate(
    Species ~ seq_trim,
    data = seqs_complete,
    FUN = function(x) paste(unique(x), collapse = "__OR__")
  )
  
  conflict_details <- conflict_details[
    conflict_details$seq_trim %in% conflicts$seq_trim,
  ]
  
  write.csv2(
    conflict_details,
    "sequence_species_conflicts.csv",
    row.names = FALSE
  )
  
  warning(
    paste0(
      "Found ", nrow(conflicts),
      " sequence(s) mapping to multiple species.\n",
      "Details written to sequence_species_conflicts.csv"
    ),
    call. = FALSE
  )
}

seqs_unique <- seqs_complete[
  !duplicated(seqs_complete$seq_trim), ] %>%
  mutate( Species_all = Species )

seqs_unique$Species_all[ match( 
  conflict_details$seq_trim, seqs_unique$seq_trim)
] = conflict_details$Species 



### Get full taxonomy ####


# using NORMAL species column
# reconstruct the whole taxonomy of the species
taxo_log <- list()
taxonomy <- seqs_unique %>%
  mutate(
    Genus = map_chr(Species, ~ get_parent(.x, taxo_RCM, "species")),
    Family = map_chr(Genus, ~ get_parent(.x, taxo_RCM, "genus")),
    
    Order = map_chr(Family, ~ get_parent(.x, taxo_Adl, "family")),
    Subclass = map_chr(Order, ~ get_parent(.x, taxo_Adl, "order")),
    Class = map_chr(Subclass, ~ get_parent(.x, taxo_Adl, "subclass")),
    Subphylum = map_chr(Class, ~ get_parent(.x, taxo_Adl, "class")),
    Subkingdom = map_chr(Subphylum, ~ get_parent(.x, taxo_Adl, "subphylum")),
    Kingdom = map_chr(Subkingdom, ~ get_parent(.x, taxo_Adl, "subkingdom")),
    
    Empire = "Eukaryota"
  )

if (length(taxo_log) > 0){
  warning("Failed attempts of searching parent taxon are reported in taxonomic_linking_issues.csv. Restart the script after correcting these issues in the Species column of sequences_info sheet or in the taxo_RCM and taxo_Adl sheets of the database xlsx")
  log_df <- enframe(taxo_log, name = "level", value = "taxa") %>%
            unnest(taxa)
  

  write.csv2(log_df, "taxonomic_linking_issues.csv", row.names = FALSE)
  
}


# using column with AMBIGUOUS species 
# reconstruct the whole taxonomy of the species
taxo_log <- list()
taxonomy_amb <- seqs_unique %>%
  mutate(
    Genus = map_chr(Species_all, ~ get_parent_ambiguous_taxa(.x, taxo_RCM, "species")),
    Family = map_chr(Genus, ~ get_parent_ambiguous_taxa(.x, taxo_RCM, "genus")),
    
    Order = map_chr(Family, ~ get_parent_ambiguous_taxa(.x, taxo_Adl, "family")),
    Subclass = map_chr(Order, ~ get_parent_ambiguous_taxa(.x, taxo_Adl, "order")),
    Class = map_chr(Subclass, ~ get_parent_ambiguous_taxa(.x, taxo_Adl, "subclass")),
    Subphylum = map_chr(Class, ~ get_parent_ambiguous_taxa(.x, taxo_Adl, "class")),
    Subkingdom = map_chr(Subphylum, ~ get_parent_ambiguous_taxa(.x, taxo_Adl, "subphylum")),
    Kingdom = map_chr(Subkingdom, ~ get_parent_ambiguous_taxa(.x, taxo_Adl, "subkingdom")),
    
    Empire = "Eukaryota"
  )

if (length(taxo_log) > 0){
  warning("Failed attempts of searching parent taxon are reported in taxonomic_linking_issues_ambiguous.csv. Restart the script after correcting these issues in the Species column of sequences_info sheet or in the taxo_RCM and taxo_Adl sheets of the database xlsx")
  log_df <- enframe(taxo_log, name = "level", value = "taxa") %>%
    unnest(taxa)

  write.csv2(log_df, "taxonomic_linking_issues_ambiguous.csv", row.names = FALSE)
}

warning("See taxonomy_ambiguous.csv for full taxonomy of sequences that had >1 species assigned")
write.csv2(taxonomy_amb[grepl("__OR__",taxonomy_amb$Species_all),c(4,3,5:13,2)],
           taxonomy_ambiguous.csv, row.names = FALSE)


# MAKE FASTA for DADA2  ####

# header
taxonomy <- taxonomy %>% mutate(
  tax_string = paste0(Empire, ";", Kingdom, ";", Subkingdom, ";", Subphylum, ";", Class, ";", Subclass, ";", Order, ";", Family, ";", Genus, ";", Species, ";"
))

taxonomy_amb <- taxonomy_amb %>% mutate(
  tax_string = paste0(Empire, ";", Kingdom, ";", Subkingdom, ";", Subphylum, ";", Class, ";", Subclass, ";", Order, ";", Family, ";", Genus, ";", Species, ";"
  ))

taxonomy_without_amb = taxonomy_amb[!grepl("__OR__",taxonomy_amb$Species_all),]

# Export
dna <- DNAStringSet(taxonomy_without_amb$seq_trim)
names(dna) <- taxonomy_without_amb$tax_string
writeXStringSet(dna,  format = "fasta",
  filepath = gsub(" ","_", gsub(".xlsx", "tax_assign_dada2.fa", dbfile)))

names(dna) <- paste(1:nrow(taxonomy_without_amb), taxonomy_without_amb$Species)
writeXStringSet(dna,  format = "fasta",
  filepath = gsub(" ","_", gsub(".xlsx", "spe_assign_dada2.fa", dbfile)))

dna <- DNAStringSet(taxonomy_amb$seq_trim)
names(dna) <- taxonomy_amb$tax_string
writeXStringSet(dna,  format = "fasta",
  filepath = gsub(" ","_", gsub(".xlsx", "tax_withAMBIGUITIES_assign_dada2.fa", dbfile)))

names(dna) <- paste(1:nrow(taxonomy), taxonomy$Species_all)
writeXStringSet(dna,  format = "fasta",
  filepath = gsub(" ","_", gsub(".xlsx", "spe_withAMBIGUITIES_assign_dada2.fa", dbfile)))
