### process arguments ###
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop('At least one argument is required:
    1. subassembly of insulator fragments,
    2. length of the insulator fragments (optional; default = 170)')
} else if (length(args) == 1) {
  args[2] <- 170
} else if (length(args) > 2) {
  warning(paste0('More than two arguments passed. Ignoring arguments 3 - ', length(args), '.'))
}

subass <- args[1]
termlen <- args[2]


### test if files exist ###
if (! file.exists(subass)) {
  stop(paste0('The file "', subass, '" does not exist.'))
}


### load libraries ###
library(readr)
library(dplyr)
library(tidyr)


### load subassembly ###
subassembly <- read_tsv(subass) %>%
  mutate(
    length = as.numeric(termlen),
    truncation = case_when(
      start > 1 & stop < length ~ paste0('1-', start - 1, '&', stop + 1,'-', length),
      start > 1 ~ paste0('1-', start - 1),
      stop < length ~ paste0(stop + 1,'-', length),
      TRUE ~ NA_character_
    ),
    length = stop - start + 1
  ) %>%
  select(-start, -stop)


### find barcodes linked to a single insulator fragment ###
subassembly_okay <- subassembly %>%
  group_by(barcode) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  select(-assembly_count)


### find barcodes with more than one associated insulator fragment ###
subassembly_to_fix <- subassembly %>%
  filter(! barcode %in% subassembly_okay$barcode)


### combine barcodes with multiple variants of the same insulator fragment ###
# use most common variant if it is at least 10 times more common than the second most common one
subassembly_to_fix <- subassembly_to_fix %>%
  group_by(across(c(-variant, -truncation, -assembly_count))) %>%
  arrange(desc(assembly_count)) %>%
  summarise(
    across(
      c(variant, truncation),
      function(x) case_when(
        n_distinct(x) == 1 ~ first(x),
        first(assembly_count) >= 10 * nth(assembly_count, 2) ~ first(x),
        TRUE ~ paste('multiple (', paste(x, collapse = '/'), ')', sep = '')
      )
    ),
    assembly_count = first(assembly_count)
  ) %>%
  ungroup()


### combine barcodes with multiple insulator fragments ###
# use most common insulator fragment if it is at least 20 times more common than the second most common one
subassembly_to_fix <- subassembly_to_fix %>%
  group_by(barcode) %>%
  arrange(desc(assembly_count)) %>%
  summarise(
    id = case_when(
      n() == 1 ~ first(id),
      first(assembly_count) >= 20 * nth(assembly_count, 2) ~ first(id),
      TRUE ~ paste('multiple (', paste(id, collapse = '/'), ')', sep = '')
    ),
    across(c(-id, -assembly_count), function(x) ifelse(grepl('multiple', id), NA, first(x)))
  ) %>%
  ungroup()


### combine subassembly_okay and subassembly_to_fix ###
subassembly <- bind_rows(
  subassembly_okay,
  subassembly_to_fix
)


### filter out barcodes linked to multiple insulator fragments ###
subassembly <- subassembly %>%
  filter(! grepl('multiple', id, fixed = TRUE) & ! grepl('multiple', variant, fixed = TRUE))


### write filtered subassembly to standard out ###
subassembly %>%
  format_tsv() %>%
  write(stdout())

