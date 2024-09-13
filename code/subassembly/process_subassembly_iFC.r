### process arguments ###
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop('Three arguments are required:
    1. subassembly of single insulators,
    2. subassembly of double insulators
    3. barcodes of the control sequences')
} else if (length(args) > 3) {
  warning(paste0('More than three arguments passed. Ignoring arguments 4 - ', length(args), '.'))
}

subass_single <- args[1]
subass_double <- args[2]
ctrls <- args[3]


### test if files exist ###
if (! file.exists(subass_single)) {
  stop(paste0('The file "', subass_single, '" does not exist.'))
}
if (! file.exists(subass_double)) {
  stop(paste0('The file "', subass_double, '" does not exist.'))
}
if (! file.exists(ctrls)) {
  stop(paste0('The file "', ctrls, '" does not exist.'))
}


### load libraries ###
library(readr)
library(tidyr)
library(dplyr)


### process and filter subassembly ###

# load single insulator subassembly and adjust columns
single  <- read_tsv(subass_single) %>%
  mutate(
    variant = if_else(variant == 'WT' & is.na(truncation), 'WT', 'mut'),
    type = 'single'
  ) %>%
  select(barcode, 'id_1' = id, 'orientation_1' = orientation, 'variant_1' = variant, type)

# load double insulator subassemblies, rename variants and assign third insulator
double <- read_tsv(subass_double) %>%
  group_by(across(-c(starts_with('variant'), assembly_count))) %>%
  summarise(
    across(starts_with("variant"), ~ if_else(n_distinct(.x) == 1, .x[1], "WT?"))
  ) %>%
  ungroup() %>%
  mutate(
    bc = paste0(substr(barcode, 1, 1), substr(barcode, 4, 4), substr(barcode, 7, 7), substr(barcode, 10, 10), substr(barcode, 13, 13), substr(barcode, 16, 16)),
    id_3 = case_when(
      bc == 'GGGAAA' ~ 'lambda-EXOB_415/rev',
      bc == 'GACAGA' ~ 'beta-phaseolin_1712/rev',
      bc == 'CGACAA' ~ 'lambda-EXOB_1/fwd',
      bc == 'AAGAGC' ~ 'lambda-EXOB_332/fwd',
      bc == 'AAACGG' ~ 'beta-phaseolin_1633/fwd'
    ),
    type = if_else(is.na(id_3), 'double', 'triple')
  ) %>%
  separate(
    id_3,
    into = c('id_3', 'orientation_3'),
    sep = '/'
  ) %>%
  select(-bc)

# load controls
controls <- read_tsv(ctrls) %>%
  mutate(
    id_1 = 'control',
    variant_1 = sub('_none', '', enhancer),
    type = paste(id_1, variant_1, sep = '-')
  ) %>%
  select(-enhancer)

# merge subassemblies and remove duplicate barcodes
subassembly <- bind_rows(single, double, controls) %>%
  filter(! barcode %in% barcode[duplicated(barcode)])


### write filtered subassembly to standard out ###
subassembly %>%
  format_tsv() %>%
  write(stdout())
