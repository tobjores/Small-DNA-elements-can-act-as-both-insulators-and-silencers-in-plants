library(readr)
library(dplyr)
library(tidyr)
library(tibble)

### helpers to order data ###
assay_systems <- ordered(
  c('tobacco', 'maize'),
  levels = c('tobacco', 'maize')
)

insulator_order <- c('noEnh', 'noIns', 'beta-phaseolin', 'TBS', 'lambda-EXOB', 'BEAD-1C', 'UASrpg', 'sIns1', 'sIns2', 'gypsy')

enhancer_order <- c('none', '35S', 'AB80', 'Cab-1', 'rbcS-E9')


### function to load and preprocess experiment data (barcode count files) ###
# input:        character or connection; data file with input read counts
# output:       character or connection; data file with output read counts
# subassembly:  R object; subassembly data frame
# rcc:          numeric; read count cutoff to be applied to the input and output counts
load_experiment_bc <- function(input, output, subassembly, rcc = 1) {
  # load and merge files (only keep barcodes detected in both input and output)
  data_inp <- read_table(input, col_names = c('count', 'barcode'), col_types = 'ic') |>
    filter(count >= rcc)
  
  data_out <- read_table(output, col_names = c('count', 'barcode'), col_types = 'ic') |>
    filter(count >= rcc) |>
    inner_join(data_inp, by = 'barcode', suffix = c('_out', '_inp'))
  
  # merge with subassembly
  data_out <- inner_join(data_out, subassembly, by = 'barcode')
  
  # return result
  return(data_out)
}


### load full-length insulator sequences ###
FL_insulator_seqs <- read_lines('data/refseq/FL_insulators.fa')
FL_insulator_seqs <- tibble(
  id = paste0(substring(FL_insulator_seqs[seq(1, length(FL_insulator_seqs), 2)], 2), '_FL'),
  sequence = FL_insulator_seqs[seq(2, length(FL_insulator_seqs), 2)],
  length = nchar(sequence),
  GC = nchar(gsub('[GC]', '', sequence)) / length
) |>
  select(-sequence)

FL_insulator_len <- FL_insulator_seqs |>
  mutate(
    insulator = gsub('_FL', '', id)
  ) |>
  select(insulator, length) |>
  deframe()


### FLins library (full-length insulators) ###
# load and process subassembly
barcode_map_FLins <- read_tsv('data/misc/barcode-orientation-map_FLins.tsv') |>
  select(barcode, orientation) |>
  deframe()

subassembly_FLins <- read_tsv('data/subassembly/FLins/barcodes_FLins.tsv') |>
  bind_rows(read_tsv('data/misc/controls_FLins.tsv')) |>
  mutate(
    insulator = case_when(
      is.na(insulator) & enhancer == 'none' ~ 'noEnh',
      is.na(insulator) & enhancer == '35S' ~ 'noIns',
      .default = insulator
    ),
    enhancer = replace_na(enhancer, '35S')
  ) |>
  mutate(
    bc = paste0(substr(barcode, 1, 1), substr(barcode, 4, 4), substr(barcode, 7, 7), substr(barcode, 10, 10), substr(barcode, 13, 13), substr(barcode, 16, 16)),
    orientation = barcode_map_FLins[bc],
    insulator = ordered(insulator, levels = insulator_order)
  ) |>
  select(-bc)

# load experiment data
reps_FLins <- c(1, 2, 3)
sys_FLins <- assay_systems

data_FLins <- expand_grid(rep = reps_FLins, sys = sys_FLins) |>
  filter(sys == 'maize' | rep <= 2) |>
  nest_by(rep, sys) |>
  reframe(
    load_experiment_bc(
      input = paste0('data/barcode_counts/FLins/', sys, '/rep', if_else(sys == 'maize', 1, rep), '/barcode_pPSm_FLins_', sys, '_', as.roman(if_else(sys == 'maize', 1, rep)), '_input.count.gz'),
      output = paste0('data/barcode_counts/FLins/', sys, '/rep', rep, '/barcode_pPSm_FLins_', sys, '_', as.roman(rep), '_output.count.gz'),
      subassembly = subassembly_FLins,
      rcc = 100
    )
  ) |>
  ungroup()

# calculate enrichment and normalize first to library median then to control construct without insulator (noEnh)
data_FLins_norm <- data_FLins |>
  group_by(sys, rep) |>
  mutate(
    enrichment = log2((count_out / sum(count_out)) / (count_inp / sum(count_inp))),
    enrichment = enrichment - median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    enrichment = enrichment - median(enrichment[insulator == 'noEnh'])
  ) |>
  ungroup()

# save data
save(data_FLins_norm, file = 'data/RData/FLins_data_main.Rdata')


### iLib (insulator fragments) ###
# load and process subassembly
barcode_map_iLib <- read_tsv('data/misc/barcode-enhancer-map_iLib.tsv') |>
  deframe()

subassembly_iLib <- read_tsv('data/subassembly/iLib/subassembly_pPSm_iLib_filtered.tsv.gz') |>
  filter(length == 170 & variant == 'WT' & is.na(truncation)) |>
  mutate(
    bc = paste0(substr(barcode, 1, 1), substr(barcode, 4, 4), substr(barcode, 7, 7), substr(barcode, 10, 10), substr(barcode, 13, 13), substr(barcode, 16, 16)),
    enhancer = barcode_map_iLib[bc]
  ) |>
  filter(! is.na(enhancer)) |>
  select(-bc, -variant, -length, -truncation) |>
  bind_rows(read_tsv('data/misc/controls_iLib.tsv')) |>
  separate_wider_delim(
    enhancer,
    delim = '_',
    names = c('enhancer', 'enhancer_2')
  ) |>
  filter(enhancer_2 == 'none') |>
  select(-enhancer_2) |>
  separate_wider_delim(
    id,
    delim = '_',
    names = c('insulator', 'start')
  ) |>
  mutate(
    start = as.numeric(start),
    stop = start + 169,
    insulator = case_when(
      is.na(insulator) & enhancer == 'none' ~ 'noEnh',
      is.na(insulator) ~ 'noIns',
      .default = insulator
    ),
    insulator = ordered(insulator, levels = insulator_order),
    enhancer = ordered(enhancer, levels = enhancer_order)
  )

# load experiment data
reps_iLib <- c(1, 2, 3)
sys_iLib <- assay_systems

data_iLib <- expand_grid(rep = reps_iLib, sys = sys_iLib) |>
  filter(sys == 'maize' | rep <= 2) |>
  nest_by(rep, sys) |>
  reframe(
    load_experiment_bc(
      input = paste0('data/barcode_counts/iLib/', sys, '/rep', if_else(sys == 'maize' & rep == 2, 1, rep), '/barcode_pPSm_iLib_', sys, '_', as.roman(if_else(sys == 'maize' & rep == 2, 1, rep)), '_input.count.gz'),
      output = paste0('data/barcode_counts/iLib/', sys, '/rep', rep, '/barcode_pPSm_iLib_', sys, '_', as.roman(rep), '_output.count.gz'),
      subassembly = subassembly_iLib,
      rcc = 5
    )
  ) |>
  ungroup()

# remove data for weak/inactive enhancers (rbcS-E9 in tobacco; AB80, Cab-1, and rbcS-E9 in maize)
data_iLib_filtered <- data_iLib |>
  filter(enhancer != 'rbcS-E9' & (sys == 'tobacco' | enhancer %in% c('none', '35S')))

# aggregate barcodes (sum of individual counts)
data_iLib_ag <- data_iLib_filtered |>
  group_by(pick(-c(count_inp, count_out, barcode))) |>
  summarise(
    n_bc = n(),
    count_inp = sum(count_inp),
    count_out = sum(count_out)
  ) |>
  ungroup()

# calculate enrichment and normalize first to library median then to control construct without insulator (noEnh)
data_iLib_norm <- data_iLib_ag |>
  group_by(sys, rep) |>
  mutate(
    enrichment = log2((count_out / sum(count_out)) / (count_inp / sum(count_inp))),
    enrichment = enrichment - median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    enrichment = enrichment - mean(enrichment[insulator == 'noEnh'])
  ) |>
  ungroup()

# calculate mean enrichment across replicates
data_iLib_mean <- data_iLib_norm |>
  group_by(pick(-c(rep, n_bc, count_inp, count_out, enrichment))) |>
  summarise(
    n_experiments = n(),
    min_bc = min(n_bc),
    min_ci = min(count_inp),
    min_co = min(count_out),
    enrichment = mean(enrichment)
  ) |>
  ungroup()

# save data
save(data_iLib_norm, file = 'data/RData/iLib_data_reps.Rdata')
save(data_iLib_mean, file = 'data/RData/iLib_data_main.Rdata')


### iFC (insulator fragment combinations) ###
# load and process subassembly
subassembly_iFC <- read_tsv('data/subassembly/iFC/subassembly_pPSm-35_iFC_final.tsv.gz') |>
  filter(if_all(starts_with('variant'), ~ is.na(.x) | .x != 'mut')) |>
  mutate(
    across(starts_with('variant'), ~ if_else(.x == 'WT?', 'WT', .x)),
    id_1 = case_match(
      variant_1,
      'none' ~ 'noEnh',
      '35S' ~ 'noIns',
      .default = id_1
    ),
    variant_1 = if_else(grepl('control', type, fixed = TRUE), type, variant_1),
    type = if_else(grepl('control', type, fixed = TRUE), 'control', type)
  )

# load experiment data
reps_iFC <- c(1, 2)
sys_iFC <- assay_systems

data_iFC <- expand_grid(rep = reps_iFC, sys = sys_iFC) |>
  nest_by(rep, sys) |>
  reframe(
    load_experiment_bc(
      input = paste0('data/barcode_counts/iFC/', sys, '/rep', if_else(sys == 'maize' & rep == 2, 1, rep), '/barcode_pPSm_iFC_', sys, '_', as.roman(if_else(sys == 'maize' & rep == 2, 1, rep)), '_input.count.gz'),
      output = paste0('data/barcode_counts/iFC/', sys, '/rep', rep, '/barcode_pPSm_iFC_', sys, '_', as.roman(rep), '_output.count.gz'),
      subassembly = subassembly_iFC,
      rcc = 5
    )
  ) |>
  ungroup()

# aggregate barcodes (sum of individual counts)
data_iFC_ag <- data_iFC |>
  group_by(across(-c(count_inp, count_out, barcode))) |>
  summarise(
    n_bc = n(),
    count_inp = sum(count_inp),
    count_out = sum(count_out)
  ) |>
  ungroup()

# calculate enrichment and normalize first to library median then to control construct without insulator (noEnh)
data_iFC_norm <- data_iFC_ag |>
  group_by(sys, rep) |>
  mutate(
    enrichment = log2((count_out / sum(count_out)) / (count_inp / sum(count_inp))),
    enrichment = enrichment - median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    enrichment = enrichment - median(enrichment[id_1 == 'noEnh'])
  ) |>
  ungroup()

# calculate mean enrichment across replicates
data_iFC_mean <- data_iFC_norm |>
  group_by(pick(-c(rep, n_bc, count_inp, count_out, enrichment))) |>
  summarise(
    n_experiments = n(),
    min_bc = min(n_bc),
    min_ci = min(count_inp),
    min_co = min(count_out),
    enrichment = mean(enrichment)
  ) |>
  ungroup()

# save data
save(data_iFC_norm, file = 'data/RData/iFC_data_reps.Rdata')
save(data_iFC_mean, file = 'data/RData/iFC_data_main.Rdata')


### iDE (insulator fragments with downstream enhancer) ###
# load and process subassembly
barcode_map_iDE <- read_tsv('data/misc/barcode-enhancer-map_iDE.tsv') |>
  deframe()

subassembly_iDE <- read_tsv('data/subassembly/iDE/subassembly_pPSm_iDE_filtered.tsv.gz') |>
  filter(length == 170 & variant == 'WT' & is.na(truncation)) |>
  mutate(
    bc = paste0(substr(barcode, 1, 1), substr(barcode, 4, 4), substr(barcode, 7, 7), substr(barcode, 10, 10), substr(barcode, 13, 13), substr(barcode, 16, 16)),
    enhancer = barcode_map_iDE[bc]
  ) |>
  filter(! is.na(enhancer)) |>
  select(-bc, -variant, -length, -truncation) |>
  bind_rows(read_tsv('data/misc/controls_iDE.tsv')) |>
  separate_wider_delim(
    enhancer,
    delim = '_',
    names = c('enh_up', 'enh_down')
  ) |>
  separate_wider_delim(
    id,
    delim = '_',
    names = c('insulator', 'start')
  ) |>
  mutate(
    start = as.numeric(start),
    stop = start + 169,
    insulator = case_when(
      is.na(insulator) & enh_up == 'none' & enh_down == 'none' ~ 'noEnh',
      is.na(insulator) ~ 'noIns',
      .default = insulator
    ),
    insulator = ordered(insulator, levels = insulator_order),
    enh_up = ordered(enh_up, levels = enhancer_order),
    enh_down = ordered(enh_down, levels = enhancer_order)
  )

# load experiment data
reps_iDE <- c(1, 2)
sys_iDE <- 'tobacco'

data_iDE <- expand_grid(rep = reps_iDE, sys = sys_iDE) |>
  nest_by(rep, sys) |>
  reframe(
    load_experiment_bc(
      input = paste0('data/barcode_counts/iDE/', sys, '/rep', rep, '/barcode_pPSm_iDE_', sys, '_', as.roman(rep), '_input.count.gz'),
      output = paste0('data/barcode_counts/iDE/', sys, '/rep', rep, '/barcode_pPSm_iDE_', sys, '_', as.roman(rep), '_output.count.gz'),
      subassembly = subassembly_iDE,
      rcc = 5
    )
  ) |>
  ungroup()

# aggregate barcodes (sum of individual counts)
data_iDE_ag <- data_iDE |>
  group_by(across(-c(count_inp, count_out, barcode))) |>
  summarise(
    n_bc = n(),
    count_inp = sum(count_inp),
    count_out = sum(count_out)
  ) |>
  ungroup()

# calculate enrichment and normalize first to library median then to control construct without insulator (noEnh)
data_iDE_norm <- data_iDE_ag |>
  group_by(sys, rep) |>
  mutate(
    enrichment = log2((count_out / sum(count_out)) / (count_inp / sum(count_inp))),
    enrichment = enrichment - median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    enrichment = enrichment - median(enrichment[insulator == 'noEnh'])
  ) |>
  ungroup()

# calculate mean enrichment across replicates
data_iDE_mean <- data_iDE_norm |>
  group_by(pick(-c(rep, n_bc, count_inp, count_out, enrichment))) |>
  summarise(
    n_experiments = n(),
    min_bc = min(n_bc),
    min_ci = min(count_inp),
    min_co = min(count_out),
    enrichment = mean(enrichment)
  ) |>
  ungroup()

# save data
save(data_iDE_norm, file = 'data/RData/iDE_data_reps.Rdata')
save(data_iDE_mean, file = 'data/RData/iDE_data_main.Rdata')


### i/sLib (insulator fragments uptream or downstream of the 35S enhancer) ###
# load and process subassembly
barcode_map_isLib <- read_tsv('data/misc/barcode-construct-map_isLib.tsv') |>
  unite(
    col = 'construct',
    construct,
    orientation
  ) |>
  deframe()

FL_insulators_isLib <- read_tsv('data/misc/barcodes_isLib_FL.tsv') |>
  mutate(
    bc = paste0(substr(barcode, 1, 1), substr(barcode, 4, 4), substr(barcode, 7, 7), substr(barcode, 10, 10), substr(barcode, 13, 13), substr(barcode, 16, 16)),
    construct = barcode_map_isLib[bc]
  ) |>
  filter(! is.na(construct)) |>
  separate(
    col = construct,
    into = c('construct', 'orientation'),
    sep = '_'
  ) |>
  select(-bc, -variant) |>
  inner_join(
    FL_insulator_seqs,
    by = 'id'
  )

controls_isLib <- read_tsv('data/misc/controls_iLib.tsv') |>
  mutate(
    id = case_match(
      enhancer,
      'none_none' ~ 'noEnh',
      '35S_none' ~ 'noIns'
    ),
    construct = 'control'
  ) |>
  drop_na(id) |>
  select(-enhancer)

subassembly_isLib <- bind_rows(
  'insulator' = read_tsv('data/subassembly/isLib/subassembly_pPSm-35-isLib_filtered.tsv.gz'),
  'silencer' = read_tsv('data/subassembly/isLib/subassembly_pPSm-isLib-35_filtered.tsv.gz'),
  .id = 'construct'
) |>
  filter(length == 170 & variant == 'WT' & is.na(truncation)) |>
  select(-variant, -truncation) |>
  bind_rows(
    FL_insulators_isLib,
    controls_isLib
  ) |>
  filter(! barcode %in% barcode[duplicated(barcode)]) |>
  separate_wider_delim(
    id,
    delim = '_',
    names = c('insulator', 'type'),
    too_few = 'align_start'
  ) |>
  mutate(
    type = replace_na(type, 'control'),
    start = case_match(
      type,
      'control' ~ NA,
      'FL' ~ '1',
      .default = type
    ),
    type = if_else(type %in% c('control', 'FL'), type, 'fragment'),
    start = as.numeric(start),
    stop = start + length - 1,
    insulator = ordered(insulator, levels = insulator_order)
  ) |>
  select(-length)


# load experiment data
reps_isLib <- c(1, 2)
sys_isLib <- assay_systems

data_isLib <- expand_grid(rep = reps_isLib, sys = sys_isLib) |>
  nest_by(rep, sys) |>
  reframe(
    load_experiment_bc(
      input = paste0('data/barcode_counts/isLib/', sys, '/rep', if_else(sys == 'maize' & rep == 2, 1, rep), '/barcode_pPSm_isLib_', sys, '_', as.roman(if_else(sys == 'maize' & rep == 2, 1, rep)), '_input.count.gz'),
      output = paste0('data/barcode_counts/isLib/', sys, '/rep', rep, '/barcode_pPSm_isLib_', sys, '_', as.roman(rep), '_output.count.gz'),
      subassembly = subassembly_isLib,
      rcc = 5
    )
  ) |>
  ungroup()

# aggregate barcodes (sum of individual counts)
data_isLib_ag <- data_isLib |>
  group_by(across(-c(count_inp, count_out, barcode))) |>
  summarise(
    n_bc = n(),
    count_inp = sum(count_inp),
    count_out = sum(count_out)
  ) |>
  ungroup()

# calculate enrichment and normalize first to library median then to control construct without insulator (noEnh)
data_isLib_norm <- data_isLib_ag |>
  group_by(sys, rep) |>
  mutate(
    enrichment = log2((count_out / sum(count_out)) / (count_inp / sum(count_inp))),
    enrichment = enrichment - median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    enrichment = enrichment - median(enrichment[insulator == 'noEnh'])
  ) |>
  ungroup()

# calculate mean enrichment across replicates
data_isLib_mean <- data_isLib_norm |>
  group_by(pick(-c(rep, n_bc, count_inp, count_out, enrichment))) |>
  summarise(
    n_experiments = n(),
    min_bc = min(n_bc),
    min_ci = min(count_inp),
    min_co = min(count_out),
    enrichment = mean(enrichment)
  ) |>
  ungroup()

# save data
save(data_isLib_norm, file = 'data/RData/isLib_data_reps.Rdata')
save(data_isLib_mean, file = 'data/RData/isLib_data_main.Rdata')


### ExI (enhancer and insulator combinations) ###
# load and process subassembly
barcode_map_ExI <- read_tsv('data/misc/barcode-construct-map_ExI.tsv') |>
  unite(
    col = 'construct',
    construct,
    enhancer
  ) |>
  deframe()

subassembly_ExI <- read_tsv('data/subassembly/ExI/barcodes_ExI.tsv')  |>
  mutate(
    bc = paste0(substr(barcode, 1, 1), substr(barcode, 4, 4), substr(barcode, 7, 7), substr(barcode, 10, 10), substr(barcode, 13, 13), substr(barcode, 16, 16)),
    construct = barcode_map_ExI[bc]
  ) |>
  filter(! is.na(construct)) |>
  select(-bc) |>
  separate_wider_delim(
    col = construct,
    delim = '_',
    names = c('construct', 'enhancer')
  ) |>
  separate_wider_delim(
    col = id,
    delim = '_',
    names = c('insulator', 'start'),
    too_few = 'align_start'
  ) |>
  mutate(
    construct = if_else(enhancer == 'none', 'noEnh', construct),
    type = case_when(
      insulator == 'noIns' ~ 'control',
      is.na(start) ~ 'FL',
      .default = 'fragment'
    ),
    insulator = if_else(insulator == 'noIns' & enhancer == 'none', 'noEnh', insulator),
    start = if_else(type == 'FL', 1, as.numeric(start)),
    stop = start + if_else(type == 'FL', FL_insulator_len[insulator], 170) - 1,
    insulator = ordered(insulator, levels = insulator_order)
  )

# load experiment data
reps_ExI <- c(1, 2)
sys_ExI <- assay_systems

data_ExI <- expand_grid(rep = reps_ExI, sys = sys_ExI) |>
  nest_by(rep, sys) |>
  reframe(
    load_experiment_bc(
      input = paste0('data/barcode_counts/ExI/', sys, '/rep', if_else(sys == 'maize', 1, rep), '/barcode_pPSm_ExI_', sys, '_', as.roman(if_else(sys == 'maize', 1, rep)), '_input.count.gz'),
      output = paste0('data/barcode_counts/ExI/', sys, '/rep', rep, '/barcode_pPSm_ExI_', sys, '_', as.roman(rep), '_output.count.gz'),
      subassembly = subassembly_ExI,
      rcc = 5
    )
  ) |>
  ungroup()

# remove At-1 enhancer (not recovered well in tobacco experiments)
data_ExI_filtered <- data_ExI |>
  filter(enhancer != 'At-1')

# aggregate barcodes (sum of individual counts)
data_ExI_ag <- data_ExI_filtered |>
  group_by(across(-c(count_inp, count_out, barcode))) |>
  summarise(
    n_bc = n(),
    count_inp = sum(count_inp),
    count_out = sum(count_out)
  ) |>
  ungroup()

# calculate enrichment and normalize first to library median then to control construct without insulator (noEnh)
data_ExI_norm <- data_ExI_ag |>
  group_by(sys, rep) |>
  mutate(
    enrichment = log2((count_out / sum(count_out)) / (count_inp / sum(count_inp))),
    enrichment = enrichment - median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    enrichment = enrichment - median(enrichment[insulator == 'noEnh'])
  ) |>
  ungroup()

# calculate mean enrichment across replicates
data_ExI_mean <- data_ExI_norm |>
  group_by(pick(-c(rep, n_bc, count_inp, count_out, enrichment))) |>
  summarise(
    n_experiments = n(),
    min_bc = min(n_bc),
    min_ci = min(count_inp),
    min_co = min(count_out),
    enrichment = mean(enrichment)
  ) |>
  ungroup()

# save data
save(data_ExI_norm, file = 'data/RData/ExI_data_reps.Rdata')
save(data_ExI_mean, file = 'data/RData/ExI_data_main.Rdata')

# save enhancer only barcode enrichment
data_ExI_enhancers <- data_ExI_filtered |>
  group_by(sys, rep) |>
  mutate(
    enrichment = log2((count_out / sum(count_out)) / (count_inp / sum(count_inp))),
    enrichment = enrichment - median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    enrichment = enrichment - median(enrichment[insulator == 'noEnh'])
  ) |>
  ungroup() |>
  filter(insulator %in% c('noEnh', 'noIns')) |>
  select(sys, rep, enhancer, barcode, count_inp, count_out, enrichment)  |>
  group_by(sys, enhancer) |>
  mutate(
    rank = median(enrichment)
  ) |>
  group_by(sys) |>
  mutate(
    rank = rank(rank),
    rank = case_match(
      enhancer,
      '35S' ~ max(rank) + 1,
      'none' ~ min(rank) - 1,
      .default = rank
    ),
    rank = as.numeric(as.ordered(rank))
  ) |>
  ungroup()

save(data_ExI_enhancers, file = 'data/Rdata/ExI_data_enhancers.Rdata')