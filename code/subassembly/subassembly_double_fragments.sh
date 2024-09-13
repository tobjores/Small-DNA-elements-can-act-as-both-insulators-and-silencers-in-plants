#!/bin/bash

# usage:
# extract_barcode.sh <basename of reads>

# requires:
# - PANDAseq (version 2.11)
# - bioawk (version fd40150)
# - bowtie2 (version 2.4.1)
# - R (version 4.0.0)


### define variables ###
NSLOTS=${NSLOTS:-1}

SAMPLE=${1}

if [[ ${SAMPLE} == *"/"* ]]; then
  SUBDIR=`dirname ${SAMPLE}`
  SUBDIR="/"${SUBDIR}
  SAMPLE=`basename ${SAMPLE}`
else
  SUBDIR=""
fi

READDIR="../../reads/subassembly${SUBDIR}"
OUTDIR="../../data/subassembly${SUBDIR}"
mkdir -p ${OUTDIR}

if [ -z "$TMPDIR" ]; then
  TMPDIR=${OUTDIR}/tmp${RANDOM}
  mkdir -p ${TMPDIR}
fi

REFSEQ="../../data/refseq/insulator_fragments.fa"

SINGLE="../../data/subassembly/iFC/subassembly_pPSm-35_iFC-single_filtered.tsv.gz"

CONTROLS="../../data/misc/controls_iFC.tsv"

BCLENGTH=18

RCC=5


### print temporary directory name ###
echo "reads are at: ${READDIR}"
echo "curent working directory is at: ${OUTDIR}"
echo "temporary directory is at: ${TMPDIR}"


### assemble barcodes ###
echo "assembling reads ($(date +"%T"))"

mkdir -p ${TMPDIR}/panda_logs

pandaseq -N -f ${READDIR}/${SAMPLE}_barcode_1.fastq.gz -r ${READDIR}/${SAMPLE}_barcode_2.fastq.gz -d bfsrkm -G ${TMPDIR}/panda_logs/log_${SAMPLE}_barcode.txt.bz2 -w ${TMPDIR}/${SAMPLE}_barcode.fa


### align insulator sequences ###
echo "aligning insulator ($(date +"%T"))"

# build index #
mkdir -p ${TMPDIR}/bowtie2-index
bowtie2-build ${REFSEQ} ${TMPDIR}/bowtie2-index/index 1>&2


# align second fragment #
bowtie2 -p ${NSLOTS} --xeq --no-unal -x ${TMPDIR}/bowtie2-index/index -U ${READDIR}/${SAMPLE}_insert_1.fastq.gz 2> ${OUTDIR}/alignment_${SAMPLE}_frag2.txt > ${TMPDIR}/${SAMPLE}_frag2.sam

# align first fragment #
bowtie2 -p ${NSLOTS} --xeq --no-unal -x ${TMPDIR}/bowtie2-index/index -U ${READDIR}/${SAMPLE}_insert_2.fastq.gz 2> ${OUTDIR}/alignment_${SAMPLE}_frag1.txt > ${TMPDIR}/${SAMPLE}_frag1.sam


### join barcodes and alignment info ###
echo "joining barcodes and alignment info ($(date +"%T"))"

bioawk -c sam -t -v TMPDIR=${TMPDIR} -v BASENAME=${SAMPLE} -v BCLEN=${BCLENGTH} -v RCC=${RCC} '
    BEGIN{
      i = 1
      while (getline < (TMPDIR "/" BASENAME "_barcode.fa")) {
        if (i == 1) {sub(/:[ACGTN0+]*;.*$/, "", $1); READ = substr($1, 2); i --}
        else {BARCODE = $1; i ++}
        if (length(BARCODE) == BCLEN) assembly[READ] = BARCODE
      }
      print "barcode", "id_1", "orientation_1", "variant_1", "id_2", "orientation_2", "variant_2", "assembly_count"
    } {
      if ($qname in assembly) {
        if (FILENAME ~ /frag1/) {
          if (and($flag, 16)) {
            ORI = "fwd"
            if ($pos == 24 && $cigar == "147=") VAR = "WT"
            else VAR = "mut"
          } else {
            ORI = "rev"
            if ($pos == 1 && $cigar == "147=") VAR = "WT"
            else VAR = "mut"
          }
        } else {
          if (and($flag, 16)) {
            ORI = "rev"
            if ($pos == 22 && $cigar == "149=") VAR = "WT"
            else VAR = "mut"
          } else {
            ORI = "fwd"
            if ($pos == 1 && $cigar == "149=") VAR = "WT"
            else VAR = "mut"
          }
        }
        assembly[$qname] = assembly[$qname]"|"$rname":"ORI":"VAR
      }
    } END{
      for (read in assembly) if (assembly[read] ~ /\|.*\|/) count[assembly[read]] ++
      for (combo in count) if (count[combo] >= 5) {
        split(combo, parts, "|")
        split(parts[2], frag1, ":")
        split(parts[3], frag2, ":")
        print parts[1], frag1[1], frag1[2], frag1[3], frag2[1], frag2[2], frag2[3], count[combo]
      }
    }
  ' ${TMPDIR}/${SAMPLE}_frag1.sam ${TMPDIR}/${SAMPLE}_frag2.sam \
  > ${TMPDIR}/subassembly_${SAMPLE}.tsv


### merge with single insulator subassembly ###
echo "merging subassemblies ($(date +"%T"))"

Rscript process_subassembly_iFC.r ${SINGLE} ${TMPDIR}/subassembly_${SAMPLE}.tsv ${CONTROLS} \
  > ${TMPDIR}/subassembly_${SAMPLE%-*}_final.tsv


### compress final files and copy to main directory ###
echo "copying files ($(date +"%T"))"

pigz -c ${TMPDIR}/subassembly_${SAMPLE%-*}_final.tsv \
  > ${OUTDIR}/subassembly_${SAMPLE%-*}_final.tsv.gz

echo "all done ($(date +"%T"))"
