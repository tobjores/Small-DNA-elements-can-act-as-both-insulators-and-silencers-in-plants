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

BCLENGTH=18

RCC=5


### print temporary directory name ###
echo "reads are at: ${READDIR}"
echo "curent working directory is at: ${OUTDIR}"
echo "temporary directory is at: ${TMPDIR}"


### assemble inserts and barcodes ###
echo "assembling reads ($(date +"%T"))"

mkdir -p ${TMPDIR}/panda_logs

pandaseq -N -f ${READDIR}/${SAMPLE}_insert_1.fastq.gz -r ${READDIR}/${SAMPLE}_insert_2.fastq.gz -d bfsrkm -G ${TMPDIR}/panda_logs/log_${SAMPLE}_insert.txt.bz2 -w ${TMPDIR}/${SAMPLE}_insert.fa
pandaseq -N -f ${READDIR}/${SAMPLE}_barcode_1.fastq.gz -r ${READDIR}/${SAMPLE}_barcode_2.fastq.gz -d bfsrkm -G ${TMPDIR}/panda_logs/log_${SAMPLE}_barcode.txt.bz2 -w ${TMPDIR}/${SAMPLE}_barcode.fa


### join barcodes and inserts ###
echo "joining barcodes and inserts ($(date +"%T"))"

bioawk -c fastx -t -v TMPDIR=${TMPDIR} -v BASENAME=${SAMPLE} -v BCLEN=${BCLENGTH} -v RCC=${RCC} '
    BEGIN{
      i = 1
      while (getline < (TMPDIR "/" BASENAME "_barcode.fa")) {
        if (i == 1) {sub(/:[ACGTN0+]*;.*$/, "", $1); READ = substr($1, 2); i --}
        else {BARCODE = $1; i ++}
        if (length(BARCODE) == BCLEN) barcodes[READ] = BARCODE
      }
      print "barcode", "sequence", "assembly_count"
    } {
      sub(/:[ACGTN0+]*;.*$/, "", $name);
      if ($name in barcodes) {counts[barcodes[$name]$seq]++; delete barcodes[$name]}
    } END{
      for (combo in counts) if (counts[combo] >= RCC) print substr(combo, 1,  18), substr(combo, 19), counts[combo]
    }
  ' ${TMPDIR}/${SAMPLE}_insert.fa \
  > ${TMPDIR}/subassembly_${SAMPLE}.tsv


### align insulator sequences ###
echo "aligning insulator ($(date +"%T"))"

# build index #
mkdir -p ${TMPDIR}/bowtie2-index
bowtie2-build ${REFSEQ} ${TMPDIR}/bowtie2-index/index 1>&2

# convert subassembly to fasta #
bioawk 'NR > 1 {print ">" $1 ":" $3 "\n" $2}' ${TMPDIR}/subassembly_${SAMPLE}.tsv \
  > ${TMPDIR}/${SAMPLE}.fa

# align #
bowtie2 -p ${NSLOTS} --xeq --no-unal -f -x ${TMPDIR}/bowtie2-index/index -U ${TMPDIR}/${SAMPLE}.fa 2> ${OUTDIR}/alignment_${SAMPLE}.txt > ${TMPDIR}/${SAMPLE}.sam


### extract alignment info from sam file ###
bioawk -c sam -t '
    BEGIN{print "barcode", "assembly_count", "id", "start", "cigar", "orientation", "sequence", "GC"}
    {
      split($qname, query, ":")
      tmp = $seq
      gsub(/A|T/, "", tmp)
      if (and($flag, 16)) {ORIENTATION = "rev"} else {ORIENTATION = "fwd"}
      print query[1], query[2], $rname, $pos, $cigar, ORIENTATION, $seq, length(tmp)/length($seq)
    }
  ' ${TMPDIR}/${SAMPLE}.sam \
  > ${TMPDIR}/${SAMPLE}.tsv


### convert CIGAR string to variant info ###
echo "convert CIGAR string to variant info ($(date +"%T"))"
Rscript cigar_to_variant.r ${TMPDIR}/${SAMPLE}.tsv ${REFSEQ}
mv ${TMPDIR}/${SAMPLE}_variant.tsv ${TMPDIR}/subassembly_${SAMPLE}.tsv


### filter subassembly ###
echo "filter subassembly ($(date +"%T"))"
Rscript filter_subassembly.r ${TMPDIR}/subassembly_${SAMPLE}.tsv \
  > ${TMPDIR}/subassembly_${SAMPLE}_filtered.tsv


### compress final files and copy to main directory ###
echo "copying files ($(date +"%T"))"

pigz -c ${TMPDIR}/subassembly_${SAMPLE}_filtered.tsv \
  > ${OUTDIR}/subassembly_${SAMPLE}_filtered.tsv.gz

echo "all done ($(date +"%T"))"
