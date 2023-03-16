#!/bin/bash

echo "Your Bash shell version is: $BASH_VERSION. Good luck!"

# Parallel blasting blastn

# Get genome (locate accesion number)

mkdir reference/
bio fetch GCA_006182925.3 --format fasta > reference/genome.fa

# Create database

makeblastdb -dbtype nucl -in reference/genome.fa -out reference/genome

# Get the sequences names into a txt file (have them in fasta):

ls sequences/ > sequences.txt

# Run blast in parallel

cat sequences.txt | parallel blastn -db reference/genome -query {}  -outfmt 7
