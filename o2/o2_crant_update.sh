#!/bin/bash
#SBATCH -c 1                             # Request cores
#SBATCH -t 0-00:10                       # Runtime in D-HH:MM format
#SBATCH -p short                         # Partition to run in
#SBATCH --mem-per-cpu=1G                 # Memory per core
#SBATCH -o /home/ab714/CRANTb-R/o2/jobs/banc_update_%j.out         # File to which STDOUT will be written, including job ID (%j)
#SBATCH -e /home/ab714/CRANTb-R/o2/jobs/banc_update_%j.err         # File to which STDERR will be written, including job ID (%j)

echo "RUNNING BANC UPDATE"

start=`date +%s`

module purge
module load gcc/9.2.0
module load cmake/3.22.2
module load R/4.3.1
module load gdal/3.1.4
module load udunits/2.2.28
module load geos/3.10.2

export UDUNITS2_INCLUDE=/n/app/udunits/2.2.28-gcc-9.2.0/include
export UDUNITS2_LIBS=/n/app/udunits/2.2.28-gcc-9.2.0/lib

cd /home/ab714/CRANTb-R

echo "updating CRANTb IDs in seatable"
Rscript R/crant-updateids.R

echo "updating CRANTb meta in seatable"
Rscript R/crant-update.R

