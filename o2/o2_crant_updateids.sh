#!/bin/bash
#SBATCH -c 1                             # Request cores
#SBATCH -t 0-00:10                       # Runtime in D-HH:MM format
#SBATCH -p short                         # Partition to run in
#SBATCH --mem-per-cpu=1G                 # Memory per core
#SBATCH -o /home/ab714/CRANTb-R/o2/jobs/crant_updateids_%j.out         # File to which STDOUT will be written, including job ID (%j)
#SBATCH -e /home/ab714/CRANTb-R/o2/jobs/crant_updateids_%j.err         # File to which STDERR will be written, including job ID (%j)

echo "RUNNING CRANT UPDATE IDS"

start=`date +%s`

source /etc/profile
module purge
module load gcc/14.2.0
module load R/4.4.2
module load cmake/3.31.2
module load java/jdk-23.0.1

export UDUNITS2_INCLUDE=/n/app/udunits/2.2.28-gcc-9.2.0/include
export UDUNITS2_LIBS=/n/app/udunits/2.2.28-gcc-9.2.0/lib

cd /home/ab714/CRANTb-R

echo "updating CRANTb IDs in seatable"
Rscript R/crant-updateids.R

end=`date +%s`
runtime=$((end-start))
echo "script completed in: "
echo $runtime
