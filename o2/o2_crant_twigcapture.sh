#!/bin/bash
#SBATCH -c 1                             # Request cores
#SBATCH -t 0-02:00                       # Runtime in D-HH:MM format
#SBATCH -p short                         # Partition to run in
#SBATCH --mem-per-cpu=8G                 # Memory per core
#SBATCH --job-name=crant_twigcapture     # with singleton: one at a time
#SBATCH --dependency=singleton           # waits for any earlier one to end
#SBATCH -o /home/ab714/CRANTb-R/o2/jobs/crant_twigcapture_%j.out
#SBATCH -e /home/ab714/CRANTb-R/o2/jobs/crant_twigcapture_%j.err
#
# Nightly: Twig Capture requests from CRANTb_meta -> Twig Capture jobs.
# See R/crant-twigcapture.R. Cron (after the 01:00 update has refreshed root
# ids; a run that overlaps it is still safe):
#   30 3 * * * sbatch /home/ab714/CRANTb-R/o2/o2_crant_twigcapture.sh

echo "RUNNING CRANT TWIG CAPTURE DISPATCH"
start=`date +%s`

source /etc/profile
module purge
module load gcc/14.2.0
module load R/4.4.2
ulimit -c 0
umask 002

cd /home/ab714/CRANTb-R
Rscript R/crant-twigcapture.R

end=`date +%s`
echo "script completed in: $((end-start))"
