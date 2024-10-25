# srun --pty -p interactive -t 0-6:00 --mem 6G -c 1 bash

# # Load modules
# module purge
# module load gcc/9.2.0
# module load R/4.3.1
# module load cmake/3.22.2
# module load gdal/3.1.4
# module load udunits/2.2.28
# module load geos/3.10.2
# 
# # Export udunits variables
# export UDUNITS2_INCLUDE=/n/app/udunits/2.2.28-gcc-9.2.0/include
# export UDUNITS2_LIBS=/n/app/udunits/2.2.28-gcc-9.2.0/lib
# 
# # Start R
# R

### Start R session
# screen -S R
# srun --pty -p interactive -t 0-10:00 --mem 5G -c 1 bash
# module load gcc/9.2.0 R/4.3.1
# R
