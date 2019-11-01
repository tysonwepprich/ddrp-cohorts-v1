#!/bin/bash
# run cron job to create GCA maps for SERDP with DDRP v2 (cohorts)
cd /home/tyson/REPO/ddrp-cohorts-v1
./DDRP_cohorts_v1.R --spp APH --forecast_data PRISM --start_year 2018 --start_doy 1 --end_doy 365 --keep_leap 0 --region_param CONUS --exclusions_stressunits 0 --pems 0 --mapA 0 --mapE 0 --mapL 0 --mapP 0 --out_dir APHA_2018_test --out_option 1 --ncohort 5 --odd_gen_map 0 --do_photo 1 --cp_mean 14.1 --cp_sd 0.5
