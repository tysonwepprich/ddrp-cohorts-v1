#!/bin/bash
# run cron job to create GCA maps for SERDP with DDRP v2 (cohorts)
cd /home/tyson/REPO/ddrp-cohorts-v1
./DDRP_cohorts_v1.R --spp GCA --forecast_data PRISM --start_year 2018 --start_doy 1 --end_doy 365 --keep_leap 0 --region_param NORTHWEST --exclusions_stressunits 0 --pems 0 --mapA 0 --mapE 0 --mapL 0 --mapP 0 --out_dir GCA_2018_test_short --out_option 1 --ncohort 7 --odd_gen_map 0 --do_photo 1 --cp_mean 14.5 --cp_sd 0.2
