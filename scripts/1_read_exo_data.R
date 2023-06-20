## This script is based off the now-archived 230411_read_exo_test.R script. The
## goal is to read in all EXO data from the Summer/Fall 2023 deployments in two
## mesocosm tanks. These files come in two flavors: 1) 5-minute time-series which
## are the actual dataset of interest, and 2) bucket tests at 5s intervals which
## we are using to assess how comparable the sondes are reading in an identical
## controlled environment (ie a bucket of seawater...). Additionally, code is 
## partially sourced from a previous script that did basically the same thing:
## https://github.com/peterregier/eed_ldrd_asv/blob/main/scripts/1_read_in_exo.R
## 
## 2023-06-20
## Peter Regier
##
# ########### #
# ########### #