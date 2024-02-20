** This scripts load in the data from the NICE area and creates LTPA groups for the Cox analyses

** Import .csv file

import delimited "\\fil.nice.ntnu.no\nice\p758\Data master\hunt_3_4_cleaned_full_data.csv"

** HUNT 3

** Encode variables to be ordered categorical

encode exe_f_nt3blq1, generate(exe_f_nt3blq1_encoded)
encode exe_int_nt3blq1, generate(exe_int_nt3blq1_encoded)
encode exe_du_nt3blq1, generate(exe_du_nt3blq1_encoded)

drop exe_f_nt3blq1
drop exe_int_nt3blq1
drop exe_du_nt3blq1

rename exe_f_nt3blq1_encoded exe_f_nt3blq1
rename exe_int_nt3blq1_encoded exe_int_nt3blq1
rename exe_du_nt3blq1_encoded exe_du_nt3blq1

**  Differentiate into LTPA

* Create a new variable where missing (.) is default if nothing else is provided in the logic

gen ltpa_index_nt3 =.

* Inactive

replace ltpa_index_nt3 = 1 if exe_f_nt3blq1<=2 | exe_f_nt3blq1==3 & exe_du_nt3blq1==1 | exe_f_nt3blq1==4 & exe_du_nt3blq1==1 & exe_int_nt3blq1<3 /*inactive*/

* Low

replace ltpa_index_nt3 = 2 if exe_f_nt3blq1==5 & exe_du_nt3blq1==1 & exe_int_nt3blq1<3 /*Low*/

replace ltpa_index_nt3 = 3 if exe_f_nt3blq1==3 & exe_du_nt3blq1==2 & exe_int_nt3blq1==1 /*Low*/

replace ltpa_index_nt3 = 4 if exe_f_nt3blq1==3 & exe_du_nt3blq1==3 & exe_int_nt3blq1==1 /*Low*/

replace ltpa_index_nt3 = 5 if exe_f_nt3blq1==3 & exe_du_nt3blq1==2 & exe_int_nt3blq1==2 /*Low*/

replace ltpa_index_nt3 = 6 if exe_f_nt3blq1==3 & exe_du_nt3blq1==3 & exe_int_nt3blq1==2 /*Low*/

replace ltpa_index_nt3 = 7 if exe_f_nt3blq1==3 & exe_du_nt3blq1==4 & exe_int_nt3blq1==2 /*Low*/

replace ltpa_index_nt3 = 8 if exe_f_nt3blq1==3 & exe_du_nt3blq1==2 & exe_int_nt3blq1==3 /*Low*/

replace ltpa_index_nt3 = 9 if exe_f_nt3blq1==4 & exe_du_nt3blq1==2 & exe_int_nt3blq1==1 /*Low*/

replace ltpa_index_nt3 = 10 if exe_f_nt3blq1==4 & exe_du_nt3blq1==2 & exe_int_nt3blq1==2 /*Low*/

replace ltpa_index_nt3 = 11 if exe_f_nt3blq1==4 & exe_du_nt3blq1==1 & exe_int_nt3blq1==3 /*Low*/

replace ltpa_index_nt3 = 12 if exe_f_nt3blq1==5 & exe_du_nt3blq1==2 & exe_int_nt3blq1==1 /*Low*/

replace ltpa_index_nt3 = 13 if exe_f_nt3blq1==3 & exe_du_nt3blq1==3 & exe_int_nt3blq1==1 /*Low*/

replace ltpa_index_nt3 = 12.9 if exe_f_nt3blq1==3 & exe_du_nt3blq1==4 & exe_int_nt3blq1==1 /*Low*/

replace ltpa_index_nt3 = 12.8 if exe_f_nt3blq1==. & exe_du_nt3blq1==4 & exe_int_nt3blq1 >1 /*high*/

replace ltpa_index_nt3 = 12.6 if exe_f_nt3blq1==4 & exe_du_nt3blq1==2 & exe_int_nt3blq1==3 /*moderate*/

replace ltpa_index_nt3 = 12.7 if exe_f_nt3blq1==3 & exe_du_nt3blq1==3 & exe_int_nt3blq1==3 /*moderate*/

replace ltpa_index_nt3 = 12.5 if exe_f_nt3blq1==4 & exe_du_nt3blq1==3 & exe_int_nt3blq1==1 /*moderate*/

replace ltpa_index_nt3 = 12.4 if exe_f_nt3blq1==3 & exe_du_nt3blq1==4 & exe_int_nt3blq1==3 /*moderate*/

* Moderate

replace ltpa_index_nt3 = 14 if exe_f_nt3blq1==4 & exe_du_nt3blq1==4 & exe_int_nt3blq1==1 /*moderate*/

replace ltpa_index_nt3 = 15 if exe_f_nt3blq1==4 & exe_du_nt3blq1==4 & exe_int_nt3blq1==2 /*moderate*/

replace ltpa_index_nt3 = 16 if exe_f_nt3blq1==4 & exe_du_nt3blq1==3 & exe_int_nt3blq1==2 /*moderate*/

replace ltpa_index_nt3 = 17 if exe_f_nt3blq1==4 & exe_du_nt3blq1==4 & exe_int_nt3blq1==2 /*moderate*/

replace ltpa_index_nt3 = 22 if exe_f_nt3blq1==5 & exe_du_nt3blq1==2 & exe_int_nt3blq1==2 /*moderate*/

replace ltpa_index_nt3 = 23 if exe_f_nt3blq1==5 & exe_du_nt3blq1==1 & exe_int_nt3blq1==3 /*moderate*/

replace ltpa_index_nt3 = 23.1 if exe_f_nt3blq1==5 & exe_du_nt3blq1==3 & exe_int_nt3blq1==1 /*moderate*/

replace ltpa_index_nt3 = 23.2 if exe_f_nt3blq1==5 & exe_du_nt3blq1==3 & exe_int_nt3blq1 ==. /*moderate*/

* High

replace ltpa_index_nt3 = 24 if exe_f_nt3blq1==4 & exe_du_nt3blq1>=3 & exe_int_nt3blq1==3 /*high*/

replace ltpa_index_nt3 = 25 if exe_f_nt3blq1==5 & exe_du_nt3blq1==4 /*high*/

replace ltpa_index_nt3 = 26 if exe_f_nt3blq1==5 & exe_du_nt3blq1==3 & exe_int_nt3blq1>=2 /*high*/

replace ltpa_index_nt3 = 27 if exe_f_nt3blq1==5 & exe_du_nt3blq1==2 & exe_int_nt3blq1==3 /*high*/

recode ltpa_index_nt3 (1=0 "Inactive") (2/13=0 "Low") (14/27=1 "Moderate/high"), gen (ltpa_group_nt3)

** HUNT 4

** Encode variables to be ordered categorical

encode exe_f_nt4blq1, generate(exe_f_nt4blq1_encoded)
encode exe_int_nt4blq1, generate(exe_int_nt4blq1_encoded)
encode exe_du_nt4blq1, generate(exe_du_nt4blq1_encoded)

drop exe_f_nt4blq1
drop exe_int_nt4blq1
drop exe_du_nt4blq1

rename exe_f_nt4blq1_encoded exe_f_nt4blq1
rename exe_int_nt4blq1_encoded exe_int_nt4blq1
rename exe_du_nt4blq1_encoded exe_du_nt4blq1

**  Differentiate into LTPA

* Create a new variable where missing (.) is default if nothing else is provided in the logic

gen ltpa_index_nt4 =.

* Inactive

replace ltpa_index_nt4 = 1 if exe_f_nt4blq1<=2 | exe_f_nt4blq1==3 & exe_du_nt4blq1==1 | exe_f_nt4blq1==4 & exe_du_nt4blq1==1 & exe_int_nt4blq1<3 /*inactive*/

* Low

replace ltpa_index_nt4 = 2 if exe_f_nt4blq1==5 & exe_du_nt4blq1==1 & exe_int_nt4blq1<3 /*Low*/

replace ltpa_index_nt4 = 3 if exe_f_nt4blq1==3 & exe_du_nt4blq1==2 & exe_int_nt4blq1==1 /*Low*/

replace ltpa_index_nt4 = 4 if exe_f_nt4blq1==3 & exe_du_nt4blq1==3 & exe_int_nt4blq1==1 /*Low*/

replace ltpa_index_nt4 = 5 if exe_f_nt4blq1==3 & exe_du_nt4blq1==2 & exe_int_nt4blq1==2 /*Low*/

replace ltpa_index_nt4 = 6 if exe_f_nt4blq1==3 & exe_du_nt4blq1==3 & exe_int_nt4blq1==2 /*Low*/

replace ltpa_index_nt4 = 7 if exe_f_nt4blq1==3 & exe_du_nt4blq1==4 & exe_int_nt4blq1==2 /*Low*/

replace ltpa_index_nt4 = 8 if exe_f_nt4blq1==3 & exe_du_nt4blq1==2 & exe_int_nt4blq1==3 /*Low*/

replace ltpa_index_nt4 = 9 if exe_f_nt4blq1==4 & exe_du_nt4blq1==2 & exe_int_nt4blq1==1 /*Low*/

replace ltpa_index_nt4 = 10 if exe_f_nt4blq1==4 & exe_du_nt4blq1==2 & exe_int_nt4blq1==2 /*Low*/

replace ltpa_index_nt4 = 11 if exe_f_nt4blq1==4 & exe_du_nt4blq1==1 & exe_int_nt4blq1==3 /*Low*/

replace ltpa_index_nt4 = 12 if exe_f_nt4blq1==5 & exe_du_nt4blq1==2 & exe_int_nt4blq1==1 /*Low*/

replace ltpa_index_nt4 = 13 if exe_f_nt4blq1==3 & exe_du_nt4blq1==3 & exe_int_nt4blq1==1 /*Low*/

replace ltpa_index_nt4 = 12.9 if exe_f_nt4blq1==3 & exe_du_nt4blq1==4 & exe_int_nt4blq1==1 /*Low*/

replace ltpa_index_nt4 = 12.8 if exe_f_nt4blq1==. & exe_du_nt4blq1==4 & exe_int_nt4blq1 >1 /*high*/

replace ltpa_index_nt4 = 12.6 if exe_f_nt4blq1==4 & exe_du_nt4blq1==2 & exe_int_nt4blq1==3 /*moderate*/

replace ltpa_index_nt4 = 12.7 if exe_f_nt4blq1==3 & exe_du_nt4blq1==3 & exe_int_nt4blq1==3 /*moderate*/

replace ltpa_index_nt4 = 12.5 if exe_f_nt4blq1==4 & exe_du_nt4blq1==3 & exe_int_nt4blq1==1 /*moderate*/

replace ltpa_index_nt4 = 12.4 if exe_f_nt4blq1==3 & exe_du_nt4blq1==4 & exe_int_nt4blq1==3 /*moderate*/

* Moderate

replace ltpa_index_nt4 = 14 if exe_f_nt4blq1==4 & exe_du_nt4blq1==4 & exe_int_nt4blq1==1 /*moderate*/

replace ltpa_index_nt4 = 15 if exe_f_nt4blq1==4 & exe_du_nt4blq1==4 & exe_int_nt4blq1==2 /*moderate*/

replace ltpa_index_nt4 = 16 if exe_f_nt4blq1==4 & exe_du_nt4blq1==3 & exe_int_nt4blq1==2 /*moderate*/

replace ltpa_index_nt4 = 17 if exe_f_nt4blq1==4 & exe_du_nt4blq1==4 & exe_int_nt4blq1==2 /*moderate*/

replace ltpa_index_nt4 = 22 if exe_f_nt4blq1==5 & exe_du_nt4blq1==2 & exe_int_nt4blq1==2 /*moderate*/

replace ltpa_index_nt4 = 23 if exe_f_nt4blq1==5 & exe_du_nt4blq1==1 & exe_int_nt4blq1==3 /*moderate*/

replace ltpa_index_nt4 = 23.1 if exe_f_nt4blq1==5 & exe_du_nt4blq1==3 & exe_int_nt4blq1==1 /*moderate*/

replace ltpa_index_nt4 = 23.2 if exe_f_nt4blq1==5 & exe_du_nt4blq1==3 & exe_int_nt4blq1 ==. /*moderate*/

* High

replace ltpa_index_nt4 = 24 if exe_f_nt4blq1==4 & exe_du_nt4blq1>=3 & exe_int_nt4blq1==3 /*high*/

replace ltpa_index_nt4 = 25 if exe_f_nt4blq1==5 & exe_du_nt4blq1==4 /*high*/

replace ltpa_index_nt4 = 26 if exe_f_nt4blq1==5 & exe_du_nt4blq1==3 & exe_int_nt4blq1>=2 /*high*/

replace ltpa_index_nt4 = 27 if exe_f_nt4blq1==5 & exe_du_nt4blq1==2 & exe_int_nt4blq1==3 /*high*/

recode ltpa_index_nt4 (1=0 "Inactive") (2/13=0 "Low") (14/27=1 "Moderate/high"), gen (ltpa_group_nt4)

** Write .csv

export delimited using "\\fil.nice.ntnu.no\nice\p758\Data master\hunt_3_4_cleaned_full_data.csv", replace