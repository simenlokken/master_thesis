** This scripts load in the data from the NICE area and creates LTPA groups for the Cox analyses

** Import .csv file

import delimited "\\fil.nice.ntnu.no\nice\p758\Data master\hunt_1_3_cleaned_full_data.csv"

** HUNT 1

** Encode variables to be ordered categorical

encode exe_f_nt1blq2, generate(exe_f_nt1blq2_encoded)
encode exe_int_nt1blq2, generate(exe_int_nt1blq2_encoded)
encode exe_du_nt1blq2, generate(exe_du_nt1blq2_encoded)

drop exe_f_nt1blq2
drop exe_int_nt1blq2
drop exe_du_nt1blq2

rename exe_f_nt1blq2_encoded exe_f_nt1blq2
rename exe_int_nt1blq2_encoded exe_int_nt1blq2
rename exe_du_nt1blq2_encoded exe_du_nt1blq2

**  Differentiate into LTPA

* Create a new variable where missing (.) is default if nothing else is provided in the logic

gen ltpa_index_nt1 =.

* Inactive

replace ltpa_index_nt1 = 1 if exe_f_nt1blq2<=2 | exe_f_nt1blq2==3 & exe_du_nt1blq2==1 | exe_f_nt1blq2==4 & exe_du_nt1blq2==1 & exe_int_nt1blq2<3 /*inactive*/

* Low

replace ltpa_index_nt1 = 2 if exe_f_nt1blq2==5 & exe_du_nt1blq2==1 & exe_int_nt1blq2<3 /*Low*/

replace ltpa_index_nt1 = 3 if exe_f_nt1blq2==3 & exe_du_nt1blq2==2 & exe_int_nt1blq2==1 /*Low*/

replace ltpa_index_nt1 = 4 if exe_f_nt1blq2==3 & exe_du_nt1blq2==3 & exe_int_nt1blq2==1 /*Low*/

replace ltpa_index_nt1 = 5 if exe_f_nt1blq2==3 & exe_du_nt1blq2==2 & exe_int_nt1blq2==2 /*Low*/

replace ltpa_index_nt1 = 6 if exe_f_nt1blq2==3 & exe_du_nt1blq2==3 & exe_int_nt1blq2==2 /*Low*/

replace ltpa_index_nt1 = 7 if exe_f_nt1blq2==3 & exe_du_nt1blq2==4 & exe_int_nt1blq2==2 /*Low*/

replace ltpa_index_nt1 = 8 if exe_f_nt1blq2==3 & exe_du_nt1blq2==2 & exe_int_nt1blq2==3 /*Low*/

replace ltpa_index_nt1 = 9 if exe_f_nt1blq2==4 & exe_du_nt1blq2==2 & exe_int_nt1blq2==1 /*Low*/

replace ltpa_index_nt1 = 10 if exe_f_nt1blq2==4 & exe_du_nt1blq2==2 & exe_int_nt1blq2==2 /*Low*/

replace ltpa_index_nt1 = 11 if exe_f_nt1blq2==4 & exe_du_nt1blq2==1 & exe_int_nt1blq2==3 /*Low*/

replace ltpa_index_nt1 = 12 if exe_f_nt1blq2==5 & exe_du_nt1blq2==2 & exe_int_nt1blq2==1 /*Low*/

replace ltpa_index_nt1 = 13 if exe_f_nt1blq2==3 & exe_du_nt1blq2==3 & exe_int_nt1blq2==1 /*Low*/

replace ltpa_index_nt1 = 12.9 if exe_f_nt1blq2==3 & exe_du_nt1blq2==4 & exe_int_nt1blq2==1 /*Low*/

replace ltpa_index_nt1 = 12.8 if exe_f_nt1blq2==. & exe_du_nt1blq2==4 & exe_int_nt1blq2 >1 /*high*/

replace ltpa_index_nt1 = 12.6 if exe_f_nt1blq2==4 & exe_du_nt1blq2==2 & exe_int_nt1blq2==3 /*moderate*/

replace ltpa_index_nt1 = 12.7 if exe_f_nt1blq2==3 & exe_du_nt1blq2==3 & exe_int_nt1blq2==3 /*moderate*/

replace ltpa_index_nt1 = 12.5 if exe_f_nt1blq2==4 & exe_du_nt1blq2==3 & exe_int_nt1blq2==1 /*moderate*/

replace ltpa_index_nt1 = 12.4 if exe_f_nt1blq2==3 & exe_du_nt1blq2==4 & exe_int_nt1blq2==3 /*moderate*/

 

* Moderate

replace ltpa_index_nt1 = 14 if exe_f_nt1blq2==4 & exe_du_nt1blq2==4 & exe_int_nt1blq2==1 /*moderate*/

replace ltpa_index_nt1 = 15 if exe_f_nt1blq2==4 & exe_du_nt1blq2==4 & exe_int_nt1blq2==2 /*moderate*/

replace ltpa_index_nt1 = 16 if exe_f_nt1blq2==4 & exe_du_nt1blq2==3 & exe_int_nt1blq2==2 /*moderate*/

replace ltpa_index_nt1 = 17 if exe_f_nt1blq2==4 & exe_du_nt1blq2==4 & exe_int_nt1blq2==2 /*moderate*/

replace ltpa_index_nt1 = 22 if exe_f_nt1blq2==5 & exe_du_nt1blq2==2 & exe_int_nt1blq2==2 /*moderate*/

replace ltpa_index_nt1 = 23 if exe_f_nt1blq2==5 & exe_du_nt1blq2==1 & exe_int_nt1blq2==3 /*moderate*/

replace ltpa_index_nt1 = 23.1 if exe_f_nt1blq2==5 & exe_du_nt1blq2==3 & exe_int_nt1blq2==1 /*moderate*/

replace ltpa_index_nt1 = 23.2 if exe_f_nt1blq2==5 & exe_du_nt1blq2==3 & exe_int_nt1blq2 ==. /*moderate*/

 

* High

replace ltpa_index_nt1 = 24 if exe_f_nt1blq2==4 & exe_du_nt1blq2>=3 & exe_int_nt1blq2==3 /*high*/

replace ltpa_index_nt1 = 25 if exe_f_nt1blq2==5 & exe_du_nt1blq2==4 /*high*/

replace ltpa_index_nt1 = 26 if exe_f_nt1blq2==5 & exe_du_nt1blq2==3 & exe_int_nt1blq2>=2 /*high*/

replace ltpa_index_nt1 = 27 if exe_f_nt1blq2==5 & exe_du_nt1blq2==2 & exe_int_nt1blq2==3 /*high*/

recode ltpa_index_nt1 (1=0 "Inactive") (2/13=0 "Low") (14/27=1 "Moderate/high"), gen (ltpa_group_nt1)

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

** Write .csv

export delimited using "\\fil.nice.ntnu.no\nice\p758\Data master\hunt_1_3_cleaned_full_data.csv" , replace
