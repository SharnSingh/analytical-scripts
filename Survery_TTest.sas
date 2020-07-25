
* Survey T-Test Models
* This script was written to produce multiple T-Tests and descriptive analytics for survey analysis.
* Written by Sharn Singh

**--------------------------
**      Data Import        - 
**--------------------------

Proc import file = "~\Workingdat_04_05_19.xlsx"
	dbms = xlsx out = cdat replace ;
	getnames = yes; 
	run;
proc print; run; 


Proc Import file = "~/updated_data.xlsx"
           out = update_data DBMS = XLSX replace;
           getnames = yes; 
run;
proc print; run; 


**--------------------------
**      Data Analysis      - 
**--------------------------

proc freq data=cdat;
tables Q1 Q2 Q3 Q4 Q5 Q8_1 Q8_2 Q8_3 Q9_1 Q9_2 Q9_3 Q9_4 Q9_5 Q9_6 Q9_7 Q13_1 Q13_2 Q13_3 Q13_4 Q13_5 Q13_6 Q13_7 Q10 Q11_1 Q12_1;
run;


***********DATA CLEANING********; 
data new; 
set update_data; 
if Q4="Surgery" then Q4new=0; 
if Q4="Peds" then Q4new=1; 
if Q4="Medicine" then Q4new=2;
if Q4="Psych/Neuro" then Q4new=3; 
if Q4="Family/OB" then Q4new=4;
if Q8_1= "Definitely not" then Q8_1new=0;
if Q8_1= "Probably not" then Q8_1new=1;
if Q8_1= "Might or might not" then Q8_1new=2;
if Q8_1= "Probably yes" then Q8_1new=3;
if Q8_1= "Definitely yes" then Q8_1new=4;
if Q8_2= "Definitely not" then Q8_2new=0;
if Q8_2= "Probably not" then Q8_2new=1;
if Q8_2= "Might or might not" then Q8_2new=2;
if Q8_2= "Probably yes" then Q8_2new=3;
if Q8_2= "Definitely yes" then Q8_2new=4;
if Q8_3= "Definitely not" then Q8_3new=0;
if Q8_3= "Probably not" then Q8_3new=1;
if Q8_3= "Might or might not" then Q8_3new=2;
if Q8_3= "Probably yes" then Q8_3new=3;
if Q8_3= "Definitely yes" then Q8_3new=4;
run; 
proc print; run; 

***************** t test***************; 
************** Within Surgery Pre vs. Post Code ***************; 
data new_dat; 
set new; 
where Q4new=0; 
run; 
proc print; run; 

***** Q8_1*****; 
proc ttest data=new_dat;
class treatment; 
var Q8_1new; 
run; 

***** Q8_2*****; 
proc ttest data=new_dat;
class treatment; 
var Q8_2new; 
run; 

****** Q8_3*****; 
proc ttest data=new_dat;
class treatment; 
var Q8_3new; 
run; 

************* Control Group (neuro and Med) not including surgery vs. Intervention (Family, Peds, Surgery(Post))****; 
data new_dat_q2; 
set new; 
if Q4="Surgery" and treatment=1 then Q4new2=0;
if Q4="Peds" then Q4new2=0; 
if Q4="Family/OB" then Q4new2=0; 
if Q4="Psych/Neuro" then Q4new2=1; 
if Q4="Medicine" then Q4new2=1; 
run; 
proc print; run; 

***** Q8_1*****; 
proc ttest data=new_dat_q2;
class Q4new2; 
var Q8_1new; 
run; 

***** Q8_2*****; 
proc ttest data=new_dat_q2;
class Q4new2; 
var Q8_2new; 
run; 

****** Q8_3*****; 
proc ttest data=new_dat_q2;
class Q4new2; 
var Q8_3new; 
run; 

************* Control Group (neuro and Med and surgery) vs. Intervention (Family, Peds)****; 
data new_dat_q3; 
set new; 
if Q4="Surgery" and treatment=0 then Q4new3=0;
if Q4="Peds" then Q4new3=0; 
if Q4="Family/OB" then Q4new3=0; 
if Q4="Psych/Neuro" then Q4new3=1; 
if Q4="Medicine" then Q4new3=1; 
run; 
proc print; run; 

***** Q8_1*****; 
proc ttest data=new_dat_q3;
class Q4new3; 
var Q8_1new; 
run; 

***** Q8_2*****; 
proc ttest data=new_dat_q3;
class Q4new3; 
var Q8_2new; 
run; 

****** Q8_3*****; 
proc ttest data=new_dat_q3;
class Q4new3; 
var Q8_3new; 
run; 
