
* Descriptive Statistics 
* This script was written to calculate descriptive statistics across multiple data subsets. 
* Written by Sharn Singh

proc import datafile= "~/report_data.xlsx"
	dbms = xlsx out = dat;
 run;
proc print data=dat; run; 

data tele;
set dat;
where TypePrep = "Telehealth";
run;

data IP;
set dat;
where TypePrep = "In-person";
run;

%macro Chi(var);
	proc freq data=dat;
	tables TypePrep*&var / chisq;
	run;
%mend;

%Chi(Screening);
%Chi(For___FIT_FOBT);
%Chi(Hx_polyps);
%Chi(FHx_of_CRC__colorectal_cancer_);
%Chi(VAR11);
%Chi(anemia);
%Chi(other);
%Chi(polyp);
%Chi(Sessile_polyp);
%Chi(Adenoma);
%Chi(Adenocarcinoma);
%Chi(VAR23);
%Chi(race);


%macro freq(var);
	proc freq data=dat;
	tables &var;
	run;
%mend;

%freq(Screening);
%freq(For___FIT_FOBT);
%freq(Hx_polyps);
%freq(FHx_of_CRC__colorectal_cancer_);
%freq(VAR11);
%freq(anemia);
%freq(other);
%freq(polyp);
%freq(Sessile_polyp);
%freq(Adenoma);
%freq(Adenocarcinoma);
%freq(VAR23);
%freq(race);

proc means data=dat;
var BMI;
run;

proc means data = tele;
var BMI;
run;

proc means data = IP;
var BMI;
run;

proc means data=dat;
var AgeAtFirstAppt;
run;

proc means data = tele;
var AgeAtFirstAppt;
run;

proc means data = IP;
var AgeAtFirstAppt;
run;

proc means data=dat;
var Distance__Miles__Address_to_LLVA;
run;

proc means data = tele;
var Distance__Miles__Address_to_LLVA;
run;

proc means data = IP;
var Distance__Miles__Address_to_LLVA;
run;

proc freq data=dat;
tables LLVA_vs_CBOC_vs_NoPCP;
run;

proc freq data = dat;
tables LLVA_vs_CBOC_vs_NoPCP*If_Y__Where_ / chisq;
run;

proc means data=dat;
var bmi;
class If_Y__Where_;
run;

proc means data=dat;
var AgeAtFirstAppt;
class If_Y__Where_;
run;
