/* Exercise 7.1 */

* part a;
data yellowstone;
input times @@;
cards;
65 82 84 54 85 58 79 57 88 68 76 78 74 85 75
65 76 58 83 50 87 78 78 74 66 84 84 98 93 59
;

proc univariate data = yellowstone;
	histogram times/midpercents cfill = orange;
run;

* part b;
proc univariate data = yellowstone;
	histogram times/ cfill = gray midpoints = 50 to 100 by 5
		kernel(k= normal quadratic triangular 
					l = 1 2 3);
run;

proc univariate data = yellowstone;
	histogram times/ cfill = gray midpoints = 55 to 100 by 5
		kernel(c = 0.6 k= normal quadratic triangular 
					l = 1 2 3);
run;

/* Exercise 7.2 */

* part a;
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter7\DATA for Exercise 7.2.csv"
out = work.heights
dbms = CSV
;
run;

data difference;
set heights;
diff = husband - wife;
run;

proc univariate data = difference;
	histogram diff/midpercents;
run;

* part b;
proc means data = difference;
run;

* minimum -4 maximum 16;

proc univariate data = difference;
	histogram diff/ midpoints = -4 to 16 by 3
		kernel(k= normal quadratic triangular 
					l = 1 2 3);
run;

proc univariate data = difference;
	histogram diff/ midpoints = -4 to 16 by 3
		kernel(c = 0.7 k= normal quadratic triangular 
					l = 1 2 3);
run;

proc univariate data = difference;
	histogram diff/ midpoints = -4 to 16 by 3
		kernel(c = 0.6 k= normal quadratic triangular 
					l = 1 2 3);
run;

/* Exercise 7.3 */

* part a;
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter7\DATA for Exercise 7.3.csv"
out = work.leaf
dbms = CSV
;
run;

proc univariate data = leaf;
	histogram length/midpercents;
run;

* part b;
proc means data = leaf;
run;

proc univariate data = leaf;
	histogram length/ midpoints = 0.3 to 7.3 by 0.4
		kernel(k= normal quadratic triangular 
					l = 1 2 3);
run;

proc univariate data = leaf;
	histogram length/ midpoints = 0.3 to 7.3 by 0.4
		kernel(c = 0.8 k= normal quadratic triangular 
					l = 1 2 3);
run;

proc univariate data = leaf;
	histogram length/ midpoints = 0.3 to 7.3 by 0.4
		kernel(c = 0.6 k= normal quadratic triangular 
					l = 1 2 3);
run;


/* exercise 8.1 */
proc univariate data = yellowstone;
	var times;
		output out = stats n = n_obs mean = mean_all;
run;

data _null_;
	set stats;
		call symput ('n_obs', n_obs);
			call symput ('mean_all', mean_all);
run;

data jackknife_samples;
	do sample = 1 to &n_obs;
		do record = 1 to &n_obs;
			set yellowstone point = record;
				if sample ne record then output;
		end;
	end;
		stop;
run;

proc univariate data = jackknife_samples;
	var times;
		by sample;
output out = jackknife_replicates mean = mean_revenue;
run;

data jackknife_replicates;
	set jackknife_replicates;
		pseudomean = &n_obs*&mean_all-(&n_obs-1)*mean_revenue;
	by sample;
		keep pseudomean;
run;

proc means data = jackknife_replicates;
	var pseudomean;
		output out = CI LCLM=CI_95_lower UCLM = CI_95_upper;
run;

proc print data = CI;
run;

/* Exercise 8.2 */
proc corr data= heights outp = corr_all;
	var husband wife;
run;

data _null_; set corr_all;
	if (_type_='N') then call symput ('n_obs', husband);
	if (_type_='CORR' and _name_='husband') then
		call symput ('corr_all', wife);
run;

data jacksamps;
 do sample = 1 to &n_obs;
 	do record = 1 to &n_obs;
 		set heights point = record;
			if sample ne record then output;
 	end; 
 end;
stop;
run;

proc corr data = jacksamps outp = jackreps;
	var husband wife;
		by sample;
run;

data jackreps; 
	set jackreps;
		if (_type_='CORR' and _name_='husband');
		pseudo_corr = &n_obs*&corr_all - (&n_obs-1)*wife;
		keep pseudo_corr;
run;

proc means data=jackreps alpha=0.01;
	var pseudo_corr;
	output out=corr_ci lclm=CI_95_lower uclm=CI_95_upper;
run;

proc print data=corr_ci;
run;

/* Exercise 8.3 */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter3\DATA for Exercise 3.1.csv"
out = work.gilk
dbms = CSV
;
run;

proc corr data= gilk outp = corr_all;
	var pgas pmilk;
run;

data _null_; 
set corr_all;
	if (_type_='N') then call symput ('n_obs', pgas);
	if (_type_='CORR' and _name_='pgas') then
		call symput ('corr_all', pmilk);
run;

data jacksamps;
 do sample = 1 to &n_obs;
 	do record = 1 to &n_obs;
 		set gilk point = record;
			if sample ne record then output;
 	end; 
 end;
stop;
run;

proc corr data = jacksamps outp = jackreps;
	var pgas pmilk;
		by sample;
run;

data jackreps; 
	set jackreps;
		if (_type_='CORR' and _name_='pgas');
		pseudo_corr = &n_obs*&corr_all - (&n_obs-1)*pmilk;
		keep pseudo_corr;
run;

proc means data=jackreps alpha=0.05;
	var pseudo_corr;
	output out=corr_ci lclm=CI_95_lower uclm=CI_95_upper;
run;

proc print data=corr_ci;
run;

/* Exercise 8.4 */
proc univariate data = leaf noprint;
	var length;
	output out=stats n=n_obs var=var_all;
run;

data _null_; 
set stats;
	call symput ('n_obs', n_obs);
	call symput ('var_all', var_all);
run;

data jacksamp;
	do sample = 1 to &n_obs;
		do record = 1 to &n_obs;
			set leaf point=record;
				if sample ne record then output;
 		end; 
	end;
stop;
run;

proc univariate data=jacksamp noprint;
	var length;
		by sample;
output out=jackreps var=var_length;
run; 

data jackreps; 
set jackreps;
	pseudo = &n_obs*&var_all-(&n_obs-1)*var_length;
		by sample;
			keep pseudo;
run;

proc means noprint data=jackreps alpha=0.1;
 var pseudo;
 output out=CI lclm=CI_90_LOWER uclm=CI_90_UPPER;
run;

proc print data=CI;
run;

/* Exercise 8.5 */
proc surveyselect data = yellowstone out = bootsamps
outhits seed = 1 method = urs samprate = 1 rep=1000;
run;

proc univariate data = bootsamps;
	var times;
		by replicate;
output out=bootreps mean=avg_mins;
run;

proc univariate data = bootreps;
	var avg_mins;
output out = CI
pctlpre = ci_mean pctlpts = 2.5 97.5 pctlname = CI_95_LOWER CI_95_UPPER;
run; 

proc print data = CI;
run;

/* Exercise 8.6 */
proc surveyselect data = heights out = bootsamp
outhits seed = 1 method = urs samprate = 1 rep = 1000;
run;

proc corr noprint data = bootsamp outp = corrstat;
 var husband wife;
 by replicate;
run;

data bootreps; 
set corrstat;
	if (_type_='CORR' and _name_='wife');
	pcorr = husband;
keep pcorr;
run;

proc univariate data = bootreps;
	var pcorr;
	output out = CI
	pctlpre=ci99 pctlpts= 0.5 99.5 pctlname=LOWER_CI UPPER_CI;
run;

proc print data = CI;
run;

/* Exercise 8.7 */
proc surveyselect data = gilk out = bootsamp
outhits seed = 1 method = urs samprate = 1 rep=1000;
run;

proc corr noprint data = bootsamp outs = corrstat;
 var pgas pmilk;
 by replicate;
run;

data bootreps; 
set corrstat;
	if (_type_='CORR' and _name_='pgas');
	scorr = pmilk;
	keep scorr;
run;

proc univariate data = bootreps;
 var scorr;
 output out = CI
 pctlpre=ci pctlpts=2.5 97.5 pctlname= LOWER UPPER;
run;

proc print data = CI;
run;

/* Exercise 8.8 */
proc surveyselect data = leaf out = bootsamp
outhits seed = 1 method = urs samprate = 1 rep = 1000;
run;

proc univariate data=bootsamp;
	var length;
		by replicate;
output out = bootreps var = vlen;
run;

proc univariate data = bootreps;
	var vlen;
output out = CI
pctlpre =  CI pctlpts=5 95 pctlname= LOWER UPPER;
run;

proc print data = CI;
run;
