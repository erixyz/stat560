/* Exercise 6.1 */

data panc;
input week surv @@;
cards;
3 0 4 0 4 0 4 1
8 0 8 1 16 0 22 0
24 1 30 1
;

proc lifetest data = panc plots = (survival);
	time week*surv(1);
run;

/* Exercise 6.2 */

data warranty;
input years fail @@;
cards;
1.1 0 2.6 0 2.8 0 3.1 0 3.4 0 3.5 0 3.5 0
3.6 0 3.7 0 3.8 0 3.8 0 4.0 0 4.1 0 5.6 0
;

proc lifetest data = warranty plots = (survival);
	time years*fail(1);
run;

/* Exercise 6.3 */

data insurance;
input gender $ time event @@;
cards;
M 4.2 1 M 5.0 1 M 5.3 1 M 6.7 1 M 8.2 1 M 10 0 M 10 0
F 3.6 1 F 6.7 1 F 8.2 1 F 9.3 1 F 10 0 F 10 0 F 10 0 F 10 0
;

proc lifetest data = insurance plots = (survival);
	time time*event(0);
	 strata gender;
run;

/* Exercise 6.4 */
data smokers;
input status $ years censored @@;
cards;
S 1.1 0 S 1.6 0 S 2.1 0 S 2.4 0 S 2.7 0 S 3.6 0 S 4.7 0 S 4.8 1 S 5.1 1
N 3.6 0 N 4.5 0 N 4.6 1 N 4.8 0 N 5.7 0 N 5.8 0 N 6.7 0 N 7.8 0
N 10.5 1 N 11.3 0 N 12.6 1
;

proc lifetest data = smokers plots = (survival);
	time years*censored(1);
	 strata status;
run;

/* Exercise 6.5 */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter6\DATA for Exercise 6.5.csv"
out = work.pres
dbms = CSV
;
run; 

* Part A;
proc phreg data = pres outest = betas;
	model lifespan*assassinated(1) = age yearstart yearsinoffice;
		baseline out = outdata survival = sbar;
run;

* Part B;
proc phreg data = pres outest = betas;
	model lifespan*assassinated(1) = age;
		baseline out = outdata survival = sbar;
run;

proc print data = sbar;
run;

* Part C used outputs from above;

/* Exercise 6.6 */

proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter6\DATA for Exercise 6.6.csv"
out = work.aorta
dbms = CSV
;
run;

* Part A;
data aorta1; set aorta;
 	NYHA1 = (NYHAclass = 'I');
 	NYHA2 = (NYHAclass = 'II');
 	NYHA3 = (NYHAclass = 'III');
 	M = (gender = 'M');
drop gender NYHAclass;
run;

proc print data = aorta1;
run;

proc phreg data = aorta1 outest = betas;
	model duration*censored(1) = age diameter NYHA1 NYHA2 NYHA3 M;
		baseline out = outdata survival = sbar;
run;

proc print data = outdata;
run;

* part D;
proc phreg data = aorta outest = betas;
	class gender(ref = 'F') NYHAclass / param = ref;
	model duration*censored(1) = age gender diameter NYHAclass;
		baseline out = outdata survival = sbar;
run;

proc print data = outdata;
run;

proc print data = betas;
run;
