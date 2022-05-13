/* Exercise 4.1 */

/* importing data */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter4\e4_1.csv"
out = work.crime
dbms = CSV
;
run;

data crimeAdj;
 set crime;
 	pop = population/100000000;
 	tot = totalcrimes/1000000;
run;

/* part a */
symbol1 color = red value = dot;

proc gplot data = crimeAdj;
 plot tot*pop;
run;

/* part b */
proc loess data = crimeAdj;
	model tot = pop / degree=1 clm;
	ods output OutputStatistics=outstats;
run;

symbol1 color=red value=dot;
symbol2 color=black value=none interpol=join line=1;
symbol3 color=black value=none interpol=join line=2;
symbol4 color=black value=none interpol=join line=2;

proc gplot data=outstats;
 by SmoothingParameter;
 plot (DepVar Pred LowerCL UpperCL)*pop / overlay;
run;

proc print data=outstats;
run;

/* part c */

proc loess data = crimeAdj;
	model tot = pop / degree=2 clm;
	ods output OutputStatistics = outstats
;
run;

symbol1 color=red value=dot;
symbol2 color=black value=none interpol=join line=1;
symbol3 color=black value=none interpol=join line=2;
symbol4 color=black value=none interpol=join line=2;

proc gplot data = outstats;
 by SmoothingParameter;
 plot (DepVar Pred LowerCL UpperCL)*pop / overlay;
run;

proc print data = outstats;
run;

/* Exercise 4.2 */
/* importing the data */

proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter4\e4_2.csv"
out = work.money
dbms = CSV replace
;
run;

data boot;
	set work.money;
	month = _N_;
		drop VAR2 VAR3;
		if nmiss( of _numeric_ ) and cmiss(of _character_) then delete ;
run;

/* part a */

proc loess data=boot;
 model revenue = month / clm alpha = 0.1;
 ods output OutputStatistics = outstats;
run;
symbol1 color=black value=dot;
symbol2 color=green value=none interpol=join line=1;
symbol3 color=green value=none interpol=join line=2;
symbol4 color=green value=none interpol=join line=2;

proc gplot data=outstats;
 by SmoothingParameter;
 plot (DepVar Pred LowerCL UpperCL)*month / overlay;
run;

/* Exercise 4.3 */

/* part a */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter4\e4_3.csv"
out = work.snacc
dbms = CSV
;

proc g3d data = work.snacc;
	scatter sodas*fries = fruits/ color = "blue";
run;

data gridPoints;
	do sodas = 0 to 24 by 1;
		do fries = 0 to 8 by 1;
		 output;
		end;
	end;
run;

proc loess data = snacc;
	model fruits = sodas fries;
		ods output ScoreResults=scoreResults;
			score data = gridPoints;
run;

proc g3d data = scoreResults;
	plot sodas*fries = p_fruits/ctop = indigo cbottom = blue;
run;

/* part b */
data pointPred;
input sodas fries;
cards;
14 2
;

proc loess data = snacc;
	model fruits = sodas fries;
		ods output ScoreResults = predicted;
			score data = pointPred;
run;

proc print data = predicted;
run;

/* Exercise 4.4 */

/* Smoothing parameter of 2*/
proc tpspline data=crimeAdj;
 model tot = (pop) / m=2;
 output out=result1 pred lclm uclm;
run;

title 'm=2'; symbol1 color=black value=dot;
symbol2 color=green value=none interpol=join line=1;
symbol3 color=green value=none interpol=join line=2;
symbol4 color=green value=none interpol=join line=2; 

proc gplot data=result1;
 plot (tot P_tot LCLM_tot UCLM_tot)*pop /
 overlay name='g1';
run;

/* Smoothing parameter of 3*/
proc tpspline data=crimeAdj;
 model tot = (pop) / m=3;
 output out=result2 pred lclm uclm;
run;

title 'm=3'; symbol1 color=black value=dot;
symbol2 color=green value=none interpol=join line=1;
symbol3 color=green value=none interpol=join line=2;
symbol4 color=green value=none interpol=join line=2; 

proc gplot data=result2;
 plot (tot P_tot LCLM_tot UCLM_tot)*pop /
 overlay name='g2';
run;

/* Smoothing parameter of 4*/
proc tpspline data=crimeAdj;
 model tot = (pop) / m=4;
 output out=result3 pred lclm uclm;
run;

title 'm=4'; symbol1 color=black value=dot;
symbol2 color=green value=none interpol=join line=1;
symbol3 color=green value=none interpol=join line=2;
symbol4 color=green value=none interpol=join line=2; 

proc gplot data=result3;
 plot (tot P_tot LCLM_tot UCLM_tot)*pop /
 overlay name='g3';
run;

goption display;
proc greplay nofs tc=sashelp.templt template=l2r2;
 igout gseg; treplay 1:g1 2:g2 3:g3;
run;

/* Exercise 4.5 */

/* Smoothing parameter of 2*/
proc tpspline data=boot;
 model revenue = (month) / m=2;
 output out=result2 pred lclm uclm;
run;

title 'm=2'; symbol1 color=black value=dot;
symbol2 color=pink value=none interpol=join line=1;
symbol3 color=pink value=none interpol=join line=2;
symbol4 color=pink value=none interpol=join line=2; 

proc gplot data=result2;
 plot (revenue P_revenue LCLM_revenue UCLM_revenue)*month /
 overlay name='g2';
run;

/* Smoothing parameter of 3*/
proc tpspline data=boot;
 model revenue = (month) / m=3;
 output out=result3 pred lclm uclm;
run;

title 'm=3'; symbol1 color=black value=dot;
symbol2 color=pink value=none interpol=join line=1;
symbol3 color=pink value=none interpol=join line=2;
symbol4 color=pink value=none interpol=join line=2; 

proc gplot data=result3;
 plot (revenue P_revenue LCLM_revenue UCLM_revenue)*month /
 overlay name='g3';
run;

/* Smoothing parameter of 4 */
proc tpspline data=boot;
 model revenue = (month) / m=4;
 output out=result4 pred lclm uclm;
run;

title 'm=4'; symbol1 color=black value=dot;
symbol2 color=blue value=none interpol=join line=1;
symbol3 color=blue value=none interpol=join line=2;
symbol4 color=blue value=none interpol=join line=2; 

proc gplot data=result4;
 plot (revenue P_revenue LCLM_revenue UCLM_revenue)*month /
 overlay name='g4';
run;

/* Smoothing parameter of 5 */
proc tpspline data=boot;
 model revenue = (month) / m=5;
 output out=result5 pred lclm uclm;
run;

title 'm=5'; symbol1 color=black value=dot;
symbol2 color=blue value=none interpol=join line=1;
symbol3 color=blue value=none interpol=join line=2;
symbol4 color=blue value=none interpol=join line=2; 

proc gplot data=result5;
 plot (revenue P_revenue LCLM_revenue UCLM_revenue)*month /
 overlay name='g5';
run;

goption display;
proc greplay nofs tc=sashelp.templt template=l2r2;
 igout gseg; treplay 1:g2 2:g4 3:g3 4:g5;
run;

/* Exercise 4.6 */

/* part a */
data gridPoints;
	do sodas = 0 to 24 by 1;
		do fries = 0 to 8 by 1;
		 output;
		end;
	end;
run;

proc tpspline data = snacc;
	model fruits = (fries sodas);
		score data = gridPoints out = result;
run;

proc g3d data = result;
	plot fries*sodas = p_fruits/ ctop = indigo cbottom = blue;
run;

/* part b */
data pointPred;
input sodas fries;
cards;
14 2
;

proc tpspline data = snacc;
	model fruits = (sodas fries);
			score data = pointPred out = predicted;
run;

proc print data = predicted;
run;

/* Exercise 4.7 */

/* part a */
proc tpspline data = snacc;
	model fruits = sodas (fries) / m=2;
		score data = gridPoints out = results;
run;

proc g3d data = results;
 plot sodas*fries = p_fruits / ctop=orange cbottom=blue;
run;

/* part b */

proc tpspline data = snacc;
 model fruits = sodas (fries) / m=2;
 score data = pointPred out = guess;
run;

proc print data=guess;
run;
