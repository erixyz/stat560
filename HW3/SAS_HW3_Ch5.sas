/* Exercise 5.1 */

/* part a */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter5\DATA for Exercise 5.1.csv"
out = work.sports
dbms = CSV
;
run;

proc gam data = sports;
model won(event='yes') = loess(margin) / 
	link = logit dist=binomial;
output out=result pred;
run;

/* part b */
data pointPred;
input margin; 
cards;
-11
;
run;

proc gam data = sports;
model won(event='yes')=loess(margin) / 
	link=logit dist=binomial;
output out=result pred; score data=pointPred out=outcome;
run;

proc print data = outcome;
run;

/* part c */
proc sort data=result;
	by margin;
run;

symbol1 color = indigo value=none interpol=join line=1;
proc gplot data=result;
 plot p_won*margin;
run;

/* part d */

/* a redo */
proc gam data = sports;
model won(event='yes')=spline(margin) / 
	link=logit dist=binomial;
output out=result pred;
run;

proc print data=result; 
run;

/* b redo */
proc gam data = sports;
model won(event='yes')=spline(margin) / 
	link=logit dist=binomial;
output out=result pred; 
score data = pointPred out=cubicWin;
run;

proc print data = cubicWin;
run;

/* Exercise 5.2 */

/* part a */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter5\DATA for Exercise 5.2.csv"
out = work.medic
dbms = CSV
;
run;

data medic1;
set medic;
	if gender = '' then delete;
run;

proc gam data = medic1;
	class gender intervention;
	model adherence(event='yes') = param(gender) param(intervention) spline(age) / 
		link = logit dist = binomial;
output out = modelA pred;
run;

proc print data = modelA; 
run;

/* part b */

proc sort data=modelA;
	by gender intervention age;
run;

symbol1 color = indigo value=none interpol=join line=1;
proc gplot data = modelA;
	plot p_adherence*age / name='plots';
		by gender intervention;
run;

proc greplay igout=work.gseg tc=sashelp.templt template=l2r2 nofs;
	treplay 1:plots 2:plots1 3:plots2 4:plots3;
run;

/* Exercise 5.3 */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter5\DATA for Exercise 5.3.csv"
out = work.vetAff
dbms = CSV
;
run;

data trt;
	set vetAff;
		if gender = '' then delete;
		ptsd = (score >= 50);
run;


/* part a */
data forPred;
input age gender $ injury $ deployment;
cards;
25 M yes 12
;

proc gam data = trt;
	class gender injury;
	model ptsd(event='1') = param(gender injury) spline2(age, deployment) /
		link = logit dist = binomial;
output out = modelB pred;
score data = forPred out = thePred;
run;

proc print data = thePred;
run;

/* part b */
proc gam data = trt;
	class gender injury;
	model ptsd(event='1') = param(gender injury) spline(age) spline(deployment)/
    	link = logit dist = binomial;
output out = modelC pred; 
score data = forPred out = thePred1;
run;

proc print data = thePred1;
run;

/* Exercise 5.4 */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter5\DATA for Exercise 5.4.csv"
out = work.traff
dbms = CSV
;
run;

data traffAcc;
	set traff;
		if time_bound = '' then delete;
		drop VAR4 VAR5;
run;

/* part a */
proc gam data=traffAcc;
	class time_bound;
	model naccidents = param(time_bound) loess(brighness)/
 		link=log dist=poisson;
output out=result pred;
run;

/* part b */
proc gam data=traffAcc;
	class time_bound;
	model naccidents = param(time_bound) spline(brighness)/
 	  	link=log dist=poisson;
output out=result1 pred;
run;

/* Exercise 5.5 */
proc import datafile = "C:\Users\casti\OneDrive\Desktop\School_Stuff\Masters_Stuff\STAT 560\STUDY_MATERIALS_NEW\STUDY_MATERIALS\CSV_Data_Exercises\Chapter5\DATA for Exercise 5.5.csv"
out = work.news
dbms = CSV
;
run;

data papel;
	set news;
		if nmiss(of _numeric_)  then delete;
		drop VAR4 VAR5;
run;

/* part a */
proc gam data = papel;
	model ntypos = spline(circulation) spline(cost)/
    	link=log dist=poisson;
output out = paperMod pred;
run;

proc print data = paperMod;
run;

/* part b */
data papelPred1;
input circulation cost;
cards;
13.5 1.75
;
run;

proc gam data = papel;
	model ntypos = spline(circulation) spline(cost)/
    	link=log dist=poisson;
output out = paperMod pred;
score data = papelPred1 out = finalAns1;
run;

proc print data = finalAns1;
run;

/* part c */
proc summary data = papel print;
    var circulation cost;
run;


data gridForPlot;
	do circulation = 0 to 45 by 1;
		do cost = 0 to 6 by 1;
			output; 
		end; 
	end;
run;

proc gam data = papel;
	model ntypos = spline(circulation) spline(cost)/ 
		link = log dist = poisson;
output out = result pred; 
score data = gridForPlot out = surf;
run;

proc g3d data = surf;
	plot circulation*cost = p_ntypos /
		ctop=indigo cbottom=pink;
run;

/* part d */

/* part a redo */
proc gam data = papel;
	model ntypos = spline2(circulation, cost)/
		link = log dist = poisson;
output out = flimflam pred;
score data = papelPred1 out = quadPred;
score data = gridForPlot out = newSurf;
run;

proc print data = flimflam;
run;

/* part b redo */
proc print data = quadPred;
run;

/* part c redo */
proc g3d data = newSurf;
plot circulation*cost = p_ntypos/
	ctop = indigo cbottom = pink;
run;
