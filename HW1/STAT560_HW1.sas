/* Problem 1.1 */

data supp;
input start end;
diff = start - end;
cards;
22.125 23.375
23.5 23.125
23.5 24.75
25.875 25.75
26.375 26.75
21.375 22.625
28.875 29
24.625 24
23.125 24.125
25 27.250
;

proc univariate data=supp;
var diff;
run;

/* Problem 1.2 */

data mentScores;
input interv cont;
diff = interv - cont;
cards;
36 17
22 15
10 -8
12 -11
28 14
12 20
23 24
6 6
;

proc univariate data = mentScores;
var diff;
run;

/* Problem 1.3 */

data stocks;
input week1 week2;
diff = week1 - week2;
cards;
405.65 403.02
400.51 399.49
408.25 396.10
401.34 403.59
409.09 405.68
;


proc univariate data = stocks;
var diff;
run;

/* Problem 1.7 */

data knees;
input surgery $ days @@;
cards;
tx 5 tx 4 tx 6 tx 4 tx 3 tx 4 tx 4 tx 3 tx 5 tx 5
cx 7 cx 8 cx 12 cx 10 cx 8 cx 9 cx 10
;

proc npar1way data = knees wilcoxon;
	class surgery;
		var days;
	exact;
run;

/* Problem 1.8 */

data know;
input grp $ score @@;
cards;
ado 15 ado 8 ado 8 ado 10 ado 6 ado 9 ado 7 ado 8
adu 13 adu 17 adu 10 adu 12 adu 13 adu 17 adu 15 adu 17 
adu 17 adu 19
;

proc npar1way data = know wilcoxon;
	class grp;
		var score;
	exact;
run;

/* Problem 1.9 */

data frens;
input sex $ score @@;
cards;
F 10 F 7 F 11 F 8 F 5 F 12 F 13
M 7 M 6 M 8 M 5 M 3 M 6 M 7 M 6
M 3 M 2
;

proc npar1way data = frens wilcoxon;
	class sex;
		var score;
	exact;
run;

data kin;
input place $ hours @@;
cards;
work 1 work 4 work 14 work 7 work 11 work 1 work 8 work 10
home 1 home 3 home 5 home 5 home 4 home 3 home 4 home 5
;

proc npar1way data = kin wilcoxon;
	class place;
		var hours;
	exact;
run;

proc npar1way data = kin AB;
class place;
	var hours;
exact;
run;

data shoulder;
input loc $ score @@;
cards;
u 27 u 37 u 40 u 63 u 31 u 81 u 63 u 57 u 90 u 94
c 56 c 78 c 60 c 55 c 67 c 68 c 64
;

proc npar1way data = shoulder wilcoxon;
	class loc;
		var score;
	exact;
run;

proc npar1way data = shoulder AB;
class loc;
	var score;
exact;
run;


