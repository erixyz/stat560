/* Problem 1.12 */
data scars;
input group $ score @@;
cards;
tx 2.1 tx 1.6 tx 3.8 tx 3.2 tx 4.0
cx 3.7 cx 7.2 cx 2.8 cx 5.3 cx 8.6
;

proc npar1way data = scars;
class group;
	var score;
exact;
run;

/* Problem 1.13 */
data chemo;
input group $ redu @@;
cards;
tx 2.5 tx 2.4 tx 2.1 tx 3.4 tx 4.2 tx 1.1 tx 1.9
cx -0.9 cx 1.5 cx 2.3 cx -1.6 cx -3.4 cx 0.3 cx 2.0 
cx -1.1 cx 1.6
;

proc npar1way data = chemo;
	class group;
		var redu;
	exact;
run;

/* Problem 1.14 */
data shoulder;
input type $ score @@;
cards;
u 27 u 37 u 40 u 63 u 31 u 81 
u 63 u 57 u 90 u 94 c 56 c 78
c 60 c 55 c 67 c 68 c 64
;

proc npar1way data = shoulder;
		class type;
			var score;
		exact;
run;

/* Problem 2.1 */
data reading;
input student $ month $ read @@;
cards;
1 1 2 1 2 4 1 3 3 1 4 4
2 1 0 2 2 1 2 3 3 2 4 4
3 1 4 3 2 5 3 3 4 3 4 7
4 1 3 4 2 3 4 3 4 4 4 3
5 1 0 5 2 0 5 3 1 5 4 3
6 1 4 6 2 3 6 3 5 6 4 5
7 1 5 7 2 5 7 3 4 7 4 2
;

proc sort data = reading;
	by student;
run;

proc rank data = reading out = reading1;
	var read;
		by student;
			ranks rank;
run;

proc freq data = reading1;
	table student*month*rank/ noprint cmh;
run;
quit;

data contact;
input customer method $ score @@;
cards;
1 L 0.4 1 P 0.3 1 T 0.1
2 L 0.8 2 P 0.4 2 T 0.3
3 L 0.5 3 P 0.4 3 T 0.1
4 L 0.7 4 P 0.6 4 T 0.2
5 L 0.6 5 P 0.3 5 T 0.2
6 L 0.6 6 P 0.5 6 T 0.4
7 L 0.6 7 P 0.4 7 T 0.3
8 L 0.7 8 P 0.6 8 T 0.2
;

proc sort data = contact;
	by customer;
run;

proc rank data = contact out = contactRanked;
	var score;
		by customer;
			ranks rank;
run;

proc freq data = contactRanked;
	table customer*method*rank/noprint cmh;
run;

data customer;
input L P T;
	diff_LP = L-P;
	diff_LT = L-T;
	diff_PT = P-T;
cards;
0.4 0.3 0.1
0.8 0.4 0.3
0.5 0.4 0.1
0.7 0.6 0.2
0.6 0.3 0.2
0.6 0.5 0.4
0.6 0.4 0.3
0.7 0.6 0.2
;

proc univariate data = customer;
	var diff_LP diff_LT diff_PT;
run;


data fish;
input pond $ lead @@;
cards;
A 3 A 4 A 4 A 5 A 7 A 8
B 10 B 11 B 11 B 12 B 15 B 18
C 4 C 5 C 6 C 6 C 9 C 10
;

proc npar1way data = fish wilcoxon;
	class pond;
		var lead;
	exact;
run;

proc npar1way data = fish wilcoxon;
	class pond;
		var lead;
	exact;
	where (pond = "A" or pond = "B");
run;

proc npar1way data = fish wilcoxon;
	class pond;
		var lead;
	exact;
	where (pond = "B" or pond = "C");
run;

proc npar1way data = fish wilcoxon;
	class pond;
		var lead;
	exact;
	where (pond = "A" or pond = "C");
run;

data germination;
input temp $ rate @@;
cards;
24 88 24 54 24 65 24 55
28 67 28 72 28 76 28 80
32 93 32 82 32 84 32 78
36 86 36 87 36 81 36 73
;

proc npar1way data = germination wilcoxon;
	class temp;
		var rate;
	exact;
run;

data galk;
input gas milk;
cards;
1.78 1.30
2.11 1.70
2.01 1.88
2.17 2.15
2.45 2.20
2.76 2.25
3.12 2.19
3.24 2.45
3.56 2.87
3.70 2.99
3.42 3.15
3.24 3.06
;

PROC CORR DATA = galk SPEARMAN;
	VAR gas milk;
RUN;

proc freq data=galk;
 table gas*milk; exact scorr;
run; 

data violence;
input education $ violence $;
cards;
HSGRAD NEVER
HS+ OFTEN
HSGRAD OFTEN
HSGRAD SOMETIMES
HS+ NEVER
HS+ NEVER
HS+ OFTEN
HSGRAD OFTEN
<HS OFTEN
HS+ NEVER
<HS OFTEN
<HS NEVER
HSGRAD SOMETIMES
HS+ SOMETIMES
<HS SOMETIMES
;

data violenceLevels; 
set violence;
 if education='<HS' then ed_level=1;
 if education='HSGRAD' then ed_level=2;
 if education='HS+' then ed_level=3;
 if violence='NEVER' then violence_level=1;
 if violence='SOMETIMES' then violence_level=2;
 if violence='OFTEN' then violence_level=3;
run;

PROC CORR DATA = violenceLevels SPEARMAN;
	VAR ed_level violence_level;
RUN;

proc freq data=violenceLevels;
 table ed_level*violence_level; 
 exact scorr;
run; 

data KOBE;
input year points @@;
cards;
2000 1938 2001 2019 2002 2461 2003 1557 2004 1819
2005 2832 2006 2430 2007 2323 2008 2201 2009 1970
2010 2078
;

proc freq data = KOBE;
table year*points;
exact scorr;
run;

proc corr data = KOBE spearman;
	var year points;
run;

data valve;
input ageGroup $ center $ count @@;
cards;
20s-30s S 5 40s-50s S 10 60s S 13 70s S 12
20s-30s W 8 40s-50s W 11 60s W 4 70s W 3
20s-30s D 2 40s-50s D 14 60s D 8 70s D 0
;

PROC FREQ DATA = valve ORDER=DATA;
	weight count;
		table ageGroup*center/FISHER;
RUN;

data schoo;
input act $ gender $ count @@;
cards;
St B 12 Sp B 11 Fr B 4
St G 8 Sp G 2 Fr G 13
;

PROC FREQ DATA = schoo ORDER=DATA;
	weight count;
		table gender*act/FISHER;
RUN;

data penName;
do counter=1 to 6;
	time = 'before';
	 pen = 'true';
	output;
end;

do counter=1 to 9;
	time = 'before';
	 pen = 'false';
	output;
end;

do counter=1 to 4;
	time = 'after';
	 pen = 'true';
	output;
end;

do counter=1 to 11;
	time = 'after';
	 pen = 'false';
	output;
end;
;


proc freq data = penName;
	table time*pen/fisher;
run;

data tribes;
input tribe $ occ $ count @@;
cards;
A Fa 9 A H 3 A G 4 A T 2 A Fi 2
B Fa 6 B H 2 B G 3 B T 3 B Fi 6
C Fa 7 C H 0 C G 5 C T 3 C Fi 5
;

proc freq data = tribes order=data;
	weight count;
		table tribe*occ/FISHER;
run;




