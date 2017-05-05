proc import datafile = "C:\Users\jyuan4\Downloads\data1.csv"
out = mydata dbms=csv replace;
getnames=yes;
run;

ods pdf file="output.pdf";

proc mixed data=Mydata;
	class blocks geo origin;
	model height = geo origin geo*origin;
	random blocks blocks*geo;
	lsmeans geo origin geo*origin / pdiff;
run;

ods pdf close;
