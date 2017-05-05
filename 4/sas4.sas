proc import datafile = "C:\Users\jyuan4\Downloads\data3.csv"
out = mydata3 dbms=csv replace;
getnames=yes;
run;

proc contents data = Mydata3;
run;

data Mydata4;
  set Mydata3(rename=diameter_mm=diameter_mm_old);
  diameter=input(diameter_mm_old,best12.); *whatever is appropriate informat for your variable;
run;

proc contents data = Mydata4;
run;

ods pdf file="output.pdf";

proc mixed data=Mydata4;
	class blocks geo_num origin;
	model diameter = geo_num origin geo_num*origin;
	random blocks blocks*geo_num;
	lsmeans geo_num origin geo_num*origin / pdiff;
run;

ods pdf close;
