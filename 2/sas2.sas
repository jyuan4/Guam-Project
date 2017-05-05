proc import datafile = "C:\Users\jyuan4\Downloads\data2.csv"
out = mydata2 dbms=csv replace;
getnames=yes;
run;

proc contents data = Mydata2;
run;

data Mydata3;
  set Mydata2(rename=root_damage_severity=root_damage_severity_old);
  root_damage_severity=input(root_damage_severity_old,best12.); *whatever is appropriate informat for your variable;
run;

proc contents data = Mydata3;
run;

ods pdf file="output.pdf";

proc mixed data=Mydata3;
	class blocks geo_num origin;
	model root_damage_severity = geo_num origin geo_num*origin;
	random blocks blocks*geo_num;
	lsmeans geo_num origin geo_num*origin / pdiff;
run;

ods pdf close;
