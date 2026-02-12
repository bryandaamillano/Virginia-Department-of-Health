/*BRFSS Base Code-KSB 1.14.2025*/

libname BRFSS 'K:\OFHS_New\DPE\PopHealth\Surveys\VAHS\Data\SAS\Datasets\PublicUse';

*Notes: 
Locality codes- Region=HLTH_REGION_ID DIstrict HLTH_DIST_NAME, County=CTYCODE2 NOT in public use dataset
Interview Year= IYear;

*checking that data pulled in correctly;
proc contents data=data.brpub23;
run;
PROC PRINT DATA=brfss.brpub23 (OBS=10);
RUN;
proc freq DATA=brfss.brpub23;
table HLTH_DIST_NAME;
run;
proc freq DATA=brfss.brpub23;
table age race;
run;

proc format;

value GENHLTH
1 = 'Excellent'
2 = 'Very good'
3 = 'Good'
4 = 'Fair'
5 = 'Poor'
7 = 'Dont Know/ Not sure'
9 = 'Refused';

value noyes
0 = 'No'
1 = 'Yes';

value age
1='18 to 24'
2='25 to 34'
3='35 to 44'
4='45 to 54'
5='55 to 64'
6='65+';

value race
1='White'
2='Black'
3='Hispanic'
4='Other';

value income
1='<25'
2='25-50'
3='50-75'
4='75+';

value education
1='Didnt Graduate HS'
2='Graduated HS'
3='Some College'
4='Graduated College';

value employ
1='Employed'
2='Unemployed'
3='Retired';

value sex
1='Male'
2='Female';

VALUE nyf
1= 'No'
2= 'Yes';

value yesno
1='Yes'
2='No';

value priminsr
1='Yes'
2='No';

value PRIMINSR
1= 'Employer or Union'
2= 'Private'
3= 'Medicare'
4= 'Medigap'
5= 'Medicaid'
6= 'CHIP'
7= 'CHAMPUS, CHAMP-VA, VA health Care'
8= 'Indian Health Services'
9= 'Other Government Program'
10= 'No Coverage';

value CHECKUP1
1= 'Within past year (anytime less than 12 months ago)'
2= 'Within past 2 years (1 year but less than 2 years ago)'
3= 'Within past 5 years (2 years but less than 5 years ago)'
4= '5 or more years ago'
7= 'Not sure';

value GoodHealth
1= "Good, Very good, or Excellent Health"
2= "Fair or Poor Health";

*creating smaller datasets to later merge;
proc SQL;
	create table BRFSS_21 AS
	select SEQNO, iyear, _ststr, _psu, _llcpwt, GENHLTH, HLTH_DIST_NAME, sex, race, _AGE_G, employ1, _HLTHPLN, CHECKUP1, INCOME3, _educag, _IMPRACE, DIABETE4, HAVARTH4, CHCCOPD3, ADDEPEV3
from BRFSS.brpub21;
*can include where statement here if needed (i.e. only need males: where sex=1);
quit;
proc SQL;
	create table BRFSS_22 AS
	select SEQNO, iyear, _ststr, _psu, _llcpwt, GENHLTH, HLTH_DIST_NAME, sex, race, _AGE_G, employ1, _HLTHPLN, CHECKUP1, INCOME3, _educag, _IMPRACE, DIABETE4, HAVARTH4, CHCCOPD3, ADDEPEV3
from BRFSS.brpub22;
*can include where statement here if needed (i.e. only need males: where sex=1);
quit;
proc SQL;
	create table BRFSS_23 AS
	select SEQNO, iyear, _ststr, _psu, _llcpwt, GENHLTH, HLTH_DIST_NAME, sex, race, _AGE_G, employ1, _HLTHPL1, CHECKUP1, INCOME3, _educag, _IMPRACE, DIABETE4, HAVARTH4, CHCCOPD3, ADDEPEV3
from BRFSS.brpub23;
*can include where statement here if needed (i.e. only need males: where sex=1);
quit;

*Sort before merging;
proc sort data=BRFSS_21; by SEQNO; run;
proc sort data=BRFSS_22; by SEQNO; run;
proc sort data=BRFSS_23; by SEQNO; run;

*Merge new smaller datasets together;
data All_BRFSS;
   merge work.BRFSS_21 work.BRFSS_22 work.BRFSS_23;
   by SEQNO;
run;


data BRFSS_clean;
set All_BRFSS; 

*/if employ1=9 then employ1=.;/*employment status*/
*/if _educag=9 then _educag=.; /*level of education completed*/
*/if age in (7,9) then age=.; /*6 age categories*/
*/if PRIMINSR in (7,9) then PRIMINSR=.; /*Health insurance*/

/*clean up demographic and control variables of interest*/
/*5 category age*/
if _age_g=. then age2=.;
else if _age_g=1 then age2=1; /*18 to 24*/
else if _age_g=2 then age2=2; /*25 to 34*/
else if _age_g=3 then age2=3; /*35 to 44*/
else if _age_g=4 then age2=4; /*45 to 54*/
else if _age_g=5 then age2=5; /*55 to 64*/
else if _age_g=6 then age2=6; /*65+*/

/*3 category employment*/ 
if employ1=. then employ=.;
else if employ1 in (1,2) then employ=1; /*employed or self-employed*/ /*ref*/
else if employ1 in (3,4,5,6,8) then employ=2; /*out of work, homemaker, student*/
else if employ1 =7 then employ=3; /*retired*/

/*income*/
if INCOME3 in (99,77, .) then income=.; /*missing*/
else if INCOME3 in (1,2,3,4) then income=1; /*<25*/
else if INCOME3 in (5,6) then income=2; /*25-50*/
else if INCOME3=7 then income=3; /*50-75*/
else if INCOME3 in (8,9,10,11) then income=4; /*highest income 75,000+ is the ref*/

/*education*/
if _educag=. then education=.;
else if _educag=1 then education=1; /*<HS*/
else if _educag=2 then education=2; /*HS*/
else if _educag=3 then education=3; /*attended collage*/
else if _educag=4 then education=4; /*graduated from college*/ 

/*Health Insurance*/
if _HLTHPLN in (7,9, .) then insurance=.; 
else if _HLTHPLN=1 then insurance=1;    /*has insurance*/
else if _HLTHPLN=2 then insurance=2;    /*no insurance*/

/*depression*/
if ADDEPEV3 in (7,9, .) then depression=.;
else if ADDEPEV3=1 then depression=2; *has depression;
else if  ADDEPEV3=2 then depression=1; *no depression;

/*routine checkup*/
if CHECKUP1=1 then checkyear=1; *checkup in the past 2 years;
else checkyear=2;

/*General health*/
if GENHLTH=1 or GENHLTH=2 or GENHLTH=3 then GOODHLTH=1;
else GOODHLTH=2;


/*diabetes*/
if DIABETE4=1 then diabetes=2; else diabetes=1;
/*arthritis*/
if HAVARTH4 ne . then do;
if HAVARTH4=1 then arthritis=2; else arthritis=1;end;
/*COPD*/
if CHCCOPD3=1 then COPD=2; else COPD=1;

/*chronic disease*/
if DIABETE4=1 or HAVARTH4=1 or CHCCOPD3=1 then Chrodis=2;
else Chrodis=1;
run;

*Test run without weighting variables- ensuring no errors in recode;
proc freq data=BRFSS_clean;
tables diabetes arthritis COPD chrodis/norow nocol nopercent;
format diabetes nyf. arthritis nyf. COPD nyf. chrodis nyf.;
*where IYear="2021";
run;

*testing if variable recoded appropriately;
proc surveyfreq data=BRFSS_clean;
strata _ststr;
cluster _psu;
weight _llcpwt; 
tables GoodHLTH;
format GoodHLTH nyf.;
run;


*running for rates/percents;
proc surveyfreq data=All_BRFSS;
strata _ststr;
cluster _psu;
weight _llcpwt; 
tables HLTH_DIST_NAME*sex*age*GENHLTH/row nofreq cl nostd;
format GENHLTH GENHLTH. sex sex. race race. age age.;
run;
ods tagsets.excelxp close;

*Can run cross-tab and run chi-square test;
*Cross-tab;
proc surveyfreq data=brfss_clean;
strata _ststr;
cluster _psu;
weight _llcpwt;  
tables HLTH_DIST_NAME*chrodis*Depression/row cl chisq;
format chrodis nyf. Depression nyf.;
run;
*Can run cross-tab and run chi-square test;
*Cross-tab;
proc surveyfreq data=brfss_clean;
strata _ststr;
cluster _psu;
weight _llcpwt; 
tables HLTH_DIST_NAME*CHECKyear /row cl chisq;
run;

*Can run cross-tab and run chi-square test;
*Cross-tab;
proc surveyfreq data=brfss_clean;
strata _ststr;
cluster _psu;
weight _llcpwt; 
tables HLTH_DIST_NAME*GoodHLTH /row cl chisq;
run;


/*Example logistic regression- bivariate*/
proc surveylogistic data=brfss_clean;
strata _ststr;
cluster _psu;
weight _llcpwt; 
class Depression (ref='1') Chrodis (ref='1') HLTH_DIST_NAME (ref='Richmond City') race (ref='1') sex (ref='1') income (ref='4') education (ref='4')/param=ref;
model  Depression = chrodis /clodds expb rsquare;
output out=preds  p=phat;
run;
/*Example logistic regression- multivariate*/
proc surveylogistic data=brfss_clean;
strata _ststr;
cluster _psu;
weight _llcpwt; 
class Depression (ref='1') Chrodis (ref='1') HLTH_DIST_NAME (ref='Richmond City') race (ref='1') sex (ref='1') income (ref='4') education (ref='4') insurance (ref='1')/param=ref;
model Depression= chrodis HLTH_DIST_NAME race sex income education insurance /clodds expb rsquare;
output out=preds  p=phat;
run;

*****end code;

