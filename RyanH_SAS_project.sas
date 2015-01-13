/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 3:29:03 PM
PROJECT: RyanH_SAS_project_011315
PROJECT PATH: P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp
---------------------------------------- */

/* Library assignment for Local.MEPSFLYR */
Libname MEPSFLYR V9 'P:\QAC\qac200\students\hryan' ;
/* Library assignment for Local.MEPSFLYR */
Libname MEPSFLYR V9 'P:\QAC\qac200\students\hryan' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MEPSFLYR)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MEPSFLYR)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MEPSFLYR  "P:\QAC\qac200\students\hryan" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "C:\Users\hryan\Desktop";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:36:05 AM
   By task: Data Set Attributes

   Input Data: C:\Users\hryan\Desktop\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MEPSFLYR.MEPS_FULLYR_2012_SUBSET);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA MEPSFLYR.MEPS_FULLYR_2012_SUBSET(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM MEPSFLYR.MEPS_FULLYR_2012_SUBSET
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=MEPSFLYR.MEPS_FULLYR_2012_SUBSET OUT=MEPSFLYR.MEPS_FULLYR_2012_SUBSET;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM MEPSFLYR.MEPS_FULLYR_2012_SUBSET
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Relevant Variables/18+   */
%LET _CLIENTTASKLABEL='Relevant Variables/18+';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MEPSFLYR.MEPS_FULLYR_2012_subset_010715);

PROC SQL;
   CREATE TABLE MEPSFLYR.MEPS_FULLYR_2012_subset_010715(label="MEPS_FULLYR_2012_subset_010715") AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Data Set Attributes1   */
%LET SYSLAST=MEPSFLYR.MEPS_FULLYR_2012_SUBSET_010715;
%LET _CLIENTTASKLABEL='Code For Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\hryan\Code For Data Set Attributes1.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 9:52:39 AM
   By task: Data Set Attributes1

   Input Data: Local:MEPSFLYR.MEPS_FULLYR_2012_SUBSET_010715
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MEPSFLYR.CONTCONTENTSFORMEPS_FULLYR_2012_);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";


PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MEPSFLYR.MEPS_FULLYR_2012_SUBSET_010715 ;

RUN;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 Adult MEPS Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 Adult MEPS Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:28 PM
   By task: One-Way Frequencies for 2012 Adult MEPS Subset

   Input Data: Local:MEPSFLYR.MEPS_FULLYR_2012_SUBSET_010715
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MEPSFLYR.MEPS_FULLYR_2012_SUBSET_010715
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42
		     , T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42
		     , T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADUPRO42, T.ADWRTH42, T.AGE12X, T.AMAEXP12, T.AMCEXP12, T.AMCHIR12, T.AMDRC12
		     , T.AMEEXP12, T.AMNEXP12, T.AMNURS12, T.AMOPTO12, T.AMTEXP12, T.AMTHER12, T.AMTOTC12, T.ANGIAGED, T.ANGIDX, T.ARTHAGED, T.ARTHDX, T.ARTHTYPE, T.ASATAK31, T.ASTHAGED, T.ASTHDX, T.BPMLDX, T.BUSNP12X, T.CANCERDX, T.CHDDX
		     , T.CHLDP12X, T.CHOLDX, T.CLMDEP12, T.CLMHIP12, T.DIABDX, T.DIVDP12X, T.DNTINS12, T.DNUNAB42, T.DSDIA53, T.DSFL1353, T.DSFLNV53, T.EDUYRDEG, T.EMPHDX, T.ERDEXP12, T.ERTOT12, T.FAMINC12, T.FOODST12, T.HHTOTD12, T.HIBPDX, T.INS12X
		     , T.IPDIS12, T.IPNGTD12, T.MARRY12X, T.MCARE12, T.MCREV12, T.MIDX, T.OBVEXP12, T.OBVTCH12, T.OPDEXP12, T.OPOEXP12, T.OPPEXP12, T.OPSEXP12, T.OPVEXP12, T.PRIDK42, T.PRIEU12, T.PROVTY42, T.RACETHX, T.REGION12, T.SAQELIG
		     , T.SAQWT12F, T.SEX, T.SFFLAG42, T.SSIP12X, T.STRKDX, T.UNINS12, T.WAGEP12X
	FROM MEPSFLYR.MEPS_FULLYR_2012_SUBSET_010715(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 Adult MEPS Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADUPRO42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES AMAEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMCEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMCHIR12 / MISSPRINT  SCORES=TABLE;
	TABLES AMDRC12 / MISSPRINT  SCORES=TABLE;
	TABLES AMEEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMNEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMNURS12 / MISSPRINT  SCORES=TABLE;
	TABLES AMOPTO12 / MISSPRINT  SCORES=TABLE;
	TABLES AMTEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMTHER12 / MISSPRINT  SCORES=TABLE;
	TABLES AMTOTC12 / MISSPRINT  SCORES=TABLE;
	TABLES ANGIAGED / MISSPRINT  SCORES=TABLE;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHAGED / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHTYPE / MISSPRINT  SCORES=TABLE;
	TABLES ASATAK31 / MISSPRINT  SCORES=TABLE;
	TABLES ASTHAGED / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES BPMLDX / MISSPRINT  SCORES=TABLE;
	TABLES BUSNP12X / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES CHDDX / MISSPRINT  SCORES=TABLE;
	TABLES CHLDP12X / MISSPRINT  SCORES=TABLE;
	TABLES CHOLDX / MISSPRINT  SCORES=TABLE;
	TABLES CLMDEP12 / MISSPRINT  SCORES=TABLE;
	TABLES CLMHIP12 / MISSPRINT  SCORES=TABLE;
	TABLES DIABDX / MISSPRINT  SCORES=TABLE;
	TABLES DIVDP12X / MISSPRINT  SCORES=TABLE;
	TABLES DNTINS12 / MISSPRINT  SCORES=TABLE;
	TABLES DNUNAB42 / MISSPRINT  SCORES=TABLE;
	TABLES DSDIA53 / MISSPRINT  SCORES=TABLE;
	TABLES DSFL1353 / MISSPRINT  SCORES=TABLE;
	TABLES DSFLNV53 / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES EMPHDX / MISSPRINT  SCORES=TABLE;
	TABLES ERDEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES FOODST12 / MISSPRINT  SCORES=TABLE;
	TABLES HHTOTD12 / MISSPRINT  SCORES=TABLE;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE;
	TABLES INS12X / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES IPNGTD12 / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12 / MISSPRINT  SCORES=TABLE;
	TABLES MCREV12 / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES OBVEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OBVTCH12 / MISSPRINT  SCORES=TABLE;
	TABLES OPDEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OPOEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OPPEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OPSEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OPVEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIDK42 / MISSPRINT  SCORES=TABLE;
	TABLES PRIEU12 / MISSPRINT  SCORES=TABLE;
	TABLES PROVTY42 / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES SAQWT12F / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES SSIP12X / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES UNINS12 / MISSPRINT  SCORES=TABLE;
	TABLES WAGEP12X / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MEPSFLYR.SUBSET_MEPS_FULLYR_2012_MANAGED);

PROC SQL;
   CREATE TABLE MEPSFLYR.SUBSET_MEPS_FULLYR_2012_MANAGED(label="SUBSET_MEPS_FULLYR_2012_MANAGED") AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          /* Health_in_General */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health in General last 12 months (recoded missing)" AS Health_in_General, 
          /* Health_Limits_MOD_Activities */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Health Limits MOD Activities last year (recoded missing)" AS Health_Limits_MOD_Activities, 
          /* Health_Limits_Climbing_Stairs */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health Limits Climbing Stairs last 12 months (recoded missing)" AS Health_Limits_Climbing_Stairs, 
          /* ACCMP_LESS_B/C_PHY_PRBS  */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="ACCMP LESS B/C PHY PRBS last 12 months (recoded missing)" AS 'ACCMP_LESS_B/C_PHY_PRBS 'n, 
          /* Work_Limit_bc_Phys_Prob */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Work Limit b/c Phys Prob last 12 months (recoded missing)" AS Work_Limit_bc_Phys_Prob, 
          /* ACCMP_LESS_B/C_MNT_PRBS */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="ACCMP LESS B/C MNT PRBS last 12 months (recoded missing)" AS 'ACCMP_LESS_B/C_MNT_PRBS'n, 
          /* WORK_LIMT_BC_MNT_PROBS */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="WORK LIMT B/C MNT PROBS last 12 months (recoded missing)" AS WORK_LIMT_BC_MNT_PROBS, 
          /* Pain_Limits_Normal_Work */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Pain Limits Normal Work last 12 months (recoded missing)" AS Pain_Limits_Normal_Work, 
          /* Felt_Calm_Peaceful */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt Calm/Peaceful last 12 months (recoded missing)" AS Felt_Calm_Peaceful, 
          /* Had_lot_of_Energy */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had a lot of Energy last 12 months (recoded missing)" AS Had_lot_of_Energy, 
          /* Felt_Downhearted_Depressed */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt Downhearted/Depressed last 12 months (recoded misssing)" AS Felt_Downhearted_Depressed, 
          /* Health_Stopped_Social_Activities */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health Stopped Social Activities last 12 months (recoded missing)" AS 
            Health_Stopped_Social_Activities, 
          /* Marital_Status */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital Status as of 12/31/12 (recoded missing)" AS Marital_Status, 
          /* DR_Gave_Specific_Instructions */
            (CASE 
               WHEN -1 = t1.ADINST42 THEN .
               WHEN -8 = t1.ADINST42 THEN .
               WHEN -9 = t1.ADINST42 THEN .
               ELSE t1.ADINST42
            END) LABEL="DR Gave Specific Instructions last 12 months (recoded missing)" AS DR_Gave_Specific_Instructions, 
          /* DR_Gave_Instr_EZ_Undrstd */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="DR Gave Instructions Easily Understood last 12 months (recoded missing)" AS 
            DR_Gave_Instr_EZ_Undrstd, 
          /* DR_ASKED_R_DESC_HOW_FOLLOW */
            (CASE 
               WHEN -1 = t1.ADTLHW42 THEN .
               WHEN -8 = t1.ADTLHW42 THEN .
               WHEN -9 = t1.ADTLHW42 THEN .
               ELSE t1.ADTLHW42
            END) LABEL="DR ASKED R DESC HOW FOLLOW last 12 months (recoded missing)" AS DR_ASKED_R_DESC_HOW_FOLLOW, 
          /* HAD_TO_FILL_OUT_FORMS  */
            (CASE 
               WHEN -1 = t1.ADFFRM42 THEN .
               WHEN -8 = t1.ADFFRM42 THEN .
               WHEN -9 = t1.ADFFRM42 THEN .
               ELSE t1.ADFFRM42
            END) LABEL="HAD TO FILL OUT/SIGN FORMS last 12 months (recoded missing)" AS 'HAD_TO_FILL_OUT_FORMS 'n, 
          /* OFFRD_HELP_FILLING_OUT_FORMS */
            (CASE 
               WHEN -1 = t1.ADFHLP42 THEN .
               WHEN -8 = t1.ADFHLP42 THEN .
               WHEN -9 = t1.ADFHLP42 THEN .
               ELSE t1.ADFHLP42
            END) LABEL="OFFRD HELP FILLING OUT FORMS last 12 months (recoded missing)" AS OFFRD_HELP_FILLING_OUT_FORMS, 
          /* RATING_OF_HEALTH_CARE */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="RATING OF HEALTH CARE last 12 months (recoded missing)" AS RATING_OF_HEALTH_CARE, 
          /* CURRENTLY_SMOKE */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="CURRENTLY SMOKE (recoded missing)" AS CURRENTLY_SMOKE, 
          /* DR_ADVISED_TO_QUIT_SMOKING */
            (CASE 
               WHEN -1 = t1.ADNSMK42 THEN .
               WHEN -9 = t1.ADNSMK42 THEN .
               ELSE t1.ADNSMK42
            END) LABEL="DR ADVISED TO QUIT SMOKING last 12 months (recoded missing)" AS DR_ADVISED_TO_QUIT_SMOKING, 
          /* DR_CHECKED_BLOOD_PRESSURE */
            (CASE 
               WHEN -1 = t1.ADDRBP42 THEN .
               WHEN -8 = t1.ADDRBP42 THEN .
               WHEN -9 = t1.ADDRBP42 THEN .
               ELSE t1.ADDRBP42
            END) LABEL="DR CHECKED BLOOD PRESSURE last 2 years (recoded missing)" AS DR_CHECKED_BLOOD_PRESSURE, 
          /* NEEDED_TO_SEE_SPECIALIST */
            (CASE 
               WHEN -1 = t1.ADSPEC42 THEN .
               WHEN -9 = t1.ADSPEC42 THEN .
               ELSE t1.ADSPEC42
            END) LABEL="NEEDED TO SEE SPECIALIST last 12 months (recoded missing)" AS NEEDED_TO_SEE_SPECIALIST, 
          /* YR_EDUCATN_OR_HIGHST_DEGREE */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="YEAR OF EDUCATION OR HIGHEST DEGREE (recoded missing)" AS YR_EDUCATN_OR_HIGHST_DEGREE, 
          /* DID_NYONE_BUY_FOOD_STMPS */
            (CASE 
               WHEN -7 = t1.FOODST12 THEN .
               WHEN -8 = t1.FOODST12 THEN .
               ELSE t1.FOODST12
            END) LABEL="DID ANYONE PURCHASE FOOD STAMPS (recoded missing)" AS DID_NYONE_BUY_FOOD_STMPS, 
          /* ARTHRITIS_DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="ARTHRITIS DIAGNOSIS (recoded missing)" AS ARTHRITIS_DIAGNOSIS, 
          /* CANCER_DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="CANCER DIAGNOSIS (recoded missing)" AS CANCER_DIAGNOSIS, 
          /* CORONARY_HRT_DISEASE_DIAG */
            (CASE 
               WHEN -7 = t1.CHDDX THEN .
               WHEN -8 = t1.CHDDX THEN .
               WHEN -9 = t1.CHDDX THEN .
               ELSE t1.CHDDX
            END) LABEL="CORONARY HRT DISEASE DIAG (recoded missing)" AS CORONARY_HRT_DISEASE_DIAG, 
          /* DID_WILL_DEDUCT_HLTH_INSUR_PREM */
            (CASE 
               WHEN -1 = t1.CLMHIP12 THEN .
               WHEN -7 = t1.CLMHIP12 THEN .
               WHEN -8 = t1.CLMHIP12 THEN .
               WHEN -9 = t1.CLMHIP12 THEN .
               ELSE t1.CLMHIP12
            END) LABEL="DID/WILL PERS DEDUCT HEALTH INSUR. PREM (recoded missing)" AS DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          /* HIGH_BLOOD_PRESSURE_DIAG */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="HIGH BLOOD PRESSURE DIAG (recoded missing)" AS HIGH_BLOOD_PRESSURE_DIAG, 
          /* DIABETES_DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.DIABDX THEN .
               WHEN -8 = t1.DIABDX THEN .
               WHEN -9 = t1.DIABDX THEN .
               ELSE t1.DIABDX
            END) LABEL="DIABETES DIAGNOSIS (recoded missing)" AS DIABETES_DIAGNOSIS, 
          /* COV_BY_PRIV_INS */
            (CASE 
               WHEN -1 = t1.PRIDK42 THEN .
               ELSE t1.PRIDK42
            END) LABEL="COV BY PRIV INS (DK PLAN) (recoded missing)" AS COV_BY_PRIV_INS, 
          /* ASTHMA_DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.ASTHDX THEN .
               WHEN -8 = t1.ASTHDX THEN .
               WHEN -9 = t1.ASTHDX THEN .
               ELSE t1.ASTHDX
            END) LABEL="ASTHMA DIAGNOSIS (recoded missing)" AS ASTHMA_DIAGNOSIS, 
          /* PROVIDER_TYPE */
            (CASE 
               WHEN -1 = t1.PROVTY42 THEN .
               ELSE t1.PROVTY42
            END) LABEL="PROVIDER TYPE (recoded missing)" AS PROVIDER_TYPE, 
          /* STROKE_DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="STROKE DIAGNOSIS (recoded missing)" AS STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X
      FROM MEPSFLYR.MEPS_FULLYR_2012_SUBSET_010715 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Code SF-12 Variables   */
%LET _CLIENTTASKLABEL='Reverse Code SF-12 Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SF12_REVCODE);

PROC SQL;
   CREATE TABLE WORK.MEPS_FULLYR_2012_SF12_REVCODE(label="MEPS_FULLYR_2012_SF12_REVCODE") AS 
   SELECT t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General AS Health_in_General1, 
          t1.Health_Limits_MOD_Activities AS Health_Limits_MOD_Activities1, 
          t1.Health_Limits_Climbing_Stairs AS Health_Limits_Climbing_Stairs1, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n AS 'ACCMP_LESS_B/C_PHY_PRBS1'n, 
          t1.Work_Limit_bc_Phys_Prob AS Work_Limit_bc_Phys_Prob1, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n AS 'ACCMP_LESS_B/C_MNT_PRBS1'n, 
          t1.WORK_LIMT_BC_MNT_PROBS AS WORK_LIMT_BC_MNT_PROBS1, 
          t1.Pain_Limits_Normal_Work AS Pain_Limits_Normal_Work1, 
          t1.Felt_Calm_Peaceful AS Felt_Calm_Peaceful1, 
          t1.Had_lot_of_Energy AS Had_lot_of_Energy1, 
          t1.Felt_Downhearted_Depressed AS Felt_Downhearted_Depressed1, 
          t1.Health_Stopped_Social_Activities AS Health_Stopped_Social_Activitie1, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          /* Health_in_General_R */
            (6-t1.Health_in_General) LABEL="Health in General (reverse coded)" AS Health_in_General_R, 
          /* Pain_Limits_Normal_Work_R */
            (6-t1.Pain_Limits_Normal_Work) LABEL="Pain Limits Normal Work (reverse coded)" AS Pain_Limits_Normal_Work_R, 
          /* Felt_Calm_Peaceful_R */
            (6-t1.Felt_Calm_Peaceful) LABEL="Felt Calm or Peaceful (reverse coded)" AS Felt_Calm_Peaceful_R, 
          /* Had_lot_of_Energy_R */
            (6-t1.Had_lot_of_Energy) LABEL="Had a lot of Energy (reverse coded)" AS Had_lot_of_Energy_R
      FROM MEPSFLYR.SUBSET_MEPS_FULLYR_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Create Aggregate SF-12   */
%LET _CLIENTTASKLABEL='Create Aggregate SF-12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SF12_SUM);

PROC SQL;
   CREATE TABLE WORK.MEPS_FULLYR_2012_SF12_SUM(label="MEPS_FULLYR_2012_SF12_SUM") AS 
   SELECT t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General1, 
          t1.Health_Limits_MOD_Activities1, 
          t1.Health_Limits_Climbing_Stairs1, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS1'n, 
          t1.Work_Limit_bc_Phys_Prob1, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS1'n, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.Pain_Limits_Normal_Work1, 
          t1.Felt_Calm_Peaceful1, 
          t1.Had_lot_of_Energy1, 
          t1.Felt_Downhearted_Depressed1, 
          t1.Health_Stopped_Social_Activitie1, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Health_in_General_R, 
          t1.Pain_Limits_Normal_Work_R, 
          t1.Felt_Calm_Peaceful_R, 
          t1.Had_lot_of_Energy_R, 
          /* SUM_SF12 */
            (SUM(t1.Health_in_General_R,t1.Health_Limits_MOD_Activities1,t1.Health_Limits_Climbing_Stairs1,t1.
            'ACCMP_LESS_B/C_PHY_PRBS1'n,t1.Work_Limit_bc_Phys_Prob1,t1.'ACCMP_LESS_B/C_MNT_PRBS1'n
            ,t1.WORK_LIMT_BC_MNT_PROBS1,t1.Pain_Limits_Normal_Work_R,t1.Felt_Calm_Peaceful_R,t1.Had_lot_of_Energy_R,t1.Felt_Downhearted_Depressed1,t1.Health_Stopped_Social_Activitie1)) 
            LABEL="Sum of SF-12 variables" AS SUM_SF12
      FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:28 PM
   By task: List Data

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Health_in_General_R, T.Health_Limits_MOD_Activities1, T.Health_Limits_Climbing_Stairs1, T."ACCMP_LESS_B/C_PHY_PRBS1"n, T.Work_Limit_bc_Phys_Prob1, T."ACCMP_LESS_B/C_MNT_PRBS1"n, T.WORK_LIMT_BC_MNT_PROBS1
		     , T.Pain_Limits_Normal_Work_R, T.Felt_Calm_Peaceful_R, T.Had_lot_of_Energy_R, T.Felt_Downhearted_Depressed1, T.Health_Stopped_Social_Activitie1, T.SUM_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_SUM as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variable Coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR Health_in_General_R Health_Limits_MOD_Activities1 Health_Limits_Climbing_Stairs1 "ACCMP_LESS_B/C_PHY_PRBS1"n Work_Limit_bc_Phys_Prob1 "ACCMP_LESS_B/C_MNT_PRBS1"n WORK_LIMT_BC_MNT_PROBS1 Pain_Limits_Normal_Work_R Felt_Calm_Peaceful_R
	  Had_lot_of_Energy_R Felt_Downhearted_Depressed1 Health_Stopped_Social_Activitie1 SUM_SF12;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Aggregated SF-12   */
%LET _CLIENTTASKLABEL='Summary Statistics for Aggregated SF-12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:28 PM
   By task: Summary Statistics for Aggregated SF-12

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_SUM(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 MEPS Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_SF12;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Aggregate SF-12   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate SF-12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:28 PM
   By task: Distribution Analysis for Aggregate SF-12

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_SUM(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: SUM_SF12 Aggregate Overall Health Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_SF12;
	HISTOGRAM   SUM_SF12 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Create Categorical Variable SF12   */
%LET _CLIENTTASKLABEL='Create Categorical Variable SF12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ AS 
   SELECT /* SUM_SF12_CATEGORICAL */
            (CASE  
               WHEN t1.SUM_SF12 >=2 and t1.SUM_SF12 <=41
               THEN 1
               WHEN t1.SUM_SF12 >41 and t1.SUM_SF12 <=48
               THEN 2
                WHEN t1.SUM_SF12 >48 and t1.SUM_SF12 <=52
               THEN 3
                ELSE 4
            END) LABEL="SF-12 aggregate categorical by quartiles" AS SUM_SF12_CATEGORICAL, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General1, 
          t1.Health_Limits_MOD_Activities1, 
          t1.Health_Limits_Climbing_Stairs1, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS1'n, 
          t1.Work_Limit_bc_Phys_Prob1, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS1'n, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.Pain_Limits_Normal_Work1, 
          t1.Felt_Calm_Peaceful1, 
          t1.Had_lot_of_Energy1, 
          t1.Felt_Downhearted_Depressed1, 
          t1.Health_Stopped_Social_Activitie1, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Health_in_General_R, 
          t1.Pain_Limits_Normal_Work_R, 
          t1.Felt_Calm_Peaceful_R, 
          t1.Had_lot_of_Energy_R, 
          t1.SUM_SF12
      FROM WORK.MEPS_FULLYR_2012_SF12_SUM t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:28 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_SF12, T.SUM_SF12_CATEGORICAL
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_SF12 /  SCORES=TABLE;
	TABLES SUM_SF12_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MEPSFLYR.MEPS_FULLYR_2012_SF12);

PROC SQL;
   CREATE TABLE MEPSFLYR.MEPS_FULLYR_2012_SF12(label="MEPS_FULLYR_2012_SF12") AS 
   SELECT t1.SUM_SF12_CATEGORICAL, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General1, 
          t1.Health_Limits_MOD_Activities1, 
          t1.Health_Limits_Climbing_Stairs1, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS1'n, 
          t1.Work_Limit_bc_Phys_Prob1, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS1'n, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.Pain_Limits_Normal_Work1, 
          t1.Felt_Calm_Peaceful1, 
          t1.Had_lot_of_Energy1, 
          t1.Felt_Downhearted_Depressed1, 
          t1.Health_Stopped_Social_Activitie1, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Health_in_General_R, 
          t1.Pain_Limits_Normal_Work_R, 
          t1.Felt_Calm_Peaceful_R, 
          t1.Had_lot_of_Energy_R, 
          t1.SUM_SF12, 
          /* INFULLYR */
            (1) AS INFULLYR
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Full Outer Join MEPS FullYr and ER   */
%LET _CLIENTTASKLABEL='Full Outer Join MEPS FullYr and ER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MEPSFLYR.MEPS_FULLYR_AND_ER_2012);

PROC SQL;
   CREATE TABLE MEPSFLYR.MEPS_FULLYR_AND_ER_2012(label="MEPS_FULLYR_AND_ER_2012") AS 
   SELECT t1.SUM_SF12_CATEGORICAL, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General1, 
          t1.Health_Limits_MOD_Activities1, 
          t1.Health_Limits_Climbing_Stairs1, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS1'n, 
          t1.Work_Limit_bc_Phys_Prob1, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS1'n, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.Pain_Limits_Normal_Work1, 
          t1.Felt_Calm_Peaceful1, 
          t1.Had_lot_of_Energy1, 
          t1.Felt_Downhearted_Depressed1, 
          t1.Health_Stopped_Social_Activitie1, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Health_in_General_R, 
          t1.Pain_Limits_Normal_Work_R, 
          t1.Felt_Calm_Peaceful_R, 
          t1.Had_lot_of_Energy_R, 
          t1.SUM_SF12
      FROM MEPSFLYR.MEPS_FULLYR_2012_SF12 t1
           FULL JOIN MEPSFLYR.MEPS_ER_2012 t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t2.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data2   */
%LET _CLIENTTASKLABEL='List Data2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:29 PM
   By task: List Data2

   Input Data: Local:MEPSFLYR.MEPS_FULLYR_AND_ER_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MEPSFLYR.MEPS_FULLYR_AND_ER_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID, T.DUPERSID1, T.INER
	FROM MEPSFLYR.MEPS_FULLYR_AND_ER_2012 as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID DUPERSID1 INER;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes1   */
%LET _CLIENTTASKLABEL='Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:29 PM
   By task: Data Set Attributes1

   Input Data: Local:MEPSFLYR.MEPS_FULLYR_AND_ER_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.MEPS_2012_FULLYR_AND_ER);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MEPSFLYR.MEPS_FULLYR_AND_ER_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.MEPS_2012_FULLYR_AND_ER(LABEL="Contents Details for MEPS_FULLYR_AND_ER_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.MEPS_2012_FULLYR_AND_ER
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_AND_ER_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.MEPS_2012_FULLYR_AND_ER OUT=WORK.MEPS_2012_FULLYR_AND_ER;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.MEPS_2012_FULLYR_AND_ER
		WHERE memname='MEPS_FULLYR_AND_ER_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode MRI and X-Ray Variables   */
%LET _CLIENTTASKLABEL='Recode MRI and X-Ray Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_AND_ER);

PROC SQL;
   CREATE TABLE WORK.MEPS_FULLYR_AND_ER(label="MEPS_FULLYR_AND_ER") AS 
   SELECT /* XRAYS_R */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
               ELSE t1.XRAYS
            END) LABEL="This Visit Did P Have X-Rays (recoded)" AS XRAYS_R, 
          /* MRI_R */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="This Visit Did P Have an MRI/CATSCAN (recoded)" AS MRI_R, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General1, 
          t1.Health_Limits_MOD_Activities1, 
          t1.Health_Limits_Climbing_Stairs1, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS1'n, 
          t1.Work_Limit_bc_Phys_Prob1, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS1'n, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.Pain_Limits_Normal_Work1, 
          t1.Felt_Calm_Peaceful1, 
          t1.Had_lot_of_Energy1, 
          t1.Felt_Downhearted_Depressed1, 
          t1.Health_Stopped_Social_Activitie1, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Health_in_General_R, 
          t1.Pain_Limits_Normal_Work_R, 
          t1.Felt_Calm_Peaceful_R, 
          t1.Had_lot_of_Energy_R, 
          t1.SUM_SF12, 
          t1.SUM_SF12_CATEGORICAL
      FROM MEPSFLYR.MEPS_FULLYR_AND_ER_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for MRI   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:29 PM
   By task: One-Way Frequencies for MRI

   Input Data: Local:WORK.MEPS_FULLYR_AND_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_AND_ER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MRI_R
	FROM WORK.MEPS_FULLYR_AND_ER as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_R / NOCUM  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for X-Rays   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for X-Rays';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:29 PM
   By task: One-Way Frequencies for X-Rays

   Input Data: Local:WORK.MEPS_FULLYR_AND_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_AND_ER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS_R
	FROM WORK.MEPS_FULLYR_AND_ER as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYS_R / NOCUM  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Create DUPERSID Count Variable   */
%LET _CLIENTTASKLABEL='Create DUPERSID Count Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_AND_E_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_AND_E_0000 AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.MEPS_FULLYR_AND_ER t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Append Table   */
%LET _CLIENTTASKLABEL='Append Table';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER);
PROC SQL;
CREATE TABLE MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER AS 
SELECT * FROM WORK.MEPS_FULLYR_AND_ER
 OUTER UNION CORR 
SELECT * FROM WORK.QUERY_FOR_MEPS_FULLYR_AND_E_0000
;
Quit;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:30 PM
   By task: Distribution Analysis

   Input Data: Local:MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:30 PM
   By task: One-Way Frequencies2

   Input Data: Local:MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID
	FROM MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Create Count of DUPERSID Categorical Variable   */
%LET _CLIENTTASKLABEL='Create Count of DUPERSID Categorical Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY AS 
   SELECT /* COUNT_of_DUPERSID_CATEGORICAL */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID=1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID>1
               THEN 2
            END) LABEL="Count of DUPERSID Categorical" AS COUNT_of_DUPERSID_CATEGORICAL, 
          t1.XRAYS_R, 
          t1.MRI_R, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General1, 
          t1.Health_Limits_MOD_Activities1, 
          t1.Health_Limits_Climbing_Stairs1, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS1'n, 
          t1.Work_Limit_bc_Phys_Prob1, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS1'n, 
          t1.WORK_LIMT_BC_MNT_PROBS1, 
          t1.Pain_Limits_Normal_Work1, 
          t1.Felt_Calm_Peaceful1, 
          t1.Had_lot_of_Energy1, 
          t1.Felt_Downhearted_Depressed1, 
          t1.Health_Stopped_Social_Activitie1, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Health_in_General_R, 
          t1.Pain_Limits_Normal_Work_R, 
          t1.Felt_Calm_Peaceful_R, 
          t1.Had_lot_of_Energy_R, 
          t1.SUM_SF12, 
          t1.SUM_SF12_CATEGORICAL, 
          t1.COUNT_of_DUPERSID
      FROM MEPSFLYR.SUBSET_MEPS_2012_FULLYR_ER t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:30 PM
   By task: Table Analysis2

   Input Data: Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID, T.COUNT_of_DUPERSID_CATEGORICAL
	FROM WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID_CATEGORICAL * COUNT_of_DUPERSID /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:30 PM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID_CATEGORICAL
	FROM WORK.QUERY_FOR_SUBSET_MEPS_2012_FULLY as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Doctor Variables   */
%LET _CLIENTTASKLABEL='Recode Doctor Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_DR_RECODED);

PROC SQL;
   CREATE TABLE WORK.MEPS_FULLYR_2012_DR_RECODED(label="MEPS_FULLYR_2012_DR_RECODED") AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          /* DR_GAVE_SPECIFIC_INSTRUCTIONS_R */
            (CASE 
               WHEN -1 = t1.ADINST42 THEN .
               WHEN 2 = t1.ADINST42 THEN 0
               WHEN -8 = t1.ADINST42 THEN .
               WHEN -9 = t1.ADINST42 THEN .
               ELSE t1.ADINST42
            END) LABEL="Doctor Gave Specific Instructions (recoded)" AS DR_GAVE_SPECIFIC_INSTRUCTIONS_R, 
          /* DR_LISTENED_TO_YOU_R */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="Doctor Listened to You (recoded)" AS DR_LISTENED_TO_YOU_R, 
          /* DR_EXPLAINED_SO_EASILY_UNDRSTD_R */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="Doctor Explained So Easily Understood (recoded)" AS DR_EXPLAINED_SO_EASILY_UNDRSTD_R, 
          /* DR_SHOWED_RESPECT_R */
            (CASE 
               WHEN -1 = t1.ADRESP42 THEN .
               WHEN -9 = t1.ADRESP42 THEN .
               ELSE t1.ADRESP42
            END) LABEL="Doctor Showed Respect (recoded)" AS DR_SHOWED_RESPECT_R, 
          /* DR_SPENT_ENOUGH_TIME_R */
            (CASE 
               WHEN -1 = t1.ADPRTM42 THEN .
               WHEN -9 = t1.ADPRTM42 THEN .
               ELSE t1.ADPRTM42
            END) LABEL="Doctor Spent Enough Time with You (recoded)" AS DR_SPENT_ENOUGH_TIME_R, 
          /* DR_GIVEN_INSTR_EZ_UNDRSTD_R */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="Doctor Given Instructions Easily Understood (recoded)" AS DR_GIVEN_INSTR_EZ_UNDRSTD_R
      FROM MEPSFLYR.SUBSET_MEPS_FULLYR_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Create Aggregate DR   */
%LET _CLIENTTASKLABEL='Create Aggregate DR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_DR_SUM);

PROC SQL;
   CREATE TABLE WORK.MEPS_FULLYR_2012_DR_SUM(label="MEPS_FULLYR_2012_DR_SUM") AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.DR_GAVE_SPECIFIC_INSTRUCTIONS_R, 
          t1.DR_LISTENED_TO_YOU_R, 
          t1.DR_EXPLAINED_SO_EASILY_UNDRSTD_R, 
          t1.DR_SHOWED_RESPECT_R, 
          t1.DR_SPENT_ENOUGH_TIME_R, 
          t1.DR_GIVEN_INSTR_EZ_UNDRSTD_R, 
          /* DR_SUM */
            
            (SUM(t1.DR_GAVE_SPECIFIC_INSTRUCTIONS_R,t1.DR_LISTENED_TO_YOU_R,t1.DR_EXPLAINED_SO_EASILY_UNDRSTD_R,t1.DR_SHOWED_RESPECT_R,t1.DR_SPENT_ENOUGH_TIME_R,t1.DR_GIVEN_INSTR_EZ_UNDRSTD_R)) 
            LABEL="Aggregate for questions about doctor" AS DR_SUM
      FROM WORK.MEPS_FULLYR_2012_DR_RECODED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data1   */
%LET _CLIENTTASKLABEL='List Data1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:31 PM
   By task: List Data1

   Input Data: Local:WORK.MEPS_FULLYR_2012_DR_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_DR_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_GAVE_SPECIFIC_INSTRUCTIONS_R, T.DR_LISTENED_TO_YOU_R, T.DR_EXPLAINED_SO_EASILY_UNDRSTD_R, T.DR_SHOWED_RESPECT_R, T.DR_SPENT_ENOUGH_TIME_R, T.DR_GIVEN_INSTR_EZ_UNDRSTD_R, T.DR_SUM
	FROM WORK.MEPS_FULLYR_2012_DR_SUM as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR DR_GAVE_SPECIFIC_INSTRUCTIONS_R DR_LISTENED_TO_YOU_R DR_EXPLAINED_SO_EASILY_UNDRSTD_R DR_SHOWED_RESPECT_R DR_SPENT_ENOUGH_TIME_R DR_GIVEN_INSTR_EZ_UNDRSTD_R DR_SUM;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Aggregate DR   */
%LET _CLIENTTASKLABEL='Summary Statistics for Aggregate DR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:31 PM
   By task: Summary Statistics for Aggregate DR

   Input Data: Local:WORK.MEPS_FULLYR_2012_DR_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_DR_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_SUM
	FROM WORK.MEPS_FULLYR_2012_DR_SUM(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR DR_SUM;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Aggregate DR   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate DR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:31 PM
   By task: Distribution Analysis for Aggregate DR

   Input Data: Local:WORK.MEPS_FULLYR_2012_DR_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_DR_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_SUM
	FROM WORK.MEPS_FULLYR_2012_DR_SUM(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: DR_SUM Aggregate Doctor Evaluations";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR DR_SUM;
	HISTOGRAM   DR_SUM / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Create Categorical Variable DR   */
%LET _CLIENTTASKLABEL='Create Categorical Variable DR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_DR_CATEGORICAL);

PROC SQL;
   CREATE TABLE WORK.MEPS_FULLYR_2012_DR_CATEGORICAL(label="MEPS_FULLYR_2012_DR_CATEGORICAL") AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X, 
          t1.DR_GAVE_SPECIFIC_INSTRUCTIONS_R, 
          t1.DR_LISTENED_TO_YOU_R, 
          t1.DR_EXPLAINED_SO_EASILY_UNDRSTD_R, 
          t1.DR_SHOWED_RESPECT_R, 
          t1.DR_SPENT_ENOUGH_TIME_R, 
          t1.DR_GIVEN_INSTR_EZ_UNDRSTD_R, 
          t1.DR_SUM, 
          /* DR_SUM_CATEGORICAL */
            (CASE  
               WHEN t1.DR_SUM>=0 and t1.DR_SUM<=15
               THEN 1
               WHEN t1.DR_SUM>15 and t1.DR_SUM<=18
               THEN 2
               WHEN t1.DR_SUM>18 and t1.DR_SUM <=21
               THEN 3
            END) LABEL="Aggregate Doctor Evaluations Categorical by quartiles" AS DR_SUM_CATEGORICAL
      FROM WORK.MEPS_FULLYR_2012_DR_SUM t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:31 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.MEPS_FULLYR_2012_DR_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_DR_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.DR_SUM, T.DR_SUM_CATEGORICAL
	FROM WORK.MEPS_FULLYR_2012_DR_CATEGORICAL as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES DR_SUM /  SCORES=TABLE;
	TABLES DR_SUM_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Marriage/Education Variables   */
%LET _CLIENTTASKLABEL='Recode Marriage/Education Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_2012_MARRY_EDU_RECODED);

PROC SQL;
   CREATE TABLE WORK.MEPS_2012_MARRY_EDU_RECODED(label="MEPS_2012_MARRY_EDU_RECODED") AS 
   SELECT /* MARITAL_STATUS_R */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital Status (recoded)" AS MARITAL_STATUS_R, 
          /* EDUCATION_R */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="Years of Education or Highest Degree (recoded)" AS EDUCATION_R, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X
      FROM MEPSFLYR.SUBSET_MEPS_FULLYR_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Marriage/Education Categorical   */
%LET _CLIENTTASKLABEL='Marriage/Education Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_2012_MARRY_EDU_CATEGORICAL);

PROC SQL;
   CREATE TABLE WORK.MEPS_2012_MARRY_EDU_CATEGORICAL(label="MEPS_2012_MARRY_EDU_CATEGORICAL") AS 
   SELECT /* MARITAL_STATUS_CATEGORICAL */
            (CASE  
               WHEN t1.MARITAL_STATUS_R>=0 and t1.MARITAL_STATUS_R<=1
               THEN 1
               WHEN t1.MARITAL_STATUS_R>1 and t1.MARITAL_STATUS_R<=4
               THEN 2
               WHEN t1.MARITAL_STATUS_R>4 and t1.MARITAL_STATUS_R<=6
               THEN 3
            END) LABEL="Marital Status categorical by married, no longer married, never married" AS 
            MARITAL_STATUS_CATEGORICAL, 
          /* EDUCATION_CATEGORICAL */
            (CASE  
               WHEN t1.EDUCATION_R>=0 and t1.EDUCATION_R<=12
               THEN 1
               WHEN t1.EDUCATION_R>12 and t1.EDUCATION_R<=15
               THEN 2
               WHEN t1.EDUCATION_R>15 and t1.EDUCATION_R<=18
               THEN 3
               WHEN t1.EDUCATION_R>18 and t1.EDUCATION_R<=21
               THEN 4
            END) LABEL="Years of Education or Highest Degree categorical" AS EDUCATION_CATEGORICAL, 
          t1.MARITAL_STATUS_R, 
          t1.EDUCATION_R, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ANGIAGED, 
          t1.ANGIDX, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASATAK31, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHLDP12X, 
          t1.CHOLDX, 
          t1.CLMDEP12, 
          t1.CLMHIP12, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSDIA53, 
          t1.DSFL1353, 
          t1.DSFLNV53, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERDEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HHTOTD12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MCREV12, 
          t1.MIDX, 
          t1.OBVEXP12, 
          t1.OBVTCH12, 
          t1.OPDEXP12, 
          t1.OPOEXP12, 
          t1.OPPEXP12, 
          t1.OPSEXP12, 
          t1.OPVEXP12, 
          t1.PRIDK42, 
          t1.PRIEU12, 
          t1.RACETHX, 
          t1.SEX, 
          t1.PROVTY42, 
          t1.REGION12, 
          t1.Health_in_General, 
          t1.Health_Limits_MOD_Activities, 
          t1.Health_Limits_Climbing_Stairs, 
          t1.'ACCMP_LESS_B/C_PHY_PRBS'n, 
          t1.Work_Limit_bc_Phys_Prob, 
          t1.'ACCMP_LESS_B/C_MNT_PRBS'n, 
          t1.WORK_LIMT_BC_MNT_PROBS, 
          t1.Pain_Limits_Normal_Work, 
          t1.Felt_Calm_Peaceful, 
          t1.Had_lot_of_Energy, 
          t1.Felt_Downhearted_Depressed, 
          t1.Health_Stopped_Social_Activities, 
          t1.Marital_Status, 
          t1.DR_Gave_Specific_Instructions, 
          t1.DR_Gave_Instr_EZ_Undrstd, 
          t1.DR_ASKED_R_DESC_HOW_FOLLOW, 
          t1.HAD_TO_FILL_OUT_FORMS, 
          t1.OFFRD_HELP_FILLING_OUT_FORMS, 
          t1.RATING_OF_HEALTH_CARE, 
          t1.CURRENTLY_SMOKE, 
          t1.DR_ADVISED_TO_QUIT_SMOKING, 
          t1.DR_CHECKED_BLOOD_PRESSURE, 
          t1.NEEDED_TO_SEE_SPECIALIST, 
          t1.YR_EDUCATN_OR_HIGHST_DEGREE, 
          t1.DID_NYONE_BUY_FOOD_STMPS, 
          t1.ARTHRITIS_DIAGNOSIS, 
          t1.CANCER_DIAGNOSIS, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.DID_WILL_DEDUCT_HLTH_INSUR_PREM, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.DIABETES_DIAGNOSIS, 
          t1.COV_BY_PRIV_INS, 
          t1.ASTHMA_DIAGNOSIS, 
          t1.PROVIDER_TYPE, 
          t1.STROKE_DIAGNOSIS, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X
      FROM WORK.MEPS_2012_MARRY_EDU_RECODED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:31 PM
   By task: Table Analysis

   Input Data: Local:WORK.MEPS_2012_MARRY_EDU_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_2012_MARRY_EDU_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARITAL_STATUS_R, T.MARITAL_STATUS_CATEGORICAL
	FROM WORK.MEPS_2012_MARRY_EDU_CATEGORICAL as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARITAL_STATUS_CATEGORICAL * MARITAL_STATUS_R /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:28:31 PM
   By task: Table Analysis1

   Input Data: Local:WORK.MEPS_2012_MARRY_EDU_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_2012_MARRY_EDU_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUCATION_R, T.EDUCATION_CATEGORICAL
	FROM WORK.MEPS_2012_MARRY_EDU_CATEGORICAL as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for MEPS 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Hannah Ryan";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDUCATION_CATEGORICAL * EDUCATION_R /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
LIBNAME ECLIB000 "C:\Users\hryan\Desktop";


%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
