/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 2:59:54 PM
PROJECT: RyanH_SAS_project_010915
PROJECT PATH: P:\QAC\qac200\students\hryan\RyanH_SAS_project_010915.egp
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

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGRTFX TEMP;
ODS RTF(ID=EGRTFX) FILE=EGRTFX
    ENCODING='utf-8'
    STYLE=Rtf
    NOGTITLE
    NOGFOOTNOTE
;
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MEPSFLYR  "P:\QAC\qac200\students\hryan" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "C:\Users\hryan\Desktop";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:59:22 PM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_010915.egp';

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
      FROM EC100008.meps_fullyr_2012 t1
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_010915.egp';
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:59:23 PM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\hryan\RyanH_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='RyanH_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.SUBSET_MEPS_FULLYR_2012_MANAGED);

PROC SQL;
   CREATE TABLE WORK."SUBSET_MEPS_FULLYR_2012_MANAGED"n AS 
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
