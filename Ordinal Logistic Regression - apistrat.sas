* Import the apistrat data provided with R's survey package;
	PROC IMPORT DATAFILE='/home/u43009382/Software Comparisons/apistrat_data.sav'
		REPLACE
		DBMS=SAV
		OUT=WORK.apistrat;
	RUN;
	
* Run an ordinal logistic regression;
	PROC SURVEYLOGISTIC DATA=work.apistrat varmethod=taylor;
		WEIGHT pw;
		STRATUM School_Type;
		CLASS Parent_Col_Grad_Category School_Type (REF='E' DESC) School_Type (ORDER = INTERNAL) / PARAM=REFERENCE;
		MODEL Parent_Col_Grad_Category (order = INTERNAL) = School_Type / TECHNIQUE=FISHER CLPARM;
	RUN;