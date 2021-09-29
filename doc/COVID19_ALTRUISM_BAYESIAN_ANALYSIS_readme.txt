This DATSETNAMEreadme.txt file was generated on 2021-08-02 by Stephen Pierzchajlo.

GENERAL INFORMATION

1. Title of Dataset: COVID-19 Altruism Bayesian Analysis

2. Author Information
	A. Principal Investigator Contact Information
		Name: Joana Maria Barbosa Vieira
		Institution: Karolinska Institutet
		Address: 
		Email: 

	B. Associate or Co-investigator Contact Information
		Name: Stephen Pierzchajlo
		Institution: Stockholm University
		Address: 
		Email: 

	C. Alternate Contact Information
		Name: 
		Institution: 
		Address: 
		Email: 

3. Date of data collection: From 2020-03-23 To 2020-04-13.

4. Geographic location of data collection: United States via Qualtrics. Researchers located in Stockholm, Sweden at Karolinska Institutet and Stockholm University.

5. Information about funding sources that supported the collection of the data: 


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: 

2. Links to publications that cite or use the data: 

3. Links to other publicly accessible locations of the data:

6. Recommended citation for this dataset: 


DATA & FILE OVERVIEW

1. File List:

 a. file tree:
Covid-19-Altruism-Bayesian-Analysis
│   	Covid-19-Altruism-Bayesian-Analysis.Rproj
│
├───bin
│       counterfactual_plot.R
│       counterfactual_plot_all.R
│       posterior_plot.R
│       survey_graph.R
│
├───data
│   ├───processed
│   │       cva_bayes.csv
│   │       cva_full.csv
│   │       total_cases_and_deaths.csv
│   │
│   └───raw
│           covid_us_deaths_wide.csv
│           covid_us_wide.csv
│           cva.csv
│
├───doc
│       COVID19_ALTRUISM_BAYESIAN_ANALYSIS_readme.txt
│
├───models
│       covid_bayes_0.96.rds
│       covid_bayes_0.97.rds
│       covid_bayes_0.98.rds
│       covid_bayes_0.99.rds
│       covid_bayes_2nd_0.99.rds
│       covid_bayes_model.rds
│       covid_bayes_model_2_0.99.rds
│       covid_bayes_model_2_0.995.rds
│       covid_bayes_model_2_final.rds
│       m_no_pooled_week1.rds
│       m_no_pooled_week2.rds
│       m_no_pooled_week3.rds
│       m_no_pooled_week4.rds
│       m_pooled.rds
│
├───results
└───src
        Bayes COVID.Rmd

 b. file description
	bin: Functions I created to process the data in R.
	data: Contains both raw and processed data used in the analysis.
	doc: Contains this readme.txt file.
	models: All Bayesian models generated in R.
	results: All figures generated from R script.
	src: R Markdown file. Contains R analysis script.


METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: Data were collected using Qualtrics.

2. Methods for processing the data: Data were collected from Qualtrics, and from a github repository containing US COVID-19 data (file extension in R Markdown script). All data were preprocessed in R.

3. Instrument- or software-specific information needed to interpret the data: Data can be looked at from the R Markdown file. The user needs to download R (which is free) and open the file.

7. People involved with sample collection, processing, analysis and/or submission: Stephen Pierzchajlo and Joana Vieira processed and analysed the data.


DATA-SPECIFIC INFORMATION FOR: covid_us_deaths_wide.csv

1. Number of variables: Variable. The data are updated daily. This .csv file contains the last downloaded example. However, this raw data is preprocessed into a version that only contains certain dates and is never changed.

2. Number of cases/rows: rows = 3343, columns = variable.

3. Variable List: 
UID: Unique ID for state and county.
iso2: Country code 1 (always US).
iso3: Country code 2 (always USA).
code3: Country code 3 (always 840).
FIPS: County-level code.
Admin2: County name.
Province_State: State name.
Country_Region: Always US.
Lat: Latitude of county.
Long: Longitude of county.
Combined_Key: County, state, country.
Population: County-level population.
Remaining Columns: Date of data collection as month/day/year.

4. Missing data codes: 
NA

5. Specialized formats or other abbreviations used: 
NA


DATA-SPECIFIC INFORMATION FOR: covid_us_wide.csv

1. Number of variables: Variable. The data are updated daily. This .csv file contains the last downloaded example. However, this raw data is preprocessed into a version that only contains certain dates and is never changed.

2. Number of cases/rows: rows = 3343, columns = variable.

3. Variable List: 
- UID: Unique ID for state and county.
- iso2: Country code 1 (always US).
- iso3: Country code 2 (always USA).
- code3: Country code 3 (always 840).
- FIPS: County-level code.
- Admin2: County name.
- Province_State: State name.
- Country_Region: Always US.
- Lat: Latitude of county.
- Long: Longitude of county.
- Combined_Key: County, state, country.
- Population: County-level population.
- Remaining Columns: Date of data collection as month/day/year.


DATA-SPECIFIC INFORMATION FOR: cva.csv

1. Number of variables: 104

2. Number of cases/rows: rows = 603, columns = 104.

3. Variable List: 
- SubID: Participant ID code.
- Timepoint: Date when data was collected.
- ID: Participant ID code.
- ProlificID: Prolific-specific participant ID code.
- Age: Age of participant.
- Gender: Gender of participant.
- SEL: Socioeconimic status.
- Country: Country participant lives in (all participants lived in the United States).
- State: State participant live in.
- Emp: Employment status.
- Edu: Education level.
- SRA1 - SRA20:
- VAR029: 
- PSI1 - outgroup: 	
- VAR033: 	
- ownn - help: 	
- VAR039: 	
- PS1 - PS10: 	
- VAR050: 	
- D1 - D21:	
- VAR072:	
- RP1 - RP10: 	
- VAR083: 	
- socialm - personalexp: 	
- VAR089:	
- alc1 - control:	
- VAR094:


DATA-SPECIFIC INFORMATION FOR: total_cases_and_deaths.csv

1. Number of variables: 11

2. Number of cases/rows: rows = 205, columns = 11.

3. Variable List: 
- week: Week number that data were collected (1, 2, 3, 4).
- n: Number of observations (always 1).	
- total_state_cases: Total COVID-19 cases in that state for that week. 	
- total_state_deaths: Total COVID-19 deaths in that state for that week.	
- total_us_cases: Total COVID-19 cases in the US for that week.	
- total_us_deaths: Total COVID-19 deaths in the US for that week.	
- cases_per_10000: Total COVID-19 cases in that state for that week (per 10,000 people).	
- cases_per_100000: Total COVID-19 cases in that state for that week (per 100,000 people).	
- deaths_per_10000: Total COVID-19 deaths in that state for that week (per 10,000 people).	
- deaths_per_100000: Total COVID-19 deaths in that state for that week (per 100,000 people).	
- state: State measurement was taken from.

DATA-SPECIFIC INFORMATION FOR: cva_bayes.csv

1. Number of variables: 10

2. Number of cases/rows: rows = 602, columns = 10.

3. Variable List: 
week: Week data was collected.	
rp_c: 	
pss_c: 	
anxiety_c: Anxiety score.	
depression_c: Depression score.	
age_c: Participant's age. 	
gender2: Participant's gender. 	
employment: Participant's employment status. 	
financ: Participant's financial status.	
sra_c: Self-Reported Altruism score.

