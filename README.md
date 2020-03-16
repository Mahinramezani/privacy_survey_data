# privacy_survey_data

1. Download Qualtrics data (put it in the data folder). Rename it to "MTURK_02_20_qualtrics.csv".
2. Download Conjointly data (put them in the data folder):
	* View report -> Review participants -> Download CSV (rename this file to "MTURK_02_20_Respondents.csv".
	* View report -> Excel export 
		- In the Excel export file, go to the raw responses sheet, copy the table (excluded the first 2 rows) to a new excel file and save it as a csv file ("MTURK_02_20_raw_responses.csv")
3. Open the R project ("privacy_survey_data.Rproj") and run this file: "mturk_participants.R" in the code folder.
4. You can find the results in the result folder.


For Dynata, you need to repeat these steps (only replace the word "MTURK_02_20_" --> "Dynata_02_12_" in steps 1 and 2 and run "dynata_participants.R").
