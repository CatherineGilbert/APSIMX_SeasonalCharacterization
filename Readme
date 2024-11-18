Uses APSIM + the R apsimx package to produce seasonal covariates for maize/soy trials. "apsimx.R" is the script to use if you want to run the process outside of the web app. 

The input file format is a .csv with the columns "Site", "Latitude", "Longitude", "Genetics," and "PlantingDate".
* “Site” should be the name identifier for that location. 
* “Latitude” and “Longitude” are standard numeric WGS84 coordinates. 
* “Genetics” refers to the cultivar maturity. 
  * For soybean, Genetics is a numeric in the range -2 -- 10 (corresponding to maturity groups 000 -- X). There are early, standard, and late variants of each of these cultivar maturities. Adding 0 -- 0.33 to the Genetics value will classify the maturity as "early" within a maturity group, between 0.34 -- 0.66 will classify it as standard ("mid"), and between 0.67 -- 0.99 will classify it as "late." For example, an input Genetics value of 3.8 would be a late maturity III, 0.2 would be an early maturity 0, and -1.5 would be a standard maturity 000. 
  * For maize, Genetics is a character string with the approximate number of days to maturity for the cultivar and a letter, A or B, for early or late maturing varieties respectively. The cultivar maturities available are 80, 90, 95, 100, 103, 105, 108, 110, 112, 115, 120, and 130. The input will be matched to the closest of these values and the early or late variant. Several input formats (ex: "A_100", "A100", "100a") are acceptable. 
* “Planting” is when the trial is sown. This can be provided as a date in YYYY-MM-DD format, or as only the year. If no planting date is specified, the simulation will sow the trial on the first suitable day of the year. The parameters used to decide when to sow the crop if no date is specified can be modified in the template model's "Sowing" module.

On lines 17 and 18, set codes_dir to the directory of the script, and set the working directory to the folder where the output will go.

