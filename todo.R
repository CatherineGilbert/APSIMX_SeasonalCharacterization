Todo:
# for heatmaps, remove top dendrogram (columns should be in order by number), add text for the value on each tile, figure out where the NAs are coming from and get rid of it.
# reinstate progress bar for analysis
# make it so the download result button is not accessible until downloaded results are available
# error handling with input < 10
# get boxplots working again, make sure the x axis has each location
# on the same page as Upload and Analyse, have info to describe what cract_x, trials_x, dialy_charact_x are 
   #(this can be found in the Seasonal Characterization Tool powerpoint, or I can write it if you just leave a placeholder so I know where to put the text).
# make sure the downloads aren't named .csv.csv at the end. 
# get the visualizations running on the main branch
# in view results, tab the data display to be able to show all three outputs (trials_x, charact_x, daily_charact_x)
# set up example soy and maize datasets for the final tool. verify.csv can become "soy_example.csv",
and I can grab a maize example from Iris
# download all files as .zip

# internally:
# make N/A sowing dates a viable choice
# make sure the after-harvest period isn't being returned as NA


Major Issues:
1. trials_df not found: process with shiny app and parallel programming, not updated environment after each step. 
2. display of facted comparision and between sites, similar issues with environmental variables. 
3. small_charact_dt has 141 lines but only 97 apsimx trials. Why? 
 > likely because of an issue with ISRIC, the soil database for soils outside the US. it returns errors, so the APSIM
   files can't be generated, and the trials can't be run. 





Fixed Bugs:


2024/6/28:

# descriptions of graphs, descriptions of the info in each output (see Seasonal Characterization Slides that were shared)
# be able to select genetics in the visualizations

2024/6/21:
# descriptions and info boxes for the tool
# progress bar for process

2024/6/14:
1. make sure that "download results" button downloads all three files
2. make sure that everything is up to date with the newest version of the code and tools
3. option to select a folder where to put the results
4. if you hit the "run analysis" button while an analysis is already running, it won't queue another analysis
5. when showing results, round digits to two places
