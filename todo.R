Todo:
# descriptions of graphs, descriptions of the info in each output (see Seasonal Characterization Slides that were shared)
# view results barplot should have the x axis be qualitative- i.e. a bar for each site
# when creating graphs, generate the data to be turned into graphs ahead of time, and switch which one is shown with the button
  (don't continually run the same code to generate the graphs from scratch every time they're selected)
# be able to select genetics in the visualizations

# internally:
# make N/A sowing dates a viable choice


Major Issues:
1. trials_df not found: process with shiny app and parallel programming, not updated environment after each step. 
2. display of facted comparision and between sites, similar issues with environmental variables. 
3. small_charact_dt has 141 lines but only 97 apsimx trials. Why? 





Fixed Bugs:

2024/6/21:
# descriptions and info boxes for the tool
# progress bar for process


2024/6/14:
1. make sure that "download results" button downloads all three files
2. make sure that everything is up to date with the newest version of the code and tools
3. option to select a folder where to put the results
4. if you hit the "run analysis" button while an analysis is already running, it won't queue another analysis
5. when showing results, round digits to two places
