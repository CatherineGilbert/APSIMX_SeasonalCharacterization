Todo:
# descriptions of graphs, descriptions of the info in each output (see Seasonal Characterization Slides that were shared)
# view results barplot should have the x axis be qualitative- i.e. a bar for each site
# for heatmaps, remove top dendrogram (columns should be in order by number), add text for the value on each tile, figure out where the NAs are coming from and get rid of it.
# reinstate progress bar for analysis
# make it so the download result button is not accessible until downloaded results are available

# internally:
# make N/A sowing dates a viable choice


Major Issues:
1. trials_df not found: process with shiny app and parallel programming, not updated environment after each step. 
2. display of facted comparision and between sites, similar issues with environmental variables. 
3. small_charact_dt has 141 lines but only 97 apsimx trials. Why? 





Fixed Bugs:


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
