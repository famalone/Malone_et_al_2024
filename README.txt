Author: Fin Malone

This folder contains all that is needed to reproduce my results from the collected data
and R scripts.

Here are the steps:

1. Open all 5 R scripts and set the work.dir variables to the directory of this folder
	on your computer. An example work.dir is already supplied in each script.
	Replace these with your own. Here are the locations of each work.dir variable:
	
	0_MasterScript.R: lines 6, 11, 16, and 21
	1_Geospatial.R: line 31
	2_Fit_Growth_Curves.R: line 16
	3_Analysis.R: line 25
	4_SI.R: line 16

2. Download SAGA GIS (version used: 7.3.0). This is necessary for reproducing the geospatial
	data from the digital elevation map in R. The code will use the RSAGA package to
	automatically run the spatial analyses.

3. Run 0_MasterScript.R line by line. This will run the other scripts and write
	new excel files with processed data. This will also fill "Figures" and "Stat_Output"
	folders with images and results relating to the respective analyses. It should
	take less than 5 min (probably much less) to run everything, depending on whether the
	relavant packages are installed or not and how much compute you have.

4. That should be it! Metadata for all raw and processed datasets are in the
	"_1_metadata_thesis.xlsx" file. Raw field data, SNOTEL precipitation data, and
	geospatial data can be found in the "_2_lubrecht_data_thesis.xlsx" file. Check out
	script documentation for additional details regarding the analysis.


Lastly it is important to note that this was all done on Linux Ubuntu 22.04.4 LTS. Other OS's
should work, but compatibility or formatting issues may occur.

Contact finmalone@gmail.com for questions.
