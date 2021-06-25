I will attempt to untangle the spaghetti herein, for context this was produced in a relatively rushed manner during my first term. 

The general scope of the code Map Creation.py is to produce, and plot real time data from the gas grid. This is done in three main steps:

-----------------------------------------------------------------
1) Get data from the National Grid instantaneous view webpage 

- Source is here: https://mip-prd-web.azurewebsites.net/InstantaneousView
- nts_data_collect.py uses Selenium to retrieve this information.
- After usage this data is saved in an excel file, subsequent calls then use this data in addition to new data.
	This is how the timeseries is 'stored' for now.
- This means that as long as the code runs, new data is added.
- This occurs every 720 seconds, as per a full update of the National Grid webpage.

------------------------------------------------------------------
2) Plot this data.

- The data is plotted using a .html plotting library (I think Bokeh)
	in order to show within the plot within a Folium popup. 
- The HTML plots are then saved within a folder.

-------------------------------------------------------------------
3) Display this data on a map 

- Mapping is done with the Folium library.
- Locations of the offtakes are taken from location.xlsx 
- HTML plots are called from previously created plotted files 
- All saved as a HTML Map. 


Final Notes
----------------------------------------------------------
The overall workflow is:
Collect Data --> Save Data --> Produce Plots --> Map locations and plots --> Wait 720s... Repeat
- The file Map.html just has some existing static plots, to allow for a decent visualisation without waiting ages for python to call data starting at when you run it.


