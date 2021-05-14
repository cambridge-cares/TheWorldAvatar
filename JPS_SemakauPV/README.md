# JPS Semakau Simulator
## Python Dependencies
1. (from DES) requests
2. (from DES) caresjpsutil (for logging, see installation notes in JPS_BASE/PythonCustomLibraries/setup.py)

## Java Dependencies
1. jps-base-lib
2. Junit
3. javax servlet
4. jena-arq (for query builder)
5. com.cmclinnovations.mods **.Mods_Java_APIv0.3.3** (This jar file has to be downloaded from Claudius as it's proprietary. Hunt in the libraries in .m2 folder/repositories. )

### Websites referenced: 
1. (from DES) [Solar Repository Institute, NUS](https://www.solar-repository.sg/ftp_up/weather/500_Weather.png)
2. (from DES) [data.gov.sg](https://api.data.gov.sg/v1/environment/air-temperature)
3. (Optional) if the above doesn't work, then JPS_DES/python/ocrv2.py's reliant address is also from the solar repository, but at a different [site](https://www.solar-repository.sg/ftp_up/irradiance/NSR_IrrMap.png)

## What to do for deployment? 
1. You need the simulation folder from Claudius, under the folder `/Sim_PV1`. This contains models, variable files, and other necessary jars for simulation
2. You need to create two files: `TOMCAT\webapps\ROOT\kb\sgp\semakauisland\semakauelectricalnetwork\PV-002.owl` and `TOMCAT\webapps\ROOT\kb\sgp\semakauisland\semakauelectricalnetwork\EBus-006.owl` in the respective folders. You can do that by running TimeSeriesConverter.java main method, or copying from Claudius. 
3. Have python installed. Have java installed. Both should be added to your system environment. 
4. mvn clean install JPS_SemakauPV
5. Expected Result: Open browser page, four graphs should be shown: 
 - Solar Irradiance (taken from solar repository page) under the header of *Inputs*
 - Actual Phase Angle, Actual Voltage, Actual Active Power, and Actual Reactive Power under the header of *Outputs*
6. For hourly execution, have Semakau.bat called regularly. 


## What's wrong with the visualization? 
1. Check DES readme for further help if you don't have a graph
2. If DES is working, then:
   	1. You're missing ModsAPI and/or the simulation folder. To check if it's this, run the test `testrunMods` in TestSemakauPV.java. If you fail at the runMods function, it's most likely you don't have the OntModel or you don't have the necessary simulation files. 
   	2. You're missing an OWL file. See Step 2. of *What to do for deployment?*