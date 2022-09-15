 
                      AERMAP (dated 09040) Test Cases                 02/09/2009

This folder contains five sets of tests for the AERMAP Terrain Preprocessor,
for version dated 09040.  To run the test cases, unzip the contents with the 
embedded folder names.  A separate folder is provided for each set of tests.
The batch file provided in the main folder, called 'RunAERMAP_Tests.bat', can
be used to run all of the tests.  Each subfolder also includes a batch file,
called 'RunAERMAP.bat', that runs the test(s) associated with that folder.
The batch files use relative path references for the AERMAP executable file,
called 'aermap.exe', included in the main folder.  The main folder also 
contains the NADCON grid shift files (*.las and *.los) for the CONUS and 
Alaska domains needed for running the tests.  The AERMAP input files in the 
subfolders include the new NADGRIDS keyword on the CO pathway to specify the 
pathname for the NADCON grid shift files using a relative pathname.

A brief description of the AERMAP test cases included with this update to
AERMAP is provided below:

Folder Name               Description
--------------------      ------------------------------------------------------
AK-MixedDEM_CrossUTM      Test case for Alaska with "mixed" DEM files, including
                          7.5-min, 15-min, and 1-degree DEMs, and cross UTM zone
                          domain.

Cross_UTM_Zone            A cross UTM zone test case in Moffat County, Colorado,
                          including two NAD27 7.5-minute DEM files that cross the 
                          boundary for UTM zones 12 and 13.  Separate tests are
                          included based on a NAD27 anchor point and based on an
                          equivalent NAD83 anchor point.

NAD_Gap                   A test case including receptors located within "gaps"
                          between adjacent 7.5-minute files due to a NAD shift.
                          The test also utilizes the new 'FILLGAPS' option for
                          DEM data to estimate elevations for receptors located
                          within these gaps based on the closest nodes.

NW_Durham                 This includes the test cases packaged with the previous
                          version of AERMAP, dated 06341, based on a 10-meter
                          resolution 7.5-minute DEM file for Northwest Durham,
                          NC.  The original tests based on both NAD27 and NAD83
                          anchor points are included, plus equivalent tests for
                          the same receptor domain using NED elevation data.

Columbia_CrossEquator     Test case for Columbia including a domain that crosses
                          the equator.  Two DEM-formatted files were generated
                          from 3 arc-second SRTM data for the area.
