# Description

The code and data collected in this folder is dedicated to reproduce the figures in **[Preprint 323](https://como.ceb.cam.ac.uk/preprints/323/): Impact of heat pumps and future energy prices on regional inequalities.**

## Data
Please download the data (there should be two folders called `Data` and `GB_shapefile` respectively) from the project folder in the archive for preprint 323 (available on request from the corresponding author) and use it to replace the contents of the `./Data` and `./GB_shapefile` folders.

## Project structure
Once the data has been sorted, the project should be organised as follows:
```bash
code/ # The code to calculate, and generate the figures
├── datamodel/
│   ├── __init__.py
│   └── functionality.py # Utility functions
├── dataoutput/
│   ├── __init__.py
│   ├── dataoutput.py # Calculation functions and core figure generation functions
│   └── Figure_generation.py # KEY script, you will need to run this script to reproduce the figure. 
├── dataretrieval/
│   ├── __init__.py
│   └── dataretrieval.py # Functions to retrieve the data.
└── __init__.py
Data/ # Data used in this project
│   ├── pickles/
│   │     └── geometry # Geometry boundaries (we used LSOAs)
│   ├── properties_csv/ # Data with respect to each geometry (we used LSOAs), separated by year
│   │        ├──2010 
│   │        ├──2011
│   │        └── ... 
│   ├── source_data/ # The source data which format was unchanged
│   │        ├──2021-sub-regional-fuel-poverty-tables.xlsx
│   │        └── ... 
│   └── ... 
figure_output/ # The generated figures will be stored here
GB_shapefile/ # The shape files of the UK boundaries to draw maps.
│   ├── GBR_adm2.cpg
│   └── ... 
README.md
setup.py
```
The code to calculate, and generate the figures are mainly stored at `./code`, The source data are stored at `./Data`, `./GB_shapefile` stores the shape files of the UK boundaries to draw maps. The generated figures will be stored at `./figure_output`, which should be initially empty.

`./code/datamodel/functionality.py` stores some utility functions, `./code/dataoutput/dataoutput.py` stores some calculation functions and core figure generation functions. `./code/dataretrieval/dataretrieval.py` stores functions to retrieve the data. All those three scripts are used to serve the main script `.\code\dataoutput\Figure_generation.py`, you will need to run this script to reproduce the figure. 

`./Data/pickles/geometry` saves the geometry boundaries (we used LSOAs), `./Data/properties_csv/` saves data for each lsoa or for the UK, separated by year, such as electricity and gas prices, consumption, temperature etc...

**Note** that for many reasons, much of the data has been saved (converted) as csv files. For example, the source data for temperature was raster files, which need days of calculation to get the vector data for temperature with respect to LSOA! Other source data will also need treatment to convert to the format that will be needed in this project. In these cases, using csv files will significantly save the cost.

For the data source, how to calculate the temeperature data for each LSOA, and how  to calculate monthly profiles of electricity and gas consumptions, please refer to the [Preprint 323](https://como.ceb.cam.ac.uk/preprints/323/) and [LSOAInputagent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/LSOAInputAgent). 

Please follow the following steps to setup the environments and complete the reproduction. If in doubt, contact Jieyang Xu (jx309@cam.ac.uk & 1643529603@qq.com)

## Virtual environment setup

`(Windows)`

```cmd
$ python -m venv <venv_name>
$ <venv_name>\Scripts\activate.bat
(<venv_name>) $
```

`(Linux)`
```sh
$ python3 -m venv <venv_name>
$ source <venv_name>/bin/activate
(<venv_name>) $
```

The above commands will create and activate the virtual environment `<venv_name>` in the current directory.
## Install packages
```sh
pip install .
```
The above command will run `./setup.py` and install all python packages required for this project. 
**Remember:** If you are in dev mode, i.e., if you made changes to the code, remember to run the above command as well to save the changes.
## Run the script
In `.\code\dataoutput\Figure_generation.py`, loads of functions has been provided to generate all kinds of figure (there are much more than figures in the preprints!). You can also uncomment the function I specified to reproduce the figure in this script. Once this is prepared, run the following command:

`(Windows)`

```cmd
$ python .\code\dataoutput\Figure_generation.py
```

`(Linux)`
```sh
$ python3 .\code\dataoutput\Figure_generation.py
```

# Author
Jieyang Xu (jx309@cam.ac.uk & 1643529603@qq.com)
