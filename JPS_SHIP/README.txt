Please note that this project is no longer in use and in development, it may be removed soon

A Python Virtual Environment will now be created in the ROOT folder of your Tomcat directory during 'mvn clean install' for the JPS_SHIP project.  

Prerequisite for non-Windows user:
Please install Python 3.6.8 via preferred download management tool (e.g. Homebrew for MacOS) or the installer available on https://www.python.org/downloads/release/python-368/ before running 'mvn clean install' on the JPS_SHIP project. 

To use this virtual environment for SpeedLoadMapWrapper, please do the following:
1) Change the directory of the virtual environment in JPS_SHIP/src/resources/jpsship.properties to that of your local Tomcat directory. 

	speed.load.map.venv.dir = ${tomcatPath}/webapps/ROOT/pyvenv/PYVENV_JPS_SHIP/.venv

e.g.
	speed.load.map.venv.dir = /Users/russ1337/apache-tomcat-8.5.59/webapps/ROOT/pyvenv/PYVENV_JPS_SHIP/.venv

2. Run the command 'mvn clean install' in the JPS_SHIP project directory. 