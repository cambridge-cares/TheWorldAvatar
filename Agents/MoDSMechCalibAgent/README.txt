############################################
## MoDSMechCalibAgent deploy instructions ##
############################################

This file gives the basic instructions for deploying the MoDSMechCalibAgent. 
The instructions for setting up environment on CSD3 are given first followed by deploying agents on Windows. 


*********************************
** Setting Environment On CSD3 **
*********************************

Required tools
==============

You will need to have following tools installed:
  Name          (recommended version)                       Example 'module' command to be added to .bashrc or similar
  Git           (2.14.1+)                                   module load git-2.14.1-gcc-5.4.0-acb553e
    Older versions might not support "submodules",
    which are used by some of the external dependencies.
  GCC           (4.8.5)                                     Preinstalled
    This is the default version on most modern Linux
    distributions, the code should compile with newer
    versions but the resulting binaries are unlikely
    to be portable.
  Conda         (Miniconda3)                                This requires user to install by themselves. 
    Following the instruction of installation in link: 
    https://conda.io/projects/conda/en/latest/user-guide/install/linux.html
    It should be noted that if you would like to 
    download the Miniconda installer to your Windows
    host then transfer it to CSD3, you would need to
    run "chmod +x Miniconda3-latest-Linux-x86_64.sh"
    before run "bash Miniconda3-latest-Linux-x86_64.sh"
    in the instruction. Remember to select "yes" when
    you are asked to answer: 
    Do you wish the installer to initialize Miniconda3
    by running conda init? [yes|no]

Required 3rd-Party Libraries
============================

In the conda environment where you want Cantera (2.4.0+) to be installed, you would need to make sure below libraries are installed:
  ipython, matplotlib, pandas

All of the above external library dependencies will be downloaded and installed while installing Cantera following below instructions.

Setting up executables
======================

* Create a folder to hold kinetics executable
   - Create a "kinetics" directory by running "mkdir kinetics"
   - Upload srm_driver64_20200819build and UnitDictionary.xml to "kinetics" folder
   - Go into the "kinetics" folder and run "chmod +x srm_driver64_20200819build"
   - Take a note of the absolute path to "kinetics" folder <yourAbsolutePathToKineticsDir>, this will be used later on while deploying agents on server
* Create a folder to hold MoDS executable
   - Create a "mods" directory by running "mkdir mods"
   - Upload MoDS_mpi to "mods" folder
   - Go into the "mods" folder and run "chmod +x MoDS_mpi"
   - Take a note of the absolute path to MoDS_mpi executable <yourAbsolutePathToMoDSMPI>, i.e., the path should be something like ~/mods/MoDS_mpi, this will be used later on while sending job requests
* Install Cantera, this should only be done after the Miniconda/Anaconda is correctly installed and your session to CSD3 is restarted
   - Make sure the question was answered as "yes" when you are asked below while installing Miniconda
     - Do you wish the installer to initialize Miniconda3 by running conda init? [yes|no]
   - Install Cantera to a conda environment by running "conda create --name <yourEnvName> --channel cantera cantera ipython matplotlib pandas"
   - If you would like to install Cantera to an existing environment, run "conda activate <yourEnvName>" and "conda install --channel cantera cantera", also run "conda install ipython matplotlib pandas" if you do not have those libs installed yet
   - Take a node of the <yourEnvName> you provided, this will be used later on while sending job requests
   - For more information, refer to https://cantera.org/install/conda-install.html


*********************************
** Deploying Agents On Windows **
*********************************

Required tools
==============

You will need to have following tools installed:
  Name          (recommended version)     URL
  Apache Tomcat (7.0.96+)                 https://tomcat.apache.org/download-70.cgi
    This is used to deploy the agents on server. 
    To be able to deploy the agents and build agents from source, you will need to set "CATALINA_HOME" environment variable correctly set. 
  Java8.0         (jdk1.8.0_201+)           https://www.oracle.com/uk/java/technologies/javase/javase-jdk8-downloads.html
    This is used to build agents from source if required. 
    To be able to deploy the agents and build agents from source, you will need to set "JAVA_HOME" environment variable correctly set. 

Deploying agents with war file
==============================

* Open jps-base-lib-1.3.0.jar as archive, get into file jps.properties
   - This file can be found in your .m2 folder under \.m2\repository\uk\ac\cam\cares\jps\jps-base-lib\1.3.0
   - Modify ${host} and ${port} according to the server
   - To open .war files as archive, a tool like 7-zip is required
   - Alternatively, set ${test} to true in order to use settings in jpstest.properties of the same archive folder
* Open MoDSMechCalibAgent.war as archive, get into file \MoDSMechCalibAgent.war\WEB-INF\classes\modsmechcalib-agent.properties
   - Modify ${kinetics.folder.path} with <yourAbsolutePathToKineticsDir> absolute path to "kinetics" directory on CSD3
   - Modify ${kinetics.executable.name} if you changed name of executable when uploading it to CSD3
   - Provide ${hpc.server.login.user.name} and ${hpc.server.login.user.password}
   - You may also want to change ${agent.initial.delay.to.start} and ${agent.periodic.action.interval}, they are in order of seconds
   - Modify ${rdf4j.server.url} accordingly if you host kg on different server
* Start Tomcat, this should only be done after "CATALINE_HOME" is correctly set:
   - Open cmd
   - $ %CATALINA_HOME%\bin\startup.bat
   - Copy modified JPS_BASE##1.3.0.war and MoDSMechCalibAgent.war into folder %CATALINA_HOME%\webapps\
* Now you can send job request in the format of HTTP request starting with "/MoDSMechCalibAgent/job/request?query=<yourQueryString>"
   - <yourQueryString> should be the URL format of your Json string, which can be obtained by converting using https://onlinejsontools.com/url-encode-json
   - In below example, replace the <yourEnvName> with your name of conda environment where Cantera is installed on CSD3
   - Also replace the <yourAbsolutePathToMoDSMPI> with your absolute path to MoDS_mpi on CSD3

   {"json":{"cantera":{"environment":"<yourEnvName>"},"kinetics":{"numerical":{"simEnd":"500"}},"mods":{"executable":{"path":"<yourAbsolutePathToMoDSMPI>"},"reactionMustInclude":["35","38","39"],"calibrationAlg":{"initPoints":"1","nIters":"400","rho":"0.2","rhoFactor":"0.5","epsilon":"0.001","objectiveFunction":"SumOfSquares","responseRatio":"6.3",},"sensAna":{"relPerturbation":"1e-3","maxORavg":"max","topN":"10"},"samplingAlg":{"sobolPoints":"10000","outputInterval":"1000","rangeOfMultipliers":"100.0"},"ignDelayOption":{"method":"1","species":"AR"},"flameSpeedOption":{"tranModel":"mix-average"}},"ontochemexpIRI":{"ignitionDelay":["https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000","https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800","https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600"],"flameSpeed":["https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400"]},"ontokinIRI":{"reactionList":["http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316492940_40","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493060_160","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316492938_38","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316492935_35","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493074_174","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493073_173","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316492939_39","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493093_193","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316492937_37","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316492950_50","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493038_138","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493054_154","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493037_137","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493040_140","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493052_152","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493067_167","http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ChemicalReaction_6651394316493048_148"],"mechanism":"http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ReactionMechanism_6651394316481501"}}}

Building the code from source
=============================

* Clone JPS git repo from https://github.com/cambridge-cares/TheWorldAvatar
* Modify \TheWorldAvatar\JPS_BASE_LIB\src\main\resources\jps.properties in the same way of modifying \.m2\repository\uk\ac\cam\cares\jps\jps-base-lib\1.3.0\jps-base-lib-1.3.0.jar\jps.properties while deploying from MoDSMechCalibAgent.war
* Modify \TheWorldAvatar\Agents\MoDSMechCalibAgent\src\main\resources\modsmechcalib-agent.properties in the same way of modifying \MoDSSensAnaAgent.war\WEB-INF\classes\modssensana-agent.properties while deploying from MoDSMechCalibAgent.war
* Make sure the ${tomcatPath} profile is correctly setup in settings.xml in \user.home\.m2 folder
* Make sure the "CATALINA_HOME" environment variable is correctly setup
* Build JPS_BASE
   - Get into \TheWorldAvatar\JPS_BASE directory from cmd
   - Run "mvn clean install -DskipTests"
* Build JPS_BASE_LIB
   - Get into \TheWorldAvatar\JPS_BASE_LIB directory from cmd
   - Run "mvn clean install -DskipTests"
* Build CMCLOntoKin
   - Get into \TheWorldAvatar\ontology-tools\CMCLOntoKin directory from cmd
   - Run "mvn clean install -DskipTests"
* Build MoDSMechCalibAgent
   - Get into \TheWorldAvatar\Agents\MoDSMechCalibAgent directory from cmd
   - Run "mvn clean install -DskipTests"
