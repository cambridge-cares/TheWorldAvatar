# README #

## What is this repository for? ##

Repository Name: thermochemistry

* Authors: Philipp Buerger <pb556.como@gmail.com>, Nenad Krdzavac
* Mantis: File bugs under applicable parent project (e.g. Thermochemistry)
* Historical origin (just for information - do not use):
  \\vienna\CoMoCommon\Archive\Projects\Preprints\c4e\c4e-179-pb556-DetailedMethodology\Repositories\java-Git

## How do I get set up? ##

### Summary of set up ###

### Configuration ###

### Dependencies ###

* CMCL libraries:
  Maven needs to be configured (both on Windows and Linux) so that it finds the CMCL dependencies (libraries from the Mechanism Viewer).
  The ~/.m2 directory needs to contain the two XML files settings.xml and settings-security.xml. Contact CMCL support if you need them.
  It may also be necessary to set environment variables: JAVA_HOME needs to be set to the location of the JDK, M2_HOME to the location of the maven installation directory.
  To do: Update pom.xml files to use the latest version of any of the Mechanism Viewer libraries (1.1.0 or later).

* Python scripts for thermodata calculations:
  git clone ssh://vienna.cheng.cam.ac.uk/home/userspace/CoMoCommon/Ongoing/Projects/c4e-dln22-TDC
  NB Execution of the scripts likely requires a Python 3.x installation, and additionally, if applicable, that the "python" executable points to Python 3, not 2.
  See notes on deployment below.
  To do: Move this to bradman. Set it up as a git submodule of this git repository.
  
* OpenBabel:
  This is a headache.
  The project JOpenBabel (v2.3.1 or v2.4.1) does not seem to exist in any maven repository any more.
  The maven repository at the Chemistry Department also does not exist any more.
  Therefore, both have been commented out in all pom.xml files.
  For the CoMoThermodynamics project, this seems to be without consequence, as there does not appear to be a genuine dependency.
  The CoMoOpenBabel project still builds, but in order to run the tests, one needs the babel command-line executable as well as the DLL/shared object library (but no jar apparently).
  NB The name of the DLL/shared object library is hard-coded in CoMoOpenBabel/src/main/.../openbabel/util/OpenBabelUtil.java, currently as openbabel_java (works for Windows only).
  The CoMoTools project is the only project with a genuine OpenBabel dependency, through the source file CoMoTools/src/main/.../tools/structure/util/OpenBabelCompoundConverter.java, but it does not appear to be used by anything in the repository so it has been excluded from the build (by renaming the file).
  References:
  https://openbabel.org/docs/index.html
  https://sourceforge.net/projects/openbabel/ (NB This does install a .jar file.)
  sudo yum install {openbabel,openbabel-devel} (NB This installs babel command-line executable and shared library libopenbabel.so, but no jars.)
  Note perhaps also: https://openbabel.org/docs/UseTheLibrary/Java.html#macosx-and-linux
  https://openbabel.org/docs/Installation/install.html

* Jmol/JSmol:
  https://sourceforge.net/projects/jmol/files/ (https://sourceforge.net/projects/jsmol/ is deprecated)
  Download and extract the latest version of the archive. NB The contents of the file jsmol.zip within the extracted folder may need to be added to molhub/WebContent/jsmol/. DO NOT COMMIT THEM TO THIS GIT REPOSITORY!
  We do need the Jmol jar files (Jmol.jar, JmolData.jar, JmolLib.jar), as at least one of them is called on the command line to generate molecule images upon upload of Gaussian files to MolHub. They may need to be located in molhub/WebContent/WEB-INF/lib/. DO NOT COMMIT THEM TO THIS GIT REPOSITORY!
  See deployment notes below.

* GLPK (GNU Linear Programming Kit):
  https://www.gnu.org/software/glpk/
  Download from http://ftp.gnu.org/gnu/glpk/
  Versions that have been used successfully include 4.52.1, 4.58, and 4.59.

* Gaussian files and other files that may be useful for unit testing can be found in an older (and much bigger) version of this git repository:
  vienna/home/userspace/CoMoCommon/Archive/Codes/CARES/thermochemistry-BEFORE-CLEAN-2020-12-07
  DO NOT COMMIT ANY OF THESE FILES TO THIS GIT REPOSITORY!

### Database configuration ###

MolHub connects to an RDF4J triple-store.

### How to run tests ###

Build projects via 'mvn clean install', i.e. omitting '-DskipTests'.

### Deployment instructions ###

Drop molhub.war into /var/lib/tomcat/webapps/.
In addition, in $CATALINA_HOME (which defaults to /usr/share/tomcat/ on CEntOS) we need the following files:

* Copy
  CoMoOntology/src/test/resources/ontology/sparql_query/query_all.sparql
  into
  conf/Catalina/sparql_query/
  NB Path is hard-coded in molhub/src/.../action/CalculationAction.java

* Copy
  CoMoOntology/src/test/resources/xml_schema/schema.xsd
  into
  conf/Catalina/xml_schema/
  NB Path is hard-coded in molhub/src/.../action/UploadAction.java

* Copy
  \CoMoOntology/src/test/resources/xslt/ontochem_rdf.xsl
  into
  conf/Catalina/xslt/
  NB Path is hard-coded in molhub/src/.../action/UploadAction.java

* Copy *.jar from the extracted Jmol archive (see Dependencies above) into conf/Catalina/jmol/.

* git clone the Python scripts (see Dependencies above) into conf/Catalina/.
  NB Path is hard-coded in molhub/src/.../action/CalculationAction.java

* Copy the contents of
  CoMoOntology/src/test/resources/ontology/compchem_ontology/
  into
  conf/Catalina/compchem_ontology/
  NB This is not currently used (neither is the copy in RDF4J triple-store), but add it anyway as work-in-progress.

To do: Write a deploy.sh Linux shell script.

* To set up on Claudius
  - To upload it on Claudius just copy war file into "webapps" Tomcat folder.
  - On Claudius, in Tomcat folder "/conf/Catalina/" we store Python code, xslt transformation, jar files, that are necessaary for full functionally of molhub service. Do not delete it.
  - To run "upload" molhub functionality, one may need to set up parser enviroment.
    i.  Download Anaconda (if applicable)
    ii. Select “Gaussian_parser” environment that is required before running the Python parser

## Contribution guidelines ##

* Writing tests
* Code review
* Other guidelines

## Usage notes ##

* Uploaded Gaussian files as well as processed files are stored in UUID-named subfolders of /var/lib/tomcat/webapps/ROOT/.
* NB The file CoMoOntology/src/test/resources/g09/Cl2.g09 is an example of a file which fails to be parsed by Jmol/JSmol.

## Who do I talk to? ##

* Repo owner or admin
* Other community or team contact