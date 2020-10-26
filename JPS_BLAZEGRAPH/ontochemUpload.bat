@ECHO OFF
set arg1=%1
set arg2=%2
set arg3=%3
java -cp ontology-uploader.jar uk.ac.cam.cares.jps.blazegraph.KnowledgeRepository %arg1% %arg2% %arg3%
