@ECHO OFF
set arg1=%1
java -cp ontochem.jar com.cmclinnovations.ontochem.model.converter.owl.OwlConverter %arg1%