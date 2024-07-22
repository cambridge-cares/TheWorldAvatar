import os
from py4jps.resources import JpsBaseLib

jpsBaseLibGW            = JpsBaseLib()
jpsBaseLibGW.launchGateway()
jpsBaseLib_view         = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.converter.*")
tbox_generation         = jpsBaseLib_view.TBoxGeneration()
script_dir              = os.path.dirname(os.path.abspath(__file__))
in_directory            = os.path.join(script_dir, "../Data/OntoSynTbox/")
tbox_generation.generateTBox(in_directory+'OntoSyn.csv')
