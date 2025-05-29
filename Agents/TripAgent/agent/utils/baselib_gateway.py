from twa.resources import JpsBaseLib

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

baselib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(
    baselib_view, "uk.ac.cam.cares.jps.base.query.*")
jpsBaseLibGW.importPackages(
    baselib_view, "uk.ac.cam.cares.jps.base.timeseries.*")
