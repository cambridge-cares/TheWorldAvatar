from entityrdfizer.aboxgenerator.csv2abox import run_abox_generator
from entityrdfizer.tboxgenerator.csv2tbox import run_tbox_generator


def csv2rdf_wrapper(
        csvFileOrDirPath,
        csvType,
        outDir,
        csvTbox=None):

    if csvType.lower() == 'abox' or None == csvType:
        #print( "The TBox path is '" + str(csvTbox) + "'." )
        run_abox_generator(csvFileOrDirPath,outDir,csvTbox)
    elif csvType.lower() == 'tbox':
        run_tbox_generator(csvFileOrDirPath,outDir)
    else:
        print("Error: Wrong csvType argument. Please choose between the 'abox' or 'tbox' options.")


