from entityrdfizer.aboxgenerator.csv2abox import run_abox_generator
from entityrdfizer.tboxgenerator.csv2tbox import run_tbox_generator


def csv2rdf_wrapper(
        csvFileOrDirPath,
        csvType,
        outDir):

    if csvType.lower() == 'abox':
        run_abox_generator(csvFileOrDirPath,outDir)
    elif csvType.lower() == 'tbox':
        run_tbox_generator(csvFileOrDirPath,outDir)
    else:
        print("Error: Wrong csvType argument. Please choose between the 'abox' or 'tbox' options.")


