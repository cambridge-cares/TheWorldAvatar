from entityrdfizer.javagateway.gateway import jpsBaseLibGW
from py4j import Py4JJavaError, Py4JError
import glob
import os

# create the separate JVM view
jpsBaseLibGW_view = jpsBaseLibGW.createModuleView()
# import required Java packages into the created JVM view
jpsBaseLibGW.importPackages(jpsBaseLibGW_view,'uk.ac.cam.cares.jps.base.converter.*')

def run_tbox_generator(
    csvFileOrDirPath,
    outDir
):

    if os.path.isfile(csvFileOrDirPath):
        csv2tbox(csvFileOrDirPath, outDir)
    elif os.path.isdir(csvFileOrDirPath):
        csv_files = glob.glob(os.path.join(csvFileOrDirPath, "*.csv"))
        for csv_file in csv_files:
            csv2tbox(csv_file, outDir)
    else:
        print(f"File or directory {csvFileOrDirPath} does not exists")

def csv2tbox(csvFile, outDir):
    print(f"Converting tbox {csvFile} into owl format.")
    TBoxGenerator = jpsBaseLibGW_view.TBoxGeneration()
    try:
        TBoxGenerator.generateTBox(csvFile)
    except Py4JJavaError as e:
        raise e(f"Tbox generation failed with the following error {e.java_exception}")
    except Py4JError as e:
        raise e(f"Tbox generation failed with the following error {e.java_exception}")

    print('a')
