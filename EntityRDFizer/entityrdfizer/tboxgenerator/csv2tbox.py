from entityrdfizer.javagateway.gateway import jpsBaseLibGW
from py4j.java_gateway import Py4JJavaError, Py4JError
import textwrap
import pathlib
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
        if outDir is None: outDir = os.path.dirname(csvFileOrDirPath)
        os.makedirs(outDir, exist_ok=True)
        outDir =os.path.abspath(outDir)

        csv2tbox(csvFileOrDirPath, outDir)
    elif os.path.isdir(csvFileOrDirPath):
        if outDir is None: outDir = csvFileOrDirPath
        os.makedirs(outDir, exist_ok=True)

        csv_files = glob.glob(os.path.join(csvFileOrDirPath, "*.csv"))
        for csv_file in csv_files:
            csv2tbox(csv_file, outDir)
    else:
        print(f"File or directory {csvFileOrDirPath} does not exists")

def csv2tbox(csvFile, outDir):
    print(f"Converting tbox {csvFile} into rdf format.")
    outFile = os.path.join(outDir,pathlib.Path(csvFile).stem+'.owl')
    TBoxGenerator = jpsBaseLibGW_view.TBoxGeneration()
    try:
        # the file name is added to the outDir, as the java code expects output file path
        TBoxGenerator.generateTBox(csvFile, outFile)
    except (Py4JJavaError, Py4JError) as e:
        print(textwrap.dedent("""
            Error: Tbox generation failed. Add 'redirect_stderr' parameter to the launch
                   gateway method to see a more detailed error log."""))
        raise e(f"Tbox generation failed with the following error {e.java_exception}. ")
    print(f"Conversion complete. Tbox created at {outFile}")