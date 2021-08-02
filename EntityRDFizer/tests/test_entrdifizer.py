#from entityrdfizer import convertDir, convertFile
from entityrdfizer.ABoxTemplateCSVFilesToRDF import convert as convertDir
from entityrdfizer.ABoxTemplateCSVFileToRDF import convert_into_rdf as convertFile
import pytest
import os
import re

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

rdfRDF_re = re.compile('<rdf:RDF.*?>',re.DOTALL)
namesp_re = re.compile('xmlns:(.*?=.*?http:.*?")+?',re.DOTALL)
nodeId_re = re.compile(':node[iI][dD]=".*?"',re.MULTILINE)

def get_namespaces(fileString):
    namespacesDict = {}
    rdfRDFBlock = rdfRDF_re.search(fileString)
    if rdfRDFBlock:
        namespaceBlock = namesp_re.findall(rdfRDFBlock.group())
        if namespaceBlock:
            for namesp in namespaceBlock:
                namespaceBlock = namesp.split('=')
                namespacesDict[namespaceBlock[0]] = namespaceBlock[1]
    return namespacesDict

def removeNodeId(fileString):
    return re.sub(nodeId_re,'',fileString)

def prepareFileForComparison(filePath):
    """This function expands the namespace tags and removes the
       random nodeId attribtue from the ref and test owl files.
       This is because the rdfizer csv->owl converter does not
       preserve which tag is associated to which namespace and
       also adds a random nodeId so the raw files created by
       the converter are not easily comparable."""
    with open(filePath, 'r') as file:
        fileString = file.read()
    fileString = removeNodeId(fileString)
    namespacesDict = get_namespaces(fileString)
    if namespacesDict:
        for namespTag, namespValue in namespacesDict.items():
            fileString = fileString.replace(namespTag,namespValue)
    preparedFile = '\n'.join(sorted(fileString.split('\n')))
    with open(filePath, 'w') as file:
        file.write(preparedFile)
    return preparedFile

#@pytest.mark.skip
@pytest.mark.parametrize("testDir, testFile, regenerateResult",
[
('ontospecies','ontospecies_abox_1.csv', False),
('ontocropenergy','ontocropenergy_abox_1.csv', False),
('ontokgrouter','ontokgrouter_abox_1.csv', False),
('ontolanduse','ontolanduse_abox_1.csv', False),
('ontocompchem','ontocompchem_abox_1.csv', False)
]
)
def test_csv2abox(testDir, testFile, regenerateResult, regenerateAllResults=False):
    print('========================================================')
    print('TEST DIR: ', testDir)
    print('TEST FILE: ', testFile)
    print()
    print()
    testDir= os.path.join(THIS_DIR, 'test_aboxes', testDir)
    testFile= os.path.join(testDir, testFile)
    refOWLFile= testFile+'_ref.owl'
    targetOWLFile = testFile +'.owl'
    if regenerateResult or regenerateAllResults:
        if os.path.exists(refOWLFile): os.remove(refOWLFile)
        convertFile(testFile)
        refOwlFileString = prepareFileForComparison(targetOWLFile)
        os.rename(targetOWLFile,refOWLFile)

    convertFile(testFile)
    testOwlFileString = prepareFileForComparison(targetOWLFile)

    refOwlFileString = prepareFileForComparison(refOWLFile)
    assert refOwlFileString==testOwlFileString

    os.remove(targetOWLFile)
    print()
    print()