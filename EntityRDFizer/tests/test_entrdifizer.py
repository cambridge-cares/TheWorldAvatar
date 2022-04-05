from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import convert_into_rdf as convertFile
import entityrdfizer.app as app
import pytest
from itertools import zip_longest
import os
import re

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

rdfRDF_re = re.compile('<rdf:RDF.*?>', flags=re.DOTALL)
namesp_re = re.compile('xmlns:(.*?=.*?http:.*?")+?', flags=re.DOTALL)
comments_re = re.compile('<!--.*?-->',flags=re.DOTALL)
nodeId_re = re.compile(':node[iI][dD]=".*?">', flags=re.DOTALL)
rdfs_comments_m_re = re.compile('<rdfs:comment.*?(</rdfs:comment>|/>)', flags=re.DOTALL)
rdfs_date_re = re.compile('<dc:date.*?(</dc:date>|/>)', flags=re.DOTALL)
rdfs_gitcommithash_re = re.compile('<gitCommitHash.*?(</gitCommitHash>|/>)', flags=re.DOTALL)
right_lt_re = re.compile('^[^<].*?>', flags=re.DOTALL)

def get_namespaces(fileString):
    #reads the <rdf:RDF ... > block and extracts the namespaces
    namespacesDict = {}
    rdfRDFBlock = rdfRDF_re.search(fileString)
    if rdfRDFBlock:
        namespaceBlock = namesp_re.findall(rdfRDFBlock.group())
        if namespaceBlock:
            for namesp in namespaceBlock:
                namespaceBlock = namesp.split('=')
                namespacesDict[namespaceBlock[0]] = namespaceBlock[1]
    return namespacesDict

def retag_namespaces(namespacesDict):
    # given:                outputs:
    #  "rdfs":"http://b"     "ns1":"nmsptag1"
    #  "ns1":"http://c"      "ns2":"nmsptag0"
    #  "ns2":"http://a"
    nmsp_array = [[tag, val] for tag, val in namespacesDict.items() if re.search('ns\d+',tag)]
    nmsp_array = sorted(nmsp_array, key=lambda x: x[1])
    retaged_nsmp_dict = {}
    i = 0
    for tab_val in nmsp_array:
        retaged_nsmp_dict[tab_val[0]] = 'nmsptag'+str(i)
        i+=1
    return retaged_nsmp_dict

def prepareFileForComparison(filePath):
    """This function expands the namespace tags and removes the
       random nodeId attribtue from the ref and test owl files.
       This is because the rdfizer csv->owl converter does not
       preserve which tag is associated to which namespace and
       also adds a random nodeId so the raw files created by
       the converter are not easily comparable."""
    with open(filePath, 'r') as file:
        fileString = file.read()
    fileString = re.sub(nodeId_re,':nodeID="testID">',fileString)
    fileString = re.sub(comments_re,'',fileString)
    # the line below strip off any rdfs:comment lines
    # uncomment if you wish to compare ontologies that
    # differ only in terms of comments / descriptions
    fileString = re.sub(rdfs_comments_m_re,'',fileString)
    # remove git commit hash and date
    fileString = re.sub(rdfs_gitcommithash_re,'',fileString)
    fileString = re.sub(rdfs_date_re,'',fileString)
    namespacesDict = get_namespaces(fileString)
    # old code to make sure that different order of namespace tags
    # wont disrupt the owl comparison. I dont like it
    # as it produces files that are hard to compare manually as all
    # namespace tags are expanded in full
    #if namespacesDict:
    #    for namespTag, namespValue in namespacesDict.items():
    #        fileString = fileString.replace(namespTag,namespValue)
    # new code to make sure that different order of namespace tags
    # wont disrupt the owl comparison. The code simply gathers all
    # owl namespaces of type ns\d+, sorts them alphabetically and retags
    if namespacesDict:
        retaged_namespaces = retag_namespaces(namespacesDict)
        for oldnmspTag, newnmspTag in retaged_namespaces.items():
            fileString = fileString.replace(oldnmspTag,newnmspTag)

    fileString = fileString.split('\n')
    # remove any empty lines
    fileStringNoEmptyLines = [line.strip() for line in fileString if line.strip()]
    # remove '>' characters if a line it appers doesnt have the openning '<' - this takes care
    # of multilne <...\n...> tags, e.g.
    # the comparison of the two example files would file if the last > wasnt removed
    # owl file 1              owl file 2
    # <rdf:RDF                <rdf:RDF
    #   xmlns:ns1="a"           xmlns:ns1="b"
    #   xmlns:ns1="b">          xmlns:ns1="a">
    fileStringNoEmptyLines = [re.sub('>$', '', line)
                              if right_lt_re.search(line)
                              else line
                              for line in fileStringNoEmptyLines]
    # now sort the lines to take care of random owl line order
    fileStringNoEmptyLines = sorted(fileStringNoEmptyLines)
    # write the prepared owl to a file for a manual inspection (if desired)

    with open(filePath+'_to_compare', 'w') as file:
        file.write('\n'.join(fileStringNoEmptyLines))

    return fileStringNoEmptyLines

def compareOwlFiles(targetOWLFile, refOWLFile):
    test_owl_lines = prepareFileForComparison(targetOWLFile)
    ref_owl_lines = prepareFileForComparison(refOWLFile)

    for test_line,ref_line in zip_longest(test_owl_lines,
                               ref_owl_lines,fillvalue=None):
        assert test_line == ref_line

#@pytest.mark.skip
@pytest.mark.parametrize("testDir, testFile, regenerateResult",
[
('ontospecies','ontospecies_abox.csv', False),
('ontocropenergy','ontocropenergy_abox.csv', False),
('ontokgrouter','ontokgrouter_abox.csv', False),
('ontolanduse','ontolanduse_abox.csv', False),
('ontocompchem','ontocompchem_abox.csv', False)
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
        app.csv2rdf_wrapper(testFile, csvType='abox', outDir=None)
        os.rename(targetOWLFile,refOWLFile)

    app.csv2rdf_wrapper(testFile, csvType='abox', outDir=None)
    compareOwlFiles(targetOWLFile, refOWLFile)

@pytest.mark.parametrize("testDir, testFile, regenerateResult",
[
('ontospecies','ontospecies_tbox.csv', False),
('ontocropenergy','ontocropenergy_tbox.csv', False),
('ontokgrouter','ontokgrouter_tbox.csv', False),
('ontolanduse','ontolanduse_tbox.csv', False),
('ontocompchem','ontocompchem_tbox.csv', False)
]
)
def test_csv2tbox(testDir, testFile, regenerateResult, regenerateAllResults=False):
    print('========================================================')
    print('TEST DIR: ', testDir)
    print('TEST FILE: ', testFile)
    print()
    print()
    testDir= os.path.join(THIS_DIR, 'test_tboxes', testDir)
    testFile= os.path.join(testDir, testFile)
    refOWLFile= testFile+'_ref.owl'
    targetOWLFile = testFile +'.owl'
    if regenerateResult or regenerateAllResults:
        if os.path.exists(refOWLFile): os.remove(refOWLFile)
        app.csv2rdf_wrapper(testFile, csvType='tbox', outDir=None)
        os.rename(targetOWLFile,refOWLFile)

    app.csv2rdf_wrapper(testFile, csvType='tbox', outDir=None)
    compareOwlFiles(targetOWLFile, refOWLFile)