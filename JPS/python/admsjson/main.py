import config
import OntologyWriter 
import json

def SRM2OntoConvert(fileloc, IRI, parseddata):
    writer = OntologyWriter.OntologyWriter(fileloc, config.iomapLocation, IRI)
    writer.write(parseddata)




if __name__ == '__main__':
    '''
    test
    '''
    with open('./template/adms.json') as jsonfile:
        print('open json input file')
        SRM2OntoConvert('./template/Plant-001.owl','http://www.theworldavatar.com/Plant-001.owl', json.load(jsonfile))