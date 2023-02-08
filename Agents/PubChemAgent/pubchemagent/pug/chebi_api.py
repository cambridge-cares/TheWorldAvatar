from bioservices import ChEBI, logger
import logging

logging.getLogger('urllib3').setLevel(logging.CRITICAL)
logging.getLogger('requests').setLevel(logging.CRITICAL)
logging.getLogger('requests_cache').setLevel(logging.CRITICAL)
logging.getLogger('bioservices').setLevel(logging.CRITICAL)
logging.getLogger('matplotlib').setLevel(logging.CRITICAL)
logging.getLogger('suds').setLevel(logging.CRITICAL)


def chebi_request(inchi: str) -> dict:
    ch = ChEBI()
    chebi_prop = {}
    try: 
        res1 = ch.getLiteEntity(inchi, searchCategory='ALL', maximumResults=100, stars='ALL')
        k=0
        for item in res1:
            res = ch.getCompleteEntity(res1[k].chebiId)
            if res['inchi'] == inchi:
                i=0
                chebi_prop[i] = {}
                chebi_prop[i]['key']='ChebiId'
                chebi_prop[i]['type']='identifier'
                chebi_prop[i]['value']=res1[k].chebiId
                chebi_prop[i]['description']=''
                chebi_prop[i]['reference'] = "https://www.ebi.ac.uk/chebi/"
                i = i+1
                if 'OntologyParents' in res:
                    for item in res['OntologyParents']:
                        type = item['type']
                        if type == 'is a':
                            chebi_prop[i] = {}
                            chebi_prop[i]['key']='ChemicalClass'
                            chebi_prop[i]['type']='classification'
                            chebi_prop[i]['value']=str(item['chebiName'])
                            chebi_prop[i]['description']='ChEBI Classification'
                            chebi_prop[i]['chebiID']=item['chebiId']
                            i = i+1
                        elif type == 'has role':
                            chebi_prop[i] = {}
                            chebi_prop[i]['key']='Use'
                            chebi_prop[i]['type']='use'
                            chebi_prop[i]['value']=str(item['chebiName'])
                            chebi_prop[i]['description']='ChEBI Role'
                            i = i+1
                if 'OntologyChildren' in res:
                    for item in res['OntologyChildren']:
                        type = item['type']
                        if type == 'is substituent group from':
                            chebi_prop[i] = {}
                            chebi_prop[i]['key']='FunctionalGroup'
                            chebi_prop[i]['type']='group'
                            chebi_prop[i]['value']=str(item['chebiName'])
                            chebi_prop[i]['description']=''
                            chebi_prop[i]['chebiID']=item['chebiId']
                            i = i+1
                break
            k=k+1
    except Exception:
        pass
        
    return chebi_prop