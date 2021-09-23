#utlity function
import requests
import rdflib.plugins.sparql.results.jsonresults as jsresult
from rdflib import Graph, URIRef, Namespace, RDF, RDFS
import json
import codecs
import nltk
from spiral import ronin
from nltk.stem import WordNetLemmatizer, SnowballStemmer
from owlready2 import *

class getOwlClassNames(object):
    def __init__(self,addr):
        #nltk.download('wordnet')
        self.address = addr
        self.onto = get_ontology(addr).load()
        self.dict = {}
        self.importSet = set()

        pass    

    def getClassKeywords(self):
        return self.processLabels(self.onto.classes())

    def getPropertyKeywords(self):
        op = list(self.onto.object_properties())
        dp = list(self.onto.data_properties())
        op.extend(dp)
        print(op)

        return self.processLabels(op)


    def getImports(self):
        pass


    def getAllKeywords(self):
        all = self.getClassKeywords()+self.getPropertyKeywords()
        print(all)
        return all


    def getRawClasses2(self, addr):
        #parse as rdf, extract classes
        self.connectDB(addr, 'parse')
        classes = self.getAllClass()
        base = self.getBase()
        print(base)
        print(classes)
        def replaceBase(str, blist):
            for b in blist:
                str = str.replace(b, "")
            return str    

        base.extend(["#", "/"])
        classes = [ replaceBase(aclass, base) for aclass in classes]
        print(classes)
        return classes
        

    @staticmethod
    def lemmatize_stemming(text):
        #return SnowballStemmer("english").stem(WordNetLemmatizer().lemmatize(text, pos='v'))
        return WordNetLemmatizer().lemmatize(text, pos='v')


    def processLabels(self, labels):
        #split:'helloworld' = >['hello', 'word']
        #stopword + <3characters removed + lemmatized + stemmed
        #form dict    
        allwords = []

        for label in labels:
            label = str(label)
            prefix = label.split('.')[1]
            words= ronin.split(prefix)# 'helloworld' = >['hello', 'word']
            #print(words)
            ptokens = [ getOwlClassNames.lemmatize_stemming(word.lower()) for word in words if len(word)>3 and word not in nltk.corpus.stopwords.words("english") ]
            allwords.extend(ptokens)
            #print('add to dict')
            self.dict[label] = ptokens

        return list(allwords)
            



    def getBase(self):
        baseq = self.query('''
       PREFIX owl:<http://www.w3.org/2002/07/owl#>
            SELECT DISTINCT  ?base ?inherit
            WHERE {
            {?base a owl:Ontology.}
            UNION{
             ?a owl:imports ?base. 
            }
            }
            ''')

        base = [row['base'].toPython() for row in baseq]
        print(base)
        return base


    def filterByLabel(self, g):
        #for each class in g, compare
        #keep all nodes that passes the test
        pass 
    


    def filterByGraph(self, g):
        #tranverse whole graph&generate hypo of same nodeNum(size)
        #compare graph to graph
        #pick highest score hypothesis
        #q: why keep only one?
        pass





    def searchEntity(self, name):
        #re = self.onto.search(iri="*"+name+"*")
        #print('searched result:')
        #print(re)
        re = []
        #print(self.dict)
        for entity in self.dict.keys():
            if name in self.dict[entity]:
                re.append( (entity,self.dict[entity] ))


        return re


    def deleteEntity(self, name):
        destroy_entity(name)




    def save(self, content, format = 'txt'):
        with open('db.txt', 'w') as f:
            f.write(content)


            
    def json2owl(self, results):


        def add(pairs, type):
            print(type)
            if type is 'class':
                oType = OWL.Class
                pType = RDFS.subClassOf
                for pair in pairs:
                    store.add((URIRef(self.wikibase+pair[0]), pType, URIRef(self.wikibase+pair[1])))
                    store.add( (URIRef(self.wikibase+pair[0]) , RDF.type, oType ))
                    store.add( (URIRef(self.wikibase+pair[1]) , RDF.type, oType ))
            elif type is 'property':
                oType = OWL.ObjectProperty 
                pType = RDFS.domain   
                for pair in pairs:
                    store.add((URIRef(self.wikibase+pair[1]), pType, URIRef(self.wikibase+pair[0])))#sequence matters
                    store.add( (URIRef(self.wikibase+pair[1]) , RDF.type, oType ))
            else:
                return 

        store = Graph()
        #replace wikidata label to owl label, then write to a owl file
        OWL = Namespace('http://www.w3.org/2002/07/owl#')
        store.bind('owl', 'http://www.w3.org/2002/07/owl#')
        for result in results:
            add(result[0], result[1])
        store.serialize("wikidata.rdf", format="pretty-xml", max_depth=3)


    def extractData(self, jsonData):
        pass

    def queryEndpoint(self, str):
        def literal2TLit(sparqlres):

            if 'results' not in sparqlres:
                return
            for row in sparqlres['results']['bindings']:
                for name,value in row.items():
                    if value['type'] == 'literal' and 'datatype' in value:
                        value['type'] = "typed-literal"

        print('requesting @ '+self.address+" with query:")
        #print(str)
        resp = requests.get(self.address, params = {'query':str, "format":"json"}, timeout = 15000, headers = {'user-agent': 'my-app/0.0.1'})
        print('raw resp:')

        raw = resp.content
        print(raw)
        result = json.loads(raw.decode('utf-8'))

        literal2TLit(result)

        print(result)
        qres = jsresult.JSONResult(result)#json decoded
        print('after parse:')
        print(qres)
        return qres


    
    def queryLocalGraph(self, str): 
        qres = self.g.query(str)
        return qres

    def Uri2Local(uri):
        '''replace a uri to local address
        inputs:
        uri - uri to be changed
        returns: string - local address
        '''
        return uri.replace("http://www.jparksimulator.com",config.root ).replace("http://www.theworldavatar.com",config.root)

    def remote2local(self, func):
        '''decorator to change connection function to local connection by replacing given iri to local address
        '''
        def functionWrapper(self, address):
            address = self.Uri2Local(address)
            func(self, address)
        return functionWrapper    
    
    def connectDB(self, address, connectType = 'endpoint'):
        '''connect to db anyhow (web use rdflib graph parse now)
        '''
        def connectDBActual( address):
            '''
            Actual method to connect to db
            '''
            #obsolete: use rdflib locally
            self.address = address
            if connectType is 'parse':
                self.g = Graph()#comment out in future
                self.g.parse(address)#comment out in future


        self.qmethodMap = {'parse': self.queryLocalGraph, 'endpoint':self.queryEndpoint}

        if not sameGraph(address, self.address):
            print ('parsing graph: '+ address)
            if connectType not in self.qmethodMap:
                raise exception('db connection method not defined')
            #self.connectType = connectType
            self.query = self.qmethodMap[connectType]
            connectDBActual(address)                 
       

        
               


def sameGraph(uri1, uri2):
    def trimloc(uri):
        if uri is None:
            return None
        else:
            return uri.split('#')[0]
    return trimloc(uri1) == trimloc(uri2)


def uri2name(uri):
    base = 'http://www.theworldavatar.com/'
    return uri.split('#')[1]

if __name__ == "__main__":
    onto_path.append("C:/Users/Shaocong/WORK/ontoMatchData/")
    q = getOwlClassNames("C:/Users/Shaocong/WORK/ontoMatchData/ontology/PowerPlant.owl").getAllKeywords()
    #p =getOwlClassNames("file://C:/Users/Shaocong/WORK/ontoMatchData/dbpedia_2014.owl").getAllKeywords()
    content = ' '.join(q)
    #contentp = ' '.join(p)
    with open('powerplantclasses.txt', 'w') as f:
        f.write(content)
    #with open('dbpediaclasses.txt', 'w') as f:
    #    f.write(contentp)