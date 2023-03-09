from spiral import ronin
from nltk.stem import WordNetLemmatizer, PorterStemmer
from owlready2 import *
import nltk
from gensim import *
import re
import os
from valueMap import *
from matchers.UnitConverter import UnitConverter
import pickle

class Ontology():
    def __init__(self, addr,no_stem = False):
        print('init ',addr)
        self.tokensDict = {}
        self.classTree = {}
        self._addr = addr
        self.rangeMap = {}#classId to range
        self.domainMap = {}
        self._load(no_stem)
        self.getRdfLevelDef()
        self.individualList, self.individualNames, self.instanceDict, self.instanceTokensDict,self.icmap, self.ipmap,self.valueMap = self.buildValueMap()




    def _load(self,no_stem = False):
        '''
        load the ontology entities, divide into words entry
        '''

        onto = get_ontology(self._addr).load()
        self.procesLEX(onto,no_stem)
        self.baseiri = onto.base_iri
        self.importStack = [ o.base_iri for o in onto.imported_ontologies]
        self.imports = []
        self._loadImportsRecur()
        #self.classes = list( onto.classes())
        #self.properties =list( onto.properties())



    def _loadImportsRecur(self):
        while len(self.importStack)!=0:
            toImport = self.importStack.pop()
            self.imports.append(toImport)
            importO = get_ontology(toImport).load()
            importI = [ o.base_iri for o in importO.imported_ontologies]
            for item in importI:
                if item not in self.imports and item not in self.importStack:
                    self.importStack.append(item)



    @staticmethod
    def lemmatize_stemming(text, no_stem = False):
        '''
        lemmatize:  remove verb form
        stemming:   to word stem
        :param text:
        :param no_stem: bool, do stem or not
        :return: token after lem&stem
        '''
        if no_stem:
            return WordNetLemmatizer().lemmatize(text, pos='v')
        else:
        #return SnowballStemmer("english").stem(WordNetLemmatizer().lemmatize(text, pos='v'))
            return PorterStemmer().stem(WordNetLemmatizer().lemmatize(text, pos='v'))


    def processLabel(self, label, no_stem = False):
        '''
        process single label into lem&stemed tokens
        :param label:
        :param no_stem:
        :return:
        '''
        #split:'helloworld' = >['hello', 'word']
        #stopword + <3characters removed + lemmatized + stemmed
        label = str(label)
        words= ronin.split(label)# 'helloworld' = >['hello', 'word']
        pattern = re.compile("^[a-zA-Züä]+$")
        ptokens = [ Ontology.lemmatize_stemming(word.lower(),no_stem) for word in words if len(word)>3 and word not in nltk.corpus.stopwords.words("english") and pattern.match(word) is not None ]
        #allwords.extend(ptokens)
        #print('add to dict')
        return ptokens

    def procesLEX(self,onto, no_stem = False):
        '''
        read entitie names into a dict and a vector dict
        :return:
        '''
        #form a dictionary
        self.dictionary = dictionary = corpora.Dictionary()
        self.entities = [x.name  for x in onto.classes()]
        self.labels = [x.name+' '+' '.join(x.label.en)  for x in onto.classes()]
        #self.classTree = [[a.name for a in list(x.ancestors()) + list(x.descendants())] for x in onto.classes() ]
        self.types = ['class']*len(list(onto.classes()))+['property']*len(list(onto.properties()))
        self.entities.extend([x.name for x in onto.properties()])#+' '+' '.join(x.label.en)
        self.labels.extend([x.name+' '+' '.join(x.label.en) for x in onto.properties()])#+' '+' '.join(x.label.en

        for p in onto.object_properties():
            rangeList = p.range
            domainList = p.domain

            '''

            idP = self.item2index(p)

            for cls in rangeList:
                if cls is None:
                    continue
                idC = self.item2index(cls)
                if idC in me.rangeMap:
                    me.rangeMap[idC].append(idP)
                else:
                    me.rangeMap[idC] = [idP]
            for cls in domainList:
                if cls is None:
                    continue
                idC = self.item2index(cls)
                if idC in me.domainMap:
                    me.domainMap[idC].append(idP)
                else:
                    me.domainMap[idC] = [idP]
            '''


        for idx, item in enumerate(self.labels):
            self.tokensDict[idx] = t = self.processLabel(str(item),no_stem)#get tokens for each
            self.dictionary.add_documents([t])


    def item2index(self, item):
        name = item.name
        return self.entities.index(name)


    def entitiesAsTxt(self):
        all = []
        for tokens in self.tokensDict.values():
            all.extend(tokens)
        return ' '.join(all)

    def name2id(self,name):
        if name not in self.entities:
            return None
        return self.entities.index(name)



    def typeCheck(self,name, type):
        '''
        check entity type(class/property) of certain entity
        :param name:
        :return:
        '''
        idx = self.entities.index(name)
        return self.types[idx] == type


    def queryG(self, qstr):
        onto = get_ontology(self._addr).load()
        graph = onto.as_rdflib_graph()
        return list(graph.query(qstr))


    def getRdfLevelDef(self):
        g = rdflib.Graph()
        g.parse(self._addr)
        self.rdfsClasses = self.getRDFSClasses(g)
        self.rdfProperties = self.getRDFProp(g)


    def buildValueMap(self):
        '''
        Build a map [entity ID, (value, property)]
        Search is bottom up, from all literal values
        All parental entities assosciated with the value (even by several level) are included
        :return:
        '''
        ####initiation############################################################
        g = rdflib.Graph()
        g.parse(self._addr)
        valuemap,idlist,namelist,instanceTokenDict,icmap,ipmap = {},[],[],{},{},{}
        instanceDict = corpora.Dictionary()
        li = self.getAllLiteralInstances(g)#search first for all triples with literal value
        ##############################################################################
        for s,p,v in li:
            if isinstance(v, rdflib.term.Literal):#filter in only Literal
                iri = s.n3().replace('<','').replace('>','')
                name  = self.getName(iri)
                clist = self.query4class(g, iri)
                ###unit convert#################
                u = self.query4unit(g, iri) #query for custom unit if exist
                if u is not None:
                    v = UnitConverter.unitConvert(u, v)
                ###add entity to valuemap if not exist##
                if iri not in idlist:
                    idlist.append(iri)
                    namelist.append(name)
                    id = len(idlist) - 1
                    instanceTokenDict[id] = t = self.processLabel(str(name), False)
                    instanceDict.add_documents([t])
                    valuemap[id] = []
                    icmap[id] = clist
                    ipmap[id] = [p]
                id = idlist.index(iri)
                valuemap[id].append((p,v))
                ###add all parental entities associated to the literal to valuemap###
                tree = self.traceInstanceTree(s,g)
                for parent,pproperty in tree:
                    if parent not in idlist:#parent is not recorded yet
                        idlist.append(parent)
                        pname = self.getName(parent)
                        namelist.append(pname)
                        id = len(idlist) - 1
                        instanceTokenDict[id] = t = self.processLabel(str(pname), False)
                        instanceDict.add_documents([t])
                        valuemap[id] = []
                        pclist = self.query4class(g, parent)
                        icmap[id] = pclist
                        ipmap[id] = [pproperty]
                    id = idlist.index(parent)
                    valuemap[id].append((pproperty, v))


        return idlist,namelist,instanceDict,instanceTokenDict,icmap, ipmap, valueMap(idlist,valuemap)


    def getName(self,iri):
        if '#' in iri:
            s= iri.split('#')
            return s[len(s)-1]
        elif '/' in iri:
            s = iri.split('/')
            return s[len(s)-1]
        else:
            return iri


    def query4unit(self,g, siri):
        qstr = """
        PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> 
        SELECT ?m WHERE {{
         <{}> sys:hasUnitOfMeasure ?m.
        }}"""
        qre = g.query(qstr.format(siri))
        if list(qre) is not None and len(list(qre)) > 0:
            #print(list(qre))
            unit = list(qre)[0][0].n3().replace('<','').replace('>','')

            if unit is not None:
                return unit
        return None


    def query4class(self,g, siri):
        qstr = """
        PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> 
        PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>

        SELECT ?c WHERE {{
         <{}>  a ?c.
        }}"""
        qre = g.query(qstr.format(siri))
        if list(qre) is not None and len(list(qre)) > 0:
            #print(list(qre))
            insclass = [self.getName(i[0].n3().replace('<','').replace('>','')) for i in list(qre)]
            print('listclas', insclass)

            if insclass is not None:
                return insclass
        return None

    def getAllLiteralInstances(self,g):
        monto = get_ontology(self._addr).load()
        literalps = list(monto.properties())
        literalps.extend(self.rdfProperties)
        #print(self.rdfProperties)
        #print(literalps)
        re = []
        for i in literalps:
            qstr = """
            PREFIX owl:<http://www.w3.org/2002/07/owl#> 
            SELECT ?s ?p ?v WHERE {{
             ?s {} ?v.
             FILTER NOT EXISTS {{ ?s a owl:Ontology. }}

            
            }}"""
            if type(i) is rdflib.term.URIRef:
                iri = name = i.n3()
                #print(name)
            else:
                iri = '<'+i.iri+'>'
                name = i.name
            tere = g.query(qstr.format(iri))
            #for t in list(tere):
                #print(len(t),*t)

            re.extend([(s,name,v) for s,p,v in list(tere)])
        return re


    def traceInstanceTree(self, s,g):
        '''
        Given a instance triple, track it down until no other triples can be drawn

        :return:
        '''

        pl = []
        pset = []
        def traceOne(mg, s):
            qstr = """
            SELECT  ?a ?p WHERE {{
             ?a ?p <{}>
            }}"""
            #print(qstr.format(s.n3()))
            re = list(mg.query(qstr.format(s)))
            if len(re) is not 0 and re[0][0].n3().replace('<','').replace('>','') not in pset:
                iri =re[0][0].n3().replace('<','').replace('>','')
                p =self.getName(re[0][1].n3().replace('<','').replace('>',''))
                pset.append(iri)
                pl.append((iri,p))
                traceOne(mg,iri)



        if isinstance(s,str):
            siri = s
        else:
            siri = s.n3()
        traceOne(g,siri)
        return pl

    def getRDFSClasses(self,mg):
        qstr = """
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT  ?c  WHERE {{
         ?c a rdfs:Class.

        }}"""
        print('rdfsclass')
        re = list(mg.query(qstr))
        print(re)
        return [i[0] for i in re]

    def getRDFProp(self, mg):
        qstr = """
        PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
        SELECT  ?p  WHERE {{
         ?p a rdf:Property.

        }}"""
        print('rdf:p')
        re = list(mg.query(qstr))
        print(re)

        return [i[0] for i in re]


    def __getstate__(self):
        odict = self.__dict__.copy() # copy the dict since web change it

        return odict

    def __setstate__(self, d):
        self.__dict__ = d




'''
    def entitiesAsListToken(self):
        l = []
        for t in self.bowDict.values():
            l.extend(t)
        return l
'''






if __name__ == '__main__':

    #addr = "C:/Users/Shaocong/WORK/ontoMatchData/simMatch/test/germany/Altbach_Coal_Power_Plant_Germany.owl"
    #addr2 = "http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl"
    #addr3 = "http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl"

    #temp = PlusImport( [addr,addr2, addr3])
    #on = Ontology(temp.name)
    #print('edn')

    #for i,d in o.tokensDict.items():
    #    print('!!!'+o.entities[i]+'    ' +str(d))
    ##print(str(len(o.entities)))
    #ontologyIRI = sys.argv[1]
    #pklAddress = sys.argv[2]
    ontologyIRI = "D:/workwork/ontoMatchFiles/ppbatch/jpsppbatch7.rdf"
    pklAddress = "D:/workwork/jpslatest/JParkSimulator-git/JPS_ONTOMATCH/tmp/jpsppbatch7pt.pkl"

    ontoObject = Ontology(ontologyIRI)
    try:
        fw = open(pklAddress, 'wb')
        # Pickle the list using the highest protocol available.
        pickle.dump(ontoObject, fw, -1)
        print("success")
    except Exception as e:
        print(str(e))

