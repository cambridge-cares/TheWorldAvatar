import logging
import re

from gensim import *
import nltk
from nltk.stem import WordNetLemmatizer, PorterStemmer
#from owlready2 import * #get_ontology, default_world
import owlready2
from spiral import ronin
from tqdm import tqdm

from ontomatch.utils.valueMap import *
#from ontomatch.matchers.UnitConverter import UnitConverter

logging.getLogger("gensim").setLevel(logging.CRITICAL)

class Ontology():
    def __init__(self, addr, ontology, graph, use_comment=False, no_stem=False, skip_labels=True):
        self._addr = addr
        self.ontology = ontology
        self.graph = graph
        self.useComment = use_comment
        self.tokensDict = {}
        self.tokensDictLong = {}
        self.classTree = {}
        self.rangeMap = {} #classId to range
        self.domainMap = {}
        self._load(no_stem, skip_labels)

    def _load(self, no_stem = False, skip_labels=True):
        '''
        load the ontology entities, divide into words entry
        '''
        self.procesLEX(self.ontology,no_stem)
        self.baseiri = self.ontology.base_iri
        #self.classes = list( self.ontology.classes())
        #self.properties =list( self.ontology.properties())
        #self.getRdfLevelDef()
        self.ontoName = self._addr.replace('.','')
        self.individualList, self.individualNames, self.instanceDict, self.instanceTokensDict, self.icmap, self.ipmap, self.valueMap = self.buildValueMap(skip_labels)

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
        ##print('add to dict')
        return ptokens

    def procesLEX(self,onto, no_stem = False):
        '''
        read entitie names into a dict and a vector dict
        :return:
        '''
        #form a dictionary
        self.dictionary = dictionary = corpora.Dictionary()
        self.entities = [x.name  for x in onto.classes()]
        #self.labels = [x.name+' '+' '.join(x.label) for x in onto.classes()]
        self.labels = []
        self.comments = []
        allCs = list(onto.classes())
        allPs = list(onto.properties())
        allEs = allCs+allPs
        for x in allEs:
            xcomment = list(owlready2.default_world.sparql("""
               SELECT ?v
               {{ <{}> rdfs:comment ?v. }}""".format(x.iri)))
            if len(xcomment) != 0:
                self.comments.append(' '.join(xcomment[0]))
            else:
                self.comments.append('')

            if len(x.label.en) != 0:
                xlabels =x.name+' '+' '.join(x.label.en)
                self.labels.append(xlabels)
            else:
                xlabels =x.name+' '+' '.join(x.label)
                self.labels.append(xlabels)

        #self.classTree = [[a.name for a in list(x.ancestors()) + list(x.descendants())] for x in onto.classes() ]
        self.types = ['class']*len(list(onto.classes()))+['property']*len(list(onto.properties()))
        self.entities.extend([x.name for x in onto.properties()])#+' '+' '.join(x.label.en)
        #self.labels.extend([x.name+' '+' '.join(x.label.en) for x in onto.properties()])#+' '+' '.join(x.label.en

        #for p in onto.object_properties():
        #    rangeList = p.range
        #    domainList = p.domain
        #    #print(p.range)



        for idx, tokenStr in enumerate(self.labels):
            if self.useComment:
                commentStrs = self.comments[idx]
                tokenStr = tokenStr + ' '+commentStrs
            self.tokensDict[idx]  = t = self.processLabel(str(self.entities[idx]),no_stem)
            self.tokensDictLong[idx] = self.processLabel(str(tokenStr),no_stem)
            self.dictionary.add_documents([t])


    def item2index(self, item):
        name = item.name
        return self.entities.index(name)


    def entitiesAsTxt(self):
        all = []
        #print('bowdict:')
        #print(self.tokensDict.values())
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


    def getRdfLevelDef(self):
        #self.rdfsClasses = self.getRDFSClasses(g)
        self.rdfProperties = self.getRDFProp(self.graph)


    def buildValueMap(self, skip_labels):
        '''
        Build a map [entity ID, (value, property)]
        Search is bottom up, from all literal values
        All parental entities assosciated with the value (even by several level) are included
        :return:
        '''
        ####initiation############################################################
        g = self.graph
        valuemap,idlist,namelist,instanceTokenDict,icmap,ipmap = {},[],[],{},{},{}
        instanceDict = corpora.Dictionary()
        li = self.getAllLiteralInstances(g)#search first for all triples with literal value
        ##############################################################################

        #TODO-AE 211021 HACK missing literal values (such as Bremen) if same value is object for different properties
        #vset = set()
        # vset_dict = {prop : set of values}
        vset_dict = {}
        count = 0
        for s,p,v in tqdm(li):

            #TODO-AE 211021 HACK missing literal values
            vset = vset_dict.get(p)
            if vset is None:
                vset = set()
                vset_dict[p] = vset
                logging.debug('added value set for prop=%s', p)

            #TODO-AE: BNA1866 - ask Shaocong why < 60 was required
            #if isinstance(v, rdflib.term.Literal) and len(str(v.value))<60:#filter in only Literal and string that is not too long
            if isinstance(v, rdflib.term.Literal):
                if skip_labels:
                    if hasattr(v,'language') and v.language is not None and v.language!='en':
                        continue
                    if p == rdflib.term.URIRef('http://www.w3.org/2000/01/rdf-schema#label'):
                        continue
                    if p == rdflib.term.URIRef('http://www.w3.org/2000/01/rdf-schema#comment'):
                        continue
                instanceIri = s.n3().replace('<','').replace('>','')
                name  = self.getName(instanceIri)
                clist = self.query4class(g, instanceIri)
                ###unit convert#################
                #u = self.query4unit(g, iri) #query for custom unit if exist
                #if u is not None:
                #    v = UnitConverter.unitConvert(u, v)
                ###add entity to valuemap if not exist and not bnode##
                if type(s) is rdflib.term.BNode:
                    if v in vset:
                        continue
                    else:
                        vset.add(v)
                if type(s) is not rdflib.term.BNode:
                    if instanceIri not in idlist:
                        idlist.append(instanceIri)
                        namelist.append(name)
                        id = len(idlist) - 1
                        instanceTokenDict[id] = t = self.processLabel(str(name), False)
                        instanceDict.add_documents([t])
                        valuemap[id] = []
                        icmap[id] = clist
                        ipmap[id] = [p]
                    id = idlist.index(instanceIri)
                    valuemap[id].append((p,v))
                ###add all parental entities associated to the literal to valuemap###
                tree = self.traceInstanceTree(s,g,p,v)
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
                    if(pproperty, v) not in valuemap[id]:
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
            return ''


    def query4unit(self,g, siri):
        qstr = """
        PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        SELECT ?m WHERE {{
         <{}> sys:hasUnitOfMeasure ?m.
        }}"""
        qre = g.query(qstr.format(siri))
        if list(qre) is not None and len(list(qre)) > 0:
            ##print(list(qre))
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
            ##print(list(qre))
            insclass = [self.getName(i[0].n3().replace('<','').replace('>','')) for i in list(qre)]
            #print('listclas', insclass)

            if insclass is not None:
                return insclass
        return None

    def getAllLiteralInstances(self,g):
        monto = self.ontology
        literalps = list(monto.properties())
        #literalps = self.rdfProperties
        #literalps.extend(self.rdfProperties)
        re = []
        #sys.stdout = open('propertyList.txt', 'w')
        #print(literalps)

        #for i in literalps:
        qstr = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX owl: <http://www.w3.org/2002/07/owl>
        SELECT ?s ?p ?v WHERE {
         ?s ?p ?v.
         FILTER isLiteral(?v)
        FILTER NOT EXISTS { ?s a rdfs:Class. }
         FILTER NOT EXISTS { ?s a owl:Class. }
         FILTER NOT EXISTS { ?s a rdf:Property. }
         FILTER NOT EXISTS { ?s a owl:DatatypeProperty. }
        }"""
        #if type(i) is rdflib.term.URIRef:
        #    iri = name = i.n3()
            ##print(name)
        #else:
        #    iri = '<'+i.iri+'>'
        #    name = i.name
        tere = g.query(qstr)
        #for t in list(tere):
            ##print(len(t),*t)

        re.extend([(s,p,v) for s,p,v in list(tere)])

        return re


    def traceInstanceTree(self, s,g,p,v):
        '''
        Given a instance triple, track it down until no other triples can be drawn

        :return:
        '''
        pl = []
        pset = []
        qstrBNode = """
                    SELECT  ?a ?p {} WHERE {{
                     ?a ?p ?s0.
                     {} {} {}.
                    }}"""
        def traceOne(mg, s, pChainSoFar):
            qstr = """
            SELECT  ?a ?p WHERE {{
             ?a ?p <{}>
            }}"""

            re = list(mg.query(qstr.format(s)))
            if len(re) != 0 and re[0][0].n3().replace('<','').replace('>','') not in pset:
                for res in re:
                    if res[0].n3().replace('<','').replace('>','') not in pset:
                        iri =res[0].n3().replace('<','').replace('>','')
                        p =self.getName(res[1].n3().replace('<','').replace('>',''))
                        pset.append(iri)
                        thispChain = pChainSoFar.copy()
                        thispChain.append(p)
                        pl.append((iri,thispChain))
                        #Handle p chain
                        traceOne(mg,iri, thispChain)

        def traceUntilNotBNode(mg,pOne ,v):
            re = list(mg.query(qstrBNode.format("","?s0",pOne.n3(),v.n3())))
            #Assume only one level of blank node
            if len(re) != 0 and type(re[0][0]) is not rdflib.term.BNode and  re[0][0].n3().replace('<','').replace('>','') not in pset:
                for res in re:
                    if type(res[0]) is not rdflib.term.BNode :
                        iri =res[0].n3().replace('<','').replace('>','')
                        pZero =self.getName(res[1].n3().replace('<','').replace('>',''))
                        pset.append(iri)
                        pl.append((iri,[pZero, self.getName(pOne)]))
            else:#First level still blank, go down another
                godownBlank(mg, pOne , v, 1)

        def godownBlank(mg, pOne , v ,level):
            bstr,qstr = ("","")
            for idx in range(level):
                bstr = bstr+"?s"+str(idx)+" ?p"+str(idx)+" ?s"+str(idx+1)+"."
                qstr = qstr + " ?p"+str(idx)
            bstr = bstr+" ?s"+str(level)
            re = list(mg.query(qstrBNode.format(qstr, bstr, pOne.n3(), v.n3())))
            if level >= 5:
                print("warning: deep level 5, could be lopping")
                return
            if len(re) != 0 and type(re[0][0]) is not rdflib.term.BNode and re[0][0].n3().replace('<','').replace('>','') not in pset:
                for res in re:
                    if type(res[0]) is not rdflib.term.BNode:
                        iri =res[0].n3().replace('<','').replace('>','')
                        pChain = [self.getName(someP.n3().replace('<','').replace('>','')) for someP in res[1:]]
                        #print(pChain)
                        pChain.append(self.getName(pOne))
                        pset.append(iri)
                        pl.append((iri,pChain))
            else:#Found nothing, go down one level
                godownBlank(mg,p,v,level+1)


        if isinstance(s,str):
            siri = s
        else:
            siri = s.n3()

        if type(s) is rdflib.term.BNode:
            traceUntilNotBNode(g,p,v)
        else:
            traceOne(g,siri,[])
        return pl

    def getRDFSClasses(self,mg):
        qstr = """
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT  ?c  WHERE {{
         ?c a rdfs:Class.

        }}"""
        #print('rdfsclass')
        re = list(mg.query(qstr))
        #print(re)
        return [i[0] for i in re]

    def getRDFProp(self, mg):
        qstr = """
        PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT  ?p  WHERE {{
         ?p a rdf:Property.

        }}"""
        #print('rdf:p')
        re = list(mg.query(qstr))
        #print(re)

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
    startTime = time.time()
    from ontomatch.utils.PlusImport import PlusImport
    ontologyIRI ="C:/Users/Shaocong/WORK/ontoMatchData/simMatch/testFiles/PowerPlant.owl"
    dbp = "testFiles/dbpedia_2014.owl"
    pklAddress = "D:/workwork/ontoMatchFiles/what.pkl"

    dbfiles = ['testFiles/dbpediaExtraProperty.owl', './testFiles/dbpedia_2014.owl',
               'testFiles/testOnePlant.xml'
               ]
    # todo:how to find right imports files for dbpedia?
    import owlready2
    #tempdb = PlusImport(dbfiles, './temp/tempOneplant.xml')
    ontoObject = Ontology('testFiles/1_gppdb_small.owl',False)

    print(ontoObject.valueMap.map)
    print(ontoObject.individualList)
    print(ontoObject.icmap)
    runtime = time.time()-startTime
    print(str(runtime))
        #fw = open(pklAddress, 'wb')
    # Pickle the list using the highest protocol available.
    #pickle.dump(ontoObject, fw, -1)
    #print("success")

    #for i,d in o.tokensDict.items():
    #    #print('!!!'+o.entities[i]+'    ' +str(d))
    ##print(str(len(o.entities)))
