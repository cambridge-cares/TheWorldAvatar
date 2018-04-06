import csv
import json
import rdflib

class OntologyWriter():
    def __init__(self, tempLoc, mapLoc, uriBase = ''):
        self.tempLoc = tempLoc
        self.mapLoc = mapLoc
        self.uriBase =uriBase
        #self.saveLoc = saveLoc
        self.address = None


    
    def write(self, jinput, inputformat ='json'):
        '''
        main function
        '''
        def processOneJsonLayer(nesteddict):
            for name,inputitem in nesteddict.items():
                print ('get attrname: '+ name)
                if name in iomap.keys():#check if this property is defined in input map
                    if isinstance(inputitem, list):#unfortunately it is a list
                        print('write for {}'.format(name))
                        outputMetas = iomap[name]
                        assert len(outputMetas) == 4
                        #print(outputMetas[1:])
                        updateStrs.extend(self.deleteSPARQLEntity(*outputMetas[1:]))

                        for listinput in inputitem:                                                    
                            inputname = listinput['name']
                            print ('write for input'+inputname)
                            ontoname = str(outputMetas[0]).replace('%', inputname)
                            print('update:')
                            newS = self.formatValWEntity(ontoname,*outputMetas[1:], listinput)

                            updateStrs.extend(newS)

                    else: 
                        updateStrs.extend( self.formatSPARQLAttr(iomap[name][0], inputitem))
                if isinstance(inputitem, dict):#the value is also a list, process next layer
                    print('process to next layer of nested json')
                    processOneJsonLayer(inputitem)            


        self.connectDB(self.tempLoc)
        iomap  = self.readMap(self.mapLoc)
        #print (iomap)
        updateStrs = []
        #for each in input, find in map, write to output
        processOneJsonLayer(jinput)

        self.sparqlUpdate(updateStrs)    
        #delete original?
        self.commit(self.tempLoc)
        

    



    def commit(self, loc, format = 'xml'):
        self.g.serialize(destination= loc, format='xml')

    def formatValWEntity(self,name, parentname, parentP, ontotype, value):
        #print ('entity:')
        strs = self.formatSPARQLEntity(name, parentname, parentP, ontotype)
        strs.extend(self.formatSPARQLAttr(name, value,writeunit = True))
        return strs


    def deleteSPARQLEntity(self, parentname, parentP, ontotype):

        uriParent  = "<"+self.uriBase +"#"+ parentname+">"
        ontotype = "<"+ontotype+">"

        template = '''
         PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
        DELETE WHERE   {{
        {0} {1} ?e . # parent p uri
        ?e a {2}.
        ?e system:hasValue ?v.
        ?v ?vp ?vo.
        ?e ?ep ?eo
        }}
        '''
        print(template.format(uriParent, parentP, ontotype))
        return [template.format(uriParent, parentP, ontotype),]

    def formatSPARQLEntity(self, name, parentname, parentP, ontotype):
        uri = "<"+self.uriBase+'#'+name+">"
        uriV = "<"+self.uriBase+'#V_'+name+">"
        uriParent  = "<"+self.uriBase +"#"+ parentname+">"
        ontotype = "<"+ontotype+">"

        template = '''
      PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
        INSERT DATA {{
        {0} {1} {2} . # parent p uri
        {2} a {3}.
        {2} system:hasValue {4}.
        }}
        '''

        return [template.format(uriParent, parentP, uri, ontotype, uriV),]

    def formatSPARQLAttr(self, name, value, writeunit = False):
        templateD = """
          PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
          DELETE WHERE {{
          <{0}> system:numericalValue ?o.
          }}
        """
        templateA = """
      PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
      PREFIX unit:<http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#>
        INSERT DATA {{
        <{0}> system:numericalValue "{1}"^^<http://www.w3.org/2001/XMLSchema#float>.
        {2}
        }}
        """

        uri = self.uriBase+'#V_'+name
        templateUnit =  """
        <{0}> a system:ScalarValue.
        <{0}> system:hasUnitOfMeasure <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#{1}>""".format(uri, value['unit'])
        unitstr = templateUnit if writeunit else ""
        return [templateD.format(uri), templateA.format(uri, value['value'], unitstr)]   
        



        ''' <owl:NamedIndividual rdf:about="http://www.theworldavatar.com/Plant-001.owl#V_ChemSpecies_1_MolecularMass">
        <rdf:type rdf:resource="http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue"/>
        <system:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#g"/>
        <system:numericalValue rdf:datatype="http://www.w3.org/2001/XMLSchema#double">0.0</system:numericalValue>
    </owl:NamedIndividual>'''

    def sparqlUpdate(self,strs):
        for str in strs:
            #print('sparql update for\n\n '+str)
            self.g.update(str)


    def readMap(self, address, type = 'csv'):
        mmap = {}
        if type == 'csv':
            mmap = self.readCSVasDict(address)
        return mmap

    def readCSVasDict(self, address):
        mymap = {}
        with open(address) as csvfile:
            readCSV = csv.reader(csvfile, delimiter=',')
            for row in readCSV:
                mymap[row[0]] = row[1:]

        return mymap
        







    def connectDB(self, address):
        if not self.sameGraph(address, self.address):
            self.address = address
            self.g = rdflib.Graph()#comment out in future
            self.g.parse(address)#comment out in future

    def sameGraph(self, uri1, uri2):
        def trimloc(uri):
            if uri is None:
                return None
            else:
                return uri.split('#')[0]
        return trimloc(uri1) == trimloc(uri2)        



if __name__ == '__main__':
    '''
    test
    '''

    print('test')
    writer  = OntologyWriter('./template/Plant-001.owl', './template/map.csv',  'http://www.theworldavatar.com/Plant-001.owl')
    #put an address here
    with open('./template/adms.json') as jsonfile:
        print('open json input file')
        writer.write(json.load(jsonfile))
    '''    
    writer  = OntologyWriter('./template/Plant-001.owl', './template/map.csv',  'http://www.theworldavatar.com/Plant-001.owl')

    writer.connectDB(writer.tempLoc)
    writer.sparqlUpdate([
         PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
        DELETE WHERE   {
        <http://www.theworldavatar.com/Plant-001.owl#Mixture_1> system:hasProperty ?e . # parent p uri
        ?e a <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate>.
        ?e system:hasValue ?v.
        ?v ?vp ?vo.
        ?e ?ep ?eo.
        }

        ])
    writer.commit(writer.tempLoc)
    '''
