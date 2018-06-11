import csv
import json
import rdflib
import re
class OntologyWriter():
    def __init__(self, root, mapLoc, uriBase = ''):
        self.root = root
        self.mapLoc = mapLoc
        self.uriBase =uriBase
        #self.saveLoc = saveLoc
        self.address = None


    
    def write(self, jinput, inputformat ='json'):
        '''
        main function
        '''
        #todo: process parentinfo to determine which file to write to
        #todo: add parameter to specify which file to update

        
        filelist,iomap  = self.readMapNFileList(self.mapLoc)
        print (iomap)
        #for each in input, find in map, write to output


        #todo"Connect db for each in updateresults

        updateStrs = self.process(jinput, filelist, iomap)

        for file in filelist:    
            self.connectDB(file)
            print('updating {} with {} querys:'.format(file,len(updateStrs[file])))
            self.sparqlUpdate(updateStrs[file])    
            #delete original?
            self.commit(file)
        



    def process(self, jinput, filelist, iomap):
        '''
        process the input json file
        parameters:
            inputfile
            list of files to be updated
            input output map

        returns: update strings
        '''

        updateStrs  = { file:[] for file in filelist}#init a entry to store updatestrings for each file




        def processOneJsonLayer(nesteddict, parentinfo = {'name':''}):
            '''
            process one layer of json, add results to current updatedStrs array
            params:
                nesteddict: a dict of name value pair of this layer to be processed
                parentinfo: a dict: info of parent layer
            returns:none

            '''
            #todo: might be a better way to handle this name discrepancy
            substanceNameMap = {"CO2":"Carbon__dioxide","CO":"Carbon__monoxide", "NO2":"Nitrogen__dioxide", "NOx" :"Nitrogen__oxides","HC": "Unburned_Hydrocarbon"}

            def getFunctionFile2UpdateBytype(rule):
                '''
                construct search function for typename-file to update relationship
                params:
                    rule: a dictionary {typename: filename}
                returns: the search function

                '''
                def get(key):
                    '''
                    inner closure, the search function to be constructed and return
                    '''
                    print('rule forming :{}'.format(rule))
                    if not rule.get(key):
                        raise Exception('This type {} is not defined, can not find corresponding owl file'.format(key))
                    return rule[key]
                return get

            def proceed2NextLayer(value, mparentinfo):
                if isinstance(value, dict):#the value is also a list, process next layer
                    print('process to next layer of nested json')
                    processOneJsonLayer(value, mparentinfo)

            def getUpdateAction(actiontype):
                def wrapperAttrUpdate( name, parentname, parentP, ontotype, value):
                    return self.formatSPARQLAttr(name, value)
                if actiontype == '1':#action: delete all and constr
                    return self.deleteAllFormatNew
                elif actiontype == '0':
                    return wrapperAttrUpdate  
                else:
                    raise Exception('action type {} undefined, check the io map'.format(actiontype))
                    



            for name,inputitem in nesteddict.items(): #process name, value pair of this layer of json
                fullname  = parentinfo['name'] + name
                mparentinfo = {'name' : name} 
                print ('get attrname: '+ fullname)
                if fullname in iomap.keys():#check if this attributeName is defined in input map

                    outputMetas = iomap[fullname]
                    typeAttrName =  None
                    
                    if len(outputMetas) >= 7 and outputMetas[6]!='':#if rules specified for children, passed into children info
                        print('parse updatefile chosing rule')
                        rulejson  = outputMetas[6]
                        jsonPattern = re.compile('^{.*}$')
                        if jsonPattern.match(rulejson):#The file to update isconditional,defined as a json string
                            print('update str valid json')
                            rulestr = json.loads(rulejson)
                            typeAttrName = rulestr['key']
                            mparentinfo['ruleFile2Update'] = getFunctionFile2UpdateBytype(rulestr['options'])
                        else:    
                            raise Exception("json string for conditional determination of file 2 update of [{}] is wrong in iomap".format(name))
                            

                    #determine file to update: if specifies by parent and str equals conditional use the condition, else use the name

                    file2Update, rule, typename = outputMetas[2], parentinfo.get('ruleFile2Update', None), parentinfo.get ('typeAttr', None)
                    print("file2Update:{}  rule:{} typename: {}".format(file2Update,rule, typename))
                    print ('parentinfo:{}'.format(parentinfo))

                    if file2Update == 'conditional':
                        if rule and typename:
                            file2Update = rule(typename)
                        else: 
                            raise Exception('File2update is conditional yet rule to update not defined on parent level')

                    print('write for {}'.format(name))

                    if isinstance(inputitem, list):#special case: value is a list 
                        #assert len(outputMetas) == 5
                        #print(outputMetas[1:])
                        #type check for rewrite/replace value only
                        updateFunction = getUpdateAction(outputMetas[1])

                        for listinput in inputitem:
                            print("type of lsitinput is : {}".format(type(listinput)))
                            inputname = listinput['name']
                            print ('write for input tag:   '+inputname)
                            if inputname in substanceNameMap.keys():
                                 inputname = substanceNameMap[inputname]
                            ontoname = str(outputMetas[0]).replace('%', inputname)
                            #construct update strs
                            updateStrs[file2Update].extend(updateFunction(ontoname, *outputMetas[3:6], listinput))                                                    
                            mparentinfo['listItemName'] =  inputname
                            mparentinfo['typeAttr'] = listinput.get( typeAttrName , None)

                            proceed2NextLayer(listinput, mparentinfo)                                    


                    
                    #normal case: write one attriute
                    else: 
                        attrName = outputMetas[0]       
                        if '%' in attrName:
                            print ('parentinfo:{}'.format(parentinfo))
                            print('attrname: {}'.format(attrName))
                            print (parentinfo['listItemName'])
                            attrName.replace('%', parentinfo['listItemName'])
                        updateStrs[file2Update].extend( self.formatSPARQLAttr(attrName, inputitem))
                        mparentinfo['typeAttr'] = inputitem.get( typeAttrName , None)

                        proceed2NextLayer(inputitem, mparentinfo)  

                      
        #init the recur
        processOneJsonLayer(jinput)

        return updateStrs



    def deleteAllFormatNew(self,name, parentname, parentP, ontotype, value):
        strs = self.deleteSPARQLEntity(parentname, parentP, ontotype)
        strs.extend(self.formatValWEntity(name, parentname, parentP, ontotype, value))
        return strs

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


    def readMapNFileList(self, address, type = 'csv'):
        mmapNFileList = {}
        if type == 'csv':
            mmapNFileList = self.readCSVasDict(address)
        return mmapNFileList

    def readCSVasDict(self, address):
        mymap = {}
        with open(address) as csvfile:
            readCSV = csv.reader(csvfile, delimiter=',')
            first = True
            for row in readCSV:
                if first:
                    first = False
                    print(row)
                    filelist = json.loads(row[0])
                    print("filelist :{}".format(filelist))
                else:    
                    mymap[row[0]] = row[1:]

        return (filelist, mymap)
        







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
    writer  = OntologyWriter('./', './template/mapv1.csv',  'http://www.theworldavatar.com/Plant-001.owl')
    #put an address here
    with open('./template/admsv1.json') as jsonfile:
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
