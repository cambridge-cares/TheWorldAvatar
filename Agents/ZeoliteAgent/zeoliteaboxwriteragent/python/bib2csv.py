import os
import logging
logging.basicConfig( level=logging.WARNING )

import sys
sys.path.append( 'biblib' )
import biblib.bib
#import biblib.biblib as biblib
import tools

"""
TODO
- 
- 
.

"""

import bibtexparser
import bibtexparser.middlewares as btpmiddle

"""
BibLib library: 
https://github.com/aclements/biblib/tree/master/examples
Important: there are 2 different BibLib libraries. Don't use 'pip install biblib':
https://gerardmjuan.github.io/2019/08/06/working-with-bibtex-python/
There is a problem: the library is old, and not fully compatible with python, 
for example collections.Iteratable. But I fixed it.


"""

"""
For journal abbreviation I use 'journals.json' from
this database of Journals and their abbreviations (updated around 2020-2021):
https://github.com/jxhe/bib-journal-abbreviation
"""
import json

prefixFOAF = "foaf"
prefixBIBO = "bibo"

#biboPrefix = "http://bibo/"
biboPrefix = "http://purl.org/ontology/bibo/"

foafPrefix = "http://xmlns.com/foaf/0.1/"
crystPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"

def is_http( value ):
    if value.strip().lower().startswith( "http://" ) or \
        value.strip().lower().startswith( "https://" ):
        return True
    return False

class OntoBibo:
    """
    I use biblib as the parser of the bib files.
    Input for biblib.parse() is a stream (need to open the file in advance).
    biblib has classes: 
       Parser which is a dict() of Entries.
       Entry  which is a dict() containing a single citation/article/book/etc.

    """
    __slots__ = [ "itemName", "className", "uuidDB", "errCount", 
                  "tPrefix", "aPrefix", "entries", "entryInBibFile", "btpLibrary",
                  "journalAbbreviations", "journalFullLow", "journalAltSpell", 
                  "journalShortLow", "foreignWords",
                  "fieldCountNote", "fieldCountLastchecked", 
                  "uuid" ]
    #__slots__ = [ "year", "month", "title", "journal", "volume", "number", \
    #              "pages", "abstract", "issn", "doi", "eprint" \
    #]
    def __init__( self, filePath = [], uuidDB = None,
                  tPrefix = "", aPrefix = ""  ):
        '''
        self.year    = None
        self.month   = None
        self.title   = None
        self.journal = None
        self.volume  = None
        self.number  = None
        self.pages   = None
        self.abstract = None
        self.issn    = None
        self.doi     = None
        self.eprint  = None
        #self.pages   = None
        '''
        self.itemName = "TODO_ITEM_NAME"
        self.className = "TODO_CLASS_NAME"
        self.uuidDB = None
        self.errCount = 0

        if "" == tPrefix:
            self.tPrefix = "http://tPrefix/"
        else:
            self.tPrefix = tPrefix

        if "" == aPrefix:
            self.aPrefix = "http://aPrefix/"
        else:
            self.aPrefix = aPrefix

        if None == uuidDB:
            logging.warning( " Empty uuidDB in '" + self.itemName + "'" + \
                           " in class OntoMeasureWithUncertainty." + \
                           " Using default." ) 
            self.uuidDB = tools.UuidDB()
        elif isinstance( uuidDB, tools.UuidDB ):
            self.uuidDB = uuidDB
        else:   
            logging.error( "Wrong type of uuidDB, expected 'UuidDB', got '" + \
                            str(type(uuid)) + "'. Using default instead." )
            self.uuidDB = tools.UuidDB()
            pass

        logging.error( "Not implemented _checkBibFile() in ontobibo " )

        self.entries = {}
        self.entryInBibFile = {}
        self.btpLibrary = bibtexparser.Library() # btp = Bib Tex Parser

        if [] == filePath:
            # Do nothing
            pass
        elif isinstance( filePath, list ):
            for fPath in filePath:
                self.readBib( fPath )
        elif isinstance( filePath, str ):
            self.readBib( filePath )
        else:
            logging.error( " Unknown file path in OntoBibo: '" + str(filePath) + "'." )

        self.journalAbbreviations = {}
        journals = [ "journals.json", "more-journals.json" ]
        for path in journals:
            self.loadJournal( path )

        self.updateJournalDB()

        #logging.error( ">>>>>>>>>>>>>>>>>>>>>>>" )
        #for k in self.entryInBibFile.keys():
        #    logging.error( "  > " + k )

        pass # OntoBibo.__init__()

    def loadJournal( self, path ):
        if os.path.isfile( path ):
            with open( path, encoding="utf-8" ) as fp:
                tmp = json.load( fp )
                self.journalAbbreviations |= tmp
        else:
            logging.error( "Unknown journal database: '" + path + "' in OntoBibo." )
            self.errCount += 1

        pass #OntoBibo.loadJournal()

    def updateJournalDB( self ):
        self.journalFullLow  = {}
        self.journalAltSpell = {}
        self.journalShortLow = {}
        self.foreignWords = {}

        for k in self.journalAbbreviations.keys():
            self.journalFullLow[k.lower().strip()] = k.strip()
            self.journalShortLow[self.journalAbbreviations[k].lower().strip()] = k.strip()

        path = "journal-data.json"
        if os.path.isfile( path ):
            with open( path, encoding="utf-8" ) as fp:
                tmp = json.load( fp )
                self.foreignWords = tmp["foreign"]
                for k in tmp["alternative"]:
                    self.journalAltSpell[k.lower()] = tmp["alternative"][k]

                self.foreignWords = tmp["foreign"]

        """
        print( "full low:" )
        for k in self.journalFullLow:
            #if k.find( "araday" ) >= 0:
            if k.find( "olites" ) >= 0:
                print( "   ", k, "=>", self.journalFullLow[k] )
            pass

        1/0
        print( "short low:" )
        for k in self.journalShortLow:
            if k.find( "araday" ) >= 0:
                print( "   ", k, "=>", self.journalShortLow[k] )
            pass

        print( "alt spell: " )
        for k in self.journalAltSpell:
            print( "   ", k, "=>", self.journalAltSpell[k] )
            pass
        1/0
        """
        #logging.error( " No foreignWords, no AltSpell " )

        pass #OntoBibo.updateJournalDB()
       

    #def addBib( self, filePath ):
    #    #self.btpLibrary.add( filePath )I
    #    pass # .addBib()

    def readBib( self, filePath ):
        """
        Reading a .bibtex file.
        If filePath is a filename with one or more bib entries,
                    the function return a list of all bib entries.
        If filePath is a directory, then all ".bib" files in this directory 
                    and its subdirectories are loaded, 
                    the function returns a list of bib OntoBibo.
        Empty list is returned on error.

        """
        #Bib file may contain several entries, so this function returns a list containing a number of entities of 

        if not isinstance( filePath, str ):
            logging.error( "File path must be a string, but got '" + \
                           str(filePath) + "' in bib2csv." )
            self.errCount += 1                
            return 

        if os.path.isdir( filePath ):
            print( "filePath", filePath )
            paths = os.listdir( filePath )
            for path in paths:
                self.readBib( path )

        elif os.path.isfile( filePath ):
            _, ext = os.path.splitext( filePath )
            if ".bib" != ext:
                logging.error( " In readBib() in file '" + filePath + "'," + \
                               " but expected extension .bib. Skipped." )
                self.errCount += 1                
                return                

            #self._readAsBibLib( filePath )

            self._checkBibFile( filePath )
            self._readAsBibtexParser( filePath )

        else:
            logging.error( " In bib2csv file '" + filePath + "' does not exist." )
            self.errCount += 1                
            return 

        pass # OntoBibo.readBib()

    def _readAsBibLib( self, filePath ):
        with open( filePath, encoding="utf-8" ) as fp:
            p = biblib.bib.Parser().parse( fp, log_fp=sys.stderr ).get_entries()
            p = biblib.bib.resolve_crossrefs( p )
            self.entries |= p

        pass # OntoBibo._readAsBibLib()

    def _checkBibFile( self, filePath ):
        """
        There is a problem in bibtex.parser if author and others contain capital letters.

        Need to read the file, find all entries names and list them.

        """


        pass # OntoBibo._checkBibFile()

    def _readAsBibtexParser( self, filePath ):

        layers = [
            btpmiddle.MonthIntMiddleware(True), # Months should be represented as int (0-12)
            btpmiddle.SeparateCoAuthors(True),  # Co-authors should be separated
            btpmiddle.SplitNameParts(True)      # Names should be split into first, von, last, jr parts
                 ]
        library = bibtexparser.parse_file( filePath, append_middleware=layers)

        for entry in library.entries:
            #print( "====", entry.key )
            if entry.key in self.entryInBibFile:
                logging.error( " Bibliography entry repeated in '" + \
                               filePath + "', previous definition in '" + \
                               self.entryInBibFile[entry.key] + "'." )
                self.errCount += 1
            else:
                self.entryInBibFile[entry.key] = filePath
        #for k in self.entryInBibFile:
        #    logging.error( "  - " + k )

        self.btpLibrary.add( library.entries )

        pass # OntoBibo._readAsBibtexParser()

    def _getCsvArrBibtexParser( self, subject, predicate, options ):
        errCount = 0
        output = []

        self.fieldCountNote = 0   
        self.fieldCountLastchecked = 0   

        for i, ent in enumerate(self.btpLibrary.entries):
            #print ( "entry", i )
            if "article" == ent.entry_type:
                logging.info( " Loading an article " + str(i+1) )
                out,err = self._csvBtpArticle( ent, subject, predicate, options )
                output += out
                errCount += err
                pass

            # TODO other types of entries
            else:
                logging.error( " Unknown citation type '" + ent.entry_type + "'" )

        return output, errCount
        pass # OntoBibo._getCsvArrBibtexParser()

    def pagesToStartEnd( self, pages ):
        start = None
        end   = None

        if not isinstance(pages, str):
            return start, end

        pos = pages.find( "-" )
        if pos < 0:
            # No '-', i.e. pages is pagesStart
            start = pages.strip()
        elif pos >= 0:
            start = pages[:pos].strip()
            end = pages[pos+1:]
            end = end.replace("-","").strip()

        if "" == start:
            end = None
        if "" == end:
            end = None

        return start, end
        pass # OntoBibo.pagesToStartEnd()

    def toStandardJournalName( self, name ):
        """ The journal name can be corrupted by the following 
        1) Capital to low and vice versa
        2) Misprint (extra comma, quote, etc)
        3) Wrond spelling of the journal
        4) Using short journal name instead of full name 
        5) Using foreign letters instead of English, like German fur
        6) 
        """
        # The 
        output = name.strip()

        #print( "  aaa:", output )
        if output.lower() in self.journalFullLow:
            # Case (1) Correction of 
            output = self.journalFullLow[output.lower()]

        #print( "  aaa:", output )
        if output.lower() in self.journalShortLow:
            # Case (4) Already in short form of the journal
            output = self.journalShortLow[output.lower()]

        #print( "  aaa:", output )
        if output.lower() in self.journalAltSpell:
            # Case (2) Misprint (comma, quote, etc)
            # Case (3) Misspelling
            #print( "Found alt spelling:", output )
            output = self.journalAltSpell[output.lower()]
            #print( "     New spelling = ", output )

        for word in self.foreignWords:
            # Case (5) foreign letters instead of English
            #print( "  Before:", output )
            output = output.replace( word, self.foreignWords[word] )
            #print( "   After:", output )

        return output
        pass # OntoBibo.toStandardJournalName()

    def getIssueInfo( self, entity ):

        jFull = None
        jAbbr = None
        vol   = None
        journal = "None"

        #print( " Missing journal for ", entity.key )
        #print( "datatype of fields:", type( entity.fields ) )
        for f in entity.fields:
            #print( "key:", f.key )
            if "journal" == f.key.lower():
                #print( "Assigned journal", f.value )
                journal = f.value
                if journal in self.journalAbbreviations:
                    jFull = journal
                    jAbbr = self.journalAbbreviations[journal]
                    #print( "a know entry", jAbbr )
                else:
                    jNew  = self.toStandardJournalName( journal )
                    if jNew in self.journalAbbreviations:
                        jFull = jNew
                        jAbbr = self.journalAbbreviations[jNew]
                    else:
                        file = self.entryInBibFile[entity.key]
                        logging.error( " Unknown journal name in bib file: '" + \
                                     journal + "' in bib entry '" + file + "'." )
                        #print (      "jNew = ", jNew )
                        self.errCount += 1

                        jFull = journal
                        jAbbr = journal


            elif "volume" == f.key.lower():
                #print( "    ", f.value )
                vol = str(f.value)
                #break

        if None == jFull:
            logging.error( " Unknown journal full name " + journal )
            self.errCount += 1

        if None == jAbbr:
            logging.error( " Unknown journal abbreviation " + journal )
            self.errCount += 1

        if None == vol:
            logging.error( " In bib entry '" + entity.key + "' volume is not specified" )
            vol = "None"
            self.errCount += 1

        return jFull, jAbbr, vol
        pass # OntoBibo.getIssueInfo()

    def _csvBtpArticle( self, entity, subject, predicate, options ):
        output = []
        errCount = 0

        #print( "Starting key: ",  entity.key )

        """
    print( "type: ", library.entries[0].entry_type )
    k = library.entries[0].key
        print( "Starting key: ",  library.entries[0].key )
    print( type( library.entries[0].fields ) )
    #print( library.entries[0].fields )
    for f in library.entries[0].fields:
        if "author" == f.key:
            for a in f.value:
              print( f.key, ":", a.first, a.last )
        else:
            print( f.key, ":", f.value )
 
        """
        dctPrefix = "http://purl.org/dc/terms/"
        itemName = entity.key
        className = "AcademicArticle"

        self.uuid, uuidStr = self.uuidDB.addUUID( biboPrefix + className,
                                                self.aPrefix + "Cite_" + itemName )
        output.append( [ self.uuid, "Instance", biboPrefix + className, "", "", "" ] )
        output.append( [   subject, "Instance", self.uuid, predicate, "", "" ] )

        # Issue of the Journal:
        # The name consists of the abbreviated journal name, the volume, and uuid:
        # "J.Name"_"V"str(volume)_UUID
        jFull, jAbbr, vol = self.getIssueInfo( entity )
        #print( f"'{jFull}', '{jAbbr}', '{vol}'" )
        jShort = jAbbr.replace( ".", "" ).replace( " ", "" ) 

        className = "Issue"
        iUuid, _ = self.uuidDB.addUUID( biboPrefix + className,
                                        self.aPrefix + jShort + "_V" + vol )
        output.append( [ iUuid, "Instance", biboPrefix + className, "", "", "" ] )
        output.append( [ self.uuid, "Instance", iUuid, dctPrefix + "isPartOf", "", "" ] )

        # Journal information:
        className = "Journal"
        jUuid, _ = self.uuidDB.addUUID( biboPrefix + className,
                                        self.aPrefix + "Journal_" + jShort )
        output.append( [ jUuid, "Instance", biboPrefix + className, "", "", "" ] )
        output.append( [ iUuid, "Instance", jUuid, dctPrefix + "isPartOf", "", "" ] )

        # For debugging only:
        if False:
            print( "-------------------------------------------" )
            print( "----------     In bib2csv.py     ----------" )
            print( "Citation:", self.uuid )
            print( "Journal: ", jUuid )
            print( "Issue:   ", iUuid )
            print( "-------------------------------------------" )

        #self.uuid,_ = self.uuidDB.addUUID( self.tPrefix + className,
        #                                   self.aPrefix + self.itemName )
        #output.append( [ self.uuid, "Instance", self.tPrefix + self.className, "", "", "" ] )
        #output.append( [   subject, "Instance", self.uuid, predicate, "", "" ] )

        #if self.value != None:
        #     output.append( [ omOntoPrefix + "hasNumericalValue", "Data Property", 
        #                      self.uuid, "", self.value, "decimal" ] )
        #     pass
        #output += [  ]
        for field in entity.fields:
            if   "author" == field.key.lower():
                #print( field.key, "not implemented" )
                className = "rdf:List"
                """
                alUuid, _ = self.uuidDB.addUUID( className, # al = Author List
                                        self.aPrefix + "Authors_" + self.itemName, newUuid = uuidStr )
                output.append( [ alUuid, "Instance", className, "", "", "" ] )
                output.append( [ self.uuid, "Instance", alUuid, biboPrefix + "authorList", "", "" ] )
                """

                className = "Person"
                #prev, _ = self.uuidDB.addUUID( className, # al = Author List
                #                        self.aPrefix + "Person_" + "first" )
                #output.append( [ prev, "Instance", className, "", "", "" ] )
                #output.append( [ alUuid, "Instance", prev, biboPrefix + "authorList", "", "" ] )

                for ia, author in enumerate(field.value):
                    #print( "   ", author, "not implemented" )
                    #print( "      ", " ".join(author.first), " ".join(author.last) )
                    #auth, _ = self.uuidDB.addUUID( className, # al = Author List
                    #                    self.aPrefix + "Person_" + "first" )
                    #output.append( [ prev, "Instance", className, "", "", "" ] )
                    #output.append( [ alUuid, "Instance", auth, "rdf:first", "", "" ] )

                    nameID = "".join( author.first + author.last )

                    auth, _ = self.uuidDB.addUUID( foafPrefix + className, 
                                     foafPrefix + "Person_" + nameID + str(ia+1) )
                    output.append( [ auth, "Instance", foafPrefix + className, "", "", "" ] )
                    output.append( [  self.uuid, "Instance", auth, crystPrefix + "hasAuthor", "", "" ] )
                
                    output.append( [ foafPrefix + "firstName", "Data Property", 
                                     auth, "", " ".join(author.first), "xsd:string" ] )
                    output.append( [ foafPrefix + "family_name", "Data Property", 
                                     auth, "", " ".join(author.last), "xsd:string" ] )
                    output.append( [ crystPrefix + "hasOrderId", "Data Property", 
                                     auth, "", ia+1, "xsd:integer" ] )



            elif "title" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ dctPrefix + "title", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )

            elif "abstract" == field.key.lower():
                #print( field.key, "not implemented" )
                if field.value.strip() != "":
                    output.append( [ crystPrefix + "hasAbstract", "Data Property", 
                                     self.uuid, "", field.value, "xsd:string" ] )

            elif "journal" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ dctPrefix + "title", "Data Property", 
                                 jUuid, "", field.value, "xsd:string" ] )

            elif "issn" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "issn", "Data Property", 
                                 jUuid, "", field.value, "xsd:string" ] )

            elif "publisher" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ dctPrefix + "publisher", "Data Property", 
                                 jUuid, "", field.value, "xsd:string" ] )

            elif "volume" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "volume", "Data Property", 
                                 iUuid, "", field.value, "xsd:integer" ] )

            elif "year" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ dctPrefix + "issued", "Data Property", 
                                 iUuid, "", field.value, "xsd:gYear" ] )
 
            elif "month" == field.key.lower():
                #print( field.key, "not implemented, is it supported by bibo?" )
                output.append( [ dctPrefix + "issued", "Data Property", 
                                 iUuid, "", field.value, "xsd:gMonth" ] )

            elif "day" == field.key.lower():
                #print( field.key, "not implemented, is it supported by bibo?" )
                output.append( [ dctPrefix + "issued", "Data Property", 
                                 iUuid, "", field.value, "xsd:gDay" ] )

            elif "number" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "issue", "Data Property", 
                                 iUuid, "", field.value, "xsd:integer" ]  )

            elif "issue" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "issue", "Data Property", 
                                 iUuid, "", field.value, "xsd:integer" ]  )

            elif "pages" == field.key:
                output.append( [ biboPrefix + "pages", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )

                #logging.warning( field.key + " need to add pageStart and pageEnd properly" )
                start, end = self.pagesToStartEnd( field.value )
                if start:
                    output.append( [ biboPrefix + "pageStart", "Data Property", 
                                     self.uuid, "", start, "xsd:integer" ] )
                if end:
                    output.append( [ biboPrefix + "pageEnd", "Data Property", 
                                     self.uuid, "", end, "xsd:integer" ] )

            elif "doi" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "doi", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )
 
            elif "url" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ crystPrefix + "hasUrl", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )
 
            elif "eprint" == field.key.lower():
                #print( field.key, "not implemented" )
                output.append( [ crystPrefix + "hasEPrint", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )

            elif "keywords" == field.key:
                #print( field.key, "not implemented" )
                for kw in field.value.split( "," ):
                    output.append( [ crystPrefix + "hasKeyword", "Data Property", 
                                     self.uuid, "", kw.strip(), "xsd:string" ] )
 
            elif "note" == field.key:
                if 0 == self.fieldCountNote:
                    logging.warning( " Bib entry '" + field.key + "' " + \
                                     "is not implemented (warning only once)" )
                    self.errCount += 1
                self.fieldCountNote += 1    

 
            elif "lastchecked" == field.key:
                if 0 == self.fieldCountLastchecked:
                    logging.warning( " Bib entry '" + field.key + "' " + \
                                     "is not implemented (warning only once)" )
                    self.errCount += 1
                self.fieldCountLastchecked += 1    


                """
            elif "url" == field.key:
                print( field.key, "not implemented" )
                #output.append( [ dctPrefix + "title", "Data Property", 
                #                 self.uuid, "", field.value, "xsd:string" ] )
                """


            else:
                logging.error( " Unknown key value '" + field.key + "'" + \
                               " in bibliography '" + entity.key + "'." )
                self.errCount += 1

        return output, errCount
        pass # OntoBibo._csvBtpArticle()

    def getCsvArr( self, subject, predicate, options = dict() ):

        if is_http( predicate ):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning( " Predicate in OntoBibo.getCsvArr() is '" + \
                             predicate + "'," + " but expecting it to have '" + \
                             "has" + "'." )

        errCount = 0
        output = []

        #output += self._getCsvArrBiblib( subj, predicate, options )
        out, err = self._getCsvArrBibtexParser( subject, predicate, options )
        output += out
        errCount += err
        
        return output, errCount
        pass # OntoBib.getCsvArr()

    pass # class OntoBibo

if __name__ == "__main__":
    path = os.path.join( "..", "bibfiles", "10.1524_zkri.1988.182.14.1.bib" )
    path = os.path.join( "10.1524_zkri.1988.182.14.1.bib" )
    path = "10.1002_anie.200461911.bib"
    path = "10.1038_nmat2228.bib"

    fp = open( path )
    p = biblib.bib.Parser().parse( fp, log_fp=sys.stderr ).get_entries()
    p = biblib.bib.resolve_crossrefs( p )

    #print( p )
    #print( vars(p) )
    #print( p.get_entries() )
    #print( p.get_entries().keys() )
    #print( p.get_entries().values() )

    #print( p.keys() )
    #print( p.values() )
    #print( "===============================" )
    #for ent in p.values():
    #    for k in ent.keys():
    #        print( k, ":", ent[k] )

    """
    b = OntoBibo( path )
    for ent in b.entries.keys():
        #print ( ent, ":", b.entries[ent].keys() )
        print ( ent, ":" )
        for k in b.entries[ent].keys():
            print( k, ":", b.entries[ent][k] )
    """

    #library = bibtexparser.parse_file( path )

    layers = [
        btpmiddle.MonthIntMiddleware(True), # Months should be represented as int (0-12)
        btpmiddle.SeparateCoAuthors(True), # Co-authors should be separated
        btpmiddle.SplitNameParts(True) # Names should be split into first, von, last, jr parts
        ]
    library = bibtexparser.parse_file( path, append_middleware=layers)

    """
    #print( "entry: ",library.entries[0] )
    print( "type: ", library.entries[0].entry_type )
    print( "key: ",  library.entries[0].key )
    k = library.entries[0].key
    print( type( library.entries[0].fields ) )
    #print( library.entries[0].fields )
    for f in library.entries[0].fields:
        if "author" == f.key:
            for a in f.value:
              print( f.key, ":", a.first, a.last )
        else:
            print( f.key, ":", f.value )
    # Need to convert library to a simpler form. Now it is too complicated, with redundant information.
    """

    
    b = OntoBibo( path )
    b.getCsvArr( "subj", "hasCitation" )

