import os
import logging
logging.basicConfig( level=logging.WARNING )

import sys
sys.path.append( 'biblib' )
import biblib.bib
#import biblib.biblib as biblib
import tools

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

with open( "journals.json" ) as fp:
    journalAbbreviations = json.load( fp )


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
    #__slots__ = [ "year", "month", "title", "journal", "volume", "number", \
    #              "pages", "abstract", "issn", "doi", "eprint" \
    #]
    def __init__( self, filePath, uuidDB = None,
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

        if "" == tPrefix:
            self.tPrefix = "http://tPrefix/"
        else:
            self.tPrefix = tPrefix

        if "" == aPrefix:
            self.aPrefix = "http://aPrefix/"
        else:
            self.aPrefix = aPrefix

        if None == uuidDB:
            logging.error( " Empty uuidDB in '" + self.itemName + "'" + \
                           " in class OntoMeasureWithUncertainty. Using default." ) 
            self.uuidDB = tools.UuidDB()
        elif isinstance( uuidDB, tools.UuidDB ):
            self.uuidDB = uuidDB
        else:   
            logging.error( "Wrong type of uuidDB, expected 'UuidDB', got '" + \
                            str(type(uuid)) + "'." )
            pass

        self.entries = {}
        self.btpLibrary = bibtexparser.Library() # btp = Bib Tex Parser

        self.readBib( filePath )
        pass # __INIT__()


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

        if not isinstance(filePath, str):
            logging.error( "File path must be a string, but got '" + \
                           str(filePath) + "' in bib2csv." )
            return 

        if os.path.isdir( filePath ):
            paths = os.path.listdir( filePath )
            for path in paths:
                self.readBib( path )

        elif os.path.isfile( filePath ):
            _, ext = os.path.splitext( filePath )
            if ".bib" != ext:
                logging.error( " In readBib() in file '" + filePath + "'," + \
                               " but expected extension .bib. Skipped." )
                return                

            #self._readAsBibLib( filePath )

            self._readAsBibtexParser( filePath )

        else:
            logging.error( " In bib2csv file '" + filePath + "' does not exist." )
            return 

        pass # OntoBibo.readBib()

    def _readAsBibLib( self, filePath ):
        with open( filePath, encoding="utf-8" ) as fp:
            p = biblib.bib.Parser().parse( fp, log_fp=sys.stderr ).get_entries()
            p = biblib.bib.resolve_crossrefs( p )
            self.entries |= p

        pass # OntoBibo._readAsBibLib()

    def _readAsBibtexParser( self, filePath ):

        layers = [
            btpmiddle.MonthIntMiddleware(True), # Months should be represented as int (0-12)
            btpmiddle.SeparateCoAuthors(True),  # Co-authors should be separated
            btpmiddle.SplitNameParts(True)      # Names should be split into first, von, last, jr parts
                 ]
        library = bibtexparser.parse_file( filePath, append_middleware=layers)

        self.btpLibrary.add( library.entries )

        pass # OntoBibo._readAsBibtexParser()

    def _getCsvArrBibtexParser( self, subject, predicate, options ):
        errCount = 0
        output = []

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
        end = None

        return start, end

    def getIssueInfo( self, entity ):

        jFull = None
        jAbbr = None
        vol   = None
        #print( "datatype of fields:", type( entity.fields ) )
        for f in entity.fields:
            if "journal" == f.key:
                journal = f.value
                if journal in journalAbbreviations:
                    jFull = journal
                    jAbbr = journalAbbreviations[journal]
                else:
                    logging.error( "Uknonwn journal name in bib file: " + journal )
                    jFull = journal
                    jAbbr = journal


            elif "volume" == f.key:
                #print( "    ", f.value )
                vol = str(f.value)
                #break

        if None == jFull:
            logging.error( " Unknown journal full name " + journal )

        if None == jAbbr:
            logging.error( " Unknown journal abbreviation " + journal )

        if None == vol:
            logging.error( " In bib entry '" + "xxx" + "' volume is not specified" )
            vol = "None"

        return jFull, jAbbr, vol


    def _csvBtpArticle( self, entity, subject, predicate, options ):
        output = []
        errCount = 0

        """
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
        jShort = jAbbr.replace( ".", "" ).replace( " ", "" ) 

        className = "Issue"
        iUuid, _ = self.uuidDB.addUUID( biboPrefix + className,
                                        self.aPrefix + jShort + "_V" + vol )
        output.append( [ iUuid, "Instance", biboPrefix + className, "", "", "" ] )
        output.append( [ self.uuid, "Instance", iUuid, dctPrefix + "isPartOf", "", "" ] )

        # Journal information:
        className = "Journal"
        jUuid, _ = self.uuidDB.addUUID( biboPrefix + className,
                                        self.aPrefix + "Journal_" + jShort, newUuid = uuidStr )
        output.append( [ jUuid, "Instance", biboPrefix + className, "", "", "" ] )
        output.append( [ iUuid, "Instance", jUuid, dctPrefix + "isPartOf", "", "" ] )

        # For debugging only:
        if True:
            print( "------------------------------------------" )
            print( "Citation:", self.uuid )
            print( "Journal: ", jUuid )
            print( "Issue:   ", iUuid )
            print( "------------------------------------------" )

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
            if   "author" == field.key:
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

                    auth, _ = self.uuidDB.addUUID( foafPrefix + className, # al = Author List
                                        foafPrefix + "Person_" + str(ia+1) )
                    output.append( [ auth, "Instance", foafPrefix + className, "", "", "" ] )
                    output.append( [  self.uuid, "Instance", auth, crystPrefix + "hasAuthor", "", "" ] )
                
                    output.append( [ foafPrefix + "firstName", "Data Property", 
                                     auth, "", " ".join(author.first), "xsd:string" ] )
                    output.append( [ foafPrefix + "family_name", "Data Property", 
                                     auth, "", " ".join(author.last), "xsd:string" ] )
                    output.append( [ crystPrefix + "hasOrderId", "Data Property", 
                                     auth, "", ia+1, "xsd:integer" ] )



            elif "title" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ dctPrefix + "title", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )

            elif "abstract" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ crystPrefix + "hasAbstract", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )

            elif "journal" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ dctPrefix + "title", "Data Property", 
                                 jUuid, "", field.value, "xsd:string" ] )

            elif "issn" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "issn", "Data Property", 
                                 jUuid, "", field.value, "xsd:string" ] )

            elif "volume" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "volume", "Data Property", 
                                 iUuid, "", field.value, "xsd:integer" ] )

            elif "year" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ dctPrefix + "issued", "Data Property", 
                                 iUuid, "", field.value, "xsd:gYear" ] )
 
            elif "month" == field.key:
                #print( field.key, "not implemented, is it supported by bibo?" )
                output.append( [ dctPrefix + "issued", "Data Property", 
                                 iUuid, "", field.value, "xsd:gMonth" ] )

            elif "day" == field.key:
                #print( field.key, "not implemented, is it supported by bibo?" )
                output.append( [ dctPrefix + "issued", "Data Property", 
                                 iUuid, "", field.value, "xsd:gDay" ] )

            elif "number" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "issue", "Data Property", 
                                 iUuid, "", field.value, "xsd:integer" ]  )

            elif "pages" == field.key:
                logging.warning( field.key + " need to add pageStart and pageEnd properly" )
                output.append( [ biboPrefix + "pages", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )

                start, end = self.pagesToStartEnd( field.value )
                if start:
                    output.append( [ biboPrefix + "pageStart", "Data Property", 
                                     self.uuid, "", field.value, "xsd:integer" ] )
                if end:
                    output.append( [ biboPrefix + "pageEnd", "Data Property", 
                                     self.uuid, "", field.value, "xsd:integer" ] )

            elif "doi" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ biboPrefix + "doi", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )
 
            elif "url" == field.key:
                #print( field.key, "not implemented" )
                output.append( [ crystPrefix + "hasUrl", "Data Property", 
                                 self.uuid, "", field.value, "xsd:string" ] )
 
                """
            elif "url" == field.key:
                print( field.key, "not implemented" )
                #output.append( [ dctPrefix + "title", "Data Property", 
                #                 self.uuid, "", field.value, "xsd:string" ] )
                """


            else:
                logging.error( " Unknown key value '" + field.key + "' in bibliography" + entity.key )

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

