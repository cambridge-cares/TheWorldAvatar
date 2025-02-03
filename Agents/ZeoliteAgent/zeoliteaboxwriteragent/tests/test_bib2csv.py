import unittest
from bib2csv import *


def getCsvInit( baseName, tPrefix, aPrefix ):
    output = [ ]
    output.append( [ "Source", "Type", "Target", "Relation", "Value", "Data Type" ] )

    output.append( [ baseName, "Ontology", tPrefix, 
                                                     "http://www.w3.org/2002/07/owl#imports", "", "" ] )

    output.append( [ baseName, "Ontology", aPrefix, "base", "", "" ] )
    return output
    pass # getCsvInit()

class TestOntoBibo( unittest.TestCase ):

    def test_readBibo( self ):

        path = os.path.join( "bibfiles", "citations-20230530T090958.bib" )
        path = "10.1038_nmat2228.bib"

        bib = OntoBibo( path )
        bib.readBib( path )
        #bib.readBib( "10.1002_anie.200461911.bib" )
        #bib.addBib( "10.1002_anie.200461911.bib" )

        #self.assertEqual( bib.journal, 0 )
        csv, err = bib.getCsvArr( "subj", "hasCitation" )

        init = getCsvInit( "base", "http://tPrefix", "http://aPrefix" )
        init.append( [ "subj", "Instance", "SubjClass", "", "", "" ] )
        tools.writeCsv( "bib.csv", init + csv )

        i = 0
        line = csv[i]
        #print( line )
        doc = line[0]
        #self.assertEqual( line[0], "" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://purl.org/ontology/bibo/AcademicArticle" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "subj" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], doc )
        self.assertEqual( line[3], "hasCitation" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )
 
        # Instance of Issue
        i += 1
        line = csv[i]
        #print( line )
        issue = line[0]
        #self.assertEqual( line[0], "" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://purl.org/ontology/bibo/Issue" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], doc )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], issue )
        self.assertEqual( line[3], "http://purl.org/dc/terms/isPartOf" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )
 
       # Instance of Journal
        i += 1
        line = csv[i]
        #print( line )
        journal = line[0]
        #self.assertEqual( line[0], "" )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://purl.org/ontology/bibo/Journal" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[2], journal )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[0], issue )
        self.assertEqual( line[3], "http://purl.org/dc/terms/isPartOf" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        # Checking AUTHOR(1):
        i += 1
        line = csv[i]
        #print( line )
        author = line[0]
        #self.assertEqual( line[0], doc )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://xmlns.com/foaf/0.1/Person" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], doc )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "http://www.theworldavatar.com/kg/ontocrystal/hasAuthor" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://xmlns.com/foaf/0.1/firstName" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "Christian" )
        self.assertEqual( line[5], "xsd:string" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://xmlns.com/foaf/0.1/family_name" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "Baerlocher" )
        self.assertEqual( line[5], "xsd:string" )


        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasOrderId" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 1 )
        self.assertEqual( line[5], "xsd:integer" )

        # Skipping 6 AUTHORS (5 lines per person)
        i += 6 * 5

        # Checking AUTHOR(8):
        i += 1
        line = csv[i]
        #print( line )
        author = line[0]
        #self.assertEqual( line[0], doc )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], "http://xmlns.com/foaf/0.1/Person" )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], doc )
        self.assertEqual( line[1], "Instance" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "http://www.theworldavatar.com/kg/ontocrystal/hasAuthor" )
        self.assertEqual( line[4], "" )
        self.assertEqual( line[5], "" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://xmlns.com/foaf/0.1/firstName" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "Stacey I." )
        self.assertEqual( line[5], "xsd:string" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://xmlns.com/foaf/0.1/family_name" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "Zones" )
        self.assertEqual( line[5], "xsd:string" )


        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasOrderId" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], author )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 8 )
        self.assertEqual( line[5], "xsd:integer" )

        # Checking TITLE:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/dc/terms/title" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], doc )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "Ordered silicon vacancies in the framework structure of the zeolite catalyst SSZ-74" )
        self.assertEqual( line[5], "xsd:string" )

        # Checking JOURNAL:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/dc/terms/title" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], journal )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "Nature Materials" )
        self.assertEqual( line[5], "xsd:string" )


        # Checking YEAR:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/dc/terms/issued" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], issue )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "2008" )
        self.assertEqual( line[5], "xsd:gYear" )

        # Checking MONTH:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/dc/terms/issued" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], issue )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], 8 )
        self.assertEqual( line[5], "xsd:gMonth" )

        # Checking DAY:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/dc/terms/issued" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], issue )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "01" )
        self.assertEqual( line[5], "xsd:gDay" )

        # Checking VOLUME:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/ontology/bibo/volume" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], issue )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "7" )
        self.assertEqual( line[5], "xsd:integer" )

        # Checking ISSUE NUMBER:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/ontology/bibo/issue" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], issue )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "8" )
        self.assertEqual( line[5], "xsd:integer" )

        # Checking PAGES:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/ontology/bibo/pages" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], doc )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "631-635" )
        self.assertEqual( line[5], "xsd:string" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/ontology/bibo/pageStart" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], doc )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "631" )
        self.assertEqual( line[5], "xsd:integer" )

        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/ontology/bibo/pageEnd" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], doc )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "635" )
        self.assertEqual( line[5], "xsd:integer" )

        # Checking ABSTRACT:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://www.theworldavatar.com/kg/ontocrystal/hasAbstract" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], doc )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4].startswith( "Elucidation of the framework" ), True )
        self.assertEqual( line[5], "xsd:string" )

        # Checking JOURNAL ISSN:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/ontology/bibo/issn" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], journal )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "1476-4660" )
        self.assertEqual( line[5], "xsd:string" )


        # Checking DOI:
        i += 1
        line = csv[i]
        #print( line )
        self.assertEqual( line[0], "http://purl.org/ontology/bibo/doi" )
        self.assertEqual( line[1], "Data Property" )
        self.assertEqual( line[2], doc )
        self.assertEqual( line[3], "" )
        self.assertEqual( line[4], "10.1038/nmat2228" )
        self.assertEqual( line[5], "xsd:string" )


        # Checking URL:
        # TODO

        # Final check: total number of lines.
        self.assertEqual( len(csv), 60 )

        """
        self.assertEqual( 1, 0 )
        self.assertEqual( bib.journal, 0 )
        self.assertEqual( bib.journal, 0 )
        """

        pass # TestOntoBibo.test_readBibo()

    def test_manybib( self ):

        bib = OntoBibo( )
        path = os.path.join( "..", "bibfiles" )
        files = os.listdir( path )

        files = files[ 0:800 ]
        #files = files[ 0:500 ]

        for file in files:
            tmp = os.path.join( path, file )
            bib.readBib( tmp )

        #for k in bib.entryInBibFile:
        #    print( "  > ", k, "=>", bib.entryInBibFile[k] )
 
        csv, err = bib.getCsvArr( "subj", "hasCitation" )

        init = getCsvInit( "base", "http://tPrefix", "http://aPrefix" )
        init.append( [ "subj", "Instance", "SubjClass", "", "", "" ] )
        tools.writeCsv( "bib.csv", init + csv )

        for k in bib.foreignWords:
            print( "   ", k, "=>",  bib.foreignWords[k] )
        print( "Number of warnings: ", bib.errCount )

        pass # TestOntoBibo.test_manybib()

    pass # class TestOntoBibo

if __name__ == "__main__":
    unittest.main()

