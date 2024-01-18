
import unittest
#from tboxcleaner import *
from tboxtools import *

class TestTBoxTools( unittest.TestCase ):

    def test_isDataType(self):
        tt = TBoxTools()
        nOut = 2

        res = tt.isDataType( "string" )
        self.assertEqual( isinstance(res,tuple), True )
        self.assertEqual( len(res), nOut )
        self.assertEqual( res[0], True )
        self.assertEqual( res[1], "string" )


        res = tt.isDataType( "integer" )
        self.assertEqual( isinstance(res,tuple), True )
        self.assertEqual( len(res), nOut )
        self.assertEqual( res[0], True )
        self.assertEqual( res[1], "integer" )


        res = tt.isDataType( "rdfs:Literal" )
        self.assertEqual( isinstance(res,tuple), True )
        self.assertEqual( len(res), nOut )
        self.assertEqual( res[0], True )
        self.assertEqual( res[1], "literal" )


        res = tt.isDataType( "  string" )
        self.assertEqual( isinstance(res,tuple), True )
        self.assertEqual( len(res), nOut )
        self.assertEqual( res[0], True )
        self.assertEqual( res[1], "string" )


        res = tt.isDataType( "string  " )
        self.assertEqual( isinstance(res,tuple), True )
        self.assertEqual( len(res), nOut )
        self.assertEqual( res[0], True )
        self.assertEqual( res[1], "string" )





        pass # TestTBoxTools.test_isDataType()

    def test_anyInList( self ):

        tb = TBoxTools()

        self.assertEqual( tb.anyInList( [0,1,2], [3,4,5] ), False )
        self.assertEqual( tb.anyInList( [0,1,2], [0,4,5] ), True )
        self.assertEqual( tb.anyInList( [0,1,2], [3,4,0] ), True )
        self.assertEqual( tb.anyInList( [0,1,2], [3,4,2] ), True )

        self.assertEqual( tb.anyInList( [0,1,2,10], [3,4,2] ), True )
        self.assertEqual( tb.anyInList( [0,1,2], [3,4,2,10] ), True )

        pass # TestTBoxTools.anyInList()

    def test_checkTripleTwoTBox( self ):
        """ Using two t-boxes in one owl files.
            Instead of "external" classes.
        """
        tboxFile1 = "testtbox1.csv"
        tboxFile2 = "testtbox2.csv"

        tb = TBoxTools()
        tb.readExcel( tboxFile1 )
        tb.parseInputData()

        tb.readExcel( tboxFile1 )
        tb.parseInputData()

        self.assertEqual( tb.isKnownClass( "" ), True )

        #Check for element of class:
        self.assertEqual( tb.checkTriple( "subj", "pred", "obj", "file_line" ), True )

        #Check for data property:
        self.assertEqual( tb.checkTriple( "subj", "pred", "obj", "file_line" ), True )

        #self.assertEqual( self.isKnownClass( "" ), True )

        pass # TestTBoxTools.test_twoTBox()


        pass # TestTBoxTools.
        pass # TestTBoxTools.
        pass # TestTBoxTools.

    pass # class TestTBoxTools

if __name__ == "__main__":
    unittest.main()


