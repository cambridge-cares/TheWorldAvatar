"""
TODO
  - Allow combine several xlsx files to make a single csv file.
    - There is a problem of property that has same name but different Domain/Range (use UNION)
      If different parts use the same property this should be addressed properly.
  - Remove warnings related to the code.
  - Fix TODO notes.
  - In validationXXX functions add warning for repeating entry.
  .

"""

import csv     # to save csv files
import pandas  # to read xslx files
import math    # to detect NaN values
import sys     # for command line arguments (sys.argv)
import os      # To detect file, to check filename, etc.


import logging 
logging.basicConfig( level=logging.WARNING )
#logging.basicConfig( level=logging.ERROR )

import tools

COLUMN_LETTERS = "ABCDEFGHIJ"
COLUMN_HEADERS = [ "Source",      "Type",  "Target",   "Relation", "Domain",
                   "Range", "Quantifier", "Comment", "Defined By", "Label" ]


#df = pandas.read_excel( filename )
#df  = pandas.DataFrame( tmp )

#print( "Columns = ", df.columns )

#print( "Head = ", df.head() )   # Works, but only 5 lines. 

#print( "keys =", vars(df) )

#print( "Source = ", df["Source"] )
#s = df.sheets()
for i in range( 3 ):
    #print( "row i = ", i, df.iloc[i] )
    pass

#rows = df.loc[i]
#values = df.loc[i].to_string(index = False, header=False )
#print( "values = ", values )

#print( "The size of input file '" + filename + "' is", df.shape, "." )

"""
tmp = []
for i in range( 5 ):
    v = df.loc[i].to_string(index = False, header=False )
    tmp.append( v.replace( "\n","").strip() )
print( tmp )
"""

class TBoxCleaner:
    __slots__ = { "fileIn",  "dataIn", "classes", "clNames", "objProp", "opNames", 
                  "datProp", "dpNames", "nCol", "onto", "ontoNames", "errCount",
                  "dataOut", "headers" }
    def __init__( self ):
        self.dataIn = []
        self.headers = []
        self.onto    = []
        self.ontoNames = []
        self.classes = []
        self.clNames = []
        self.objProp = []
        self.opNames = []
        self.datProp = []
        self.dpNames = []

        self.nCol = 10
        self.fileIn = ""

    def readExcel( self, filename ):
        if not os.path.isfile( filename ):
            self.fileIn = ""
            logging.error( " Input file '" + filename + "' does not exist." )
            return 

        self.fileIn = filename
        df = pandas.read_excel( filename )
        logging.info( "The size of input file '" + filename + "' is " + str(df.shape) + "." )
        
        self.dataIn = []
        self.dataIn.append( df.columns )
        for index, row in df.iterrows():
            #print(row)
            #print(row.to_dict())
            r = row.to_dict()
            line = [ r[k] for k in r.keys() ]
            #print( line )
            self.dataIn.append( line )
        #print( self.dataIn )

    def isEmptyCell( self, cell ):
        """ Returns True if the input string 'cell' is empty or nan.
        """
        if isinstance( cell, float ):
            if math.isnan( cell ):
                return True
        elif isinstance( cell, str ):
            short = cell.strip().lower()
            if "nan" == short or "" == short:
                return True
        else:
            logging.warning( " Unknown type of '" + cell + "': " + type(cell) )
        return False
        #return True


    def isCellBValue( self, line, value, file_line ):
        if len(line) < 2:
            return False

        if not isinstance( line[1], str ):
            # Here NaN value of type float if the cell is empty.
            return False

        i = 1
        expect = value

        if expect.lower() == line[i].lower():
            if expect != line[i]:
                logging.warning( " Wrong input " + file_line + " col " + \
                                 COLUMN_LETTERS[i] + ": '" + line[i] + 
                                 "' but expected '" + expect + "'." )
            return True
        else:
            return False
        pass # isCellBValue    
 
    def isLineOntology( self, line, file_line ):
        return self.isCellBValue( line, "TBox", file_line )
       
    def isLineClass( self, line, file_line ):
        return self.isCellBValue( line, "Class", file_line )

        pass

    def isLineObjProp( self, line, file_line ):
        return self.isCellBValue( line, "Object Property", file_line )
        pass

    def isLineDatProp( self, line, file_line ):
        return self.isCellBValue( line, "Data Property", file_line )
        pass


    def validateHeaders( self, headers, file_line ):
        """ Function returns number of errors + warnings detected. 

        """
        errCount = 0
        if len(headers) != len(COLUMN_HEADERS):
            errCount += 1
            logging.error( " The header row has size " + str(len(headers)) + \
                           " but expected " + str(len(COLUMN_HEADERS)) + \
                           " " + file_line + "." )

        for i in range(min(len(headers),len(COLUMN_HEADERS) )):

            if headers[i].strip() == COLUMN_HEADERS[i].strip():
                # Do nothing
                pass
            else:
                if headers[i].strip().lower() != COLUMN_HEADERS[i].strip().lower():
                    errCount += 1
                    logging.error( " Wrong header value " + file_line + \
                                   " col " + COLUMN_LETTERS[i] + ": '" + headers[i] + \
                                   " ', but expected '" + COLUMN_HEADERS[i] + "'." )
                else:
                    errCount += 1
                    logging.warning( " Wrong header value " + file_line + \
                                   " col " + COLUMN_LETTERS[i] + ": '" + headers[i] + \
                                   " ', but expected '" + COLUMN_HEADERS[i] + "'." )

        # TODO Check whether Comment exists:

        # TODO If IS-A then should exist Target value. Is it affected?

        return errCount
        pass

    def validateOntoLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines.

        """
        # TODO see function description

        logging.warning( " ontology validation is not implemented" )
        errCount = 0

        return errCount
        pass

    def validateClassLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

        """

        # TODO: class name should not be repeated, and don't match the obj prop and dat prop
        # TODO if IS-A then there should be argument

        errCount = 0
        # TODO columns 0,1 are required

        # Check IS-A Relation of the classes (columns 2,3):
        if not self.isEmptyCell( line[3] ):
            #if isinstance( line[3], str ):
            short = str(line[3]).strip()
            if "is-a" == short.lower():
                # TODO make warning for is-a and error for neither is-a nor IS-A:
                if self.isEmptyCell( line[2] ):
                    logging.error( " Missing relation class for class '" + line[0] + "' " + \
                                   file_line + " col " + COLUMN_LETTERS[3] + "." )
                    errCount += 1
            else:
                logging.error( "Unknown Relation '" + short + "' for class '" + line[0] + "' " +\
                               file_line + " col " + COLUMN_LETTERS[3] + "." )
                errCount += 1

        # Check whether 'Comment' exists (column 7):
        if self.isEmptyCell(line[7]):
            logging.error( " Missing Comment for class '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[7] + "." )
        if isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Class '" + line[0] + "' may have incompete 'comment': '" + \
                  line[7] + "' " + file_line + " col " + COLUMN_LETTERS[7] + "." )
                errCount += 1
        else:
            logging.warning( " Missing Comment for class '" + line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1

        # Check whether 'Defined In' exists (column 8):
        if self.isEmptyCell(line[8]):
            logging.error( " Missing Defined In for class '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        if isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Class '" + line[0] + "' may have incompete 'defined in': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 4, 5, 6, 9 ]:
            if self.isEmptyCell( line[i] ):
                logging.warning( " Expecting empty cell " + COLUMN_LETTERS[i] + \
                                 " but got '" + str(line[i]) + "' " + file_line + "." )
                errCount += 1


        # TODO check repeating entries

        return errCount


    def validateObjLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

        """


        # TODO: property name should not be repeated, and don't match the class and dat prop

        logging.warning( " object property validation is not implemented" )
        errCount = 0

        # Columns 0,1,4,5 are required.

        # Column 6 is optional.
        '''
        if isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Object Property " + line[0] + " may have incompete comment: '" \
                  + line[7] + "' " + file_line + " col " + COLUMN_LETTERS[7] + "." )
        else:
            logging.warning( " Missing Comment for Object Property '" + line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
        '''

        # Check whether 'Comment' exists (column 7):
        if self.isEmptyCell(line[7]):
            logging.error( " Missing Comment for Obj Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[7] + "." )
        if isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Obj Property '" + line[0] + "' may have incompete 'comment': '" + \
                  line[7] + "' " + file_line + " col " + COLUMN_LETTERS[7] + "." )
                errCount += 1
        else:
            logging.warning( " Missing Comment for Obj Property '" + line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1

        # Check whether 'Defined In' exists (column 8):
        if self.isEmptyCell(line[8]):
            logging.error( " Missing Defined In for Obj Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        if isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Obj Property '" + line[0] + "' may have incompete 'defined in': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 2, 3, 6, 9 ]:
            if self.isEmptyCell( line[i] ):
                logging.warning( " Expecting empty cell " + COLUMN_LETTERS[i] + \
                                 " but got '" + str(line[i]) + "' " + file_line + "." )
                errCount += 1
 
        return errCount
        pass

    def validateDatLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

        """


        # TODO: property name should not be repeated, and don't match the class and obj prop

        logging.warning( " data property validation is not implemented" )
        errCount = 0

        '''
        if isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Data Property " + line[0] + " may have incompete comment: '" \
                  + line[7] + "' " + file_line + " col " + COLUMN_LETTERS[7] + "." )
        else:
            logging.warning( " Missing Comment for Data Property '" + line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
        '''
        # Columns 0,1,4,5 are required.

        # Check whether 'Comment' exists (column 7):
        if self.isEmptyCell(line[7]):
            logging.error( " Missing Comment for Data Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[7] + "." )
        if isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Data Property '" + line[0] + "' may have incompete 'comment': '" + \
                  line[7] + "' " + file_line + " col " + COLUMN_LETTERS[7] + "." )
                errCount += 1
        else:
            logging.warning( " Missing Comment for Data Property '" + line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1

        # Check whether 'Defined In' exists (column 8):
        if self.isEmptyCell(line[8]):
            logging.error( " Missing Defined In for Data Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        if isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Data Property '" + line[0] + "' may have incompete 'defined in': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 2, 3, 6, 9 ]:
            if self.isEmptyCell( line[i] ):
                logging.warning( " Expecting empty cell " + COLUMN_LETTERS[i] + \
                                 " but got '" + str(line[i]) + "' " + file_line + "." )
                errCount += 1

        return errCount
        pass

    def parseInputData( self ):
        self.errCount = 0

        # Preparation:
        self.headers = []
        self.onto    = []
        self.classes = []
        self.objProp = []
        self.datProp = []

        if len(self.dataIn) == 0:
            logging.error( " in extractOntology(): dataIn is not loaded, use readExcel()" )
            return

        # TODO Other internal arrays:

        # Header row on Line 0:
        #file_line = self.fileIn + " on " + str(0)
        #self.headers = [ self.dataIn[0] ]

        # The rest of the input data:
        for il, line in enumerate(self.dataIn):
            file_line = "in '" + self.fileIn + "' on " + str(il+1) 

            if 0 == il:
                self.extractHeaders(  line, file_line )

            elif self.isLineOntology( line, file_line ):
                self.extractOntology( line, file_line )

            elif self.isLineClass(   line, file_line ):
                self.extractClasses( line, file_line )

            elif self.isLineObjProp( line, file_line ):
                self.extractObjProp( line, file_line )

            elif self.isLineDatProp( line, file_line ):
                self.extractDatProp( line, file_line )

            elif self.emptyLine( line, file_line ):
                # Do nothing
                pass
            elif self.isLineComment( line, file_line ):
                # Do nothing
                pass
            else:
                #logging.warning( " xxxxxxxxxxxxxxxxxxxx " )
                logging.warning( "Skipping line:" + str(line) )
        pass # parseInputData()

    def isLineComment( self, line, file_line ):
        """ Now the conditions are either: 
            - the 1st column is empty or equal to Comment
            - the 2nd column is empty or equal to Comment
            .
        """

        short = self.getShort( line[0], file_line )
        #print( "short = '" + short + "'" )
        if "comment" == short or "" == short: 
            return True

        short = self.getShort( line[1], file_line )
        #print( "short = '" + short + "'" )
        if "comment" == short or "" == short: 
            return True

        return False
        pass # isLineComment()

    def getShort( self, word, file_line ):
        if isinstance( word, str ):
            #print( "type = str" )
            short = word.strip().lower()
        elif isinstance( word, float ):
            #print( "type = float" )
            if math.isnan( word ):
                short = ""
            else:
                short = str(word)

        else:
            logging.error( " unknown value '" + str(word) + "', type " + \
                           str(type(word)) + " " + file_line )
            short = str(word)
        return short


    def emptyLine( self, line, file_line ):
        #isEmpty = True
        for word in line:
            if isinstance( word, float ):
                if not math.isnan( word ):
                    return False
            elif isinstance( word, str ):
                if not word.strip() == "":
                    return False
            else:
                logging.error( " Unknown type of '" + str(word) + "': " + str(type(word)) + "." )
        return True
                
    def extractHeaders( self, line, file_line ):
        #print( "sssssssssssssssssssss" )
        #print( "header line =", line )
        self.headers = [line]
        self.errCount += self.validateHeaders( line, file_line )
        
    def extractOntology( self, line, file_line ):

        if "tbox" == line[1].strip().lower():
            self.errCount += self.validateOntoLine( line, file_line )

            self.onto.append( line )
            self.ontoNames.append( line[0] )
   
        pass

    def extractClasses( self, line, file_line ):

        if "class" == line[1].strip().lower():
            self.errCount += self.validateClassLine( line, file_line )

            self.classes.append( line )
            self.clNames.append( line[0] )

        #print( self.classes )

    def extractObjProp( self, line, file_line ):

        if "object property" == line[1].strip().lower():
            self.errCount += self.validateObjLine( line, file_line )

            self.objProp.append( line )
            self.opNames.append( line[0] )

        #print( self.objProp )

    def extractDatProp( self, line, file_line ):
            if "data property" == line[1].strip().lower():
                self.errCount += self.validateDatLine( line, file_line )

                self.datProp.append( line )
                self.dpNames.append( line[0] )

        #print( self.datProp )

    def saveCsv( self, filename ):

        #print( self.headers )
        #print( self.onto )
        #print( self.classes )
        #print( self.objProp )
        #print( self.datProp )

        # TODO
        #self.CheckClasses()
        #self.CheckObjProp()
        #self.CheckDatProp()

        tmp = self.headers + self.onto    + self.classes + \
              self.objProp + self.datProp
        #print( tmp )
        #print( "===================" )

        self.dataOut = []
        for il, line in enumerate(tmp):
            l = []
            for c in line:
                if isinstance( c, float ):
                    if math.isnan( c ):
                        l.append( "" )
                    else:
                        l.append( c )
                else:
                    l.append( c )

            self.dataOut.append( l )

        for line in self.dataOut:
            #print( line[0] )
            #print( line )
            pass

        tools.writeCsv( filename, self.dataOut )
        #tools.writeCsv( filename, self.classes )
        print( "Saved '" + filename + "' file, number of lines =", 
               str(len(self.dataOut)) + "." )
        print( "Number of errors+warnings =", str(self.errCount) + "." )


if __name__ == "__main__":
    
    fileIn  = "Book2.xlsx"
    fileIn  = "ontocrystal.xlsx"

    fileOut = "default.csv"

    if len( sys.argv ) == 3:
        fileIn  = sys.argv[1]
        fileOut = sys.argv[2]
    elif len( sys.argv ) == 2:
        fileIn = sys.argv[1]
        fileBase, fileExt = os.path.splitext( fileIn )
    elif len( sys.argv ) == 1:
        # Do nothing. Use default file names.
        pass
    else:
        logging.error( " Expecting 1 or 2 command line arguments:" + \
                       " tboxcleaner.py file_in [file_out] \n" + \
                       ", but got '" + str(len(sys.argv)) + "' program call." )
        sys.exit(0) 

    fileBase, fileExt = os.path.splitext( fileIn )
    if fileExt.lower() == ".xlsx":
        fileOut = fileBase + ".csv"
    else:
        logging.error( " Input must be Excel file with extension .xlsx, " + \
                       "but got file name '" + fileIn + "'." )
        sys.exit(0)

    fileBase, fileExt = os.path.splitext( fileOut )
    if not fileExt.lower() == ".csv":
        logging.error( " Output must be CSV file with extension .csv, " + \
                       "but got file name '" + fileOut + "'." )
        sys.exit(0)

    #print( fileBase, fileExt )

    tb = TBoxCleaner()
    tb.readExcel( fileIn )
    tb.parseInputData()

# old version:
#tb.extractOntology()
#tb.extractClasses()
#tb.extractObjProp()
#tb.extractDatProp()

    tb.saveCsv( fileOut )


