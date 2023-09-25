
import csv     # to save csv files
import pandas  # to read xslx files
import math    # to detect NaN values
import logging 
import tools

filename = "Book2.xlsx"
filename = "ontocrystal.xlsx"

df = pandas.read_excel( filename )
#df  = pandas.DataFrame( tmp )

print( "Columns = ", df.columns )

#print( "Head = ", df.head() )   # Works, but only 5 lines. 

#print( "keys =", vars(df) )

#print( "Source = ", df["Source"] )
#s = df.sheets()
for i in range( 3 ):
    print( "row i = ", i, df.iloc[i] )

rows = df.loc[i]
values = df.loc[i].to_string(index = False, header=False )
print( "values = ", values )

print( "The size of input file '" + filename + "' is", df.shape, "." )

"""
tmp = []
for i in range( 5 ):
    v = df.loc[i].to_string(index = False, header=False )
    tmp.append( v.replace( "\n","").strip() )
print( tmp )
"""

class TBoxCleaner:
    __slots__ = { "filename", "dataIn", "classes", "clNames", "objProp", "opNames", 
                  "datProp", "dpNames", "nCol", "colLetters", "onto", "ontoNames",
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
        self.colLetters = "ABCDEFGHIJ"
        self.filename = ""

    def readExcel( self, filename ):
        self.filename = filename
        df = pandas.read_excel( filename )

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


    def validateOntology( self, line, file_line ):
        pass
    def validateObjLine( self, line, file_line ):
        pass
    def validateDatLine( self, line, file_line ):
        pass

    def validateClassLine( self, line, file_line ):
        if isinstance( line[7], str ):
            if len( line[7] ) < 10:
                logging.warning( " class " + line[0] + " may have incompete comment: '" \
                  + line[7] + "' " + file_line )
        else:
            logging.warning( " expected string in col H " + file_line )

        #for i in range( 2, nCol ):
        for i in [ 2, 3, 4, 5, 6, 9 ]:
            if isinstance( line[i], float ):
                if not math.isnan( line[i] ):
                    logging.warning( " expecting empty cell " + self.colLetters[i] + " " + file_line )

    def validateHeaders(self):
        pass

    def extractOntology( self ):
        if len(self.dataIn) == 0:
            logging.error( " in extractOntology(): dataIn is not loaded, use readExcel()" )
            return

        self.headers = [ self.dataIn[0] ]
        self.validateHeaders()

        self.onto = []
        for il, line in enumerate(self.dataIn):
            file_line = self.filename + " on " + str(il) 
            if "tbox" == line[1].lower():
                self.onto.append( line )
                self.ontoNames.append( line[0] )

                self.validateOntology( line, file_line )

        #print( self.classes )

   
        pass

    def extractClasses( self ):
        if len(self.dataIn) == 0:
            logging.error( " in extractClasses(): dataIn is not loaded, use readExcel()" )
            return

        self.classes = []
        for il, line in enumerate(self.dataIn):
            file_line = self.filename + " on " + str(il) 
            if "class" == line[1].lower():
                self.classes.append( line )
                self.clNames.append( line[0] )

            self.validateClassLine( line, file_line )

        print( self.classes )

    def extractObjProp( self ):
        if len(self.dataIn) == 0:
            logging.error( " in extractObjProp(): dataIn is not loaded, use readExcel()" )
            return

        self.objProp = []
        for il, line in enumerate(self.dataIn):
            file_line = self.filename + " on " + str(il) 
            if "object property" == line[1].lower():
                self.objProp.append( line )
                self.opNames.append( line[0] )

                self.validateObjLine( line, file_line )

        #print( self.classes )


    def extractDatProp( self ):
        if len(self.dataIn) == 0:
            logging.error( " in extractDatProp(): dataIn is not loaded, use readExcel()" )
            return

        self.datProp = []
        for il, line in enumerate(self.dataIn):
            file_line = self.filename + " on " + str(il) 
            if "data property" == line[1].lower():
                self.datProp.append( line )
                self.dpNames.append( line[0] )

                self.validateDatLine( line, file_line )

        #print( self.classes )

    def saveCsv( self, filename ):
        tmp = self.headers + self.onto    + self.classes + \
              self.objProp + self.datProp
        print( tmp )
        print( "===================" )

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
            print( line )
            pass

        tools.writeCsv( filename, self.dataOut )
        #tools.writeCsv( filename, self.classes )
        print( "Saved csv file, number of lines =", len(self.dataOut ) )


for il, line in enumerate(df):
    #print( "line["+str(il)+"] =", line )
    pass


tb = TBoxCleaner()
tb.readExcel( filename )
tb.extractOntology()
tb.extractClasses()
tb.extractObjProp()
tb.extractDatProp()

tb.saveCsv( "ttttt.csv" )

