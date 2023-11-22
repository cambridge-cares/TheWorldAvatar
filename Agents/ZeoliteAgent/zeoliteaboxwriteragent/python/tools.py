import logging
#logging.basicConfig( level = logging.INFO )
logging.basicConfig( level = logging.WARNING )
import csv
import os
import uuid

# TODO change uuid from dict to a separate class. For example UUID.
# To avoid error while using it.

class UuidDB:
    """
    UUID Data Base has 5 main functions:
    loadUUID from a file (before the first use). Extension .csv.
    saveUUID to a file (must be used at the end, otherwise corrections will be lost).
    getUUID() - find out whether instance exists in database, return the name and uuid.
    addUUID() - do same as getUUID + add new element to database
                return the new name and also new UUID.
 ###add element to database and 
    getUUID_Random() - what is this??

    """
    __slots__ = [ "dbFilename", "uuidDB" ]

    def __init__( self, filename = None ):
        """
        The input parameter may be:
        - string (i.e. the filename of the existing database):
          Load the file and use the data as the database.
        - an existing database of type UuidDB 
          (copy it to the current database?)
          
        - None or not specified.
          Warning and open a default data base 'uuid-default.csv' 
          or create a new file if file does not exist.
          Later save to the same file (create new, if necessary).
        """

        default = os.path.join( "uuid" + "-default.csv" )
        #default = os.path.join( "ontozeolite", "uuid", "default.csv" )

        if None == filename or "" == filename:
            self.dbFilename = default

        elif isinstance( filename, str ):
            if   os.path.isfile( filename ):
                self.dbFilename = filename
            elif os.path.isdir( filename ):
                logging.error( " Uuid database path: '" + filename + \
                               "' is a directory. Using default." )
                self.dbFilename = default
            else:
                logging.error( " Uuid database file does not exist: '" + \
                               filename + "'. Create a new one." )
                self.dbFilename = filename

        else:
            logging.error( " Uuid path must be a string, but got '" + str(type(filename)) + "'." )
            self.dbFilename = filename

        self.loadDB( self.dbFilename )

        pass # UuidDB.__init__()


    def valueIsValid( self, value ):
        """
        Checking for illegal characters ( ' ' ',' '.' ';' ).

        """
        if not isinstance( value, str ):
            logging.error( "Value must be string, but got '" + str(value) + "'." )
            return False

        short = value.strip()
        if len(short) == 0:
            logging.info( " Value '" + value + "' is empty." )
            return False

        if short.find( " " ) >= 0:
            logging.info( " Value '" + value + "' contains space." )
            return False

        if short.find( "," ) >= 0:
            logging.info( " Value '" + value + "' contains comma." )
            return False

        if short.find( ";" ) >= 0:
            logging.info( " Value '" + value + "' contains semicolon." )
            return False

        return True
        pass # UuidDB.valueIsValid()

    def loadDB( self, filePath ):
        """
        Load the database.
        """
        if not os.path.isfile( filePath ):
            logging.info( "UUID DB does not exist, initialize a new DB" )
            self.uuidDB = dict()
            return self.uuidDB
  
        array = readCsv( filePath )

  # Convert the list from csv into a dictionary:
        self.uuidDB = dict()
        if not isinstance(array,list):
            return self.uuidDB
        if len( array ) == 0:
            return self.uuidDB

  # Checking for the title line in the csv file:
        line = array[0] 
        if line[0].strip().lower() ==    "Class name".strip().lower() and \
           line[1].strip().lower() == "Instance name".strip().lower() and \
           line[2].strip().lower() ==          "UUID".strip().lower(): 
            startLine = 1
        else:
            startLine = 0

        for line in array[startLine:]:
            if len(line) == 3:
                cName = line[0]
                iName = line[1]
                uuidStr  = line[2]
                if len(line[0]) > 0 and len(line[1]) > 0 and len(line[2]) > 0:
                    if not cName in list( self.uuidDB.keys() ):
                        self.uuidDB[ cName ] = dict()
                    if not iName in list( self.uuidDB[ cName ].keys() ):
                        self.uuidDB[cName][iName] = uuidStr
                    else:
                        if uuidStr != self.uuidDB[cName][iName]:
                            logging.error( "Mismatch uuid in database" )
            else:
                logging.error( " Wrong entry in uuidDB: '" + str(line) + "'." )
        return self.uuidDB

        pass # UuidDB.loadDB()

    def saveDB( self, filename = "" ):
        """
        Save the database to file.
        """
        if "" == filename:
            #filePath = os.path.join( "ontozeolite", "uuid", "default.csv" )
            filePath = self.dbFilename
        else:
            filePath = filename

        folder = os.path.dirname( filePath )
        if "" != folder:
            if not os.path.isdir( folder ):
                os.makedirs( folder, exist_ok = True )

        array = []
        array.append( [ "Class name", "Instance name", "UUID" ] )
        for cName in list( self.uuidDB.keys() ):
            for iName in list( self.uuidDB[cName] ):
                array.append( [ cName, iName, self.uuidDB[cName][iName] ] )

        nLinesWarn = 20 * 1000
        if len( array ) > nLinesWarn:
            logging.warning( "The size of the uuid database is over " + 
                    str(nLinesWarn) + " entries: " + str(len(array)) + "." )

        writeCsv( filePath, array )

        pass # UuidDB.saveDB()

    def getUUID( self, className, instanceName ):
        """
        className    - Is saved in the database for easier error detection, 
                       but it will not be added to the instanceName.
                       It is your responsibility to write a correct instanceName.
        instanceName - Instance, exactly how it should look in the database 
                       and later in ontology.
        return None if the instance does not exist
        """

        # Display the database (for debugging):
        if False:  
            print( ">>>>>>>>> Database status: >>>>>> " )
            for c in self.uuidDB:
                for i in self.uuidDB[c]:
                    print( "   ", c, i, ">>>", self.uuidDB[c][i] )

        if not isinstance( self.uuidDB, dict ):
            logging.error( " uuid database must be a dictionary, but got " +
                           str(type(self.uuidDB)) + ". Failed to get UUID." )
            #self.uuidDB = dict()
            return None

        # Check the input names:
        if not self.valueIsValid( className ):
            logging.error( " Class '" + className + "' is not valid. Failed to get UUID. a1" )
            return None

        if not self.valueIsValid( instanceName ):
            #logging.warning( " Instance '" + instanceName + "' is not valid. Failed to get UUID. a2" )
            logging.info( " Instance '" + instanceName + "' is not valid. Removing wrong characters. a2" )
            instanceName = valueToValid( instanceName )
            #return None

        # Check whether the instanceName exists in other classes:
        # Warning! For big database this verification is slow.
        for k1 in list( self.uuidDB.keys() ):
            for k2 in list( self.uuidDB[k1].keys() ):
                if k2 == instanceName and k1 != className:
                    logging.warning( " Instance '" + k2 + "' in '" + className + \
                                     "' may be in conflict with existing '" + \
                                     k1 + "." + k2 + "'." )

        uuidStr = "" # Default value of uuidStr

        # Find and return the new UUID for the className.instanceName
        if className in list(self.uuidDB.keys()):
            if instanceName in list(self.uuidDB[className]):
                uuidStr = self.uuidDB[className][instanceName]

        return str(uuidStr)
        pass # UuidDB.getUUID()


    def addUUID(self, className, instanceName, newUuid = "" ):
        """
            newUuid - Recommended uuid for this instance.
            If uuidDB has an element with this class+instance+uuid -> use it.
            If uuidDB has class+instance different uuid -> warning.
            If uuidDB does not have class+instance -> create with given newUuid.
            Return: a tuple of two names:
              - the new full name of instance: instance_uuid
              - the same uuid as a second entity of the tuple
        """

        if not isinstance( self.uuidDB, dict ):
            logging.error( " uuid database must be a dictionary, but got " +
                           str(type(self.uuidDB)) + " (line A2)." )
            self.uuidDB = dict()
            pass

        # This will be checked inside getUUID()
        #if not self.valueIsValid( className ):
        #    logging.error( " Class '" + className + "' is not valid. a3" )

        # This will be checked inside getUUID()
        #if not self.valueIsValid( instanceName ):
        #    logging.error( " Instance '" + instanceName + "' is not valid. a4" )

        if not isinstance( newUuid, str ):
            logging.error( " newUuid must be a string, but got " + str(type(newUuid)) + "." )
            return "",""

        # Check whether the instanceName exists in other classes:
        uuidStr = self.getUUID( className, instanceName )

        # Check for duplicates in existing database:
        #for k1 in list( self.uuidDB.keys() ):
        #    for k2 in list( self.uuidDB[k1].keys() ):
        #        if k1 != className and k2 == instanceName:
        #            logging.warning( " Instance '" + k2 + "' in '" + \
        #                             className + "' is in " + \
        #                             "conflict with existing '" + k1 + "'." )

        # Find are return or assign new UUID for the className.instanceName

        #if "" != newUuid and uuidStr != newUuid:
        #    logging.error( " Existing element in database has different uuid:" +
        #                   " class '" + className + "'," + 
        #                   " instance '" + instanceName + "'," +
        #                   " in DB '" + uuidStr + "', requested '" + newUuid + "'." )
 
        # Assign the value to the database:
        if "" == newUuid:
            if "" == uuidStr:
                uuidStr = str(uuid.uuid4())
        else:
            if None == uuidStr or "" == uuidStr:
                # Do nothing, the id does not exist or has errors
                pass
            elif uuidStr != newUuid:
                logging.warning( " Existing element in database has different uuid:" +
                                 " class '" + className + "'," + 
                                 " instance '" + instanceName + "' has UUID" +
                                 " in DB '" + uuidStr + "', while the new is '" + newUuid + 
                                 "'. The new UUID has priority and is overwritten in database." )
 
            uuidStr = newUuid

        if className not in list(self.uuidDB.keys()):
            self.uuidDB[className] = dict()
        
        self.uuidDB[className][instanceName] = uuidStr
 
        output = instanceName + "_" + str(uuidStr)

        return output, uuidStr

        #if className in list(self.uuidDB.keys()): <= must be true
        if True:
            if instanceName in list(self.uuidDB[className]):

                if "" != newUuid and uuidStr != newUuid:
                    logging.error( " Existing element in database has different uuid:" +
                               " class '" + className + "'," + 
                               " instance '" + instanceName + "'," +
                               " in DB '" + uuidStr + "', requested '" + newUuid + "'." )
            else:
                if "" == newUuid:
                    uuidStr = uuid.uuid4()
                else:
                    uuidStr = newUuid
                self.uuidDB[className][instanceName] = uuidStr
      
        else:
            if "" == newUuid:
                uuidStr = uuid.uuid4()
            else:
                uuidStr = newUuid

            self.uuidDB[className] = dict()
            self.uuidDB[className][instanceName] = uuidStr

        output = instanceName + "_" + str(uuidStr)

        return output #, uuidStr
        pass # UuidDB.addUUID()


    def tmpUUID( self, name ):
        """
        Create an instance and uuid without adding them to the database.
        Why is this necessary?
        """
        #output = ""
        #output = "uuid-" + code
  
        uuidStr = str(uuid.uuid4())

        output = name + "_" + uuidStr

        #logging.warning( "Not implemented getUUID(), using '" + str(uuidStr) + "'." )

        return output, uuidStr

        pass # UuidDB.tmpUUID()

    pass # class UuidDB

def loadUUID( filename = "" ):
  if "" == filename:
    filePath = os.path.join( "ontozeolite", "uuid", "default.csv" )
  else:
    filePath = filename

  if not os.path.isfile( filePath ):
    logging.info( "UUID DB does not exist, initialize a new DB" )
    return dict()
  
  array = readCsv( filePath )

  # Convert the list from csv into a dictionary:
  db = dict()
  if not isinstance(array,list):
    return db
  if len( array ) == 0:
    return db

  # Checking for the title line in the csv file:
  line = array[0] 
  if line[0].strip().lower() ==    "Class name".strip().lower() and \
     line[1].strip().lower() == "Instance name".strip().lower() and \
     line[2].strip().lower() ==          "UUID".strip().lower(): 
    startLine = 1
  else:
    startLine = 0

  for line in array[startLine:]:
    if len(line) == 3:
      cName = line[0]
      iName = line[1]
      uuidStr  = line[2]
      if len(line[0]) > 0 and len(line[1]) > 0 and len(line[2]) > 0:
        if not cName in list( db.keys() ):
          db[ cName ] = dict()
        if not iName in list( db[ cName ].keys() ):
          db[cName][iName] = uuidStr
        else:
          if uuidStr != db[cName][iName]:
            logging.error( "Mismatch uuid in database" )

  return db
  pass # loadUUID()

def saveUUID( db, filename = "default" ):
  if "default" == filename:
    filePath = os.path.join( "ontozeolite", "uuid", "default.csv" )
  else:
    filePath = filename

  folder = os.path.dirname( filePath )
  if not os.path.isdir( folder ):
    os.makedirs( folder, exist_ok = True )

  array = []
  array.append( [ "Class name", "Instance name", "UUID" ] )
  for cName in list( db.keys() ):
    for iName in list( db[cName] ):
      array.append( [ cName, iName, db[cName][iName] ] )

  nLinesWarn = 20 * 1000
  if len( array ) > nLinesWarn:
    logging.warning( "The size of the uuid database is over " + 
                     str(nLinesWarn) + " entries: " + str(len(array)) + "." )

  writeCsv( filePath, array )
  pass # saveUUID()

def valueIsValid( value ):
  if not isinstance( value, str ):
    logging.error( " Value must be string, but got '" + str(value) + "'." )
    return False

  short = value.strip()
  if short.find( " " ) >= 0:
    logging.info( " Value '" + value + "' contains space." )
    return False

  #short = value.strip()
  if short.find( "," ) >= 0:
    logging.info( " Value '" + value + "' contains comma." )
    return False

  #short = value.strip()
  if short.find( ";" ) >= 0:
    logging.info( " Value '" + value + "' contains semicolon." )
    return False

  return True

def valueToValid( value ):
    output = value
    output = output.replace( " ", "" )
    output = output.replace( ",", "" )
    output = output.replace( ";", "" )

    return output.strip()
    return 

def getCifLineRanges( fileIn ):
  ranges = []

  if not os.path.isfile( fileIn ):
    logging.error( "File '" + fileIn + "' does not exist " + \
                   "in tools.getCifLineRanges()" )
    return ranges

  f = open( fileIn )
  ranges.append( 0 )
  lineCount = 0
  for line in f:
    # TODO detect beginning of the new CIF file

    lineCount += 1
    pass

  ranges.append( lineCount )

  f.close()

  return ranges
  pass # getCifLineRanges()

def getUUID( uuidDB, className, instanceName, newUuid = "" ):
  """
  newUuid - Recommended uuid for this instance.
            If uuidDB has an element with this class+instance+uuid -> use it.
            If uuidDB has class+instance different uuid -> warning.
            If uuidDB does not have class+instance -> create with given newUuid.
  className - Is saved in the database for easier error detection, 
              but it will not be added to the instanceName.
              It is your responsibility to write a correct instanceName.
  instanceName - Instance, exactly how it should look in the database and later in ontology.
  """
  # Check the input names:
  if not valueIsValid( className ):
    logging.error( " Class '" + className + "' is not valid. a5" )

  if not valueIsValid( instanceName ):
    logging.error( " Instance '" + instanceName + "' is not valid. a6" )

  if not isinstance( uuidDB, dict ):
    logging.error( " uuid database must be a dictionary, but got " +
                   str(type(uuidDB)) + " (line A1)." )
    return ""

  if not isinstance( newUuid, str ):
    logging.error( " newUuid must be a string, but got " + str(type(newUuid)) + "." )

  # Check whether the instanceName exists in other classes:
  tmp = []
  for k1 in list( uuidDB.keys() ):
    for k2 in list( uuidDB[k1].keys() ):
      if k1 != className and k2 == instanceName:
        logging.warning( " Instance '" + k2 + "' in '" + className + "' is in " +
                         "conflict with existing '" + k1 + "." + k2 + "'." )

  # Find are return or assign new UUID for the className.instanceName
  if className in list(uuidDB.keys()):
    if instanceName in list(uuidDB[className]):
      uuidStr = uuidDB[className][instanceName]
      if "" != newUuid and uuidStr != newUuid:
        logging.error( " Existing element in database has different uuid:" +
                       " class '" + className + "'," + 
                       " instance '" + instanceName + "'," +
                       " in DB '" + uuidStr + "', requested '" + newUuid + "'." )
    else:
      if "" == newUuid:
        uuidStr = uuid.uuid4()
      else:
        uuidStr = newUuid
      uuidDB[className][instanceName] = uuidStr
      
  else:
    if "" == newUuid:
      uuidStr = uuid.uuid4()
    else:
      uuidStr = newUuid

    uuidDB[className] = dict()
    uuidDB[className][instanceName] = uuidStr

  output = instanceName + "_" + str(uuidStr)

  return output #, uuidStr
  pass # getUUID()


def getUUID_random( code ):
  """
  Create an instance and uuid without adding them to the database.
  Why is this necessary?
  """

  #output = ""
  #output = "uuid-" + code
  
  uuidStr = str(uuid.uuid4())

  output = code + "_" + str(uuidStr)

  logging.warning( "Not implemented getUUID(), using '" + str(uuidStr) + "'." )

  return output
  pass # getUUID_random()

def strSplit( inline, sep = [" ","\t"], quotes = ["'", '"'] ):
    words = []
    w = []
    #inQuote1 = False
    #inQuote2 = False
    inQuote = ""
    if isinstance( sep, list ):
      sepArr = sep
    elif isinstance( sep, str ):
      sepArr = [sep]
    else:
      logging.error( "Unknown type '" + str(type(sep)) + "' of sep '" + 
                   str(sep) + "'. Expected str or list." )

    for ic,c in enumerate(inline.strip()):
      if inQuote == "":
        #if "'" == c or '"' == c:
        if c in quotes:
          inQuote = c
        elif c in sepArr:
          if len(w) > 0:
            words.append( "".join(w) )
            w = []
        else:
          w.append(c)

      else:
        #if "'" == c or '"' == c:
        if c in quotes:
          inQuote = ""
          words.append( "".join(w) )
          w = []

        #elif " " == c:
        #  w.append(c)

        else:
          w.append(c)
    else:
      if len(w) > 0:
        words.append( "".join(w) )

    return words

    #return line.split()
    pass

writeCsvErrCount = 0
def writeCsv( filename, array ):
    global writeCsvErrCount
    logging.info( "writeCSV to '" + filename + "'" )
    try:
        with open( filename, "w", newline = "", encoding="utf-8" ) as f:
            csvw = csv.writer( f, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL )
            for a in array:
                csvw.writerow( a )
    except IOError:
        tmpFile = "test-tmp.csv"
        logging.error( " File '" + filename + "' is protected. " +
                       "Using temporary instead: '" + tmpFile + "'." )
        writeCsvErrCount += 1
        if 1 == writeCsvErrCount :
            writeCsv( tmpFile, array )
        else:
            logging.error( " I give up. " + "You need to close the files." )

    pass # writeCsv()


def readCsv( filename ):
  output = []
  if os.path.exists( filename ):
    with open( filename, "r" ) as f:
      csvr = csv.reader( f )
      for row in csvr:
        output.append( row )
  else:
    print( "readCSV: file does not exist: '" + filename + "'." )

  return output
  pass # loadCSV()


if __name__ == "__main__":

  # Test and example of use:
  db = loadUUID()

  print( getUUID( db, "ClassA", "a1" ) )
  print( getUUID( db, "ClassA", "a2" ) )
  print( getUUID( db, "ClassA", "a3" ) )
  print( getUUID( db, "ClassB", "b1" ) )
  print( getUUID( db, "ClassB", "a1" ) )
  #print( getUUID( db, "ClassC", "c1" ) )
  #print( getUUID( db, "ClassC", "c2" ) )
  #print( getUUID( db, "ClassC", "c3" ) )
  print( getUUID( "ss", "ClassC", "c3" ) )

  for k1 in db.keys():
    #print( "Class '" + k1 +"': " )
    for k2 in db[k1].keys():
      #print( " ", k2, ":", db[k1][k2] )
      pass

  saveUUID( db )

  input_strings = [ "1 2 3 4 5", "1   2  3   4  5\n", "1 '2 3' 4 5", "1 2 3 '4   5'"  ]
  for input_string in input_strings:
    out = strSplit(input_string )
    print( f"Input: {input_string}, Out: {str(out)}" )
    #print( out )


