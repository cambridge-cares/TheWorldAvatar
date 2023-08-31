import logging
#logging.basicConfig( level = logging.INFO )
logging.basicConfig( level = logging.WARNING )
import csv
import os
import uuid


def loadUUID( filename = "default" ):
  if "default" == filename:
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
    logging.error( "Value must be string, but got '" + str(value) + "'." )
    return False

  short = value.strip()
  if short.find( " " ) >= 0:
    logging.error( "value '" + value + "' contains space." )
    return False

  short = value.strip()
  if short.find( "," ) >= 0:
    logging.error( "value '" + value + "' contains comma." )
    return False

  short = value.strip()
  if short.find( ";" ) >= 0:
    logging.error( "value '" + value + "' contains semicolon." )
    return False

  return True

def getUUID( uuidDB, className, instanceName ):
  # Check the input names:
  if not valueIsValid( className ):
    logging.error( "class '" + className + "' is not valid." )

  if not valueIsValid( instanceName ):
    logging.error( "instance '" + instanceName + "' is not valid." )

  if not isinstance( uuidDB, dict ):
    logging.error( "uuid database must be a dictionary, but got " +
                   str(type(uuidDB)) + "." )
    return "UNDEF"

  # Check whether the instanceName exists in other classes:
  tmp = []
  for k1 in list( uuidDB.keys() ):
    for k2 in list( uuidDB[k1].keys() ):
      if k1 != className and k2 == instanceName:
        logging.warning( "Instance '" + k2 + "' in '" + className + "' is in " +
                         "conflict with existing '" + k1 + "." + k2 + "'." )

  # Find are return or assign new UUID for the className.instanceName
  if className in list(uuidDB.keys()):
    if instanceName in list(uuidDB[className]):
      uuidStr = uuidDB[className][instanceName]
    else:
      uuidStr = uuid.uuid4()
      uuidDB[className][instanceName] = uuidStr
      
  else:
    uuidStr = uuid.uuid4()

    uuidDB[className] = dict()
    uuidDB[className][instanceName] = uuidStr

  output = instanceName + "_" + str(uuidStr)

  return output
  pass # getUUID()


def getUUID_random( code ):

  #output = ""
  #output = "uuid-" + code
  
  uuidStr = uuid.uuid4()

  output = code + "_" + str(uuidStr)

  logging.warning( "Not implemented getUUID(), using '" + str(uuidStr) + "'." )

  return output
  pass # getUUID_random()


def writeCsv( filename, array ):
  logging.info( "writeCSV to '" + filename + "'" )
  try:
    with open( filename, "w", newline = "" ) as f:
      csvw = csv.writer( f, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL )
      for a in array:
        csvw.writerow( a )
  except IOError:
    print( "Error! File '" + filename + "' is protected. " + 
           "Using temporary instead: '" + "test-tmp.csv" + "'." )

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


