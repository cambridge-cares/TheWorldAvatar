"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import os
import csv
import re
import zeolist

filename = os.path.join( "ontozeolite", "zeolite", "data", "Tile-signature-2010.csv" )

# Find the list of tiles in the list of zeolites dated 2010.
# 
# 

def cellToCages( cell ):
  # Return strucuture is list of cases: cellToCages
  #print( "Cell = '" + cell + "'" )
  output = []
  cellShort = cell.strip( ' "' )

  cages = cellShort.split( "+" )

  for cage in cages:
    pos = cage.find( "[" ) 
    if pos > 0:
      n = int( cage[:pos] )
      c = cage[pos:]
    else:
      n = 1
      c = cage
    output.append( [n,c] )
  return output
  pass # parseCell()

def cageToFaces( cage ):
  output = []
  cageShort = cage.strip( "[]" )

  faces = cageShort.split( "." )
  #print( "cage =", cage )
  #print( "    faces =", faces )
  for face in faces:
    pos = face.find( "^" )
    if pos > 0:
      edges = face[:pos  ]
      times = int( face[pos+1:] )
    else:
      edges = face
      times = 1
    output.append( [edges, times] )

  return output
  pass # cageToFaces()

def getDataByCode( dataIn, code ):
  for d in dataIn:
    if d[0].find( code.replace("-", "").replace("_", "") ) >= 0:
      return d
    #if d[0].find( code ) >= 0:
    #  return d
  
  #logging.error( "In tile-signature: unknown code '" + code + "'" )
  print( "In tile-signature: unknown code '" + code + "'" )
  return None

  pass # getDataByCode()

def getNEdge( face ):
  tmp = 0
  tmp = re.findall( r"\d+", face )
  #print( "face =", tmp )
  #print( face, " =>", tmp )

  return int( tmp[0].strip("'") )
  pass # getNEdge()


if __name__ == "__main__":

  dataIn = zeolist.readCsv( filename )

  # List of fundamental tiles (i.e. 4a,4b,etc)
  tileFund = []
  tileRpt  = 0
  faceFund = []

  for i in range( 20 + 1):
    faceFund.append( [] )

  for data in dataIn:
    d3 = data[3]
    if len( d3.strip() ) > 0:
      cages = cellToCages( d3 )
  #print( dataIn )
  #cages = cellToCages( dataIn[ 3][3] )
  #cages = cellToCages( dataIn[26][3] )
  #print( cages )

      for cage in cages:
        c = cage[1]
        if not c in tileFund:
          tileFund.append( c )
        else:
          tileRpt += 1
          #print( "repeated",  c, "in tile", data[0]  )

        a = cageToFaces( c )
        for ef in a: # edges and factor
          face = ef[0]
          e = getNEdge( face )
          if not face in faceFund[ e ]:
            faceFund[e].append( face )

    #print( a )

#print( faceFund )
#print( "==============================" )
  count = 0
  for i, t in enumerate(faceFund):
    if i > 0:
      count += len( t )
      print( i, "(" + str(len(t)) + "):",  sorted(t) )

  print( "Count unique faces:", count )

  print( "Count unique cages:", len( tileFund ), ", repeated :", tileRpt )

