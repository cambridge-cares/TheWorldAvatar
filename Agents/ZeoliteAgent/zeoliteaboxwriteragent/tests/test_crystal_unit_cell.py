import unittest
import numpy

from SPARQLWrapper import SPARQLWrapper, JSON


address = "http://localhost:8080/blazegraph/namespace/zeo02c/sparql"
address = "http://localhost:8080/blazegraph/namespace/zeo03c/sparql"
# wrap the dbpedia SPARQL end-point
#endpoint = SPARQLWrapper("http://dbpedia.org/sparql")
#endpoint = SPARQLWrapper("http://localhost:8080/blazegraph/zeo2b/")
#endpoint = SPARQLWrapper("http://localhost:8080/blazegraph/namespace/zeo02b/sparql")

def getUCVectors( namespace ):
  endpoint = SPARQLWrapper( namespace )
  endpoint.setQuery( """
PREFIX zeo:	<http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>

#SELECT ?x ?y ?z
#SELECT ?zeoname ?a ?b ?c ?alpha ?beta ?gamma ?ax ?ay ?az ?bx ?by ?bz ?cx ?cy ?cz ?unitA ?unitD
# ?rabc  ?unitR
SELECT ?zeoname ?ax ?ay ?az ?bx ?by ?bz ?cx ?cy ?cz ?rax ?ray ?raz ?rbx ?rby ?rbz ?rcx ?rcy ?rcz ?volume #?rax ?ray ?raz ?rcz
#SELECT ?zeoname ?rax ?ay ?az ?volume
WHERE {
  ?zeo       zeo:hasZeoliteCode     ?zeoname .
  ?zeo       zeo:hasCIF             ?cifcore .
  ?cifcore   zeo:hasCIFCoreCell     ?unitcell .

  ?unitcell  zeo:hasReciprocalUnitCellVectorSet ?rabc .

  ?rabc      zeo:hasComponent       ?vec_ra .
  ?vec_ra    zeo:hasLabel           "a" .
  ?vec_ra    om:hasUnit             ?unitR .
  ?vec_ra    zeo:hasComponent       ?uax .
  ?vec_ra    zeo:hasComponent       ?uay .
  ?vec_ra    zeo:hasComponent       ?uaz .

  ?rabc      zeo:hasComponent       ?vec_rb .
  ?vec_rb    zeo:hasLabel           "b" .
  #?vec_rb    om:hasUnit            ?unitR .
  ?vec_rb    zeo:hasComponent       ?ubx .
  ?vec_rb    zeo:hasComponent       ?uby .
  ?vec_rb    zeo:hasComponent       ?ubz .

  ?rabc      zeo:hasComponent       ?vec_rc .
  ?vec_rc    zeo:hasLabel           "c" .
  #?vec_rc    om:hasUnit             ?unitR .
  ?vec_rc    zeo:hasComponent        ?ucx .
  ?vec_rc    zeo:hasComponent        ?ucy .
  ?vec_rc    zeo:hasComponent        ?ucz .

  ?uax       zeo:hasLabel            "x" .
  ?uax       zeo:hasValue            ?rax .
  ?uay       zeo:hasLabel            "y" .
  ?uay       zeo:hasValue            ?ray .
  ?uaz       zeo:hasLabel            "z" .
  ?uaz       zeo:hasValue            ?raz .

  ?ubx       zeo:hasLabel            "x" .
  ?ubx       zeo:hasValue            ?rbx .
  ?uby       zeo:hasLabel            "y" .
  ?uby       zeo:hasValue            ?rby .
  ?ubz       zeo:hasLabel            "z" .
  ?ubz       zeo:hasValue            ?rbz .

  ?ucx       zeo:hasLabel            "x" .
  ?ucx       zeo:hasValue            ?rcx .
  ?ucy       zeo:hasLabel            "y" .
  ?ucy       zeo:hasValue            ?rcy .
  ?ucz       zeo:hasLabel            "z" .
  ?ucz       zeo:hasValue            ?rcz .
  ###
  ?unitcell  zeo:hasUnitCellVectorSet ?abc .

  ?abc       zeo:hasComponent        ?vec_a .
  ?vec_a     zeo:hasLabel            "a" .
  ?vec_a      om:hasUnit             ?unitA .
  ?vec_a     zeo:hasComponent        ?vax .
  ?vec_a     zeo:hasComponent        ?vay .
  ?vec_a     zeo:hasComponent        ?vaz .


  ?abc       zeo:hasComponent       ?vec_b .
  ?vec_b     zeo:hasLabel           "b" .
  #?vec_b     om:hasUnit            ?unitA .
  ?vec_b     zeo:hasComponent       ?vbx .
  ?vec_b     zeo:hasComponent       ?vby .
  ?vec_b     zeo:hasComponent       ?vbz .

  ?abc       zeo:hasComponent       ?vec_c .
  ?vec_c     zeo:hasLabel           "c" .
  #?vec_c     om:hasUnit             ?unitA .
  ?vec_c     zeo:hasComponent        ?vcx .
  ?vec_c     zeo:hasComponent        ?vcy .
  ?vec_c     zeo:hasComponent        ?vcz .

  ?vax       zeo:hasLabel            "x" .
  ?vax       zeo:hasValue            ?ax .
  ?vay       zeo:hasLabel            "y" .
  ?vay       zeo:hasValue            ?ay .
  ?vaz       zeo:hasLabel            "z" .
  ?vaz       zeo:hasValue            ?az .

  ?vbx       zeo:hasLabel            "x" .
  ?vbx       zeo:hasValue            ?bx .
  ?vby       zeo:hasLabel            "y" .
  ?vby       zeo:hasValue            ?by .
  ?vbz       zeo:hasLabel            "z" .
  ?vbz       zeo:hasValue            ?bz .

  ?vcx       zeo:hasLabel            "x" .
  ?vcx       zeo:hasValue            ?cx .
  ?vcy       zeo:hasLabel            "y" .
  ?vcy       zeo:hasValue            ?cy .
  ?vcz       zeo:hasLabel            "z" .
  ?vcz       zeo:hasValue            ?cz .

  ###

  ?unitcell  zeo:hasVolume          ?om_v .
  ?om_v      om:hasNumericalValue   ?volume .
  #?om_v  ?x ?y .

  }
  """ )

  endpoint.setReturnFormat(JSON)
  output = dict()

  results = endpoint.query().convert()

# interpret the results: 
  for res in results["results"]["bindings"] :
    #print( res['zeoname']['value'], res['ax']['value'] )
    a = [ float( res[ 'ax']['value'] ), float( res[ 'ay']['value'] ), float( res[ 'az']['value'] ) ]
    b = [ float( res[ 'bx']['value'] ), float( res[ 'by']['value'] ), float( res[ 'bz']['value'] ) ]
    c = [ float( res[ 'cx']['value'] ), float( res[ 'cy']['value'] ), float( res[ 'cz']['value'] ) ]
    ra= [ float( res['rax']['value'] ), float( res['ray']['value'] ), float( res['raz']['value'] ) ]
    rb= [ float( res['rbx']['value'] ), float( res['rby']['value'] ), float( res['rbz']['value'] ) ]
    rc= [ float( res['rcx']['value'] ), float( res['rcy']['value'] ), float( res['rcz']['value'] ) ]

    zeoname = res['zeoname']['value'] 
    if zeoname not in list( output.keys() ):
      output[ zeoname ] = dict()
    output[ zeoname ]['a'] = a
    output[ zeoname ]['b'] = b
    output[ zeoname ]['c'] = c
    output[ zeoname ]['ra'] = ra
    output[ zeoname ]['rb'] = rb
    output[ zeoname ]['rc'] = rc
    output[ zeoname ]['vol'] = float( res['volume']['value'] )

  return output
  pass # getUCVectors() 


def getTransformationMatrix( namespace ):
  endpoint = SPARQLWrapper( namespace )
  endpoint.setQuery( """
PREFIX zeo:	<http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>
SELECT ?zeoname ?mc_xx ?mc_xy ?mc_xz ?mc_yx ?mc_yy ?mc_yz ?mc_zx ?mc_zy ?mc_zz ?vc_x ?vc_y ?vc_z             ?mf_xx ?mf_xy ?mf_xz ?mf_yx ?mf_yy ?mf_yz ?mf_zx ?mf_zy ?mf_zz ?vf_x ?vf_y ?vf_z
WHERE {
  ?zeo       zeo:hasZeoliteCode     ?zeoname .
  ?zeo       zeo:hasCIF             ?cifcore .

  ?cifcore   zeo:hasCifCoreTransformation ?trans.
  ?trans     zeo:hasTransformationMatrixToCartesian  ?mCart;
             zeo:hasTransformationMatrixToFractional ?mFrac ;
             zeo:hasTransformationVectorToCartesian  ?vCart;
             zeo:hasTransformationVectorToFractional ?vFrac .

  ?mCart     zeo:hasComponent       ?mc_11, ?mc_12, ?mc_13,
                                    ?mc_21, ?mc_22, ?mc_23,
                                    ?mc_31, ?mc_32, ?mc_33.
  ?mc_11     zeo:hasValue           ?mc_xx ;
             zeo:hasLabel           "xx" .
  ?mc_12     zeo:hasValue           ?mc_xy ;
             zeo:hasLabel           "xy" .
  ?mc_13     zeo:hasValue           ?mc_xz ;
             zeo:hasLabel           "xz" .
  ?mc_21     zeo:hasValue           ?mc_yx ;
             zeo:hasLabel           "yx" .
  ?mc_22     zeo:hasValue           ?mc_yy ;
             zeo:hasLabel           "yy" .
  ?mc_23     zeo:hasValue           ?mc_yz ;
             zeo:hasLabel           "yz" .
  ?mc_31     zeo:hasValue           ?mc_zx ;
             zeo:hasLabel           "zx" .
  ?mc_32     zeo:hasValue           ?mc_zy ;
             zeo:hasLabel           "zy" .
  ?mc_33     zeo:hasValue           ?mc_zz ;
             zeo:hasLabel           "zz" .

  ?vCart     zeo:hasComponent       ?vc_1, ?vc_2, ?vc_3 .
  ?vc_1      zeo:hasLabel           "x";
             zeo:hasValue           ?vc_x .
  ?vc_2      zeo:hasLabel           "y";
             zeo:hasValue           ?vc_y .
  ?vc_3      zeo:hasLabel           "z";
             zeo:hasValue           ?vc_z .

  ?mFrac     zeo:hasComponent       ?mf_11, ?mf_12, ?mf_13,
                                    ?mf_21, ?mf_22, ?mf_23,
                                    ?mf_31, ?mf_32, ?mf_33.
  ?mf_11     zeo:hasValue           ?mf_xx ;
             zeo:hasLabel           "xx" .
  ?mf_12     zeo:hasValue           ?mf_xy ;
             zeo:hasLabel           "xy" .
  ?mf_13     zeo:hasValue           ?mf_xz ;
             zeo:hasLabel           "xz" .
  ?mf_21     zeo:hasValue           ?mf_yx ;
             zeo:hasLabel           "yx" .
  ?mf_22     zeo:hasValue           ?mf_yy ;
             zeo:hasLabel           "yy" .
  ?mf_23     zeo:hasValue           ?mf_yz ;
             zeo:hasLabel           "yz" .
  ?mf_31     zeo:hasValue           ?mf_zx ;
             zeo:hasLabel           "zx" .
  ?mf_32     zeo:hasValue           ?mf_zy ;
             zeo:hasLabel           "zy" .
  ?mf_33     zeo:hasValue           ?mf_zz ;
             zeo:hasLabel           "zz" .

  ?vFrac     zeo:hasComponent       ?vf_1, ?vf_2, ?vf_3 .
  ?vf_1      zeo:hasLabel           "x";
             zeo:hasValue           ?vf_x .
  ?vf_2      zeo:hasLabel           "y";
             zeo:hasValue           ?vf_y .
  ?vf_3      zeo:hasLabel           "z";
             zeo:hasValue           ?vf_z .
  }
""" )

  endpoint.setReturnFormat(JSON)
  output = dict()

  results = endpoint.query().convert()

  for toCartesian in [ True, False ]:
    if toCartesian:
      char = 'c'
    else:
      char = 'f'

# interpret the results: 
    for res in results["results"]["bindings"] :
      #print( res['zeoname']['value'], res['ax']['value'] )
      m = [ [0]*3 for i in range(3) ]
      v = [0] * 3
      m[0][0] = float( res['m'+char+'_xx']['value'] )
      m[1][0] = float( res['m'+char+'_xy']['value'] )
      m[2][0] = float( res['m'+char+'_xz']['value'] )
      m[0][1] = float( res['m'+char+'_yx']['value'] )
      m[1][1] = float( res['m'+char+'_yy']['value'] )
      m[2][1] = float( res['m'+char+'_yz']['value'] )
      m[0][2] = float( res['m'+char+'_zx']['value'] )
      m[1][2] = float( res['m'+char+'_zy']['value'] )
      m[2][2] = float( res['m'+char+'_zz']['value'] )

      v[0]    = float( res['v'+char+'_x' ]['value'] )
      v[1]    = float( res['v'+char+'_y' ]['value'] )
      v[2]    = float( res['v'+char+'_z' ]['value'] )

      zeoname = res['zeoname']['value'] 
      if zeoname not in list( output.keys() ):
        output[ zeoname ] = dict()
      if toCartesian:
        output[ zeoname ]['mToCart'] = m
        output[ zeoname ]['vToCart'] = v
      else:
        output[ zeoname ]['mToFrac'] = m
        output[ zeoname ]['vToFrac'] = v
      #output[ zeoname ]['b'] = b
      #output[ zeoname ]['c'] = c
    #output[ zeoname ]['ra'] = ra
    #output[ zeoname ]['rb'] = rb
    #output[ zeoname ]['rc'] = rc
    #output[ zeoname ]['vol'] = float( res['volume']['value'] )

  return output
  pass # getTransformationMatrix()


def dot( v1, v2 ):
    output = 0
    for _1, _2 in zip(v1,v2):
        #print( _1, "   ", _2 )
        output += float(_1) * float(_2)
    return output


"""
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dbpr: <http://dbpedia.org/resource/>
SELECT  ?x ?y ?z
WHERE { ?x ?y ?z }
"""
unitCells = getUCVectors( address ) #["ABW"]

class TestUnitCell( unittest.TestCase ):
    def init(self):
        pass

    def test_UCReciprocalVectors( self ):
        """
        1. Check whether the reciprocal vectors satifcy the equality:
           a_i \cdot a_j^* = \delta_ij
        2. Check the saved unit cell volume vs computed from the vectors a,b,c.

        """
        unitCells = getUCVectors( address ) #["ABW"]
        for k in list( unitCells.keys() ):
            uc = unitCells[k]
            self.assertAlmostEqual( dot(uc["a"],uc["ra"]), 1 )
            self.assertAlmostEqual( dot(uc["a"],uc["rb"]), 0 )
            self.assertAlmostEqual( dot(uc["a"],uc["rc"]), 0 )

            self.assertAlmostEqual( dot(uc["b"],uc["ra"]), 0 )
            self.assertAlmostEqual( dot(uc["b"],uc["rb"]), 1 )
            self.assertAlmostEqual( dot(uc["b"],uc["rc"]), 0 )

            self.assertAlmostEqual( dot(uc["c"],uc["ra"]), 0 )
            self.assertAlmostEqual( dot(uc["c"],uc["rb"]), 0 )
            self.assertAlmostEqual( dot(uc["c"],uc["rc"]), 1 )
        pass # test_ReciprocalVectors()

    def test_volume( self ):
        unitCells = getUCVectors( address ) #["ABW"]
        for k in list( unitCells.keys() ):
            uc = unitCells[k]

            tmp = [ float(x) for x in uc["a"] ]
            na = numpy.array( tmp )
            tmp = [ float(x) for x in uc["b"] ]
            nb = numpy.array( tmp )
            tmp = [ float(x) for x in uc["c"] ]
            nc = numpy.array( tmp )
 
            vol = numpy.dot( numpy.cross( na, nb ), nc ) 
            self.assertAlmostEqual( vol, uc["vol"] )
        pass # test_volume

    def test_coordinate_transformations( self ):
        # Warning! There are also shift vectors in these transformations, 
        #          but they are not defined in CIF, so there is not way to test them.

        trans = getTransformationMatrix( address )
        unitCells = getUCVectors( address ) #["ABW"]

        for ik, k in enumerate(list( trans.keys() )):
          #print( ik )
          #print( k )
          #print( numpy.array(trans[k]["mToCart"]) )

          mToCart = numpy.array( trans[k]['mToCart'] )
          mToFrac = numpy.array( trans[k]['mToFrac'] )
          vToCart = numpy.array( trans[k]['vToCart'] )
          vToFrac = numpy.array( trans[k]['vToFrac'] )
          #print( mToCart )

          # Test 1: toCart and back should give the original position
          testM = numpy.dot( mToCart , mToFrac )
          for iy in range(3):
              for ix in range(3):
                  if ix == iy:
                      self.assertAlmostEqual( testM[iy][ix], 1. )
                  else:
                      self.assertAlmostEqual( testM[iy][ix], 0. )

          # Test 2: toCart transforms [1,0,0] into a, [0,1,0] into b, and 
          #         [0,0,1] into c vectors.
          uc = unitCells[k]
          toBeA = numpy.dot( mToCart, numpy.array([1,0,0]) )
          #print( toBeA )
          self.assertAlmostEqual( toBeA[0], uc["a"][0], msg=k )
          self.assertAlmostEqual( toBeA[1], uc["a"][1], msg=k )
          self.assertAlmostEqual( toBeA[2], uc["a"][2], msg=k )

          toBeB = numpy.dot( mToCart, numpy.array([0,1,0]) )
          self.assertAlmostEqual( toBeB[0], uc["b"][0], msg=k )
          self.assertAlmostEqual( toBeB[1], uc["b"][1], msg=k )
          self.assertAlmostEqual( toBeB[2], uc["b"][2], msg=k )

          toBeC = numpy.dot( mToCart, numpy.array([0,0,1]) )
          self.assertAlmostEqual( toBeC[0], uc["c"][0], msg=k )
          self.assertAlmostEqual( toBeC[1], uc["c"][1], msg=k )
          self.assertAlmostEqual( toBeC[2], uc["c"][2], msg=k )



        pass # test_coordinate_transformations


    pass # class TestUnitCell


'''
for k in list( unitCells.keys() ):
    uc = unitCells[k]
    #print( uc["a"], uc["ra"] )
    assert( round( dot(uc["a"],uc["ra"]), 8 ) == 1. )
    assert( round( dot(uc["a"],uc["rb"]), 8 ) == 0. )
    assert( round( dot(uc["a"],uc["rc"]), 8 ) == 0. )

    assert( round( dot(uc["b"],uc["ra"]), 8 ) == 0. )
    assert( round( dot(uc["b"],uc["rb"]), 8 ) == 1. )
    assert( round( dot(uc["b"],uc["rc"]), 8 ) == 0. )

    assert( round( dot(uc["c"],uc["ra"]), 8 ) == 0. )
    assert( round( dot(uc["c"],uc["rb"]), 8 ) == 0. )
    assert( round( dot(uc["c"],uc["rc"]), 8 ) == 1. )
#    print( dot(uc["a"],uc["rb"]) )

'''

if __name__ == "__main__":
    unittest.main()


