import unittest
import numpy

from SPARQLWrapper import SPARQLWrapper, JSON


address = "http://localhost:8080/blazegraph/namespace/zeo02c/sparql"
# wrap the dbpedia SPARQL end-point
#endpoint = SPARQLWrapper("http://dbpedia.org/sparql")
#endpoint = SPARQLWrapper("http://localhost:8080/blazegraph/zeo2b/")
#endpoint = SPARQLWrapper("http://localhost:8080/blazegraph/namespace/zeo02b/sparql")

def getUCVectors( end ):
  endpoint = SPARQLWrapper( end )
  endpoint.setQuery( """
PREFIX zeo:	<http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?zeoname ?ax ?ay ?az ?bx ?by ?bz ?cx ?cy ?cz ?rax ?ray ?raz ?rbx ?rby ?rbz ?rcx ?rcy ?rcz ?volume #?rax ?ray ?raz ?rcz
WHERE {
  ?zeo       zeo:hasZeoliteCode     ?zeoname .
  ?zeo       zeo:hasCIF             ?cifcore .
  ?cifcore   zeo:hasCIFCoreCell     ?unitcell .
  ?unitcell  zeo:hasUnitCellLengths ?abc .

  ?unitcell  zeo:hasReciprocalUnitCellVectorSet ?rabc .

  ?rabc      zeo:hasComponent       ?vec_ra .
  ?vec_ra    zeo:hasLabel           "a" .
  ?vec_ra    om:hasUnit             ?rUnit .
  ?vec_ra    zeo:hasComponent        ?vax .
  ?vec_ra    zeo:hasComponent        ?vay .
  ?vec_ra    zeo:hasComponent        ?vaz .

  ?rabc      zeo:hasComponent       ?vec_rb .
  ?vec_rb    zeo:hasLabel           "b" .
  #?vec_rb    om:hasUnit             ?rUnit .
  ?vec_rb    zeo:hasComponent        ?vbx .
  ?vec_rb    zeo:hasComponent        ?vby .
  ?vec_rb    zeo:hasComponent        ?vbz .

  ?rabc      zeo:hasComponent       ?vec_rc .
  ?vec_rc    zeo:hasLabel           "c" .
  #?vec_rc    om:hasUnit             ?rUnit .
  ?vec_rc    zeo:hasComponent        ?vcx .
  ?vec_rc    zeo:hasComponent        ?vcy .
  ?vec_rc    zeo:hasComponent        ?vcz .

  ?vax       zeo:hasLabel            "x" .
  ?vax       zeo:hasValue            ?rax .
  ?vay       zeo:hasLabel            "y" .
  ?vay       zeo:hasValue            ?ray .
  ?vaz       zeo:hasLabel            "z" .
  ?vaz       zeo:hasValue            ?raz .

  ?vbx       zeo:hasLabel            "x" .
  ?vbx       zeo:hasValue            ?rbx .
  ?vby       zeo:hasLabel            "y" .
  ?vby       zeo:hasValue            ?rby .
  ?vbz       zeo:hasLabel            "z" .
  ?vbz       zeo:hasValue            ?rbz .

  ?vcx       zeo:hasLabel            "x" .
  ?vcx       zeo:hasValue            ?rcx .
  ?vcy       zeo:hasLabel            "y" .
  ?vcy       zeo:hasValue            ?rcy .
  ?vcz       zeo:hasLabel            "z" .
  ?vcz       zeo:hasValue            ?rcz .


  ?abc       zeo:hasComponent       ?paramA .
  ?abc       zeo:hasComponent       ?paramB .
  ?abc       zeo:hasComponent       ?paramC .
  ?paramA    zeo:hasLabel           "a"  ;
             zeo:hasValue           ?a   .
  ?paramB    zeo:hasLabel           "b"  ;
             zeo:hasValue           ?b   .
  ?paramC    zeo:hasLabel           "c"  ;
             zeo:hasValue           ?c   .
  #?abc       om:hasUnit             ?unitA  .

  ?unitcell  zeo:hasUnitCellAngles  ?abg .
  ?abg       zeo:hasComponent       ?paramAlpha   .
  ?abg       zeo:hasComponent       ?paramBeta   .
  ?abg       zeo:hasComponent       ?paramGamma   .
  ?paramAlpha zeo:hasLabel           "alpha"  ;
             zeo:hasValue           ?alpha   .
  ?paramBeta zeo:hasLabel           "beta"  ;
             zeo:hasValue           ?beta   .
  ?paramGamma zeo:hasLabel           "gamma"  ;
             zeo:hasValue           ?gamma   .
#  ?abg       om:hasUnit             ?unitD  .

  ?unitcell  zeo:hasUnitCellVectors ?ucvec .
  ?ucvec     zeo:hasComponent       ?ptr_a .
  ?ptr_a     zeo:hasLabel           "a";
             zeo:hasValue           ?vec_a .
  ?vec_a     zeo:hasComponent       ?vec_ax .
  ?vec_a     zeo:hasComponent       ?vec_ay .
  ?vec_a     zeo:hasComponent       ?vec_az .
  ?vec_ax    zeo:hasLabel           "x" ;
             zeo:hasValue           ?ax .
  ?vec_ay    zeo:hasLabel           "y" ;
             zeo:hasValue           ?ay .
  ?vec_az    zeo:hasLabel           "z" ;
             zeo:hasValue           ?az .
  ?vec_a     om:hasUnit             ?unitA  .

  ?ucvec     zeo:hasComponent       ?ptr_b .
  ?ptr_b     zeo:hasLabel           "b";
             zeo:hasValue           ?vec_b .
  ?vec_b     zeo:hasComponent       ?vec_bx .
  ?vec_b     zeo:hasComponent       ?vec_by .
  ?vec_b     zeo:hasComponent       ?vec_bz .
  ?vec_bx    zeo:hasLabel           "x" ;
             zeo:hasValue           ?bx .
  ?vec_by    zeo:hasLabel           "y" ;
             zeo:hasValue           ?by .
  ?vec_bz    zeo:hasLabel           "z" ;
             zeo:hasValue           ?bz .
  #?vec_b     om:hasUnit             ?unitA  .

  ?ucvec     zeo:hasComponent       ?ptr_c .
  ?ptr_c     zeo:hasLabel           "c";
             zeo:hasValue           ?vec_c .
  ?vec_c     zeo:hasComponent       ?vec_cx .
  ?vec_c     zeo:hasComponent       ?vec_cy .
  ?vec_c     zeo:hasComponent       ?vec_cz .
  ?vec_cx    zeo:hasLabel           "x" ;
             zeo:hasValue           ?cx .
  ?vec_cy    zeo:hasLabel           "y" ;
             zeo:hasValue           ?cy .
  ?vec_cz    zeo:hasLabel           "z" ;
             zeo:hasValue           ?cz .
  #?vec_c     om:hasUnit             ?unitA  .

  ?unitcell  zeo:hasVolume          ?om_v .
  ?om_v      om:hasNumericalValue   ?volume .

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
        pass

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


