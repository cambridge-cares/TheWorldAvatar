import numpy as np 
from rdflib import *
from rdflib.namespace import OWL,RDF,RDFS

g = Graph()
w = Namespace('https://example.org/')

def populate_classes(classes):
    # classes array takes string arguements and converts each to a class
    for i in classes:
        g.add((w[i],RDF.type,OWL.Class))
    return

classes = ['pipe','compression-station','intake','offtake','storage']
populate_classes(classes)

def populate_connection_relations():
    relations = [['hasInput','isInputOf'],['hasOutput','isOutputOf']]
    for i in relations:
        for j in i:
            g.add((w[j],RDF.type,OWL.ObjectProperty))
        g.add((w[i[0]],OWL.inverseOf,w[i[1]]))
    # pipe class is connected to every other class
    # (including another pipe allowing forks and cycles etc...)
    for i in relations:
            for k in classes:
                g.add((w[i[0]],RDFS.domain,w[k]))
                g.add((w[i[1]],RDFS.domain,w[k]))
    return

populate_connection_relations()

def populate_location_property():
    '''
    DESCRIPTION:
    Defines location as a property and subsequently
    defines the start and end locations as subproperties.

    Then lets every class have the property of location,
    removes this location for the pipe class and replaces it
    with the subproperty of start and end location 
    '''
    # addition of location as a property 
    g.add((w.hasLocation,RDF.type,OWL.DatatypeProperty))
    # addition of start and end as a property and 
    # subsequently as a subproperty of location 
    location_subproperties = ['hasStartLocation','hasEndLocation']
    for i in location_subproperties:
        g.add((w[i],RDF.type,OWL.DatatypeProperty))
        g.add((w[i],RDFS.subPropertyOf,w.hasLocation))

    # allowing every class to have a location, except for a pipe
    # which has a start and an end location 
    for i in classes:
        g.add((w.hasLocation,RDFS.domain,w[i]))

    g.remove((w.hasLocation,RDFS.domain,w.pipe))

    g.add((w.hasStartLocation,RDFS.domain,w.pipe))
    g.add((w.hasEndLocation,RDFS.domain,w.pipe))
    return

populate_location_property()

# defining subclass addition function
def subclass_addition(subclasses,ontoclass):
    '''
    DESCRIPTION: 
    takes an array of subclass identifiers (strings)
    and defines them as classes, and subsequently subclasses
    of the ontoclass string identifier
    ontoclass must already be a OWL.class 
    '''
    for i in subclasses:
        g.add((w[i],RDF.type,OWL.Class))
        g.add((w[i],RDFS.subClassOf,w[ontoclass]))
    return 

# addition of intake subclasses
intake_subclasses = ['coastal-terminal','onshore-field','pipeline-import',\
    'lng-import']
subclass_addition(intake_subclasses,'intake')

# addition of offtake subclasses
offtake_subclasses = ['power-station','international-export','local-distribution']
subclass_addition(offtake_subclasses,'offtake')

# addition of storage subclasses
storage_subclasses = ['lng-storage','hp-storage','salt-cavity']
subclass_addition(storage_subclasses,'storage')

def populate_DatatypeProperties(ontoclass,properties,composition_properties):
    '''
    DESCRIPTION:
    defines properties as OWL.DatatypeProperty, then assigns property to the 
    appropriate class.
    Then takes composition properties, defines them as properties, 
    subsequently subproperties of hasComposition which are assigned to the class.
    '''
    # assigning properties to the pipe class 
    # w/o location as previously defined
    for i in properties:
        g.add((w[i],RDF.type,OWL.DatatypeProperty))
        g.add((w[i],RDFS.domain,w[ontoclass]))
    # repeating for composition subproperties
    if len(composition_properties) != 0:
        for i in composition_properties:
            g.add((w[i],RDF.type,OWL.DatatypeProperty))
            g.add((w[i],RDFS.subPropertyOf,w.hasComposition))
            g.add((w[i],RDFS.domain,w[ontoclass]))
    return

pipe_properties = ['hasDiameter','hasStartPressure','hasPressureDrop',\
    'hasLength','hasThickness','hasCoating']
populate_DatatypeProperties('pipe',pipe_properties,[])

compressor_properties = ['hasPressureRatio','hasEfficiency']
populate_DatatypeProperties('compression-station',compressor_properties,[])

intake_properties = ['hasSupply']
populate_DatatypeProperties('intake',intake_properties,[])


offtake_properties = ['hasDemand']
populate_DatatypeProperties('offtake',offtake_properties,[])

storage_properties = ['hasCapacity']
populate_DatatypeProperties('storage',storage_properties,[])

def within_pipe_creation():
    # adding the 'stuff' in a pipe as a class
    g.add((w['within-pipe'],RDF.type,OWL.Class))
    g.add((w.hasWithin,RDF.type,OWL.ObjectProperty))
    for i in classes:
        g.add((w.hasWithin,RDFS.domain,w[i]))
        g.add((w.hasWithin,RDFS.range,w['within-pipe']))
        g.add((w.hasWithin,RDF.Property,w['within-pipe']))

    within_pipe_properties = ['hasComposition','hasTemperature,hasWobbeIndex']
    composition_properties = ['hasMethaneVolume','hasEthaneVolume',\
        'hasPropaneVolume','hasButaneVolume','hasNitrogenVolume',\
            'hasCarbonDioxideVolume','hasHydrogenVolume','hasHeliumVolume']

    populate_DatatypeProperties('within-pipe',within_pipe_properties,composition_properties)
    return 

g.serialize(destination='OntoGasGrid_Tbox.owl',format='n3')

#
# todo 
# make sure appropriate domains are defined for all properties