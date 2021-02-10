##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 17 Dec 2020                      #
##########################################

"""GMLParser is developed to parse crop map data encoded in GML,
structure the data by following an ontological model, and finally
represent the data using RDF."""

import time
from builtins import enumerate

from filetype.types.image import Cr2
from lxml import etree
from pandas.io.json import _json_normalize
from pyproj import Proj, transform
from rdflib.extras.infixowl import Ontology, OWL_NS

from CropMap import CropMap
from Envelope import Envelope
from rdflib import Graph, URIRef, XSD

import PropertyReader as propread
import GMLParserPropReader as gmlpropread
import ABoxGeneration as aboxgen
import EntityRDFizer as rdfizer

import uuid

OBJECT_ID = 'OBJECTID'
CROME_ID = 'cromeid'
LUCODE = 'lucode'
REF_DATE = 'refdate'
SHAPE_LENGTH = 'Shape_Length'
SHAPE_AREA = 'Shape_Area'
SURFACE_PROPERTY = 'surfaceProperty'

URL_ENVELOPE = '{http://www.opengis.net/gml}Envelope'
URL_FEATURE_MEMBER = '{http://www.opengis.net/gml}featureMember'
URL_ID = '{http://www.opengis.net/gml}id'

ATTRB_SRS_NAME = 'srsName'
ATTRB_SRS_DIMENSION = 'srsDimension'
ENVELOPE_LOWER_CORNER = 'lowerCorner'
ENVELOPE_UPPER_CORNER = 'upperCorner'

ENVELOPE_INSTANCE_PREFIX = 'Envelope_of_'
EPSG_4326 = 'EPSG:4326'
EPSG_27700 = 'EPSG:27700'

"""Declared a variable for creating a graph model"""
g = Graph()
envelope = Envelope()

"""Creates a context for parsing when the GML file name and tag is given"""
def get_context(file_name, tag):
    return etree.iterparse(file_name, events=('end',), tag=tag)

"""Parses the following attributes of GML envelope and returns the envelope
- srsName
- srsDimension
"""
def get_envelope(context):
    for event, elem in context:
        global envelope
        envelope.srsName = elem.attrib[ATTRB_SRS_NAME]
        envelope.srsDimension = elem.attrib[ATTRB_SRS_DIMENSION]
        for geospatialData in elem:
            if geospatialData.text == None or geospatialData.text == '':
                continue
            if get_tag_name(geospatialData.tag.lower()) == ENVELOPE_LOWER_CORNER.lower():
                envelope.lowerCorner = geospatialData.text
            if get_tag_name(geospatialData.tag.lower()) == ENVELOPE_UPPER_CORNER.lower():
                envelope.upperCorner = geospatialData.text
        return envelope

"""Parses properties of the current crop map"""
def get_crop_map(context):
    map_counter = 0
    file_counter = 0
    global g
    for event, elem in context:
        #print(elem)
        for map in elem:
            #print(get_tag_name(map.tag))
            cropMap = CropMap()
            cropMap.name = get_tag_name(map.tag)
            cropMap.id = map.attrib[URL_ID]
            #print('cropMap.id',cropMap.id)
            for attribute in map:
                #print('attribute.text', attribute.text)
                if attribute.text == None or attribute.text == '':
                    continue
                if get_tag_name(attribute.tag.lower()) ==  OBJECT_ID.lower():
                    cropMap.objectID = attribute.text
                    aboxgen.link_data_with_type(g, URIRef(gmlpropread.getObjectIDVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.objectID, XSD.integer)
                    #print('OBJECT_ID', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  CROME_ID.lower():
                    cropMap.cromeID = attribute.text
                    aboxgen.link_data_with_type(g, URIRef(gmlpropread.getCromeIDVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.cromeID, XSD.string)
                    if getcentre_point_from_crome_id(cropMap.cromeID) != None:
                        aboxgen.link_data_with_type(g, URIRef(gmlpropread.getCentrePoint()),
                                      URIRef(gmlpropread.getABoxIRI()
                                      + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      getcentre_point_from_crome_id(cropMap.cromeID),
                                      URIRef(gmlpropread.getDataTypeCoordinatePoint()))

                    #print('cromeid', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  LUCODE.lower():
                    cropMap.luCode = attribute.text
                    aboxgen.link_instance(g, URIRef(gmlpropread.getLucodeVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      URIRef(propread.getABoxIRI()+rdfizer.SLASH+cropMap.luCode))
                    #print('lucode', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  REF_DATE.lower():
                    cropMap.refDate = attribute.text
                    aboxgen.link_data_with_type(g, URIRef(gmlpropread.getRefDateVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.refDate, XSD.integer)
                    #print('refdate', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  SHAPE_LENGTH.lower():
                    cropMap.shapeLength = attribute.text
                    aboxgen.link_data_with_type(g, URIRef(gmlpropread.getShapeLengthVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.shapeLength, XSD.double)
                    #print('Shape_Length', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  SHAPE_AREA.lower():
                    cropMap.shapeArea = attribute.text
                    aboxgen.link_data_with_type(g, URIRef(gmlpropread.getShapeAreaVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.shapeArea, XSD.double)
                    #print('Shape_Area', attribute.text)
                if get_tag_name(attribute.tag.lower()) == SURFACE_PROPERTY.lower():
                    for surface in attribute:
                        for pathces in surface:
                            for polygonPatch in pathces:
                                for exterior in polygonPatch:
                                    for linerRing in exterior:
                                        for posList in linerRing:
                                            aboxgen.create_instance(g,
                                                                    URIRef(gmlpropread.getClassLinearRing()),
                                                                    gmlpropread.getABoxIRI() + rdfizer.SLASH
                                                                    + rdfizer.format_iri(cropMap.id),
                                                                    cropMap.name)
                                            aboxgen.link_data_with_type(g, URIRef(gmlpropread.getPropertyPosList()),
                                                              URIRef(gmlpropread.getABoxIRI()
                                                                     + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                                                     posList.text.replace(" ", "#"),
                                                                     URIRef(gmlpropread.getDataTypePolygonalPoints()))
                                            #print(cropMap.polygon)
            """Adds data and metadata to the envelope"""
            if map_counter == 0:
                aboxgen.create_instance(g,
                                        URIRef(gmlpropread.getClassEnvelope()),
                                        gmlpropread.getABoxIRI() + rdfizer.SLASH
                                        + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name),
                                        ENVELOPE_INSTANCE_PREFIX+cropMap.name)
                aboxgen.link_data_with_type(g, URIRef(gmlpropread.getSrsName()),
                                        URIRef(gmlpropread.getABoxIRI() + rdfizer.SLASH
                                        + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name)),
                                        EPSG_4326, XSD.string)
                aboxgen.link_data_with_type(g, URIRef(gmlpropread.getSrsDimension()),
                                        URIRef(gmlpropread.getABoxIRI() + rdfizer.SLASH
                                        + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name)),
                                        envelope.srsDimension, XSD.integer)
                aboxgen.link_data_with_type(g, URIRef(gmlpropread.getLowerCorner()),
                                        URIRef(gmlpropread.getABoxIRI()+ rdfizer.SLASH
                                        + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name)),
                                        envelope.lowerCorner.replace(' ', '#'),
                                        gmlpropread.getDataTypeCoordinatePoint())
                aboxgen.link_data_with_type(g, URIRef(gmlpropread.getUpperCorner()),
                                        URIRef(gmlpropread.getABoxIRI()+ rdfizer.SLASH
                                        + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name)),
                                        envelope.upperCorner.replace(' ', '#'),
                                        gmlpropread.getDataTypeCoordinatePoint())
            """Links each feature to the envelope"""
            aboxgen.link_instance(g, URIRef(gmlpropread.getBoundedBy()),
                                  URIRef(gmlpropread.getABoxIRI() + rdfizer.SLASH
                                  + rdfizer.format_iri(cropMap.id)),
                                  URIRef(gmlpropread.getABoxIRI() + rdfizer.SLASH
                                  + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name)))
        map_counter += 1
        if map_counter % int(gmlpropread.getNOfMapsInAnAboxFile()) == 0:
            save_into_disk(g, str(uuid.uuid4())+rdfizer.UNDERSCORE+str(file_counter))
            file_counter += 1
            g = Graph()
            print('Total number of feature members processed:', map_counter)
    if map_counter % int(gmlpropread.getNOfMapsInAnAboxFile()) != 0:
        save_into_disk(g, str(uuid.uuid4())+rdfizer.UNDERSCORE+str(file_counter))
        print('Total number of feature members processed:', map_counter)

"""Converts polygon coordinates represented in EPSG 27700 into WGS84"""
def convert_polygon_from_epsg27700_to_wgs84(delimiter, span, string):
    wgs84_coordinates = ''
    flag = True
    for token in split_at_span(delimiter, span, string):
        if flag:
            wgs84_coordinates = wgs84_coordinates + convert_epsg27700_to_wgs84(token, delimiter)
        else:
            wgs84_coordinates = wgs84_coordinates + delimiter + convert_epsg27700_to_wgs84(token, delimiter)
        flag = False
    return wgs84_coordinates

"""Converts coordinates represented in EPSG 27700 into WGS84, which is equivalent to EPSG 4326"""
def convert_epsg27700_to_wgs84(coordinate_String, delimiter):
    from pyproj import Transformer
    transformer = Transformer.from_crs("epsg:27700", "epsg:4326")
    x2, y2 = transformer.transform(coordinate_String.split(delimiter)[0], coordinate_String.split(delimiter)[1])
    return str(x2) + delimiter + str(y2)

"""Extracts the centre point from the CROME ID"""
def getcentre_point_from_crome_id(cromeID):
    coordinates = cromeID[3:]
    if len(coordinates) % 2 == 0:
        easting = coordinates[:int(len(coordinates)/2)]
        northing = coordinates[int(len(coordinates)/2):]
        return easting+'#'+northing
    return None

"""Saves a graph into the disk/file system"""
def save_into_disk(g, file_name):
    # g.bind('owl:imports', 'http://theworldavatar.com/ontology/ontolanduse/OntoLandUse.owl')
    g.set((g.identifier, OWL_NS['imports'], URIRef(gmlpropread.getImportOntologyURL())))
    g.serialize(destination=str(file_name) + gmlpropread.getABoxFileExtension(),
                format="application/rdf+xml")


"""Splits the given string after certain number of substrings, indicated by the span, separated by the delimeter"""
def split_at_span(delimiter, span, string):
    words = string.split(delimiter)
    return [delimiter.join(words[i:i + span]) for i in range(0, len(words), span)]

"""Extracts and returns the name of tag from a URL"""
def get_tag_name(url):
    if '}' in url:
        tokens = url.split('}')
        if len(tokens)>1:
            return tokens[1]
    return None

"""Parses a standard GML file consisting of an Envelope and a set of feature members"""
def parse_gml(file_name):
    print('Parsing in progress...')
    context = get_context(file_name, URL_ENVELOPE)
    get_envelope(context)
    context = get_context(file_name, URL_FEATURE_MEMBER)
    get_crop_map(context)

"""This block of code is the access point to this Python module"""
if __name__ == '__main__':
    parse_gml(rdfizer.select_file())
