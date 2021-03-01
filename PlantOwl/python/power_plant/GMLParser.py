##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 17 Dec 2020                      #
##########################################

"""GMLParser is developed to parse crop map data encoded in GML,
structure the data by following an ontological model, and finally
represent the data using RDF."""
import os
import sys
from pathlib import Path

from lxml import etree
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
def get_crop_map(context, output_folder_path, start_feature_number, upper_limit):
    map_counter = 0
    file_counter = 0
    flag = False
    global g
    for event, elem in context:
        #print(elem)
        map_counter += 1
        """Following two conditional statements enables the processing of feature maps within a specified range"""
        if map_counter < int(start_feature_number):
            continue
        if int(upper_limit) !=-9999 and map_counter > int(upper_limit):
            flag = True
            break
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
                                                    convert_epsg27700_to_wgs84(getcentre_point_from_crome_id(cropMap.cromeID), "#"),
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
                                                                               + rdfizer.SLASH + rdfizer.format_iri(
                                                                            cropMap.id)),
                                                                        convert_polygon_from_epsg27700_to_wgs84(
                                                                            posList.text.replace(" ", "#"), "#", 2),
                                                                        URIRef(
                                                                            gmlpropread.getDataTypePolygonalPoints()))
                                            #print(cropMap.polygon)
            """Adds data and metadata to the envelope"""
            if map_counter == 1:
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
                                        convert_epsg27700_to_wgs84(envelope.lowerCorner.replace(' ', '#'), "#"),
                                        gmlpropread.getDataTypeCoordinatePoint())
                aboxgen.link_data_with_type(g, URIRef(gmlpropread.getUpperCorner()),
                                        URIRef(gmlpropread.getABoxIRI()+ rdfizer.SLASH
                                        + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name)),
                                        convert_epsg27700_to_wgs84(envelope.upperCorner.replace(' ', '#'), "#"),
                                        gmlpropread.getDataTypeCoordinatePoint())
            """Links each feature to the envelope"""
            aboxgen.link_instance(g, URIRef(gmlpropread.getBoundedBy()),
                                  URIRef(gmlpropread.getABoxIRI() + rdfizer.SLASH
                                  + rdfizer.format_iri(cropMap.id)),
                                  URIRef(gmlpropread.getABoxIRI() + rdfizer.SLASH
                                  + ENVELOPE_INSTANCE_PREFIX + rdfizer.format_iri(cropMap.name)))
        if map_counter % 20 == 0 and map_counter % int(gmlpropread.getNOfMapsInAnAboxFile()) != 0:
            print('Total number of feature members processed:', map_counter)

        if map_counter % int(gmlpropread.getNOfMapsInAnAboxFile()) == 0:
            if '/' in output_folder_path:
                save_into_disk(g, output_folder_path+'/'+str(uuid.uuid4())+rdfizer.UNDERSCORE+str(file_counter))
            else:
                save_into_disk(g, output_folder_path + '\\' + str(uuid.uuid4()) + rdfizer.UNDERSCORE + str(
                    file_counter))
            file_counter += 1
            g = Graph()
            print('Total number of feature members processed:', map_counter)

    if flag:
        map_counter = map_counter -1
    if map_counter % int(gmlpropread.getNOfMapsInAnAboxFile()) != 0:
        if '/' in output_folder_path:
            save_into_disk(g, output_folder_path+'/'+str(uuid.uuid4())+rdfizer.UNDERSCORE+str(file_counter))
        else:
            save_into_disk(g, output_folder_path + '\\' + str(uuid.uuid4()) + rdfizer.UNDERSCORE + str(
                file_counter))
        print('Total number of feature members processed:', map_counter)

"""Converts polygon coordinates represented in EPSG 27700 into WGS84"""
def convert_polygon_from_epsg27700_to_wgs84(string, delimiter, span):
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
def parse_gml(file_name, output_folder_path, start_feature_number, upper_limit):
    p = Path(output_folder_path)
    if not p.exists():
        print('The following file output path is not valid:', p)
    print('Parsing in progress...')
    context = get_context(file_name, URL_ENVELOPE)
    get_envelope(context)
    context = get_context(file_name, URL_FEATURE_MEMBER)
    get_crop_map(context, output_folder_path, start_feature_number, upper_limit)

"""This block of code is the access point to this Python module"""
if __name__ == '__main__':
    if len(sys.argv) == 1:
        print('For HELP, run> GMLParser -h')
    if len(sys.argv) == 2 and str(sys.argv[1]) in '-h':
        print('To run the parser, provide a command as follows:>GMLParser.py [INPUT_FILE_PATH] [OUTPUT_FOLDER_PATH] [STARTING_FEATURE_NUMBER] [UPPER_LIMIT]\n')
        print('An example comm>GMLParser /home/<username>/cropmaps/Crop_Map_of_England_2019_North_Yorkshire.gml /home/<username>/kb 1 1000')
    if len(sys.argv) == 5:
        output_folder_path = ''
        if '/' in sys.argv[1]:
            output_folder_path = sys.argv[1].split('/')
            output_folder_path = output_folder_path[len(output_folder_path)-1]
            output_folder_path = output_folder_path.split('.')[0]
            output_folder_path = sys.argv[2] + '/' + output_folder_path + '/' + str(sys.argv[3]) + '-' + str(sys.argv[4])
        elif '\\' in sys.argv[1]:
            output_folder_path = sys.argv[1].split('\\')
            output_folder_path = output_folder_path[len(output_folder_path)-1]
            output_folder_path = output_folder_path.split('.')[0]
            output_folder_path = sys.argv[2] + '\\' + output_folder_path + '\\' + str(sys.argv[3]) + '-' + str(sys.argv[4])
        if len(output_folder_path) > 0:
            p_output = Path(output_folder_path)
            p_input = Path(sys.argv[1])
            if not p_output.exists():
                print('The following file output path is not valid:', p_output)
                print('The path is being created:', p_output)
                os.makedirs(output_folder_path)
            if p_input.exists() and p_output.exists() and int(sys.argv[3])>0 \
                    and (int(sys.argv[4]) == -9999 or int(sys.argv[4])>1):
                parse_gml(sys.argv[1], output_folder_path, sys.argv[3], sys.argv[4])
            else:
                if not p_input.exists():
                    print('Input file does not exist.')
                if not p_output.exists():
                    print('Output folder path could not be created.')
                if not isinstance(int(sys.argv[3]), int):
                    print('Starting feature member is not an integer.')
                if not int(sys.argv[3])>0:
                    print('Starting feature number must be greater than 0.')
                if not (int(sys.argv[4]) == -9999 or int(sys.argv[4])>1):
                    print('Upper limit is neither -9999 nor greater than 1.')
