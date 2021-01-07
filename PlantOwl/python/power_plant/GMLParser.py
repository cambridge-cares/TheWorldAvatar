##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 17 Dec 2020                      #
##########################################
from builtins import enumerate

from filetype.types.image import Cr2
from lxml import etree

from CropMap import CropMap
from Envelope import Envelope
from rdflib import Graph, URIRef

import PropertyReader as propread
import GMLParserPropReader as gmlpropread
import ABoxGeneration as aboxgen
import EntityRDFizer as rdfizer

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

"""Declared a variable for creating a graph model"""
g = Graph()

"""Creates a context for parsing when the GML file name and tag is given"""
def get_context(file_name, tag):
    return etree.iterparse(file_name, events=('end',), tag=tag)

"""Parses the following attributes of GML envelope and returns the envelope
- srsName
- srsDimension
"""
def get_envelope(context):
    for event, elem in context:
        envelope = Envelope()
        envelope.srsName = elem.attrib[ATTRB_SRS_NAME]
        envelope.srsDimension = elem.attrib[ATTRB_SRS_DIMENSION]
        return envelope

"""Parses properties of the current crop map"""
def get_crop_map(context):
    map_counter = 0
    file_counter = 0
    global g
    for event, elem in context:
        print(elem)
        for map in elem:
            print(get_tag_name(map.tag))
            cropMap = CropMap()
            cropMap.name = get_tag_name(map.tag)
            cropMap.id = map.attrib[URL_ID]
            print('cropMap.id',cropMap.id)
            for attribute in map:
                print('attribute.text', attribute.text)
                if attribute.text == None or attribute.text == '':
                    continue
                if get_tag_name(attribute.tag.lower()) ==  OBJECT_ID.lower():
                    cropMap.objectID = attribute.text
                    print('OBJECT_ID', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  CROME_ID.lower():
                    cropMap.cromeID = attribute.text
                    aboxgen.link_data(g, URIRef(gmlpropread.getCromeIDVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.cromeID)
                    print('cromeid', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  LUCODE.lower():
                    cropMap.luCode = attribute.text
                    aboxgen.link_data(g, URIRef(gmlpropread.getLucodeVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.luCode)
                    print('lucode', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  REF_DATE.lower():
                    cropMap.refDate = attribute.text
                    aboxgen.link_data(g, URIRef(gmlpropread.getRefDateVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.refDate)
                    print('refdate', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  SHAPE_LENGTH.lower():
                    cropMap.shapeLength = attribute.text
                    aboxgen.link_data(g, URIRef(gmlpropread.getShapeLengthVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.shapeLength)
                    print('Shape_Length', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  SHAPE_AREA.lower():
                    cropMap.shapeArea = attribute.text
                    aboxgen.link_data(g, URIRef(gmlpropread.getShapeAreaVocabulary()),
                                      URIRef(gmlpropread.getABoxIRI()
                                             + rdfizer.SLASH + rdfizer.format_iri(cropMap.id)),
                                      cropMap.shapeArea)
                    print('Shape_Area', attribute.text)
                if get_tag_name(attribute.tag.lower()) == SURFACE_PROPERTY.lower():
                    for surface in attribute:
                        for pathces in surface:
                            for polygonPatch in pathces:
                                for exterior in polygonPatch:
                                    for linerRing in exterior:
                                        for posList in linerRing:
                                            #cropMap.polygon = split_at_span(' ', 2, posList.text)
                                            cropMap.polygon = posList.text.replace(" ", "#")
                                            aboxgen.create_instance(g,
                                                                    URIRef(gmlpropread.getClassLinearRing()),
                                                                    gmlpropread.getABoxIRI() + rdfizer.SLASH
                                                                    + rdfizer.format_iri(cropMap.id),
                                                                    cropMap.name)

                                            print(cropMap.polygon)
        map_counter += 1
        if map_counter % int(gmlpropread.getNOfMapsInAnAboxFile()) == 0:
            save_into_disk(g, file_counter)
            file_counter += 1
            g = Graph()
            print('Total number of files:', file_counter)
    if map_counter % int(gmlpropread.getNOfMapsInAnAboxFile()) != 0:
        save_into_disk(g, file_counter)
        print('Total number of maps:', map_counter)

"""Saves a graph into the disk/file system"""
def save_into_disk(g, file_counter):
    g.serialize(destination=gmlpropread.getABoxFileName() + rdfizer.UNDERSCORE + str(
        file_counter) + gmlpropread.getABoxFileExtension(),
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
    context = get_context(file_name, URL_ENVELOPE)
    envelope = get_envelope(context)
    if envelope.srsName != None or envelope.srsName != '':
        print(envelope.srsName)
    if envelope.srsDimension !=None or envelope.srsDimension != '':
        print(envelope.srsDimension)
    context = get_context(file_name, URL_FEATURE_MEMBER)
    get_crop_map(context)

# for event, elem in context:
#     srsName = elem.attrib['srsName']
#     print('Id', srsName)
#     for c in elem:
#         print('c',c)
#         print("c.attrib['k']", c.attrib['k'])
#         if c.attrib['k'] == 'created_by' or c.attrib['k'] == 'source':  # We don't want such tags to keep in our DB.
#             continue
#
#         key = c.attrib['k']  # These are basically the tags inside the nodes having key and values
#         val = c.attrib['v']
    # You can do more filtering here if you want specific keys or values. Like if you want only 'atms' then filter the val with 'atm' using conditions.

    # Store the information in file or db or wherever you wanna use it.

    # elem.clear()

"""This block of code is the access point to this Python module"""
if __name__ == '__main__':
    file_name = 'Crop_Map_of_England_2018_Cambridgeshire_snippet.gml'
    parse_gml(file_name)