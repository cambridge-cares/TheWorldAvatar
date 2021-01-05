##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 17 Dec 2020                      #
##########################################
from builtins import enumerate

from filetype.types.image import Cr2
from lxml import etree

from CropMap import CropMap
from Envelope import Envelope

OBJECT_ID = 'OBJECTID'
CROME_ID = 'cromeid'
LUCODE = 'lucode'
REF_DATE = 'refdate'
SHAPE_LENGTH = 'Shape_Length'
SHAPE_AREA = 'Shape_Area'
SURFACE_PROPERTY = 'surfaceProperty'


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
        envelope.srsName = elem.attrib['srsName']
        envelope.srsDimension = elem.attrib['srsDimension']
        return envelope

"""Parses properties of the current crop map"""
def get_crop_map(context):
    for event, elem in context:
        print(elem)
        for map in elem:
            print(get_tag_name(map.tag))
            cropMap = CropMap()
            cropMap.name = get_tag_name(map.tag)
            cropMap.id = map.attrib['{http://www.opengis.net/gml}id']
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
                    print('cromeid', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  LUCODE.lower():
                    cropMap.luCode = attribute.text
                    print('lucode', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  REF_DATE.lower():
                    cropMap.refDate = attribute.text
                    print('refdate', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  SHAPE_LENGTH.lower():
                    cropMap.shapeLength = attribute.text
                    print('Shape_Length', attribute.text)
                if get_tag_name(attribute.tag.lower()) ==  SHAPE_AREA.lower():
                    cropMap.shapeArea = attribute.text
                    print('Shape_Area', attribute.text)
                if get_tag_name(attribute.tag.lower()) == SURFACE_PROPERTY.lower():
                    for surface in attribute:
                        for pathces in surface:
                            for polygonPatch in pathces:
                                for exterior in polygonPatch:
                                    for linerRing in exterior:
                                        for posList in linerRing:
                                            cropMap.polygon = split_at_span(' ', 2, posList.text)
                                            print(posList.text)
                                            print(len(cropMap.polygon))

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
    context = get_context(file_name, '{http://www.opengis.net/gml}Envelope')
    envelope = get_envelope(context)
    if envelope.srsName != None or envelope.srsName != '':
        print(envelope.srsName)
    if envelope.srsDimension !=None or envelope.srsDimension != '':
        print(envelope.srsDimension)
    context = get_context(file_name, '{http://www.opengis.net/gml}featureMember')
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
    file_name = 'Crop_Map_of_England_2019_North_Yorkshire.gml'
    parse_gml(file_name)