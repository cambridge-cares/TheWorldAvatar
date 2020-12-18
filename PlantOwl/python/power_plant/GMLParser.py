##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 17 Dec 2020                      #
##########################################
from builtins import enumerate
from lxml import etree
from power_plant.Envelope import Envelope

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
            for attribute in map:
                print(attribute.tag)

def get_tag_name(url):
    if '}' in url:
        tokens = url.split('}')
        if len(tokens)>1:
            return tokens[1]
    return None

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

if __name__ == '__main__':
    file_name = 'Crop_Map_of_England_2019_North_Yorkshire.gml'
    parse_gml(file_name)