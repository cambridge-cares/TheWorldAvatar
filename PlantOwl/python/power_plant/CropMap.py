##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 18 Dec 2020                      #
##########################################
"""Created to be used as the data structure for storing information of an instance of crop map"""
class CropMap():
    """The name of a crop map instance"""
    name = ''
    """The ID of a crop map instance"""
    id = ''
    """The GML specific object ID of a crop map instance"""
    objectID = ''
    """The CROME ID of a crop map instance"""
    cromeID = ''
    """The LUCODE of a crop map instance"""
    luCode = ''
    """The reference date of a crop map instance"""
    refDate = ''
    """The shape length of a crop map instance"""
    shapeLength = ''
    """The shape area of a crop map instance"""
    shapeArea = ''
    """The polygon of a crop map instance"""
    polygon = []