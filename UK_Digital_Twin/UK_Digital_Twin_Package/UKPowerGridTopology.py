"""This module declare the properties of generating UK power grid topology A-boxes"""

class UKPowerGridTopology:
    
    """Node keys"""
    BusNodeKey = "BusNode_"
    PowerGeneratorKey = "PowerGenerator_"
    OverheadLineKey = "OverheadLine_"
    
    """Elines attributes"""
    ShapeKey = "Cylinder_"
    LengthKey = "Height_"
    OHLKey = "OverheadLine_"
    
    CoordinateSystemKey = "CoordinateSystem_"
    
    valueKey = "ScalarValue_"

    def __init__(self, numOfBus, Location = 'http://dbpedia.org/resource/United_Kingdom'):
        self.numOfBus = numOfBus
        self.location = Location