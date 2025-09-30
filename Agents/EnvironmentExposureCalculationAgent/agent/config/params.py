from enum import Enum


class PointSelectionParam(Enum):
    SelectedPoints = 'selected_points'
    Region = 'region'
    SelfDefinedRegion = 'self_defined_region'
    PostalCode = 'postal_code'
    
class OutputFormatParam(Enum):
    CSV = 'csv'
    JSON = 'json'
    