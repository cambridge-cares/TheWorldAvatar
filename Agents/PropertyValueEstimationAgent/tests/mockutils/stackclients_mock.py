# The purpose of this module is to provide mocked functionality for required 
# StackClients (i.e. PostGIS and Geoserver) for testing purposes.

class PostGISClient():
    
    def __init__(self, *args, **kwargs):
        pass

    def check_table_exists(self, *args, **kwargs):
        return True
    
    def upload_property_value(self, *args, **kwargs):
        pass

class GeoserverClient():
    pass