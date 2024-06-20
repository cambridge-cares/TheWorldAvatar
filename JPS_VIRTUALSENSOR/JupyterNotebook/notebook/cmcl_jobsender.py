import requests
import time

SLEEP_DURATION = 5
LIST_Z = [0]

def call_http(http_endpoint, custom_request, method):
    response = getattr(requests, method)(http_endpoint+custom_request)
    if response.status_code != 200:
        print(response.content)
    return response

def pause(second=SLEEP_DURATION):
    time.sleep(second)

def get_polygon(scope):
    lon_min = scope['LON']-scope['dLON']
    lon_max = scope['LON']+scope['dLON']
    lat_min = scope['LAT']-scope['dLAT']
    lat_max = scope['LAT']+scope['dLAT']
    return (f'POLYGON(({lon_min} {lat_min},{lon_max} {lat_min},{lon_max} {lat_max},{lon_min} {lat_max},{lon_min} {lat_min}))')


class JobSender:
    def __init__(self, base_url):
        self.SRID = '4326'
        self.nx = '200'
        self.ny = '200'
        self.base_url = base_url

    def call_di(self, custom_request, method):
        return call_http(f'{self.base_url}dispersion-interactor/', custom_request, method)
    
    def get_metadata(self):
        return self.call_di('GetDispersionSimulations', 'get').json()
    
    def get_derivation_iri(self,label,scope):
        metadata = self.get_metadata()
        try:
            return metadata[label]['derivationIri']
        except Exception as e:
            print(f'Cannot find existing derivation: {e}, try to create it')
            polygon = get_polygon(scope)
            url_init = f'InitialiseSimulation?ewkt=SRID={self.SRID};{polygon}&nx={self.nx}&ny={self.ny}&label={label}'
            for z in LIST_Z:
                url_init = url_init+f'&z={z}'
            response = self.call_di(url_init, 'post')
            return response.json()['derivation']
    
    def generate_data(self,derivation_iri,num_step):
        for i in range(num_step):

            pause()
            self.call_di(f'GenerateDataWithShips?derivation={derivation_iri}&numsteps=1', 'post')