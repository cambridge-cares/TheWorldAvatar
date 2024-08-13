import requests
from datetime import datetime
import time
import json
import pandas as pd

SLEEP_DURATION = 5
LIST_Z = [0]

def call_http(http_endpoint, custom_request, method, quiet=True):
    start = datetime.now().strftime('%d-%m-%y.%H:%M:%S')
    t0 = time.time()
    if not quiet:
        print(f"Sent request {custom_request.split('?')[0]} at {start}")
    response = getattr(requests, method)(http_endpoint+custom_request)
    print(f"Receive response at {datetime.now().strftime('%d-%m-%y.%H:%M:%S')}")
    print(f"Duration: {time.time()-t0:.1f}s, status code: {response.status_code}")
    if response.status_code != 200:
        print("Error messages:")
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

    def call_di(self, custom_request, method, quiet=True):
        return call_http(f'{self.base_url}dispersion-interactor/', custom_request, method, quiet)
    
    def get_metadata(self):
        return self.call_di('GetDispersionSimulations', 'get').json()
    
    def get_derivation_iri(self,label,scope):
        metadata = self.get_metadata()
        try:
            print(f"Found existing simulation for {label}.")
            return metadata[label]['derivationIri']
        except Exception as e:
            polygon = get_polygon(scope)
            url_init = f'InitialiseSimulation?ewkt=SRID={self.SRID};{polygon}&nx={self.nx}&ny={self.ny}&label={label}'
            for z in LIST_Z:
                url_init = url_init+f'&z={z}'
            response = self.call_di(url_init, 'post')
            print(f"Created new simulation for {label}.")
            return response.json()['derivation']
    
    def add_ship_data(self,mmsi,df_ship):
        
        shipData = [{"MMSI":int(mmsi),
                    "SPEED":0,
                    "COURSE":0,
                    "LAT": 0.0,
                    "LON": 0.0,
                    "SHIPTYPE": 99,
                    "TIMESTAMP": '2024-01-01T12:00:00'}]
        tsData = []
        for index, row in df_ship.iterrows():
            tsData.append({"DATE": pd.to_datetime(row.date).strftime('%Y-%m-%dT%H:%M:%S'),
                    "LAT": row.lat,
                    "LON": row.lon,
                    "SOG": row.speed,
                    "COG": row.course})

        headers = {'Content-Type': 'application/json'}
        response = requests.post(f'{self.base_url}ship-input-agent/parse',
                                 data=json.dumps({"shipData": shipData, "tsData": tsData}), headers=headers)

        return response
    
    def generate_data(self,derivation_iri,num_step):
        for i in range(num_step):

            pause()
            self.call_di(f'GenerateDataWithShips?derivation={derivation_iri}&numsteps=1', 'post', quiet = False)
    
    def run_simulation(self,label,scope,num_step):
        derivation_iri = self.get_derivation_iri(label,scope)
        self.generate_data(derivation_iri,num_step)
        return "Complete."