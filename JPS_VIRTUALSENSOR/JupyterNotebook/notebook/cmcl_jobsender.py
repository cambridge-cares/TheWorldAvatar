import requests
from datetime import datetime
import time
import json
import pandas as pd

SLEEP_DURATION = 3
LIST_Z = [0]


def call_http(http_endpoint, custom_request, method, quiet=True):
    start = datetime.now().strftime('%d-%m-%y.%H:%M:%S')
    t0 = time.time()
    if not quiet:
        print(f"Sent request {custom_request.split('?')[0]} at {start}")
    response = getattr(requests, method)(http_endpoint+custom_request)
    if not quiet:
        print(
            f"Receive response at {datetime.now().strftime('%d-%m-%y.%H:%M:%S')}")
        print(
            f"Duration: {time.time()-t0:.1f}s, status code: {response.status_code}")
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

    def find_label_by_derivation_iri(self, derivation_iri):
        metadata = self.get_metadata()
        for k, v in metadata.items():
            if v['derivationIri'] == derivation_iri:
                return k

    def get_derivation_iri(self, label, scope):
        metadata = self.get_metadata()
        try:
            x = metadata[label]['derivationIri']
            print(f"Found existing simulation for {label}.")
            return metadata[label]['derivationIri']
        except Exception as e:
            polygon = get_polygon(scope)
            url_init = f'InitialiseSimulation?ewkt=SRID={self.SRID};{polygon}&nx={self.nx}&ny={self.ny}&label={label}'
            for z in LIST_Z:
                url_init = url_init+f'&z={z}'
            response = self.call_di(url_init, 'post')
            print(f"Created new simulation {label}.")
            return response.json()['derivation']

    def add_ship_data(self, mmsi, df_ship):

        shipData = [{"MMSI": int(mmsi),
                    "SPEED": 0,
                     "COURSE": 0,
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

    def generate_data_without_ship(self, derivation_iri, label, list_timestep, num_step):
        req = f'GenerateDataWithoutShips?derivation={derivation_iri}'
        if num_step == 0:
            num_step = 1000000
        count = 0
        for t in list_timestep:
            if count < num_step:
                metadata = self.get_metadata()
                if (label not in metadata):
                    print(f'Simulating {label} at {t}')
                    self.call_di(req+f'&timestep={int(t.timestamp())}', 'post')
                    count = count + 1
                elif int(t.timestamp()) not in metadata[label]['time']:
                    print(f'Simulating {label} at {t}')
                    self.call_di(req+f'&timestep={int(t.timestamp())}', 'post')
                    count = count + 1
                else:
                    print(
                        f'Skipping {t} for {label} because it has already been simulated.')

    def run_simulation_without_ship(self, label, scope, list_timestep, num_step):
        derivation_iri = self.get_derivation_iri(label, scope)
        label = self.find_label_by_derivation_iri(derivation_iri)
        print(f'Running simulation {label}...')
        self.generate_data_without_ship(
            derivation_iri, label, list_timestep, num_step)
        return "Complete."
