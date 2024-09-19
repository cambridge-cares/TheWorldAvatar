import requests
import pandas as pd
import folium
import matplotlib as mpl
import matplotlib.pyplot as plt
from datetime import datetime
import re
from shapely.geometry import Polygon


class Connector:
    def __init__(self, dome_url, api_key, provider_id):
        self.dome_url = dome_url
        self.api_key = api_key
        self.provider_id = provider_id

    def get_data(self, search_string):

        res_conn_query = requests.get(
            f"{self.dome_url}api/discover/results/{self.provider_id}",
            headers={'apikey': self.api_key},
            params={'search_string': search_string},
            timeout=10,
        )

        try:
            return res_conn_query.json()
        except:
            return res_conn_query.content


class ShipConnector(Connector):

    def find_mmsi(self, text):
        match = re.search(r'MMSI:\s*(\d+)', text)
        if match:
            return match.group(1)
        return None

    def parse_ship_data(self, json_ship):
        metadata = json_ship['metadata']
        data = json_ship['data']
        num_ship = len(metadata)
        dict_ship = {}
        for i in range(num_ship):
            dict_mmsi = {
                "date": [datetime.fromisoformat(data[i][j]["date"]) for j in range(len(data[i]))],
                "lat": [float(data[i][j]["lat"]) for j in range(len(data[i]))],
                "lon": [float(data[i][j]["lon"]) for j in range(len(data[i]))],
                "speed": [float(data[i][j]["speed"]) for j in range(len(data[i]))],
                "course": [float(data[i][j]["course"]) for j in range(len(data[i]))]
            }
            mmsi = self.find_mmsi(metadata[i]['keyword'])
            if mmsi:
                dict_ship[mmsi] = dict_mmsi
        return dict_ship

    def get_ship(self, search_string="AIS"):
        dict_ship = self.parse_ship_data(self.get_data(search_string))
        for mmsi in dict_ship:
            dict_ship[mmsi] = pd.DataFrame(dict_ship[mmsi])
        return dict_ship

    def get_scope(self, df_ship, min_d=0.01):

        # Calculate MBR
        min_lat, max_lat = min(df_ship['lat']), max(df_ship['lat'])
        min_lon, max_lon = min(df_ship['lon']), max(df_ship['lon'])

        # Calculate Centroid
        centroid_lat = (min_lat + max_lat) / 2
        centroid_lon = (min_lon + max_lon) / 2

        return {"LAT": centroid_lat, "LON": centroid_lon,
                "dLAT": max((max_lat - min_lat) * 0.5 + min_d, min_d*2),
                "dLON": max((max_lon - min_lon) * 0.5 + min_d, min_d*2)}

    def combine_scope(self, scope_all, list_mmsi, min_d=0.01):

        min_lon = 200
        min_lat = 200
        max_lon = -200
        max_lat = -200

        # prepare combined scope
        for mmsi in list_mmsi:
            scope = scope_all[mmsi]
            min_lon = min(min_lon, scope['LON']-scope['dLON'])
            max_lon = max(max_lon, scope['LON']+scope['dLON'])
            min_lat = min(min_lat, scope['LAT']-scope['dLAT'])
            max_lat = max(max_lat, scope['LAT']+scope['dLAT'])

        # Calculate Centroid
        centroid_lat = (min_lat + max_lat) / 2
        centroid_lon = (min_lon + max_lon) / 2

        return {"LAT": centroid_lat, "LON": centroid_lon,
                "dLAT": max((max_lat - min_lat) * 0.5 + min_d, min_d*2),
                "dLON": max((max_lon - min_lon) * 0.5 + min_d, min_d*2)}

    def get_scope_corners(self, scope):

        min_lon = scope['LON']-scope['dLON']
        max_lon = scope['LON']+scope['dLON']
        min_lat = scope['LAT']-scope['dLAT']
        max_lat = scope['LAT']+scope['dLAT']

        mbr_corners = [(min_lat, min_lon),
                       (min_lat, max_lon),
                       (max_lat, max_lon),
                       (max_lat, min_lon),
                       (min_lat, min_lon)]

        return mbr_corners

    def check_scope_overlap(self, scope1, scope2):

        poly1 = Polygon(self.get_scope_corners(scope1))
        poly2 = Polygon(self.get_scope_corners(scope2))

        return poly1.intersects(poly2)

    def plot_ship(self, dict_ship_all, scope, mmsi_to_id, list_mmsi):
        # Create a map
        m = folium.Map()
        colormap = plt.get_cmap('autumn')

        # draw scope

        min_lon = scope['LON']-scope['dLON']
        max_lon = scope['LON']+scope['dLON']
        min_lat = scope['LAT']-scope['dLAT']
        max_lat = scope['LAT']+scope['dLAT']

        mbr_corners = self.get_scope_corners(scope)

        folium.Polygon(locations=mbr_corners, color="none", fill=True,
                       fill_color="blue", fill_opacity=0.2, weight=0).add_to(m)

        # draw ship

        for mmsi in list_mmsi:

            df_ship = dict_ship_all[mmsi]

            trajectory = list(zip(df_ship['lat'], df_ship['lon']))

            # Add points to the map
            for i in range(len(trajectory)):
                point = trajectory[i]
                folium.CircleMarker(location=point, radius=5,
                                    popup=folium.Popup(
                                        f"MMSI:{mmsi} (Ship {mmsi_to_id[mmsi]}), {df_ship['date'][i]}"),
                                    color='none', fill=True, fill_color='black', fill_opacity=0.7).add_to(m)

            colors = [colormap(i / len(trajectory))
                      for i in range(len(trajectory))]

            # Add segments to the map with different colors
            for i in range(len(trajectory) - 1):
                folium.PolyLine(locations=[trajectory[i], trajectory[i + 1]],
                                # Convert RGBA to hex
                                color=mpl.colors.to_hex(colors[i]),
                                weight=2.5, opacity=1).add_to(m)

        # Fit bounds to ensure everything is visible
        m.fit_bounds([[min_lat, min_lon], [max_lat, max_lon]])

        return m._repr_html_()
