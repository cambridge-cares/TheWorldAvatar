import requests
import pandas as pd
import folium
import matplotlib as mpl
import matplotlib.pyplot as plt
from datetime import datetime
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
        
    def parse_ship_data(self, json_ship):
        data = json_ship['data']
        dict_mmsi = {
            "date": [datetime.fromisoformat(data[0][j]["date"]) for j in range(len(data[0]))],
            "lat": [float(data[0][j]["lat"]) for j in range(len(data[0]))],
            "lon": [float(data[0][j]["lon"]) for j in range(len(data[0]))],
            "speed": [float(data[0][j]["speed"]) for j in range(len(data[0]))],
            "course": [float(data[0][j]["course"]) for j in range(len(data[0]))]
        }
        return dict_mmsi

    def get_ship(self, search_string="AIS"):
        dict_ship = self.parse_ship_data(self.get_data(search_string))
        return pd.DataFrame(dict_ship)

    def get_scope(self, df_ship):
        
        # Calculate MBR
        min_lat, max_lat = min(df_ship['lat']), max(df_ship['lat'])
        min_lon, max_lon = min(df_ship['lon']), max(df_ship['lon'])

        # Calculate Centroid
        centroid_lat = (min_lat + max_lat) / 2
        centroid_lon = (min_lon + max_lon) / 2
        
        return {"LAT": centroid_lat, "LON": centroid_lon,
                "dLAT": max((max_lat - min_lat) * 0.5 + 0.05, 0.05),
                "dLON": max((max_lon - min_lon) * 0.5 + 0.05, 0.05)}

    def plot_ship(self, df_ship,scope):
        # Sample data: list of (latitude, longitude) tuples
        trajectory = list(zip(df_ship['lat'],df_ship['lon']))
        
        # Calculate MBR
        min_lon = scope['LON']-scope['dLON']
        max_lon = scope['LON']+scope['dLON']
        min_lat = scope['LAT']-scope['dLAT']
        max_lat = scope['LAT']+scope['dLAT']

        # Calculate Centroid
        centroid_lat = (min_lat + max_lat) / 2
        centroid_lon = (min_lon + max_lon) / 2
        centroid = (centroid_lat, centroid_lon)

        # MBR corners
        dlat = (max_lat - min_lat) * 0.02
        dlon = (max_lon - min_lon) * 0.02
        mbr_corners = [(min_lat, min_lon),
                    (min_lat, max_lon),
                    (max_lat, max_lon),
                    (max_lat, min_lon),
                    (min_lat, min_lon)]

        # Create a map centered around the first point
        m = folium.Map(location=centroid)
        
        # Add filled MBR to the map
        folium.Polygon(locations=mbr_corners,color="green",fill=True,fill_color="green",fill_opacity=0.3,weight=0).add_to(m)

        # Add points to the map
        for i in range(len(trajectory)):
            point = trajectory[i]
            folium.CircleMarker(location=point,radius=5,popup=folium.Popup(str(df_ship['date'][i])),
                                color='none', fill=True, fill_color='black', fill_opacity=0.7).add_to(m)
            
        colormap = plt.get_cmap('autumn')
        colors = [colormap(i / len(trajectory)) for i in range(len(trajectory))]

        # Add segments to the map with different colors
        for i in range(len(trajectory) - 1):
            folium.PolyLine(locations=[trajectory[i], trajectory[i + 1]],
                            color=mpl.colors.to_hex(colors[i]),  # Convert RGBA to hex
                            weight=2.5,opacity=1).add_to(m)
        
        # Fit bounds to ensure everything is visible
        m.fit_bounds([[min_lat-dlat, min_lon-dlon], [max_lat+dlat, max_lon+dlon]])
        
        return m._repr_html_()