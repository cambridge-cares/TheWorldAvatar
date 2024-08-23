# env_for_vectorization.py
import os

GEOSERVER_URL = os.getenv('GEOSERVER_URL', 'http://geoserver:8080/geoserver')
GEOSERVER_USER = os.getenv('GEOSERVER_USER', 'admin')
GEOSERVER_PASSWORD = os.getenv('GEOSERVER_PASSWORD')
POSTGIS_HOST = os.getenv('POSTGIS_HOST', 'postgis')
POSTGIS_PORT = os.getenv('POSTGIS_PORT', '5432')
POSTGIS_DB = os.getenv('POSTGIS_DB', 'postgres')
POSTGIS_USER = os.getenv('POSTGIS_USER', 'postgres')
POSTGIS_PASSWORD = os.getenv('POSTGIS_PASSWORD')
WORKSPACE = os.getenv('GEOSERVER_WORKSPACE', 'gps_trajectory')
