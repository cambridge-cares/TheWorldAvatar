import os

from pyderivationagent import PySparqlClient

from data_classes.ts_data_classes import TimeSeriesMeta, TimeSeriesInstance, ForecastMeta, parse_incomplete_time
import psycopg2 as pg
from pathlib import Path
from rdflib import Graph
TEST_INSTANCE_IRI = 'https://www.theworldavatar.com/test/TestInstance_1'
TEST_TS_META = TimeSeriesMeta(time_unit='%%Y-%%m-%%dT%%H:%%M:%%SZ',src_iri=TEST_INSTANCE_IRI)

TEST_TIMES =[2001,2002]
TEST_VALUES = [1,5]
TEST_UPDATE_TIMES = [2001,2002,2003]
TEST_UPDATE_VALUES = [1,3,5]
UPDATE_TS = TimeSeriesInstance(src_iri=TEST_INSTANCE_IRI, times= TEST_UPDATE_TIMES,values=TEST_UPDATE_VALUES)
TEST_TS = TimeSeriesInstance(src_iri=TEST_INSTANCE_IRI, times= TEST_TIMES,values=TEST_VALUES)
BASE_IRI = 'https://www.theworldavatar.com/test'

PROJECT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CONFIG_FOLDER = os.path.join(PROJECT_DIR, 'confs_files')
CONFIG_FILE = os.path.join(CONFIG_FOLDER, 'base.cfg')
TEST_TRIPLES_DIR = os.path.join(PROJECT_DIR, './tbox_dev/api_triples')
TEST_DB_NAME = 'test'
DB_URL = 'jdbc:postgresql://localhost:5432/'
DB_USER = 'postgres'
DB_PW = 'postgres'
KG_EP = 'http://localhost:9999/blazegraph/namespace/kb/sparql'
TS_CONFIG_DICT= {'db.url':DB_URL+TEST_DB_NAME,
                 'db.user':DB_USER,
                 'db.password':DB_PW,
                 'sparql.query.endpoint':KG_EP,
                 'sparql.update.endpoint':KG_EP
                 }

TEST_FORECAST_META_1 = ForecastMeta(**{
    'name':'test',
    'iri':'https://www.theworldavatar.com/test/TestInstance_1',
    'duration':30,
    'start_dt': parse_incomplete_time('2022 Jan'),
    'end_dt': parse_incomplete_time('2022 May'),
    'frequency':1,
    'unit_frequency':'month'
})




def initialise_triples():
    # Delete all triples before initialising prepared triples
    sparql_client = PySparqlClient(KG_EP,KG_EP)

    # Upload all relevant example triples provided in the test_triples folder
    pathlist = Path(TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)
        print('{} file uploaded to kg'.format(path))

def clear_kg():
    sparql_client = PySparqlClient(KG_EP,KG_EP)
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

def clear_database():
    # Deletes all tables in the database (before initialising prepared tables)
    with connect_to_rdb() as conn:
        cur = conn.cursor()
        sql_query = """
            DROP SCHEMA public CASCADE;
            CREATE SCHEMA public;
        """
        cur.execute(sql_query)


def connect_to_rdb():
    # Retrieve host and port from RDB URL assuming default format like
    # jdbc:postgresql://localhost:5432/<url_route>
    host = DB_URL.split(':')[2].replace('//', '')
    port = DB_URL.split(':')[3].split('/')[0]
    db = DB_URL[DB_URL.rfind('/') + 1:]
    return pg.connect(host=host, port=port, database=db,
                      user=DB_USER, password=DB_PW)
