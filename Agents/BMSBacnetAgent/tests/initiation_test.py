#Use dummy mapping files (set up in config)
#Inject BACNET Connection with Mock
import unittest

import config.parsedConfig
from initiation import init
from unittest.mock import patch, Mock
from config.config import *
class TestAgentInit(unittest.TestCase):
    def test_agent_init(self):
        init()
        #expect to get new table in sql and new triples in blazegraph
        #TODO: config docker to connect to host-host postsql
        import psycopg2
        #check table is created
        JAVA_PROPERTIES = config.parsedConfig.readProperties()
        keys = JAVA_PROPERTIES[DB_TABLE],JAVA_PROPERTIES[DB_USER_KEY], JAVA_PROPERTIES[DB_PASSWORD_KEY], JAVA_PROPERTIES[DB_URL_KEY]
        conn = psycopg2.connect("dbname='{}' user='{}' host='{}' password='{}'".format(*keys))
        cur = conn.cursor()
        cur.execute("select * from information_schema.tables where table_name=%s", (JAVA_PROPERTIES[DB_TABLE],))
        bool(cur.rowcount)


if __name__ == '__main__':
    unittest.main()