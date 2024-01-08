import configparser
import os.path
from pathlib import Path
import string
import importlib
from kg_access import tsclient_wrapper
from typing import List
from readers.readers import create_data_reader
from utils.conf_utils import *
from data_types.ts_data_classes import PropertiesFileProtytype

class MackayDataAgent:
    def __init__(self, confdir='./confs'):
        self.base_conf = load_conf(os.path.join(confdir, 'base.cfg'))
        self.data_confs = load_confs_from_dir(os.path.join(confdir, 'data'))
        # Download and Parse each TS from data source
        self.data_readers = [ create_data_reader(dataconf) for dataconf in self.data_confs]
        self.property_files_dict = self._create_property_files()
        print(self.data_confs)


    # Generate Java property files to call TSClient for each TS instance (each needs a new one as sparql ep is different)
    def _create_property_files(self) -> List[str]:
        outdir = self.base_conf['paths']['resourcesDir']
        sqlcfg = self.base_conf['postgresql']
        outpaths = {}
        for datacfg in self.data_confs:
            dataname = datacfg['source']['name']
            props = PropertiesFileProtytype.copy()
            allcfg = dict(datacfg['kg']).copy()
            allcfg.update(dict(sqlcfg))
            updated_props = match_properties(props, allcfg)
            outpath = os.path.abspath(os.path.join(Path(__file__).parent, outdir, "{}.properties".format(dataname)))
            write_java_properties_conf(updated_props, outpath )
            outpaths[dataname] = outpath
        return outpaths




    # Create postgresql tables and meta info triples of all timeseries data
    def instantiate_timeseries(self):
        # connection set up
        sqlcfg = self.base_conf['postgresql']
        dburls = sqlcfg['url'].split(':')
        db_name = dburls[-1]
        host_name = (':').join(dburls[:-1])
        tsclient_wrapper.create_postgres_db_if_not_exists(db_name,sqlcfg['user'],sqlcfg['password'])
        # loop through each configuration object, one for each data source
        for reader in self.data_readers:
            timeseries_meta = reader.get_tsmeta()
            tsclient_wrapper.register_timeseries(self.property_files_dict[reader.dataname], timeseries_meta)

    def add_timeseries(self):
        for reader in self.data_readers:
            timeseries_instance = reader.download_tsinstance()
            tsclient_wrapper.add_timeseries(self.property_files_dict[reader.dataname], timeseries_instance)

    # TODO: link to forecast agents
    def call_predict(self):
        pass

    # Retrieve all data conf files in a dir, one for each TS instance
