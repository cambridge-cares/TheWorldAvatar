import ontomatch
from ontomatch.kgoperations.downloadplantdatakg import downloadDataKg,upload2Kg


class Agent():
    def download(self, config_handle):
        config_json = ontomatch.utils.util.call_agent_blackboard_for_reading(config_handle, True)
        params = ontomatch.utils.util.convert_json_to_dict(config_json)
        savelocsrc = params['dataset']['src']
        saveloctgt = params['dataset']['tgt']
        if 'deu' in saveloctgt.lower():
            endpoint_label = 'powerplants'
            downloadDataKg(savelocsrc,endpoint_label,"http://kwl")
            downloadDataKg(saveloctgt,endpoint_label,"http://Germany")
        if 'gbr' in saveloctgt.lower():
            endpoint_label = 'ukpowerplants'
            downloadDataKg(savelocsrc,'powerplants', "http://dukes")
            downloadDataKg(saveloctgt,endpoint_label,None)


    def upload(self, config_handle):
        config_json = ontomatch.utils.util.call_agent_blackboard_for_reading(config_handle, False)
        params = ontomatch.utils.util.convert_json_to_dict(config_json)
        readloc = params['post_processing']['link_file']
        upload2Kg(readloc)