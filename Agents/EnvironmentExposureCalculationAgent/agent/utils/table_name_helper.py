import base64
import hashlib

class QueryIdHelper:
    def __init__(self, data:dict, args:dict):
        self.data_str = "_".join([str(val) for key, val in data.items() if key not in ['output_format']])
        self.args_str = "_".join([str(val) for key, val in args.items() if key not in ['output_format']])
        
    def get_query_id_of_dataset(self, dataset:str=''):
        hash_byte = hashlib.sha256(f'{self.data_str}_{self.args_str}_{dataset}'.encode()).digest()
        compressed_hex = base64.b32encode(hash_byte).decode()[:12]
        return compressed_hex
    
    def get_query_id(self,) -> str:
        return self.get_query_id_of_dataset('2019')