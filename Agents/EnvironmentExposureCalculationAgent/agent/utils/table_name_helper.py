import base64
import hashlib

class TableNameHelper:
    def __init__(self, data:dict, args:dict):
        self.data_str = "_".join(list(map(str, data.values())))
        self.args_str = "_".join(list(map(str, args.values())))
        
    def get_table_name(self, dataset:str=''):
        hash_byte = hashlib.sha256(f'{self.data_str}_{self.args_str}_{dataset}'.encode()).digest()
        compressed_hex = base64.b32encode(hash_byte).decode()[:12]
        return compressed_hex
    
    def get_points_table_name(self,) -> str:
        return self.get_table_name()