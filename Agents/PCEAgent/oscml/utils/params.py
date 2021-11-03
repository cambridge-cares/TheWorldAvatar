import torch

INCLUDE_HYDROGENS = 'INCLUDE_HYDROGENS'
PYTORCH_DEVICE = 'PYTORCH_DEVICE'

class Singleton(type):
    """see https://stackoverflow.com/questions/6760685/creating-a-singleton-in-python
    """
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super(Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]

class Params(metaclass=Singleton):
    
    def __init__(self):
        
        # setting device on GPU if available, else CPU
        my_device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        print('available device:', my_device)
        print()

        #Additional Info when using cuda
        if my_device.type == 'cuda':
            print(torch.cuda.get_device_name(0))
            print('Memory Usage:')
            print('Allocated:', round(torch.cuda.memory_allocated(0)/1024**3,1), 'GB')
            print('Cached:   ', round(torch.cuda.memory_cached(0)/1024**3,1), 'GB')
        
        
        self.cfg = {
            INCLUDE_HYDROGENS: True,
            PYTORCH_DEVICE: my_device,
        }
        

cfg = Params().cfg