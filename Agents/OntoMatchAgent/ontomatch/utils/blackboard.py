import os
import shutil
import time

# the local directory that the blackboard agent uses for reading and writing data
# for sake of ease, it is defined here
LOCAL_BLACKBOARD_DIR =  '../tmp/blackboard'
os.makedirs(LOCAL_BLACKBOARD_DIR, exist_ok=True)

class Agent():

    @staticmethod
    def create_handle(name):
        handle = name + '_' +str(time.time())
        handle = handle.replace('/','_').replace(':','_')
        return handle

    def write(self, name, data):
        handle = self.create_handle(name)
        path = LOCAL_BLACKBOARD_DIR + '/' + handle
        with open(path, "w") as file:
            file.write(data)
        return handle

    def read(self, handle):
        path = LOCAL_BLACKBOARD_DIR + '/' + handle
        with open(path, 'r') as file:
            data = file.read()
        return data

    def upload(self, name:str, upload_from:str) -> str:
        # at this stage, the upload function is only used for pickled files for testing
        # thus, for sake of ease, we only consider the case where the parameter upload_from
        # is a local path to a pickled file and the file is just copied into the local blackboard directory
        handle = self.create_handle(name)
        path = LOCAL_BLACKBOARD_DIR + '/' + handle
        shutil.copyfile(upload_from, path)
        return handle
