'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

from manager.assembly_workflow import workflow
import datetime
import os
import shutil

def start():
    FOLDER_NAME = 'mops_output'
    mops_output()
    workflow()
    shutil.rmtree(FOLDER_NAME+'\\temp')

def mops_output():
    FOLDER_NAME = 'mops_output'
    if os.path.isdir(FOLDER_NAME):
        print (f"The output directory {FOLDER_NAME} alredy exists, you can see it inside this folder.\nPlease rename it"+
                   " or delete it if you want to regenerate the output.\nOtherwise I am not doing anything. Exiting now.")
        return
    os.mkdir(FOLDER_NAME)
    os.mkdir(FOLDER_NAME+'\\mops_am')
    os.mkdir(FOLDER_NAME+'\\r1_cbus')
    os.mkdir(FOLDER_NAME+'\\r2_cbus')
    os.mkdir(FOLDER_NAME+'\\temp')
    os.mkdir(FOLDER_NAME+'\\mops_r1')
    os.mkdir(FOLDER_NAME+'\\mops_r2')

if __name__ == '__main__':
    print ("Started at ", datetime.datetime.now())
    start()
    print ("Finished at ", datetime.datetime.now())