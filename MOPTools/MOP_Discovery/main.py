from manager.assembly_workflow import workflow
import datetime
import os

def start():
    mops_output()
    workflow()
    #print("Running Discovery Workflow")
    #searchanalytics = workflow()
    #print("Overall results from R1")
    #print(searchanalytics[0])
    #print("Overall results from R2")
    #print(searchanalytics[1])

def mops_output():
    FOLDER_NAME = 'mops_output'
    if os.path.isdir(FOLDER_NAME):
        print (f"The output directory {FOLDER_NAME} alredy exists, you can see it inside this folder.\nPlease rename it"+
                   " or delete it if you want to regenerate the output.\nOtherwise I am not doing anything. Exiting now.")
        return
    os.mkdir(FOLDER_NAME)
    os.mkdir(FOLDER_NAME+'\\3_mops_lib2')
    os.mkdir(FOLDER_NAME+'\\2_mops_lib1')
    os.mkdir(FOLDER_NAME+'\\2_mops_lib1_type')
    os.mkdir(FOLDER_NAME+'\\1_mops_am')
    os.mkdir(FOLDER_NAME+'\\3_mops_lib2_type')
    os.mkdir(FOLDER_NAME+'\\arrange')
    os.mkdir(FOLDER_NAME+'\\4_mops_r1')
    os.mkdir(FOLDER_NAME+'\\4_mops_r2')

if __name__ == '__main__':
    print ("Started at ", datetime.datetime.now())
    start()
    print ("Finished at ", datetime.datetime.now())