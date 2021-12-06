import os
from datetime import datetime
from datetime import date

def outputfolder(this_dir):
    now = datetime.now()
    current_time = now.strftime("%H-%M-%S")
    today = date.today()
    time_date_stamp = str(today)+"-"+str(current_time)
    directory = this_dir+"\\outputs"+time_date_stamp
    os.mkdir(directory)
    print("Directory '%s' created" %directory)
    mops_lib2 = directory+'\\3_mops_lib2'
    mops_lib1 = directory+'\\2_mops_lib1'
    mops_lib1_type = directory+'\\2_mops_lib1_type'
    mops_lib2_type = directory+'\\3_mops_lib2_type'
    mops_am = directory+'\\1_mops_am'
    arrange = directory+'\\arrange'
    mops_r1 = directory+'\\4_mops_r1'
    mops_r2 = directory+'\\4_mops_r2'
    os.mkdir(mops_lib2)
    os.mkdir(mops_lib1)
    os.mkdir(mops_lib1_type)
    os.mkdir(mops_lib2_type)
    os.mkdir(mops_am)
    os.mkdir(arrange)
    os.mkdir(mops_r1)
    os.mkdir(mops_r2)