from process_directory import transform_txt_folder, extract_synthesis, upload_chemcials, upload_steps_dir, upload_characterisation_dir, link_cbu_dir
import process_directory as pdir
import os
import parameters as para
import upload as up
# get current script folder location to make code absolute path independent

# step 1 transform pdf to txt
#pdir.transform_txt_folder(para.DATA_FOLDER)
# step 2 extract chemicals
#pdir.extract_synthesis("chemicals")
# step 3 upload extracted chemicals
# if it is the first time uploading on an empty blazegraph make sure to instantiate the predefined instances with upload_predefined()
#up.upload_predefined()
#pdir.upload_chemcials()
# step 4 extract the procedure
# pdir.extract_synthesis("procedure")
# step 5 extract the pre steps:
# pdir.extract_synthesis("preSteps")
# step 6 extract steps:
#pdir.extract_synthesis("steps")
# step 7 uploading the steps
# upload_steps_dir()
# step 8 extract characterization:
#pdir.extract_synthesis("characterisation")
# step 9 upload characterisation
#upload_characterisation_dir()
# step 10 extract cbu
#pdir.extract_synthesis("cbu")
# step 11 upload cbu
link_cbu_dir()
# postprocess :) ...



