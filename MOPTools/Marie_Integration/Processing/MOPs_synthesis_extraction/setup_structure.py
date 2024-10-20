import os
import parameters as para
def create_directory(script_dir, folder_name):
    """
    Creates a directory at the specified path. If intermediate directories
    in the path don't exist, they will be created as well.

    :param path: The path of the directory to create.
    """
    input_dir_pdf                   = os.path.join(script_dir,f"Data/{folder_name}_pdf")
    input_dir_txt                   = os.path.join(script_dir,f"Data/{folder_name}_txt")
    input_dir_cbu                   = os.path.join(script_dir,f"Data/{folder_name}_cbu")
    input_dir_preSteps              = os.path.join(script_dir,f"Data/{folder_name}_preSteps")
    input_dir_steps                 = os.path.join(script_dir,f"Data/{folder_name}_steps")
    input_dir_extractedProcedure    = os.path.join(script_dir,f"Data/{folder_name}_extractedProcedure")
    input_dir_chemicals1            = os.path.join(script_dir,f"Data/{folder_name}_chemicals1")
    input_dir_characterisation      = os.path.join(script_dir,f"Data/{folder_name}_characterisation")
    dir_list                        = [input_dir_pdf, input_dir_txt, input_dir_cbu, input_dir_preSteps, input_dir_steps, input_dir_extractedProcedure, input_dir_chemicals1, input_dir_characterisation]
    for direct in dir_list:
    # The 'exist_ok=True' argument ensures that no error is raised if the directory already exists.
        os.makedirs(direct, exist_ok=True)
        print(f"Directory '{direct}' created successfully.")
if __name__ == "__main__":
    create_directory(para.FILE_DIR, para.BATCH_NAME)