__author__ = "Aleksandar Kondinski"
__license__ = "MIT"
__version__ = '1.0.0'
__status__ = "production"

from mop_operations import AssemblerWorkflow

if __name__ == "__main__":
    assembly_model_file = 'Data/Assembly_Models/assembly_models.json'
    output_dir = 'Data/Outputs'
    cbus_dir = 'Data/CBUs'
    workflow_data_file = 'Data/input.csv'

    initiation = AssemblerWorkflow(assembly_model_file, cbus_dir, output_dir)
    initiation.run_workflow(workflow_data_file)
