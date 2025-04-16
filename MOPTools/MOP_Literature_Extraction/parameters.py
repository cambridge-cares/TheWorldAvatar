# Configuration parameters for the synthesis extraction pipeline

# Name of the current batch being processed - will be used for folder names
BATCH_NAME          = "exampleBatchName"
# Name of the language model used for processing
MODEL_NAME          = "gpt-4o-2024-08-06"
# Path to the directory where data files are stored
DATA_FOLDER         = "PathToTWALocation/TheWorldAvatar/MOPTools/MOP_Literature_Extraction/Data"
# Debug mode flag (set to True to enable debug logging and verbose output)
DEBUGER_MODE        = Boolean
# GPT token cost for cost estimation in processdirectory.py
TOKEN_COST          = 5e-6