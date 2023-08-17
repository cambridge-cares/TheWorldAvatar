################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 17 Aug 2023                            #
################################################

# The purpose of this module is to provide custom model and covariate loading
# functions for pre-trained models 
#
# Own/new loading functions shall be added below and included in the 'FC_MODELS'
# mapping dictionary for their applicable forecasting models

import os
import urllib
from pathlib import Path
from darts.models import TFTModel

from py4jps import agentlogging


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


#
# 1) SPECIFY CUSTOM MODEL LOADING FUNCTIONS
# NOTE: Different (pre-trained) models, require different loading functions, 
#       tailored towards 1) model type (TFT, LSTM, ...) and 
#                        2) model specific requirements/data structures 
#

def load_tft_pirmasens_heat_demand(cfg, series):
    """
    Load pre-trained temporal fusion transformer model for Pirmasens'
    district heating heat demand

    Arguments:
        cfg (dict) - consolidated forecasting configuration dictionary as 
                     created by 'create_forecast_configuration'
        series (darts.TimeSeries) - time series object to be forecasted

    Returns:
        Updated forecasting configuration dictionary,
        Loaded pre-trained model
    """

    # Load pre-trained TFT model
    model = load_pretrained_model(cfg, TFTModel)
    
    # Verify that loaded model is suitable to derive target forecast 
    # NOTE: Other models than TFT can have different key then 'input_chunk_length'
    cfg['fc_model']['input_length'] = model.model_params['input_chunk_length']
    # Check if length of series is long enough for the model input length
    if len(series) < cfg['fc_model']['input_length']:
        msg = f'Length of series ({len(series)} time steps) is shorter than the required "input_chunk_length" of the model ({cfg["fc_model"]["input_length"]} time steps).'
        logger.error(msg)
        raise ValueError(msg)
    # Check that forecast horizon is >= output_chunk_length
    if cfg['horizon'] < model.model_params['output_chunk_length']:
        msg = f'Forecast horizon ({cfg["horizon"]} time steps) is smaller than the "output_chunk_length" of the model ({model.model_params["output_chunk_length"]} time steps). '
        logger.error(msg)
        raise ValueError(msg)
    
    return cfg, model


#
# Specify custom covariate loading function
# ()
#


###############################################################################

def load_pretrained_model(cfg, ModelClass, force_download=False):
    """
    This method downloads a pre-trained model given a model and checkpoint link,
    and then loads it into a Darts model

    Arguments:
        cfg: a dictionary containing the configuration of the model
        ModelClass: the class of the model
        force_download: boolean flag whether to download the model again if a 
                        folder already exists (optional)
    Returns:
        Darts model object
    """

    model_path_ckpt_link = cfg['fc_model']['model_path_ckpt_link']
    model_path_pth_link = cfg['fc_model']['model_path_pth_link']

    # Try to load from checkpoint link
    path_ckpt = ""
    path_pth = ""
    path_to_store = Path(__file__).parent.absolute() / \
        'Models' / cfg['fc_model']['name'] / 'checkpoints'

    if os.path.exists(path_to_store) and not force_download:
        # Model already exists
        path_ckpt = path_to_store / "best-model.ckpt"
    else:
        # Create folder
        if not os.path.exists(path_to_store):
            os.makedirs(path_to_store)

        if model_path_ckpt_link.startswith("https://"):
            # Download checkpoint model
            path_ckpt, _ = urllib.request.urlretrieve(
                model_path_ckpt_link, path_to_store / "best-model.ckpt")
            logger.info(
                f'Downloaded checkpoint model from {model_path_ckpt_link} to {path_ckpt}')

        if model_path_pth_link.startswith("https://"):
            # Download model
            path_pth, _ = urllib.request.urlretrieve(
                model_path_pth_link, path_to_store.parent.absolute() / "_model.pth.tar")
            logger.info(f'Downloaded model from {model_path_pth_link} to {path_pth}')


    # Load pre-trained model from best checkpoints
    # NOTE: TFT model has been trained and saved on a CUDA device (i.e., using GPUs);
    #       Attempting to deserialize saved model on a CPU-only machine requires
    #       torchmetrics==0.9.3 and pytorch-lightning==1.7.7 (and will fail otherwise)
    model = ModelClass.load_from_checkpoint(path_ckpt.parent.parent.as_posix())
    logger.info(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

    # Convert loaded model to device
    trainer_params = {
            "accelerator": "auto",
            "devices": "auto",
            "logger": False,
        }
    model.trainer_params.update(trainer_params)

    return model