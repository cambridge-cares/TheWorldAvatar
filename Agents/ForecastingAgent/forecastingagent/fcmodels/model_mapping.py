################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 17 Aug 2023                            #
################################################

# The purpose of this module is to provide custom model and covariate loading
# functions for pre-trained models:
# Own/new loading functions shall be added below and included in the 'FC_MODELS'
# mapping dictionary for their applicable forecasting models

import os
import urllib
from pathlib import Path

from darts.models import TFTModel

from py4jps import agentlogging

from forecastingagent.datamodel.iris import *
from forecastingagent.utils.ts_utils import *


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
# 2) SPECIFY CUSTOM COVARIATE LOADING FUNCTIONS
# NOTE: Custom covariate loading functions are required, as different pre-trained
#       models require 1) different sets of covariates
#                      2) covariate order to match order during training
#                      3) some "implicite" covariates which are not marked up in
#                         the KG (yet), e.g., day of week, etc. but can be extracted
#                         from time stamps
#

def load_pirmasens_heat_demand_covariates(covariates_dict, tsClient, lowerbound, upperbound):
    """
    Load covariates as required for pre-trained temporal fusion transformer model
    for Pirmasens' district heating heat demand

    Arguments:
        covariates_dict -- dictionary of relevant covariates, with covariate
                           IRIs as keys and RDFTYPEs as values
        tsClient -- tsClient onject
        lowerbound -- lower bound of the time series data
        upperbound -- upper bound of the time series data
        
    Returns:
        darts.TimeSeries object with all covariates to consider
    """

    # Extract time series data for covariates based on their given dataIRIs
    # NOTE: This approach only works for unique rdf types, i.e., one covariate per rdf type
    df_air_temp, df_public_holiday = None, None
    for iri, type in covariates_dict.items():
        if type == ONTOEMS_AIRTEMPERATURE and df_air_temp is None:
            logger.info(f'Loading air temperature covariate')
            df_air_temp = get_df_of_ts(iri, tsClient, lowerbound=lowerbound, 
                                       upperbound=upperbound, column_name='covariate')

        if type == OHN_ISPUBLICHOLIDAY and df_public_holiday is None:
            logger.info(f'Loading public holiday covariate')
            df_public_holiday = get_df_of_ts(iri, tsClient, lowerbound=lowerbound, 
                                             upperbound=upperbound, column_name='covariate')
            # NOTE: Boolean ts values are converted automatically by "scale_covariate"
            #       into one-hot encoded values

    # Create consolidated (scaled) covariates list incl. time covariates for the forecast
    # NOTE: The covariate list MUST have the same order as during training
    covariates = concatenate(
        [
            scale_covariate(df_air_temp, "covariate"),
            # Use times of other covariate to create/extract time covariates
            # NOTE: If no other covariate is available, the orginal series could be used;
            #       however, you would need to extend the time range of the original series
            #       in order to cover full length of forecast horizon
            #       (just for reference and not relevant for this loading function)
            get_time_covariates(df_air_temp, 
                                {"dayofyear": True, "dayofweek": True, "hour": True}),
            scale_covariate(df_public_holiday, "covariate"),
        ],
        axis="component",
    )
    logger.info(f'Created time covariates: "dayofyear", "dayofweek", "hour"')

    return covariates


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
