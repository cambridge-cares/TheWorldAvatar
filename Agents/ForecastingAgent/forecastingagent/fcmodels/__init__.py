from .model_mapping import *

# The purpose of this module is to provide a mapping between instantiated names
# for custom models and corresponding model and covariate loading functions:

# key: name of custom forecast model, i.e., rdfs:label instantiated for
#      target model instance
# value: tuple of custom model and covariate loading function as defined in
#        "model_mapping.py"
FC_MODELS = {
    'tft_pirmasens_heat_demand': (load_tft_pirmasens_heat_demand, 
                                  load_pirmasens_heat_demand_covariates),
}