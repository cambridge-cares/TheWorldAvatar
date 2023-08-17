from .model_mapping import *

# 
# key: name of custom forecast model, i.e., rdfs:label instantiated for
# target model instance
# value: (custom model loading function, custom covariate loading function)
FC_MODELS = {
    'tft_pirmasens_heat_demand': (load_tft_pirmasens_heat_demand, None),
}