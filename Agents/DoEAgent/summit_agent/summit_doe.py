from summit.utils.dataset import DataSet
from summit.domain import ContinuousVariable, Domain
from summit.strategies import TSEMO

def proposeNewExperiment(doe) -> DataSet:
    """
    The input doe should have below structure (the string at value part of a key-value pair indicates the desired datatype):
    {
        "TSEMO": {"nSpectralPoints": int, "nGenerations": int, "nRetries": int, "populationSize": int}, 
        "continuousVariables": [{"name": str, "description": str, "lower": str or float, "upper": str or float}, {"name": str, "description": str, "lower": str or float, "upper": str or float} ...],
        "systemResponses": [{"name": str, "direction": str (maximise or minimise)}, {"name": str, "direction": str (maximise or minimise)} ...],
        "historicalData": dataframe,
        "numOfExp": str or int
    }
    e.g.
    doe = { \
                "TSEMO": {"nSpectralPoints": 30, "nGenerations": 20, "populationSize": 20}, \
                "continuousVariables": [{"name": "ContinuousVariable_1", "lower": 1, "upper": 10}, 
                {"name": "ContinuousVariable_2", "lower": 0.02, "upper": 0.2},
                {"name": "ContinuousVariable_3", "lower": 5, "upper": 15},
                {"name": "ContinuousVariable_4", "lower": 30, "upper": 70}], \
                "systemResponses": [{"name": "SystemResponse_1", "direction": "maximise"}, 
                {"name": "SystemResponse_2", "direction": "minimise"}], \
                "historicalData": previous_results, \
                "numOfExp": 1}
    The previous_results is a dataframe looks like below:
        ContinuousVariable_1  ContinuousVariable_2  ContinuousVariable_3  ContinuousVariable_4  SystemResponse_1  SystemResponse_2
    0                  5.19                  0.10                  14.7                  42.0              47.9              7.44
    1                  1.59                  0.07                  13.3                  35.0               8.7              7.74
    2                  8.44                  0.16                   7.9                  62.0              54.1              6.96
    3                  8.83                  0.04                  11.8                  67.0              40.0              8.10
    4                  5.01                  0.17                   8.1                  56.0              47.7              6.83
    """
    domain = Domain()
    
    # Add all optimisation variables to domain
    for var in doe['continuousVariables']:
        domain += ContinuousVariable(name=str(var['name']), description=str(var['description'])  if 'description' in var else "", \
            bounds=[float(var['lower']), float(var['upper'])])
    # Add all system responses to domain
    for var in doe['systemResponses']:
        if "max" in var['direction']:
            domain += ContinuousVariable(name=str(var['name']), description=str(var['description']) if 'description' in var else "", \
                bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)
        else:
            domain += ContinuousVariable(name=str(var['name']), description=str(var['description']) if 'description' in var else "", \
                bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=False)
    
    # Create strategy (only supporting TSEMO at the moment)
    if 'TSEMO' in doe:
        tse = doe['TSEMO']
        strategy = TSEMO(domain, \
            n_spectral_points=int(tse['nSpectralPoints']) if 'nSpectralPoints' in tse else 1500, \
                generations=int(tse['nGenerations']) if 'nGenerations' in tse else 100, \
                    n_retries=int(tse['nRetries']) if 'nRetries' in tse else 10, \
                        pop_size=int(tse['populationSize']) if 'populationSize' in tse else 100)
    
    # Load historical data
    previous_results = DataSet.from_df(doe['historicalData'])

    # Suggest the next experiment
    next_exp = strategy.suggest_experiments(int(doe['numOfExp']) if 'numOfExp' in doe else 1, prev_res=previous_results)
    return next_exp
