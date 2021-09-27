from summit.utils.dataset import DataSet
from summit.domain import ContinuousVariable, Domain
from summit.strategies import TSEMO


# options:
# 1. strategy type (algorithm): TSEMO
# 2. optimisation variables, with constraints
# 3. objective functions
# 4. number of experiment to be suggested
# 5. historical data

# strategy = TSEMO(domain, n_spectral_points=30, generations=20, pop_size=20)
#                         experimentsdf = strategy.suggest_experiments(1, data)

# # Create domain to host the optimisation variables, corresponding constraints, and objective function


class DoEModel():
    def __init__(self):
        self.domain = Domain()
        self.strategy = TSEMO(self.domain)
        # self.historical_data = 

    def add_optimisation_variables():
        self.domain += ContinuousVariable(name="var", description="", bounds=[5, 10])

    def add_objective_functions():
        self.domain += ContinuousVariable(name="obj", description="", bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)
    
    def load_strategy():
        self.strategy = TSEMO(self.domain)
    
    def suggest():
        self.add_optimisation_variables()
        self.add_objective_functions()
        self.load_strategy()
        next_exp = TSEMO(self.domain).suggest_experiments(1)
        # next_exp = self.strategy.suggest_experiments(1, self.historical_data)
        return next_exp.to_json()

# domain = Domain()

#                 for ivarnum in range(0, totvar):
#                     tmpvarname = "var{}"
#                     tmpvarname = tmpvarname.format(ivarnum)
#                     varname[ivarnum] = tmpvarname
#                     domain += ContinuousVariable(varname[ivarnum], description='', bounds=[optlb[ivarnum], optub[ivarnum]])

#                 optobj = copy.copy(stepobj)
#                 optobj = optobj.astype(np.object, copy=True)
#                 mloutputs = []

#                 # creating domains within SUMMIT

#                 for istpnum in range(0, stepnum):
#                     tmplen = len(optobj[istpnum])
#                     for iobjnum in range(0, tmplen):
#                         if "Space-Time Yield" in optobj[istpnum][iobjnum]:
#                             optobj[istpnum][iobjnum] = "STY" + "-" + str(istpnum + 1)
#                             domain += ContinuousVariable(optobj[istpnum][iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)
#                         elif "Materials Cost" in optobj[istpnum][iobjnum]:
#                             optobj[istpnum][iobjnum] = "Cost" + "-" + str(istpnum + 1)
#                             domain += ContinuousVariable(optobj[istpnum][iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=False)
#                         elif "Impurity Minimisation" in optobj[istpnum][iobjnum]:
#                             optobj[istpnum][iobjnum] = "Impurities" + "-" + str(istpnum + 1)
#                             domain += ContinuousVariable(optobj[istpnum][iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=False)
#                         elif "Conversion" in optobj[istpnum][iobjnum]:
#                             optobj[istpnum][iobjnum] = "Conversion" + "-" + str(istpnum + 1)
#                             domain += ContinuousVariable(optobj[istpnum][iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=False)
#                         elif "E-Factor" in optobj[istpnum][iobjnum]:
#                             optobj[istpnum][iobjnum] = "E-Factor" + "-" + str(istpnum + 1)
#                             domain += ContinuousVariable(optobj[istpnum][iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)
#                         elif "EcoScore" in optobj[istpnum][iobjnum]:
#                             optobj[istpnum][iobjnum] = "EcoScore" + "-" + str(istpnum + 1)
#                             domain += ContinuousVariable(optobj[istpnum][iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)
#                         else:
#                             optobj[istpnum][iobjnum] = "Yield" + "-" + str(istpnum + 1)
#                             domain += ContinuousVariable(optobj[istpnum][iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)

#                         mloutputs.append(optobj[istpnum][iobjnum])

#                 for iobjnum in range(0, len(processobj)):
#                     if "Cost" in processobj[iobjnum]:
#                         processobj[iobjnum] = "Process-Cost"
#                         domain += ContinuousVariable(processobj[iobjnum], description='', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=False)

#                         mloutputs.append(processobj[iobjnum])

#                     mlvar = varname.tolist()



# datastore["mldata"] = pd.read_excel(xlspath, sheet_name="TSEMO Data")
#                 data = DataSet.from_df(datastore["mldata"])
#                 datastore["mldataX"] = datastore["mldata"].iloc[:,0:(datastore["mldataX"].shape[1])]
#                 datastore["mldataY"] = datastore["mldata"].iloc[:, (datastore["mldataX"].shape[1]):]


# # runs TSEMO to generate experimental variables for execution

#                         strategy = TSEMO(domain, n_spectral_points=30, generations=20, pop_size=20)
#                         experimentsdf = strategy.suggest_experiments(1, data)

#                         # finds discrete variables from descriptors based on minimum euclidean distance

#                         newcond = DataSet.data_to_numpy(experimentsdf)
#                         newvar[istpnum] = newcond[0][varstart[istpnum]:varend[istpnum]] # Added [0] to newcond