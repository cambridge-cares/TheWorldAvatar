"""
Power Flow Analysis and Optimal Flow Analysis
"""

import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
import numpy
import queryModelInput 
from UK_Digital_Twin_Package import UKPowerGridModel
from pypower.api import ppoption, runpf, isload
# import UK_Digital_Twin_Package.PYPOWER.pypower.ppoption as ppo
import UK_Digital_Twin_Package.PYPOWER.pypower.runpf as pf

from numpy import r_, c_, ix_, zeros, pi, ones, exp, union1d, array, linalg, where, logical_or, arange, \
                    ones, sort, exp, pi, diff, min, \
                    argmin, argmax, real, imag, any, delete
from numpy import flatnonzero as find
from scipy.sparse import hstack, vstack

from pypower.loadcase import loadcase
from pypower.ext2int import ext2int
from pypower.bustypes import bustypes
from pypower.makeYbus import makeYbus
from pypower.makeSbus import makeSbus
from pypower.dSbus_dV import dSbus_dV
from pypower.idx_bus import BUS_I, PD, QD, VM, VA, GS, BUS_TYPE, PV, PQ, REF
from pypower.idx_brch import PF, PT, QF, QT, F_BUS, TAP, SHIFT, T_BUS, BR_R, BR_X, BR_STATUS
from pypower.idx_gen import PG, QG, VG, QMAX, QMIN, GEN_BUS, GEN_STATUS

class powerFlowAnalysis:
    
     def __init__(self, BusModelTopNodeIRI:str, BranchModelTopNodeIRI:str, GeneratorModelTopNodeIRI:str, baseMVA: float, PFOrOPFAnalysis:bool = True):
        
        """ The IRIs of the model"""
        self.BusModelTopNodeIRI:str = BusModelTopNodeIRI
        self.BranchModelTopNodeIRI:str = BranchModelTopNodeIRI
        self.GeneratorModelTopNodeIRI:str  = GeneratorModelTopNodeIRI
        
        # The initial BusSwitchingIndicator is set to be -1 noting that there is no bus needs to be switching the type only when this number becomes positive 
        self.busToBeSwitched:int = -1
        self.ConvergeFlag:int = -1
        self.Terminate:bool = False
        
        #TODO: baseMVA should be written in the KG and can be quired via the iri
        self.baseMVA = float(baseMVA)
        self.PFOrOPFAnalysis:bool = PFOrOPFAnalysis   
        
        self.SlackBusName: list = [] 
        self.PVBusName: list = []
        self.PQBusName: list = []
        
        self.BusObjectList: list = []
        self.BranchObjectList: list = []
        self.GeneratorObjectList: list = []
        
     def ModelObjectCreator(self, buslist, branchList): 
         """
         The ModelObjectCreator is used to created the model objects (bus, branch and generator) which should be call in the first palce when proforming PF/OPF analysis
         The funtion will query from the KG of with the model enetity IRI, BusModelTopNodeIRI, BranchModelTopNodeIRI, and GeneratorModelTopNodeIRI
         
         Parameters
         ----------
         buslist : TYPE
             DESCRIPTION.
         branchList : TYPE
             DESCRIPTION.

         Returns
         -------
         None.

         """
         
         ##-- create model bus, branch and generator objects dynamically --##
         ObjectSet = locals()
# TODO: do not change the starting indiex number from 1 to 0         
         ## query bus input and create the bus objects 
     #    BusInput = queryModelInput.queryBusModelInput(self.BusModelTopNodeIRI)        
         BusInput = busList
         BusNumKeyWord = UKPowerGridModel.UKEbusModel.INPUT_VARIABLE_KEYS[0]  
         for businput in BusInput:              
             ObjectSet[UKPowerGridModel.UKEbusModel.EBusKey + str(businput[BusNumKeyWord])] = UKPowerGridModel.UKEbusModel()
             for varKey in businput.keys():
                 setattr(ObjectSet.get(UKPowerGridModel.UKEbusModel.EBusKey + str(businput[BusNumKeyWord])), varKey, businput[varKey])
                 if UKPowerGridModel.UKEbusModel.INPUT_VARIABLE_KEYS.index(varKey) == BUS_TYPE:                      
                     if int(businput[varKey]) == REF:                        
                         self.SlackBusName.append(UKPowerGridModel.UKEbusModel.EBusKey + str(businput[BusNumKeyWord]))                         
                     elif int(businput[varKey]) == PV:                        
                         self.PVBusName.append(UKPowerGridModel.UKEbusModel.EBusKey + str(businput[BusNumKeyWord]))                        
                     elif int(businput[varKey]) == PQ:                            
                         self.PQBusName.append(UKPowerGridModel.UKEbusModel.EBusKey + str(businput[BusNumKeyWord]))                         
         
         self.NumberOfBus = len(BusInput)
         self.BusObjectList = self.SlackBusName + self.PVBusName + self.PQBusName
         
         ## query branch input
     #    BranchInput = queryModelInput.queryBranchModelInput(self.BranchModelTopNodeIRI)
         i_branch = 0
         BranchInput = branchList 
         for branchinput in BranchInput:  
             ObjectSet[UKPowerGridModel.UKElineModel.ELineKey + str(i_branch)] = UKPowerGridModel.UKElineModel()
             self.BranchObjectList.append(UKPowerGridModel.UKElineModel.ELineKey + str(i_branch))
             for varKey in branchinput.keys():
                 setattr(ObjectSet.get(UKPowerGridModel.UKElineModel.ELineKey + str(i_branch)), varKey, branchinput[varKey])
             i_branch += 1                   
         
         self.NumberOfBranch = len(BranchInput)     
            
          # query generator input
     #    GeneratorInput = queryModelInput.queryGeneratorModelInput(self.GeneratorModelTopNodeIRI)
         i_generator = 0
         GeneratorInput = queryModelInput.queryGeneratorModelInput_new(10, 14, 'ukdigitaltwin_test1')         
         for geninput in GeneratorInput:              
               ObjectSet[UKPowerGridModel.UKEGenModel.EGenKey + str(i_generator)] = UKPowerGridModel.UKEGenModel()
               self.GeneratorObjectList.append(UKPowerGridModel.UKEGenModel.EGenKey + str(i_generator))
               for varKey in geninput.keys():
                   setattr(ObjectSet.get(UKPowerGridModel.UKEGenModel.EGenKey + str(i_generator)), varKey, geninput[varKey])
               i_generator += 1   
                    
         self.NumberOfGenerator = len(GeneratorInput)
         
         self.ObjectSet = ObjectSet
    
         return     
        
     def ModelInputFormatter(self):
         """
         The ModelInputFormatter is used to created the pypower PF model input from the model objects which are created from the F{ModelObjectCreator}.
         This function will be called abfer F{ModelObjectCreator}.
         
         Raises
         ------
         Exception
             If the attribute ObjectSet does not exist.

         Returns
         -------
         None.
             
         """

         if not hasattr(self, 'ObjectSet'):  
             raise Exception("The model object has not been properly created, please run the function ModelObjectCreator at first.")
         
         ##-- Format the PF analysis input, ppc --##
         ppc: dict = {"version": '2'}
         
         ## system MVA base
         ppc["baseMVA"] = float(self.baseMVA)
         
         ## bus data
         # bus_i type Pd Qd Gs Bs area Vm Va baseKV zone Vmax Vmin  
         ppc["bus"] = numpy.zeros((self.NumberOfBus, len(UKPowerGridModel.UKEbusModel.INPUT_VARIABLE_KEYS)), dtype = float)
         index_bus  = 0
         while index_bus < self.NumberOfBus:
             for key in UKPowerGridModel.UKEbusModel.INPUT_VARIABLE_KEYS:
                 index = int(UKPowerGridModel.UKEbusModel.INPUT_VARIABLE[key])
                 ppc["bus"][index_bus][index] = getattr(self.ObjectSet.get(UKPowerGridModel.UKEbusModel.EBusKey + str(index_bus + 1)), key)
             index_bus += 1
             
         ## branch data
         # fbus, tbus, r, x, b, rateA, rateB, rateC, ratio, angle, status, angmin, angmax     
         ppc["branch"] = numpy.zeros((self.NumberOfBranch, len(UKPowerGridModel.UKElineModel.INPUT_VARIABLE_KEYS)), dtype = float)
         index_br  = 0
         while index_br < self.NumberOfBranch:
             for key in UKPowerGridModel.UKElineModel.INPUT_VARIABLE_KEYS:
                 index = int(UKPowerGridModel.UKElineModel.INPUT_VARIABLE[key])
                 ppc["branch"][index_br][index] = getattr(self.ObjectSet.get(UKPowerGridModel.UKElineModel.ELineKey + str(index_br)), key)
             index_br += 1
         
         ## generator data
         # bus, Pg, Qg, Qmax, Qmin, Vg, mBase, status, Pmax, Pmin, Pc1, Pc2,
         # Qc1min, Qc1max, Qc2min, Qc2max, ramp_agc, ramp_10, ramp_30, ramp_q, apf
         ppc["gen"] = numpy.zeros((self.NumberOfGenerator, len(UKPowerGridModel.UKEGenModel.INPUT_VARIABLE_KEYS)), dtype = float)
         index_gen  = 0
         while index_gen < self.NumberOfGenerator:
              for key in UKPowerGridModel.UKEGenModel.INPUT_VARIABLE_KEYS:
                  index = int(UKPowerGridModel.UKEGenModel.INPUT_VARIABLE[key])
                  ppc["gen"][index_gen][index] = getattr(self.ObjectSet.get(UKPowerGridModel.UKEGenModel.EGenKey + str(index_gen)), key)
              index_gen += 1
         
         self.ppc = ppc
 
         return 
            
     def PowerFlowAnalysisSimulation(self, ppc: list = None):
         """
         Perform the power flow analysis.

         Parameters
         ----------
         ppc : List
             ppc is the list of the input the power flow model. The default is None.

         Raises
         ------
         Exception
             DESCRIPTION.

         Returns
         -------
         None.

         """
         
         if not hasattr(self, 'ppc'): 
             if ppc is None or not isinstance(ppc, list):
                 raise Exception("The model input has not been reformatted, please run the function ModelInputFormatter at first.")
             else:
                 self.ppc = ppc
    
         ##-- starts pf analysis --## 
         # set up numerical method: Newton's Method
         self.ppopt = ppoption(OUT_ALL = 1, VERBOSE = 2, PF_ALG = 1, PF_MAX_IT = 30, PF_TOL = 1e-8) 
         self.results, _, _ = pf.runpf(self.ppc, self.ppopt) #TODO: use the original pypower
         
         self.ConvergeFlag = self.results["success"]
         if self.ConvergeFlag == 1:
             print('-----The model is converged.-----')
         elif self.ConvergeFlag == 0:
             print('!!!!!!The model is diverged.!!!!!')
         ##-- Invake the calculation monitor --##    
             self.PowerFlowAnalysisMonitor() 
         else:
             raise Exception("The PF/OPF analysis has not been processed properly.")
            
         return
     
     def ModelOutputFormatter(self):
         """
         Reformat the result and add attributes into the objects.

         Returns
         -------
         None.

         """
                
         ## the bus, gen, branch and loss result
         bus = self.results["bus"]
         branch = self.results["branch"]
         gen = self.results["gen"]
         
         if self.ppopt['PF_DC']:
            loss = zeros(self.NumberOfBranch)
         else:
            ## create map of external bus numbers to bus indices
            i2e = bus[:, BUS_I].astype(int)
            e2i = zeros(max(i2e) + 1, int)
            e2i[i2e] = arange(bus.shape[0])
            tap = ones(self.NumberOfBranch)       ## default tap ratio = 1 for lines
            xfmr = find(branch[:, TAP])           ## indices of transformers
            tap[xfmr] = branch[xfmr, TAP]         ## include transformer tap ratios
            tap = tap * exp(-1j * pi / 180 * branch[:, SHIFT]) ## add phase shifters
            V = bus[:, VM] * exp(-1j * pi / 180 * bus[:, VA])
            loss = self.baseMVA * abs(V[e2i[ branch[:, F_BUS].astype(int) ]] / tap -
                                 V[e2i[ branch[:, T_BUS].astype(int) ]])**2 / \
                        (branch[:, BR_R] - 1j * branch[:, BR_X])
         out = find(branch[:, BR_STATUS] == 0)    ## out-of-service branches
         nout = len(out)
         loss[out] = zeros(nout)
            
         ##--Bus--##  
         ##  VM_OUTPUT, VM_OUTPUT, P_GEN, G_GEN, PD_OUTPUT, GD_OUTPUT        
         ## post processsing of the bus results   
         busPostResult = numpy.zeros((self.NumberOfBus, len(UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
         
         for i in range(self.NumberOfBus):
             busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["VM_OUTPUT"]] = bus[i][VM]
             busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["VA_OUTPUT"]] = bus[i][VA]
             
             g  = find((gen[:, GEN_STATUS] > 0) & (gen[:, GEN_BUS] == bus[i, BUS_I]) & ~isload(gen))
             ld = find((gen[:, GEN_STATUS] > 0) & (gen[:, GEN_BUS] == bus[i, BUS_I]) & isload(gen))
             
             if any(g + 1):
                busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["P_GEN"]] = sum(gen[g, PG])
                busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["G_GEN"]] = sum(gen[g, QG])
                       
             if logical_or(bus[i, PD], bus[i, QD]) | any(ld + 1):
                if any(ld + 1):
                    busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["PD_OUTPUT"]] = bus[i, PD] - sum(gen[ld, PG])
                    busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["GD_OUTPUT"]] = bus[i, QD] - sum(gen[ld, QG])    
                else:
                    busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["PD_OUTPUT"]] = bus[i][PD]
                    busPostResult[i][UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE["GD_OUTPUT"]] = bus[i][QD]
             
         ## update the object attributes with the model results
         index_bus  = 0
         while index_bus < self.NumberOfBus:
             for key in UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE_KEYS:
                 index = int(UKPowerGridModel.UKEbusModel.OUTPUT_VARIABLE[key])
                 setattr(self.ObjectSet.get(UKPowerGridModel.UKEbusModel.EBusKey + str(index_bus + 1)), key, busPostResult[index_bus][index])        
             index_bus += 1   
         
         ##--Branch--##    
         ## FROMBUSINJECTION_P, FROMBUSINJECTION_Q, TOBUSINJECTION_P, TOBUSINJECTION_Q, LOSS_P, LOSS_Q
         ## post processsing of the branch results   
         branchPostResult = numpy.zeros((self.NumberOfBranch, len(UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
         
         for i in range(self.NumberOfBranch):
             branchPostResult[i][UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE["FROMBUSINJECTION_P"]] = branch[i][PF]
             branchPostResult[i][UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE["FROMBUSINJECTION_Q"]] = branch[i][QF]
             branchPostResult[i][UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE["TOBUSINJECTION_P"]] = branch[i][PT]
             branchPostResult[i][UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE["TOBUSINJECTION_Q"]] = branch[i][QT]
             branchPostResult[i][UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE["LOSS_P"]] = loss[i].real
             branchPostResult[i][UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE["LOSS_Q"]] =  loss[i].imag
              
         ## update the object attributes with the model results
         index_br  = 0
         while index_br < self.NumberOfBranch:
             for key in UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE_KEYS:
                 index = int(UKPowerGridModel.UKElineModel.OUTPUT_VARIABLE[key])
                 setattr(self.ObjectSet.get(UKPowerGridModel.UKElineModel.ELineKey + str(index_br)), key, branchPostResult[index_br][index])        
             index_br += 1   
         
         ##--Generator--##    
         ## PG_OUTPUT, QG_OUTPUT
         ## post processsing of the generator results   
         generatorPostResult = numpy.zeros((self.NumberOfGenerator, len(UKPowerGridModel.UKEGenModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
         
         for i in range(self.NumberOfGenerator):
             if (gen[i, GEN_STATUS] > 0) & logical_or(gen[i, PG], gen[i, QG]):
                generatorPostResult[i][UKPowerGridModel.UKEGenModel.OUTPUT_VARIABLE["PG_OUTPUT"]] = gen[i][PG]
                generatorPostResult[i][UKPowerGridModel.UKEGenModel.OUTPUT_VARIABLE["QG_OUTPUT"]] = gen[i][QG]

         ## update the object attributes with the model results
         index_gen  = 0
         while index_gen < self.NumberOfGenerator:
             for key in UKPowerGridModel.UKEGenModel.OUTPUT_VARIABLE_KEYS:
                 index = int(UKPowerGridModel.UKEGenModel.OUTPUT_VARIABLE[key])
                 setattr(self.ObjectSet.get(UKPowerGridModel.UKEGenModel.EGenKey + str(index_gen)), key, generatorPostResult[index_gen][index])        
             index_gen += 1   
         return 
         
     def PowerFlowAnalysisMonitor(self):  
        """
        Monitor the Jacobian matrix of the first iteration and based on the SVD results selects the bus which is going to switched to PV for the next round of calculation.

        Returns
        -------
        None.

        """
        ## read data
        ppc = loadcase(self.ppc)
    
        ## add zero columns to branch for flows if needed
        if ppc["branch"].shape[1] < QT:
            ppc["branch"] = c_[ppc["branch"],
                               zeros((ppc["branch"].shape[0],
                                      QT - ppc["branch"].shape[1] + 1))]
    
        ## convert to internal indexing
        ppc = ext2int(ppc)
        baseMVA, bus, gen, branch = \
            ppc["baseMVA"], ppc["bus"], ppc["gen"], ppc["branch"]
    
        ## get bus index lists of each type of bus
        ref, pv, pq = bustypes(bus, gen)
    
        ## generator info
        on = find(gen[:, GEN_STATUS] > 0)      ## which generators are on?
        gbus = gen[on, GEN_BUS].astype(int)    ## what buses are they at?
     
        ## initial state
        V0  = bus[:, VM] * exp(1j * pi/180 * bus[:, VA])
        vcb = ones(V0.shape)    # create mask of voltage-controlled buses
        vcb[pq] = 0     # exclude PQ buses
        k = find(vcb[gbus])     # in-service gens at v-c buses
        V0[gbus[k]] = gen[on[k], VG] / abs(V0[gbus[k]]) * V0[gbus[k]]

        ## build admittance matrices
        Ybus, _, _ = makeYbus(baseMVA, bus, branch)
         
        ## set up indexing for updating V
        pvpq = r_[pv, pq]
        dS_dVm, dS_dVa = dSbus_dV(Ybus, V0)

        J11 = dS_dVa[array([pvpq]).T, pvpq].real
        J12 = dS_dVm[array([pvpq]).T, pq].real
        J21 = dS_dVa[array([pq]).T, pvpq].imag
        J22 = dS_dVm[array([pq]).T, pq].imag
        ## Jacobian Matrix
        J = vstack([
                hstack([J11, J12]),
                hstack([J21, J22])
            ], format="csr")
        
        ## SVD operation of the Jacobian matrix
        U, s, _V = linalg.svd(J.toarray(), full_matrices=True)        
        normalised_V = abs(_V[-1]/(linalg.norm(_V[-1])))  
        
        ## Initialise the ACCEPT flag to identify whether the selected bus is the proper one 
        ACCEPT = False
        
        ## Find the Bus that needs to be switched type
        while not ACCEPT and len(self.PVBusName) < (self.NumberOfBus - len(self.SlackBusName)):         
            indexOfMaxV_firstIteration = where(normalised_V == max(normalised_V))[0][0] 
            normalised_V = delete(normalised_V, indexOfMaxV_firstIteration)
            indexOfMaxV_firstIteration += 1 
            numberOfPEq = self.NumberOfBus - len(self.SlackBusName)
            
            if indexOfMaxV_firstIteration <= numberOfPEq: # number of the P equations
                busToBeSwitched = indexOfMaxV_firstIteration
                for sb in self.SlackBusName:
                    if busToBeSwitched >= int(getattr(self.ObjectSet.get(sb),"BUS")):
                        busToBeSwitched += 1               
            else:
                busToBeSwitched = indexOfMaxV_firstIteration - numberOfPEq
                for sb in self.SlackBusName:
                    if busToBeSwitched >= int(getattr(self.ObjectSet.get(sb),"BUS")):
                        busToBeSwitched += 1
                   
                for pvb in self.PVBusName:
                    if busToBeSwitched >= int(getattr(self.ObjectSet.get(pvb),"BUS")):
                        busToBeSwitched += 1
            
            ACCEPT = True 
            
            for pvb in self.PVBusName:
                 if busToBeSwitched == int(getattr(self.ObjectSet.get(pvb),"BUS")):
                     ACCEPT = False 
                     break
            
            if ACCEPT:
                self.busToBeSwitched = busToBeSwitched
                print('The bus that should be changed the type is:', self.busToBeSwitched)   
        
        if not ACCEPT: # if still not accepted, it means that there is no more bus that can be switched from the PQ to PV and the model has failed
            self.Terminate = True
            print("*******************There is no more bus that is allowed to be switched to PV bus. The model is FAILED.*******************")
        return 
    
     def ModelResultUpdater(self):
         
         
         
         
         return 

    
if __name__ == '__main__':        
    test_PowerFlowAnalysis_1 = powerFlowAnalysis(1,1,1, 100, True)
    busList = [{'BUS': '1', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '4305.753', 'VMIN': '0.95', 'TYPE': '3'}, {'BUS': '2', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2690.713', 'VMIN': '0.95', 'TYPE': '2'}, {'BUS': '3', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '4349.411', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '4', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2997.804', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '5', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2354.482', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '6', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2730.946', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '7', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '3493.016', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '8', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '3876.755', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '9', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '1696.433', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '10', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2756.565', 'VMIN': '0.95', 'TYPE': '2'}]
    branchList = [{'TOBUS': '2', 'B': '3.4347502328055', 'FROMBUS': '1', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.0019538132483099997', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.0188834047633', 'ANGMAX': '360.0'}, {'TOBUS': '3', 'B': '1.887069984274', 'FROMBUS': '1', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '11201.057144', 'R': '6.038073872325E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.005835736502474999', 'ANGMAX': '360.0'}, {'TOBUS': '6', 'B': '1.539446743787', 'FROMBUS': '2', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.001970312333715', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.019042866732450002', 'ANGMAX': '360.0'}, {'TOBUS': '9', 'B': '1.3219533136480002', 'FROMBUS': '2', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5410.911345', 'R': '9.259004261394184E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.01010836111654295', 'ANGMAX': '360.0'}, {'TOBUS': '4', 'B': '2.9436182372009996', 'FROMBUS': '3', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.00167443916442', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.016183282880599996', 'ANGMAX': '360.0'}, {'TOBUS': '5', 'B': '2.6635415720849998', 'FROMBUS': '3', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.0015151211756999998', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.014643490850999998', 'ANGMAX': '360.0'}, {'TOBUS': '5', 'B': '2.0370593803409998', 'FROMBUS': '4', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.002607198487245', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.02519830612035', 'ANGMAX': '360.0'}, {'TOBUS': '6', 'B': '2.0515595937679', 'FROMBUS': '5', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8211.175631', 'R': '7.04271614929686E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.007295864343325824', 'ANGMAX': '360.0'}, {'TOBUS': '8', 'B': '3.27753321567', 'FROMBUS': '5', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '11201.057144', 'R': '0.0010487150895375', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.010135723838625', 'ANGMAX': '360.0'}, {'TOBUS': '7', 'B': '2.1010674006375', 'FROMBUS': '6', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.00119516501775', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.011551147382500002', 'ANGMAX': '360.0'}, {'TOBUS': '9', 'B': '1.7635110489349999', 'FROMBUS': '6', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.0022570885185749998', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.02181452916225', 'ANGMAX': '360.0'}, {'TOBUS': '8', 'B': '3.7299925384703996', 'FROMBUS': '7', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '10821.82269', 'R': '6.531247444995235E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.007130378802263265', 'ANGMAX': '360.0'}, {'TOBUS': '10', 'B': '1.826004669239', 'FROMBUS': '7', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '2800.264286', 'R': '0.00934829226342', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.0903502864506', 'ANGMAX': '360.0'}, {'TOBUS': '10', 'B': '3.5598669251920003', 'FROMBUS': '8', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.00455621458644', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.044035346929200005', 'ANGMAX': '360.0'}]
    test_PowerFlowAnalysis_1.ModelObjectCreator(busList, branchList)
    test_PowerFlowAnalysis_1.ModelInputFormatter()
    test_PowerFlowAnalysis_1.PowerFlowAnalysisSimulation()
    test_PowerFlowAnalysis_1.ModelOutputFormatter()
    
    print("*****This are EBus results*****")
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('EBus-7').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('EBus-7'), attr))
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('EBus-8').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('EBus-8'), attr))
    
    print("*****This are ELine results*****")
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('ELine-0').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('ELine-0'), attr)) 
        
    print("*****This are EGen results*****")
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('EGen-1134').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('EGen-1134'), attr)) 
        
    
    # res = test_PowerFlowAnalysis_1.PowerFlowAnalysisSimulation(busList, branchList)
    # print(res.get('Generator_1').PG_INPUT)
    # print(res.get('Bus_1'). __dir__()[0])
    # print(res.get('Bus_1').BUS)
    #print(res.get('Branch_0').R)
    
        
    