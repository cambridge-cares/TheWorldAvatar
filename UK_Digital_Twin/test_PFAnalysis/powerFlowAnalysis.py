##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 31 March 2022        #
##########################################

import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
import numpy
import queryModelInput 
from UK_Digital_Twin_Package import UKPowerGridModel
import UK_Digital_Twin_Package.PYPOWER.pypower.ppoption as ppo
import UK_Digital_Twin_Package.PYPOWER.pypower.runpf as pf

from numpy import r_, c_, ix_, zeros, pi, ones, exp, argmax, union1d, array, linalg, where
from numpy import flatnonzero as find
from scipy.sparse import hstack, vstack

from pypower.loadcase import loadcase
from pypower.ext2int import ext2int
from pypower.bustypes import bustypes
from pypower.makeYbus import makeYbus
from pypower.makeSbus import makeSbus
from pypower.dSbus_dV import dSbus_dV
from pypower.idx_bus import PD, QD, VM, VA, GS, BUS_TYPE, PV, PQ, REF
from pypower.idx_brch import PF, PT, QF, QT
from pypower.idx_gen import PG, QG, VG, QMAX, QMIN, GEN_BUS, GEN_STATUS

class powerFlowAnalysis:
    
     def __init__(self, BusModelTopNodeIRI:str, BranchModelTopNodeIRI:str, GeneratorModelTopNodeIRI:str, baseMVA: float, PFOrOPFAnalysis:bool = True):
        
        """ The IRIs of the model"""
        self.BusModelTopNodeIRI = BusModelTopNodeIRI
        self.BranchModelTopNodeIRI = BranchModelTopNodeIRI
        self.GeneratorModelTopNodeIRI = GeneratorModelTopNodeIRI
        
        #TODO: baseMVA should be written in the KG and can be quired via the iri
        self.baseMVA = float(baseMVA)
        
        self.PFOrOPFAnalysis = PFOrOPFAnalysis
        
        self.ConvergeFlag = None
            
     def PowerFlowAnalysisSimulation(self, buslist, branchList):
         
         # Initialise the bus, branch and generator list 
         self.busInstanceList = []
         self.branchInstanceList = []
         self.generatorInstanceList = []
         
         ## create objects dynamically
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
                 
         ## query branch input
     #    BranchInput = queryModelInput.queryBranchModelInput(self.BranchModelTopNodeIRI)
         i_branch = 0
         BranchInput = branchList 
         for branchinput in BranchInput:  
             ObjectSet[UKPowerGridModel.UKElineModel.ELineKey + str(i_branch)] = UKPowerGridModel.UKElineModel()
             for varKey in branchinput.keys():
                 setattr(ObjectSet.get(UKPowerGridModel.UKElineModel.ELineKey + str(i_branch)), varKey, branchinput[varKey])
             i_branch += 1                   
                 
          # query generator input
     #    GeneratorInput = queryModelInput.queryGeneratorModelInput(self.GeneratorModelTopNodeIRI)
         i_generator = 0
         GeneratorInput = queryModelInput.queryGeneratorModelInput_new(10, 14, 'ukdigitaltwin_test1')         
         for geninput in GeneratorInput:              
               ObjectSet[UKPowerGridModel.UKEGenModel.EGenKey + str(i_generator)] = UKPowerGridModel.UKEGenModel()
               for varKey in geninput.keys():
                   setattr(ObjectSet.get(UKPowerGridModel.UKEGenModel.EGenKey + str(i_generator)), varKey, geninput[varKey])
               i_generator += 1   
                    
         ## Format the PF analysis input, ppc
         ##-----  Power Flow Data  -----##
         ppc: dict = {"version": '2'}
         
         ## system MVA base
         ppc["baseMVA"] = float(self.baseMVA)
         
         ## bus data
         # bus_i type Pd Qd Gs Bs area Vm Va baseKV zone Vmax Vmin  
         ppc["bus"] = numpy.zeros((len(BusInput), len(UKPowerGridModel.UKEbusModel.INPUT_VARIABLE_KEYS)), dtype = float)
         index_bus  = 0
         while index_bus < len(BusInput):
             for key in UKPowerGridModel.UKEbusModel.INPUT_VARIABLE_KEYS:
                 index = int(UKPowerGridModel.UKEbusModel.INPUT_VARIABLE[key])
                 ppc["bus"][index_bus][index] = getattr(ObjectSet.get(UKPowerGridModel.UKEbusModel.EBusKey + str(index_bus + 1)), key)
             index_bus += 1
          
         ## branch data
         # fbus, tbus, r, x, b, rateA, rateB, rateC, ratio, angle, status, angmin, angmax     
         ppc["branch"] = numpy.zeros((len(BranchInput), len(UKPowerGridModel.UKElineModel.INPUT_VARIABLE_KEYS)), dtype = float)
         index_br  = 0
         while index_br < len(BranchInput):
             for key in UKPowerGridModel.UKElineModel.INPUT_VARIABLE_KEYS:
                 index = int(UKPowerGridModel.UKElineModel.INPUT_VARIABLE[key])
                 ppc["branch"][index_br][index] = getattr(ObjectSet.get(UKPowerGridModel.UKElineModel.ELineKey + str(index_br)), key)
             index_br += 1
         
         ## generator data
         # bus, Pg, Qg, Qmax, Qmin, Vg, mBase, status, Pmax, Pmin, Pc1, Pc2,
         # Qc1min, Qc1max, Qc2min, Qc2max, ramp_agc, ramp_10, ramp_30, ramp_q, apf
         ppc["gen"] = numpy.zeros((len(GeneratorInput), len(UKPowerGridModel.UKEGenModel.INPUT_VARIABLE_KEYS)), dtype = float)
         index_gen  = 0
         while index_gen < len(GeneratorInput):
              for key in UKPowerGridModel.UKEGenModel.INPUT_VARIABLE_KEYS:
                  index = int(UKPowerGridModel.UKEGenModel.INPUT_VARIABLE[key])
                  ppc["gen"][index_gen][index] = getattr(ObjectSet.get(UKPowerGridModel.UKEGenModel.EGenKey + str(index_gen)), key)
              index_gen += 1
         
         self.ppc = ppc
         
         ## starts pf analysis
         # set up numerical method: Newton's Method and the tolerance, max iteration steps are default 
         ppopt = ppo.ppoption(OUT_ALL=1, VERBOSE = 2, PF_ALG = 1, PF_MAX_IT = 20) 
         results, _, J_newtonpf = pf.runpf(self.ppc, ppopt)   
         
         if results["success"] ==1:
             print('The model is converged.')
         else:
             print('!!!!!!The model is diverged.!!!!!')
             
         BusSwitchingIndicator, J_formMonitor = self.PowerFlowAnalysisMonitor()
         
         print('The maximum V reported form the pypower is:', results['BusSwitchingIndicator'])
          
         #print(J_newtonpf, J_formMonitor)
         if (J_newtonpf == J_formMonitor).all():
             print('J_formMonitor and J_newtonpf are the same of each item.')
         #print(results[0], len(results[0]))   
         
         ## Invake the calculation monitor
        # Monitor = self.PowerFlowAnalysisMonitor(results[0]["BusSwitchingIndicator"])
         
         
            
         return ObjectSet
         
     def PowerFlowAnalysisMonitor(self):   
        
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

        J = vstack([
                hstack([J11, J12]),
                hstack([J21, J22])
            ], format="csr")

        U, s, _V = linalg.svd(J.toarray(), full_matrices=True)        
        normalised_V = abs(_V[-1]/(linalg.norm(_V[-1])))       
        # indexOfMaxV = where(normalised_V == max(normalised_V))  
        indexOfMaxV_firstIteration = where(normalised_V == max(normalised_V))[0][0]     
        print('The maximum V reported form the monitor is:', indexOfMaxV_firstIteration)   
        
        return indexOfMaxV_firstIteration, J.toarray()
        
if __name__ == '__main__':        
    test_PowerFlowAnalysis_1 = powerFlowAnalysis(1,1,1, 100, True)
    busList = [{'BUS': '1', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '4305.753', 'VMIN': '0.95', 'TYPE': '3'}, {'BUS': '2', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2690.713', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '3', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '4349.411', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '4', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2997.804', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '5', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2354.482', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '6', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2730.946', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '7', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '3493.016', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '8', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '3876.755', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '9', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '1696.433', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '10', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2756.565', 'VMIN': '0.95', 'TYPE': '1'}]
    branchList = [{'TOBUS': '2', 'B': '3.4347502328055', 'FROMBUS': '1', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.0019538132483099997', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.0188834047633', 'ANGMAX': '360.0'}, {'TOBUS': '3', 'B': '1.887069984274', 'FROMBUS': '1', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '11201.057144', 'R': '6.038073872325E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.005835736502474999', 'ANGMAX': '360.0'}, {'TOBUS': '6', 'B': '1.539446743787', 'FROMBUS': '2', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.001970312333715', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.019042866732450002', 'ANGMAX': '360.0'}, {'TOBUS': '9', 'B': '1.3219533136480002', 'FROMBUS': '2', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5410.911345', 'R': '9.259004261394184E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.01010836111654295', 'ANGMAX': '360.0'}, {'TOBUS': '4', 'B': '2.9436182372009996', 'FROMBUS': '3', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.00167443916442', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.016183282880599996', 'ANGMAX': '360.0'}, {'TOBUS': '5', 'B': '2.6635415720849998', 'FROMBUS': '3', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.0015151211756999998', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.014643490850999998', 'ANGMAX': '360.0'}, {'TOBUS': '5', 'B': '2.0370593803409998', 'FROMBUS': '4', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.002607198487245', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.02519830612035', 'ANGMAX': '360.0'}, {'TOBUS': '6', 'B': '2.0515595937679', 'FROMBUS': '5', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8211.175631', 'R': '7.04271614929686E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.007295864343325824', 'ANGMAX': '360.0'}, {'TOBUS': '8', 'B': '3.27753321567', 'FROMBUS': '5', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '11201.057144', 'R': '0.0010487150895375', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.010135723838625', 'ANGMAX': '360.0'}, {'TOBUS': '7', 'B': '2.1010674006375', 'FROMBUS': '6', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.00119516501775', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.011551147382500002', 'ANGMAX': '360.0'}, {'TOBUS': '9', 'B': '1.7635110489349999', 'FROMBUS': '6', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.0022570885185749998', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.02181452916225', 'ANGMAX': '360.0'}, {'TOBUS': '8', 'B': '3.7299925384703996', 'FROMBUS': '7', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '10821.82269', 'R': '6.531247444995235E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.007130378802263265', 'ANGMAX': '360.0'}, {'TOBUS': '10', 'B': '1.826004669239', 'FROMBUS': '7', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '2800.264286', 'R': '0.00934829226342', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.0903502864506', 'ANGMAX': '360.0'}, {'TOBUS': '10', 'B': '3.5598669251920003', 'FROMBUS': '8', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.00455621458644', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.044035346929200005', 'ANGMAX': '360.0'}]
    res = test_PowerFlowAnalysis_1.PowerFlowAnalysisSimulation(busList, branchList)
    # print(res.get('Generator_1').PG_INPUT)
    # print(res.get('Bus_1'). __dir__()[0])
    # print(res.get('Bus_1').BUS)
    #print(res.get('Branch_0').R)
    
    