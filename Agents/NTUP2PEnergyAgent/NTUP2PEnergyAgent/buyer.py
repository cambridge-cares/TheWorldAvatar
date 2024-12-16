import gurobipy as gp
from gurobipy import GRB
import numpy as np
import scipy.sparse as sp
import logging

class Buyer():
    # Set model coefficients on construction
    def __init__(self):
       logging.info("Creating buyer")
       self.consumption =  None
       self.power_buying = None
       self.error = None
       self.op_cost = None
       self.dual_var = None
       
    
    # Evaluate the model at a particular value
    def optimize(self, info, phi_0, GUP, fix_E, rho, nodal_p):
    
    ################ some vars
        
        Pmax = info[:,1] 
        Pmin = info[:,2]
        quad_cost=info[:,3]
        lin_cost=info[:,4]

        n_buyers = 1
        n_sellers = len(phi_0)

        ################

        # Create a new model
        m = gp.Model("buyer")
       
        # Create variables
        lower_bound = np.concatenate((np.array([Pmin], ndmin=2), np.zeros((n_sellers,1))), axis=0)
        upper_bound = np.concatenate((np.array([Pmax], ndmin=2), np.ones((n_sellers,1))*np.inf), axis=0)
    
        x = m.addMVar(shape=np.shape(lower_bound), vtype=GRB.CONTINUOUS, lb=lower_bound , ub=upper_bound, name="x")

        #Constraint
        data = np.concatenate((np.array([1]),  np.ones((n_sellers))*-1.0))
        row = np.zeros((n_buyers+n_sellers))
        col = np.arange(0,n_sellers+n_buyers)
        A=sp.coo_matrix((data, (row, col)), shape=(1, n_buyers+n_sellers))
        
        rhs = 0

        m.addConstr(A @ x == rhs, name="c") #constraint sense

        #Linear objective
        obj = np.concatenate((np.add(-lin_cost, nodal_p), np.add(phi_0, np.add(0.5*GUP, -np.multiply(rho, fix_E)))), axis=0)
    #    m.setObjective(np.transpose(obj) @ x, GRB.MINIMIZE) #objective, modelsense

        #Quadratic objective
        rowQ = np.arange(0,n_buyers+n_sellers)
        colQ = np.arange(0,n_buyers+n_sellers)
        dataQ = np.concatenate((-quad_cost, np.add(0.5*np.squeeze(rho), 0.25*np.ones((n_sellers)))))
        Q=sp.coo_matrix((dataQ, (rowQ, colQ)), shape=(n_buyers+n_sellers, n_buyers+n_sellers))

        #Set objectives
        m.setMObjective(Q, np.squeeze(obj), GRB.MINIMIZE) #quadratic objective, linear objective, modelsense
       
        #params
        m.setParam(GRB.param.QCPDual, 1.0)
        m.setParam(GRB.param.OutputFlag, 0)

        # Optimize model
        m.optimize()
        
        #results
        self.consumption = x.X[0]
        self.power_buying= x.X[range(1,n_sellers+1)]
        self.error = self.power_buying - fix_E

        rho_sparse = sp.coo_matrix((0.5*np.squeeze(rho), (np.arange(0,n_sellers), np.arange(0,n_sellers))), shape=(n_sellers, n_sellers))
        self.op_cost = m.getAttr(GRB.attr.ObjVal) +  np.matmul(np.matmul( np.transpose(fix_E),  rho_sparse.todense()) , fix_E)              

        self.dual_var= m.getAttr(GRB.attr.Pi)

        print("Buyer optimisation complete")

        return x.X  #numpy.ndarray
    
#######################################
### Test 

#info = np.array([4,	2.20000000000000,	0.,	-0.100000000000000,	4.50000000000000], ndmin=2)
#phi_0 = np.array([[-16.8], [-16.0],	[-17.0],	[-15.5],	[-16.8], [-16.2]], ndmin=2)
#GUP = np.array([[0],[0],[0],[0],[0],[0]], ndmin=2)
#fix_E = np.array([[0],[0],[0],[0],[0],[0]], ndmin=2)
#rho = np.array([[0.5],[0.5],[0.5],[0.5],[0.5],[0.5]], ndmin=2)
#nodal_p = np.array([20], ndmin=2 )

#buyer = Buyer()
#result = buyer.optimize(info, phi_0, GUP, fix_E, rho, nodal_p)
#print("result:")
#print(result)