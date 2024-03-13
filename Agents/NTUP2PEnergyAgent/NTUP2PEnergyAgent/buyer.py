import gurobipy as gp
from gurobipy import GRB
import numpy as np
import scipy.sparse as sp

class PolyModel():
    # Set model coefficients on construction
    def __init__(self,order):
        if order < 0 or order > 2:
            raise ValueError("order must be between 0 and 2")
        all_coeffs = [2,-1,3]
        self.coeffs = all_coeffs[:order+1]
    
    # Evaluate the model at a particular value
    def evaluate(self,x):
    
         # Create a new model
        m = gp.Model("matrix1")

        # Create variables
        x = m.addMVar(shape=3, vtype=GRB.BINARY, name="x")

        # Set objective
        obj = np.array([1.0, 1.0, 2.0])
        m.setObjective(obj @ x, GRB.MAXIMIZE)

        # Build (sparse) constraint matrix
        val = np.array([1.0, 2.0, 3.0, -1.0, -1.0])
        row = np.array([0, 0, 0, 1, 1])
        col = np.array([0, 1, 2, 0, 1])

        A = sp.csr_matrix((val, (row, col)), shape=(2, 3))

        # Build rhs vector
        rhs = np.array([4.0, -1.0])

        # Add constraints
        m.addConstr(A @ x <= rhs, name="c")

#######
        #model Q, obj, A, rhs, sense, ub, lb

        m.setObjective(obj @ x, GRB.MINIMIZE) #objective, modelsense

        m.setAttr(GRB.Attr.VType, "C")

        m.setParam(GRB.param.QCPDual, 1.0)
        m.setParam(GRB.param.OutputFlag, 0)

######
        # Optimize model
        m.optimize()


        return "something"


#####################
## from MATLAB

function [OpCost, consumption, power_buying, error, dual_var]=p2p_buyer_v1(info, phi_0, GUP, fix_E, rho, nodal_p)
    ##%% extracting seller and buyer data
    Pmax=info(:,2); Pmin=info(:,3);
    quad_cost=info(:,4); lin_cost=info(:,5);

    Ns=length(phi_0); # sellers
    # forming the optimization problem
    num_var=1+Ns; # consumption+trading_amounts

    cons=sparse(1,1:num_var,[1, -ones(1,Ns)],1,num_var,num_var);
    # cons=[1, -ones(1,Ns)];
    rhs_cons=0;

    Vt=strcat(repmat('C',1,num_var));
    It=strcat('=');


    # calling 'gurobi'

    model.Q=sparse(1:num_var, 1:num_var, [-quad_cost; 0.5*rho++0.25*ones(Ns,1)]', num_var, num_var, num_var);
    model.obj=[-lin_cost+nodal_p; phi_0+0.5*GUP-rho.*fix_E];
    model.A=cons; model.rhs=rhs_cons; model.sense=It';
    model.ub=[Pmax; inf(Ns,1)]; 
    model.lb=[Pmin; zeros(Ns,1)]; model.vtype=Vt;
    model.modelsense='min';
    
    params.QCPDual=1;
    params.OutputFlag=0;

    result=gurobi(model,params);

    #parse results

    consumption=result.x(1);
    power_buying=result.x(1+(1:Ns));
    error=power_buying-fix_E;

    OpCost=result.objval +fix_E'*sparse(1:Ns,1:Ns,0.5*rho,Ns,Ns,Ns)*fix_E;
    dual_var=result.pi;


end