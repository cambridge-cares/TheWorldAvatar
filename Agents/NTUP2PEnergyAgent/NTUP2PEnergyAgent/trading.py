import numpy as np
import logging

from .buyer import Buyer
from .seller import Seller

class Trading():
    
    def __init__(self, max_iter):
        self.max_iter = max_iter
        logging.info("Start trading")
    
    def trade(self,buyer_info,seller_info,nodal_price):
        
        #initialise some variables
        n_sellers=np.shape(seller_info)[0]
        n_buyers = np.shape(buyer_info)[0]

        init_cost=seller_info[:,(4,)]-nodal_price
    
        buyer_cost=np.zeros((n_buyers,1))
        seller_cost=np.zeros((n_sellers,1))
        self.consumption=np.zeros((n_buyers,1)) 
        self.production=np.zeros((n_sellers,1))
        buyer_P_mat=np.zeros((n_sellers,n_buyers)) 
        seller_P_mat=np.zeros((n_buyers,n_sellers))
        buyer_error=np.inf*np.ones((n_sellers,n_buyers))
        seller_error=np.inf*np.ones((n_buyers,n_sellers))
        seller_P_error_k=np.zeros((self.max_iter,1)) 
        max_E_error_k=np.zeros((self.max_iter,1))

        phi=np.zeros((n_buyers,n_sellers)) 
        PI=np.zeros((n_sellers,n_buyers)) 
        phi_0=np.tile(init_cost,(1,n_buyers)); 
        rho=0.5*np.ones((n_buyers,n_sellers)) 
        dual_var=np.zeros((n_buyers+n_sellers,1))
        nodal_price_s=nodal_price*np.ones((n_sellers,1)) 
        nodal_price_b=nodal_price*np.ones((n_buyers,1))

        buyer_P_k=[]
        seller_P_k=[]
        phi_k=[]


        k=0
        while k<self.max_iter:
                        
            fix_P=0.5*(buyer_P_mat + np.transpose(seller_P_mat))
            for i in range(0,n_buyers):
                buyer = Buyer()
                buyer.optimize(buyer_info[(i,),:], phi_0[:,(i,)], PI[:,(i,)], fix_P[:,(i,)], np.transpose(rho[(i,),:]), np.array(nodal_price_b[i], ndmin=2))
                buyer_cost[i]=buyer.op_cost
                self.consumption[i] = buyer.consumption
                buyer_P_mat[:,(i,)] = buyer.power_buying
                buyer_error[:,(i,)] = buyer.error
                dual_var[i] = buyer.dual_var
            
            fix_P=0.5*(buyer_P_mat + np.transpose(seller_P_mat))
            for j in range(0,n_sellers):
                seller = Seller()
                seller.optimize(seller_info[(j,),:], np.transpose(phi_0[(j,),:]), np.transpose(PI[(j,),:]), np.transpose(fix_P[(j,),:]), rho[:,(j,)],  np.array(nodal_price_s[j], ndmin=2))
                seller_cost[j] = seller.op_cost
                self.production[j] = seller.production
                phi[:,(j,)] = seller.phi
                seller_P_mat[:,(j,)] = seller.power_selling
                seller_error[:,(j,)] = seller.error
                dual_var[n_buyers+j] = seller.dual_var

            buyer_P_k.append(buyer_P_mat)
            seller_P_k.append(seller_P_mat)
            phi_0= np.transpose(phi)
            phi_k.append(np.transpose(phi))

            E_error=abs(buyer_P_mat-np.transpose(seller_P_mat))
            max_E_error=np.max(E_error)
            max_E_error_k[k]=max_E_error
            
            if k>0:
                seller_P_error_k=abs(seller_P_k[k]-seller_P_k[k-1])

            k=k+1

            if max_E_error< 10^(-5):
                break
        
        print("Trading complete")

        # return something useful
        return self.consumption
    
##########################################
##################### Test

#max_iter=100
#nodal_price = 20

#      bus max min a b c
#buyer_info = np.array([[4,	2.20000000000000,	0.0,	-0.100000000000000,	4.50000000000000],
#                       [7,	1.30000000000000,	0.0,	-0.200000000000000,	5.0],
#                       [11,	1.60000000000000,	0.0,	-0.0500000000000000,	5.0],
#                       [15,	1.70000000000000,	0.0,	-0.0500000000000000,	4.80000000000000],
#                       [20,	1.50000000000000,	0.0,	-0.0500000000000000,	4.0],
#                       [24,	2.50000000000000,	0.0,	-0.100000000000000,	5.0],
#                       [30,	2.40000000000000,	0.0,	-0.100000000000000,	5.0]])

#seller_info = np.array([[18,	1.60000000000000,	0.0,	0.0300000000000000,	3.20000000000000],
#                       [22,	2.30000000000000,	0.0,	0.0200000000000000,	4.0],
#                       [25,	2.90000000000000,	0.0,	0.0300000000000000,	3.0],
#                       [31,	2.50000000000000,	0.0,	0.0400000000000000,	4.50000000000000],
#                       [2,	2.50000000000000,	0.0,	0.0500000000000000,	3.20000000000000],
#                       [6,	3.50000000000000,	0.0,	0.0600000000000000,	3.80000000000000]])

#trading = Trading(max_iter)
#result = trading.trade(buyer_info,seller_info,nodal_price)
#print(result)
