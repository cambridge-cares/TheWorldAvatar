'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''


This software package allows traversing the OntoMOPs knowledge graph, while returning information on:

    a) the different assembly models in the OntoMOPs KG and the associated MOP instances;

    b) information on constructable MOPs assinged as Search Radius 1 (R1) and Search Radius 2 (R2).

Within each assembly model (AM) there are two associated generic building units (GBUs).
Many chemcial building units (CBUs) act as a particular GBU.
Thus a particular GBU associated with a particular AM, represents a set of CBUs with certain spatial properties. 
GBUs from different assembly models may be unique or they may be also similar. 

Search Radius 1 assumes that the GBUs deriving from differnet assembly models are all different. Thus generation of new MOPs is based only on the available CBUs associated with the pair of GBUs. 

Search Radius 2 understands that GBUs of differnet assembly models may be similar and thus allows exchange of CBUs. The exchange of CBUs is only allowed if the two particular GBUs have at least one CBU already in common. 


Using the assembly model concept one can and studing complementary bindings of chemical building units, many MOPs can be derived, including such that are not found in the KG. 