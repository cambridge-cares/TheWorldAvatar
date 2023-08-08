"""
   Wrapper to allow MoDS to run various utility functions from the pymoo library.
"""

import numpy as np
from pymoo.util.nds.non_dominated_sorting import NonDominatedSorting

#--------------------------------------------------------------------------------------------------
def identify_pareto_pts(pts, **kwargs):
    """
     Given a numpy array of dtype=float and shape [Npts,NObj], return the indices of Pareto points
     in a 1D numpy array of dtype=int
    """
    # process kwargs
    nds_method = kwargs.pop('method','efficient_non_dominated_sort')

    # Obtain indices of Pareto optima
    pf_indices = NonDominatedSorting(method=nds_method).do(pts, only_non_dominated_front=True)

    # Return Pareto indices
    return pf_indices
#--------------------------------------------------------------------------------------------------

# Do nothing if run directly
if __name__ == "__main__":
    pass