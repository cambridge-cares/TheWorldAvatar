# This file is an interface which makes Gaussian Processes as provided by the
# scikit-learn library available as surrogates for use in MoDS.

# Prerequisites: pip install {numpy,scipy,scikit-learn}
# or add {numpy==1.15.2,scipy==1.1.0,scikit-learn==0.20.0} to requirements.txt

## NB This does not work in debug (see https://github.com/numpy/numpy/issues/11508)!
import numpy as np

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import (RBF, Matern, RationalQuadratic,
    ExpSineSquared, DotProduct, ConstantKernel)
import pickle

# Function to create a surrogate instance. Will be called by MoDS.
def construct_surrogate(the_size, the_dim, **kwargs):
    #print('python: Keyword arguments:')
    #print(kwargs)
    W = "RBF"
    noise_amplitude = 0.1
    iterations = 100
    if W == "RBF":
        kernel = ConstantKernel(1.0, (1e-3, 1e3)) * RBF(10, (1e-2, 1e2))
    elif  W == "Matern12":
        kernel = 1.0 * Matern(length_scale=1.0, length_scale_bounds=(1e-2, 1e2),nu=0.5)
    elif  W == "Matern32":
        kernel = 1.0 * Matern(length_scale=1.0, length_scale_bounds=(1e-2, 1e2),nu=1.5)
    elif  W == "Matern52":
        kernel = 1.0 * Matern(length_scale=1.0, length_scale_bounds=(1e-2, 1e2),nu=2.5)
    elif  W == "RationalQuadratic":
        kernel = 1.0 * RationalQuadratic(length_scale=1.0, alpha=0.1)
    elif  W == "ExpSineSquared":
        kernel = 1.0 * ExpSineSquared(length_scale=1.0, periodicity=3.0,
                       length_scale_bounds=(0.1, 10.0), periodicity_bounds=(1.0, 10.0))
    elif  W == "DotProduct":
        kernel = ConstantKernel(0.1, (0.01, 10.0))* (DotProduct(sigma_0=1.0, sigma_0_bounds=(0.0, 10.0)) ** 2)
    return GaussianProcessRegressor(kernel=kernel, alpha=noise_amplitude, n_restarts_optimizer=iterations)

# Function to fit a surrogate instance to data. Will be called by MoDS.
def fit_surrogate(the_surr,data_x,data_y):
    #print('python: xs: (for fitting)')
    #print(data_x)
    #print('python: ys: (for fitting)')
    #print(data_y)
    the_surr.fit(data_x,data_y)
    return

# Function to evaluate a surrogate instance. Will be called by MoDS.
def evaluate_surrogate(the_surr,the_xs):
    #print('python: xs: (for evaluation)')
    #print(type(the_xs))
    #print(the_xs.shape)
    #print(the_xs)
    y_pred=the_surr.predict(the_xs, return_std = False)
    #print('python: y: (result of evaluation)')
    #print(type(y_pred))
    #print(y_pred.shape)
    #print(y_pred)
    return y_pred

# Function to batch-evaluate a surrogate instance. Will be called by MoDS.
def batch_evaluate_surrogate(the_surr,the_xs):
    return evaluate_surrogate(the_surr,the_xs)

# Function to serialise surrogate instance to a bytes object. Will be called by MoDS.
def serialise_surrogate(the_surr):
    the_bytes = pickle.dumps(the_surr)
    return the_bytes

# Function to deserialise surrogate instance from a bytes object. Will be called by MoDS.
def deserialise_surrogate(the_bytes):
    the_surr = pickle.loads(the_bytes)
    return the_surr
