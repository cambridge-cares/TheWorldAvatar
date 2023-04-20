## Prerequisites (add the following lines to requirements.txt, or alternatively 'pip install'):
## https://download.pytorch.org/whl/cpu/torch-0.4.1-cp36-cp36m-win_amd64.whl; sys_platform == 'win32'
## https://download.pytorch.org/whl/cpu/torch-0.4.1-cp36-cp36m-linux_x86_64.whl; sys_platform == 'linux'
## git+https://github.com/cornellius-gp/gpytorch@alpha
## NB For other versions of torch, check https://pytorch.org/get-started/locally/.
## The alpha revision of gpytorch has been tested to work with torch v0.4.1. It also
## works with v1.0.x, however with deprecation warnings.
## The script below works out of the box only with the alpha revision of gpytorch.
## Several changes would be required in order to make it work with more recent versions.

## Note: There is a strong preference for single precision in the machine-learning community.
## One reason is that for neural networks, 32-bit is more than precise enough.
## Another strong reason is that GPUs are optimised for 32-bit and perform relatively poorly in 64-bit.
## For these reasons, trying to use g-/py-/torch in double precision is not recommended. Did not manage to get it to work anyway.
## NB The torch tensor type appears to default to float (32-bit), rather than double (64-bit). Nonetheless, types are explicitly declared here as float32, to avoid misunderstandings.

import math
import torch
from torch import nn, optim
from torch.autograd import Variable
import gpytorch
from gpytorch.means import ConstantMean
from gpytorch.likelihoods import GaussianLikelihood
from gpytorch.random_variables import GaussianRandomVariable
#from gpytorch.distributions import MultivariateNormal as GaussianRandomVariable #for latest gpytorch?
from gpytorch.priors import SmoothedBoxPrior

# scheduler for learning rate
from torch.optim.lr_scheduler import MultiStepLR

import numpy
import pickle

class LargeFeatureExtractor(nn.Sequential):
    def __init__(self, adim, aarch):
        super(LargeFeatureExtractor, self).__init__()
        # Linear transformation: https://pytorch.org/docs/stable/nn.html#linear
        # Rectified linear unit function: https://pytorch.org/docs/stable/nn.html#relu
        NumOutStrs = aarch.split(',')
        NumIn = adim
        i = 0
        for NumOutStr in NumOutStrs:
            i += 1
            NumOut = int(NumOutStr)
            #print('Linear ',i,': (',NumIn,', ',NumOut,')',sep='')
            self.add_module('linear'+str(i), nn.Linear(NumIn, NumOut))
            if (i<len(NumOutStrs)):
                self.add_module('relu'+str(i), nn.ReLU())
            NumIn = NumOut

class DNN_GP(gpytorch.models.ExactGP):
    def __init__(self, asize, adim, ane, alr, aarch, akernel, awnoscale):
        tl = GaussianLikelihood()#.cpu()#.cuda()
        ## NB Work-around with zeros is necessary here as we can't pass None due to sloppy programming in ExactGP!
        super(DNN_GP, self).__init__(
            torch.zeros([asize, adim], dtype=torch.float32),
            torch.zeros([asize], dtype=torch.float32), tl)
        self.m_likelihood = tl
        # See https://media.readthedocs.org/pdf/gpytorch/latest/gpytorch.pdf.
        self.mean_module = ConstantMean()#prior=SmoothedBoxPrior(-1e1, 1e+1))
        if (akernel=='RBF'):
            self.base_covar_module = gpytorch.kernels.RBFKernel()
        elif (akernel=='MaternOneHalf'):
            self.base_covar_module = gpytorch.kernels.MaternKernel(nu=0.5)
        elif (akernel=='MaternThreeHalves'):
            self.base_covar_module = gpytorch.kernels.MaternKernel(nu=1.5)
        elif (akernel=='MaternFiveHalves'):
            self.base_covar_module = gpytorch.kernels.MaternKernel(nu=2.5)
        else:
            print('WARNING: No valid kernel selected! Using RBF.')
            self.base_covar_module = gpytorch.kernels.RBFKernel()
        #RBFKernel(log_lengthscale_prior=SmoothedBoxPrior(exp(-5), exp(6), sigma=0.1, log_transform=True))
        #SpectralMixtureKernel(n_mixtures=1)
        #SpectralMixtureKernel(n_mixtures=6,n_dims=3)
        #SpectralMixtureKernel(n_mixtures=4)
        #RBFKernel(log_lengthscale_prior=SmoothedBoxPrior(exp(-5), exp(5), sigma=0.1, log_transform=True))
        #self.grid_covar_module = GridInterpolationKernel(self.base_covar_module, grid_size=50,
        #                                            grid_bounds=[(-1.1, 1.1), (-1.1, 1.1),(-1.1, 1.1)])
        if (awnoscale>0.0):
            self.covar_module = gpytorch.kernels.ScaleKernel(self.base_covar_module+
                gpytorch.kernels.WhiteNoiseKernel(variances=torch.ones(asize, dtype=torch.float32)*awnoscale))
        else:
            self.covar_module = gpytorch.kernels.ScaleKernel(self.base_covar_module)
        #self.register_parameter('log_outputscale', nn.Parameter(torch.tensor([0], dtype=torch.float32)))
        self.feature_extractor = LargeFeatureExtractor(adim, aarch)#.cpu()#.cuda()
        self.m_numepochs = ane
        self.m_lr = alr

    def forward(self, x):
        projected_x = self.feature_extractor(x)
        projected_x = projected_x - projected_x.min(0)[0]
        projected_x = 2 * (projected_x / projected_x.max(0)[0]) - 1
        mean_x = self.mean_module(projected_x)
        covar_x = self.covar_module(projected_x)
        #covar_x = covar_x.mul(self.log_outputscale.exp())
        return GaussianRandomVariable(mean_x, covar_x)

# Function to create a surrogate instance. Will be called by MoDS.
def construct_surrogate(the_size, the_dim, **kwargs):
    # Fix the random seed for the model (!). NB The construction depends on the random seed, but neither training nor evaluation do!
    torch.manual_seed(1)
    #print('python: Keyword arguments:')
    #print(kwargs)
    # These keyword arguments need to be provided as details of
    # the surrogate algorithm, with "python_" prefixed.
    # "arch" - Neural network architecture, a comma-separated list of numbers of neurons:
    try:
        the_arch = kwargs['arch']
    except KeyError as err:
        print("python: Fatal error: Detail '",err.args[0],"' not found!")
        raise err
    # "kernel" - Flag to select GP kernel:
    try:
        the_kernel = kwargs['kernel']
    except KeyError as err:
        print("python: Fatal error: Detail '",err.args[0],"' not found!")
        raise err
    # "wnoscale" - Scale for 'white noise ones', i.e. white noise with uniform variance:
    try:
        the_wnoscale = max(float(kwargs['wnoscale']),0.0)
    except KeyError as err:
        the_wnoscale = 0.0
    # "numepochs" - Number of epochs, i.e. training iterations, i.e. optimiser steps:
    try:
        the_ne = int(kwargs['numepochs'])
    except KeyError as err:
        print("python: Fatal error: Detail '",err.args[0],"' not found!")
        raise err
    # "lr" - Learning rate:
    try:
        the_lr = float(kwargs['lr'])
    except KeyError as err:
        print("python: Fatal error: Detail '",err.args[0],"' not found!")
        raise err
    return DNN_GP(the_size, the_dim, the_ne, the_lr, the_arch, the_kernel, the_wnoscale)#.cpu()#.cuda()

# Function to fit a surrogate instance to data. Will be called by MoDS.
def fit_surrogate(the_surr,data_x,data_y):
    #print('python: xs: (for fitting)')
    #print(type(data_x))
    #print(data_x.shape)
    #print(data_x)
    #print('python: ys: (for fitting)')
    #print(type(data_y))
    #print(data_y.shape)
    #print(data_y)
    data_x_tensor = torch.tensor(data_x, dtype=torch.float32)
    data_y_tensor = torch.tensor(data_y, dtype=torch.float32)
    the_surr.set_train_data(inputs=data_x_tensor, targets=data_y_tensor, strict=True)
    #set model into training mode
    the_surr.train()
    the_surr.m_likelihood.train()
    # Use the Adam optimizer (alternatively SGD?)
    n_epochs = the_surr.m_numepochs #1000
    lr = the_surr.m_lr #0.001 #0.00001 #0.1
    optimizer = torch.optim.Adam([
        #{'params': the_surr.parameters()},  # Includes likelihood.parameters (?)
        # Should the following include the log_outputscale?
        # To do: Check the list in detail and test what should be included and what shouldn't be.
        {'params': the_surr.feature_extractor.parameters()},
        {'params': the_surr.covar_module.parameters()},
        {'params': the_surr.mean_module.parameters()},
        {'params': the_surr.m_likelihood.parameters()},
    ], lr=lr)
  #  scheduler = MultiStepLR(optimizer, milestones=[0.2 * n_epochs, 0.4 * n_epochs, 0.6 * n_epochs, 0.75 * n_epochs, 0.85 * n_epochs], gamma=0.1)
    # "Loss" for GPs - the marginal log likelihood
    mll = gpytorch.mlls.ExactMarginalLogLikelihood(the_surr.m_likelihood, the_surr)
    # See dkl_mnist.ipynb for explanation of this flag
    with gpytorch.settings.use_toeplitz(False):
        for i in range(n_epochs):
            # Zero backprop gradients
  #          scheduler.step()
            optimizer.zero_grad()
            # Get output from model
            output = the_surr(data_x_tensor)
            # Calc loss and backprop derivatives
            loss = -mll(output, data_y_tensor)
            loss.backward()
            print('Iteration %d/%d: Loss= %.3f' % (i + 1, n_epochs, loss.data[0])) #loss.data.item() #for latest gpytorch?
            optimizer.step()
    return

# Function to evaluate a surrogate instance. Will be called by MoDS.
def evaluate_surrogate(the_surr,the_xs):
    #print('python: xs: (for evaluation)')
    #print(the_xs)
    #print(type(the_xs))
    #print(the_xs.shape)
    # Set model to evaluate mode
    the_surr.eval()
    the_surr.m_likelihood.eval()
    with torch.no_grad(), gpytorch.settings.use_toeplitz(False), gpytorch.settings.debug(False):
        y_pred = the_surr(torch.tensor(the_xs, dtype=torch.float32))
    #print('python: y: (result of evaluation)')
    #print(y_pred)
    return y_pred.mean().data.numpy().astype(numpy.float64)

# Function to batch-evaluate a surrogate instance. Will be called by MoDS.
def batch_evaluate_surrogate(the_surr,the_xs):
    return evaluate_surrogate(the_surr,the_xs)

# Function to serialise surrogate instance to a bytes object. Will be called by MoDS.
def serialise_surrogate(the_surr):
    the_bytes = pickle.dumps(the_surr)
    #with open('DNN_GP.bin', 'bw') as f:
    #    pickle.dump(the_surr, f)
    return the_bytes

# Function to deserialise surrogate instance from a bytes object. Will be called by MoDS.
def deserialise_surrogate(the_bytes):
    the_surr = pickle.loads(the_bytes)
    #with open('DNN_GP.bin', 'br') as f:
    #    the_surr = pickle.load(f)
    return the_surr
