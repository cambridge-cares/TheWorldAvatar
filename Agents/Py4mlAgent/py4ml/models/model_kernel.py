import sklearn
import sklearn.svm
import sklearn.ensemble


class SVRWrapper(sklearn.svm.SVR):

    def __init__(self,
                 # params for class SVR
                 kernel='rbf', degree=3, gamma='scale', coef0=0.0,
                 tol=1e-3, C=1.0, epsilon=0.1,
                 shrinking=True, cache_size=200, verbose=False, max_iter=-1,
                 # additional kernel params
                 gamma_structural=0.0, gamma_physical=0.0, alpha=None):

        self.kernel = kernel
        self.gamma_structural = gamma_structural
        self.gamma_physical = gamma_physical
        self.alpha = alpha
        self.gamma = gamma
        self.kernel_params = {'kernel': kernel, 'gamma_structural': gamma_structural,
                'gamma_physical': gamma_physical, 'alpha': alpha, 'gamma': gamma}

        svr_params = {'kernel':kernel, 'degree':degree, 'gamma':gamma, 'coef0':coef0, 'tol':tol,
                    'C':C, 'epsilon':epsilon, 'shrinking':shrinking, 'cache_size':cache_size,
                    'verbose':verbose, 'max_iter':max_iter}

        kwargs = self.update_kernel_func(svr_params, self.kernel_params)
        super().__init__(**kwargs)

    def update_kernel_func(self, svr_params, kernel_params):
        # this function can be used to define custom kernel func.
        # at the moment it simply falls into default.
        return svr_params


    def fit(self, X, y, sample_weight=None):
        #log('fitting SVR with params=', self.get_params())
        return super().fit(X, y, sample_weight)

    def get_params(self, deep=False):
        params = super().get_params(deep).copy()
        params.update(self.kernel_params)
        return params

    def set_params(self, **params):

        p = params.copy()
        if 'kernel' in p:
            v = p['kernel']
            self.kernel = v
        if 'gamma_structural' in p:
            v = p.pop('gamma_structural')
            self.gamma_structural = v
        if 'gamma_physical' in p:
            v = p.pop('gamma_physical')
            self.gamma_physical = v
        if 'alpha' in p:
            v = p.pop('alpha')
            self.alpha = v
        if 'gamma' in p:
            v = p['gamma']
            self.gamma = v

        kernel_params = {}
        kernel_params['kernel'] = self.kernel
        kernel_params['gamma_structural'] = self.gamma_structural
        kernel_params['gamma_physical'] = self.gamma_physical
        kernel_params['alpha'] = self.alpha
        kernel_params['gamma'] = self.gamma
        self.kernel_params = kernel_params

        params = self.update_kernel_func(p, self.kernel_params)
        return super().set_params(**params)