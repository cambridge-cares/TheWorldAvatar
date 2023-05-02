# NOTE This file comes from Kobi Felton (kcmf2@cam.ac.uk)

from botorch.acquisition import ExpectedImprovement as EI
from summit import *
from summit.strategies.base import Strategy, Transform
from botorch.acquisition import ExpectedImprovement as EI
from botorch.acquisition import qNoisyExpectedImprovement as qNEI
from botorch.acquisition import UpperConfidenceBound as UCB
from botorch.models import SingleTaskGP, MixedSingleTaskGP
from botorch.fit import fit_gpytorch_model
from botorch.optim import optimize_acqf, optimize_acqf_mixed
from gpytorch.mlls.exact_marginal_log_likelihood import (
    ExactMarginalLogLikelihood,
)
import numpy as np
import torch
import pandas as pd

dtype = torch.double


class CategoricalEI(EI):
    def __init__(
        self,
        domain: Domain,
        model,
        best_f,
        objective=None,
        maximize: bool = True,
        **kwargs,
    ) -> None:
        super().__init__(
            model=model, best_f=best_f, objective=objective, maximize=maximize, **kwargs
        )
        self._domain = domain

    def forward(self, X):
        X = self.round_to_one_hot(X, self._domain)
        return super().forward(X)

    @staticmethod
    def round_to_one_hot(X, domain: Domain):
        """Round all categorical variables to a one-hot encoding"""
        num_experiments = X.shape[1]
        X = X.clone()
        for q in range(num_experiments):
            c = 0
            for v in domain.input_variables:
                if isinstance(v, CategoricalVariable):
                    n_levels = len(v.levels)
                    levels_selected = X[:, q, c : c + n_levels].argmax(axis=1)
                    X[:, q, c : c + n_levels] = 0
                    for j, l in zip(range(X.shape[0]), levels_selected):
                        X[j, q, int(c + l)] = 1

                    check = int(X[:, q, c : c + n_levels].sum()) == X.shape[0]
                    if not check:
                        raise ValueError(
                            (
                                f"Rounding to a one-hot encoding is not properly working. Please report this bug at "
                                f"https://github.com/sustainable-processes/summit/issues. Tensor: \n {X[:, :, c : c + n_levels]}"
                            )
                        )
                    c += n_levels
                else:
                    c += 1
        return X


class NewSTBO(Strategy):
    """Bayesian Optimisation using BOtorch


    Parameters
    ----------

    domain : :class:`~summit.domain.Domain`
        The domain of the optimization
    transform : :class:`~summit.strategies.base.Transform`, optional
        A transform object. By default no transformation will be done
        on the input variables or objectives.
    categorical_method : str, optional
        The method for transforming categorical variables. Either
        "one-hot" or "descriptors". Descriptors must be included in the
        categorical variables for the later.

    Examples
    --------

    >>> from summit.domain import Domain, ContinuousVariable
    >>> from summit.strategies import NelderMead
    >>> domain = Domain()
    >>> domain += ContinuousVariable(name='temperature', description='reaction temperature in celsius', bounds=[0, 1])
    >>> domain += ContinuousVariable(name='flowrate_a', description='flow of reactant a in mL/min', bounds=[0, 1])
    >>> domain += ContinuousVariable(name="yld", description='relative conversion to xyz', bounds=[0,100], is_objective=True, maximize=True)
    >>> strategy = STBO(domain)
    >>> next_experiments  = strategy.suggest_experiments()
    >>> print(next_experiments)
    NAME temperature flowrate_a             strategy
    TYPE        DATA       DATA             METADATA
    0          0.500      0.500  Nelder-Mead Simplex
    1          0.625      0.500  Nelder-Mead Simplex
    2          0.500      0.625  Nelder-Mead Simplex

    """

    def __init__(
        self,
        domain: Domain,
        transform: Transform = None,
        categorical_method: str = "one-hot",
        acquisition_function: str = "EI",
        **kwargs,
    ):
        Strategy.__init__(self, domain, transform, **kwargs)
        if len(self.domain.output_variables) > 1:
            raise DomainError("STBO only works with single objective problems")
        self.categorical_method = categorical_method
        if self.categorical_method not in ["one-hot", "descriptors", None]:
            raise ValueError(
                "categorical_method must be one of 'one-hot' or 'descriptors'."
            )
        self.brute_force_categorical = kwargs.get("brute_force_categorical", False)
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.acquistion_function = acquisition_function
        self.reset()

    def suggest_experiments(self, num_experiments, prev_res: DataSet = None, **kwargs):
        # Suggest lhs initial design or append new experiments to previous experiments
        if prev_res is None:
            lhs = LHS(self.domain)
            self.iterations += 1
            k = num_experiments if num_experiments > 1 else 2
            conditions = lhs.suggest_experiments(k)
            return conditions
        elif prev_res is not None and self.all_experiments is None:
            self.all_experiments = prev_res
        elif prev_res is not None and self.all_experiments is not None:
            self.all_experiments = pd.concat([self.all_experiments, prev_res], axis=0)
        self.iterations += 1
        data = self.all_experiments

        # Get inputs (decision variables) and outputs (objectives)
        inputs, output = self.transform.transform_inputs_outputs(
            data,
            categorical_method=self.categorical_method,
            # standardize_inputs=True,
            min_max_scale_inputs=True,
            standardize_outputs=True,
        )

        # Make it always a maximization problem
        objective = self.domain.output_variables[0]
        if not objective.maximize:
            output = -1.0 * output
        fbest_scaled = output[objective.name].max()

        # Set up model
        if self.categorical_method is None:
            cat_mappings = {}
            cat_dimensions = []
            for i, v in enumerate(self.domain.input_variables):
                if v.variable_type == "categorical":
                    cat_mapping = {l: i for i, l in enumerate(v.levels)}
                    inputs[v.name] = inputs[v.name].replace(cat_mapping)
                    cat_mappings[v.name] = cat_mapping
                    cat_dimensions.append(i)

            self.model = MixedSingleTaskGP(
                torch.tensor(
                    inputs.data_to_numpy().astype(float),
                    device=self.device,
                    dtype=dtype,
                ),
                torch.tensor(
                    output.data_to_numpy().astype(float),
                    device=self.device,
                    dtype=dtype,
                ),
                cat_dims=cat_dimensions,
            )
        else:
            self.model = SingleTaskGP(
                torch.tensor(
                    inputs.data_to_numpy().astype(float),
                    device=self.device,
                    dtype=dtype,
                ),
                torch.tensor(
                    output.data_to_numpy().astype(float),
                    device=self.device,
                    dtype=dtype,
                ),
            )

        # Train model
        self.mll = ExactMarginalLogLikelihood(self.model.likelihood, self.model)
        fit_gpytorch_model(self.mll, max_retries=20)

        # Optimize acquisition function
        if self.brute_force_categorical:
            if self.acquistion_function == "EI":
                self.acq = EI(self.model, best_f=fbest_scaled.round(5), maximize=True)
            elif self.acquistion_function == "qNEI":
                self.acq = qNEI(
                    self.model,
                    X_baseline=torch.tensor(
                        inputs.data_to_numpy().astype(float),
                        device=self.device,
                        dtype=dtype,
                    ),
                )
            elif self.acquistion_function == "UCB":
                self.acq = UCB(self.model, beta=5.0)
            else:
                raise ValueError(
                    f"{self.acquistion_function} not a valid acquisition function"
                )
            if self.categorical_method is None:
                combos = self.domain.get_categorical_combinations()
                fixed_features_list = []
                for v in self.domain.input_variables:
                    if v.variable_type == "categorical":
                        combos[v.name] = combos[v.name].replace(cat_mappings[v.name])
                fixed_features_list = []
                for k, combo in combos.iterrows():
                    fixed_features_list.append(
                        {dim: combo[i] for i, dim in enumerate(cat_dimensions)}
                    )
            else:
                fixed_features_list = self._get_fixed_features()
            results, _ = optimize_acqf_mixed(
                acq_function=self.acq,
                bounds=self._get_bounds(),
                num_restarts=kwargs.get("num_restarts", 100),
                fixed_features_list=fixed_features_list,
                q=num_experiments,
                raw_samples=kwargs.get("raw_samples", 100),
            )
        else:
            if self.acquistion_function == "EI":
                self.acq = CategoricalEI(
                    self.domain, self.model, best_f=fbest_scaled, maximize=True
                )
            elif self.acquistion_function == "qNEI":
                self.acq = CategoricalqNEI(
                    self.domain,
                    self.model,
                    X_baseline=torch.tensor(
                        inputs.data_to_numpy().astype(float),
                        device=self.device,
                        dtype=dtype,
                    ),
                )
            else:
                raise ValueError(
                    f"{self.acquistion_function} not a valid acquisition function"
                )
            results, _ = optimize_acqf(
                acq_function=self.acq,
                bounds=self._get_bounds(),
                num_restarts=kwargs.get("num_restarts", 100),
                q=num_experiments,
                raw_samples=kwargs.get("raw_samples", 2000),
            )

        # Convert result to datset
        result = DataSet(
            results.cpu().detach().numpy(),
            columns=inputs.data_columns,
        )

        # Untransform
        if self.categorical_method is None:
            for i, v in enumerate(self.domain.input_variables):
                if v.variable_type == "categorical":
                    cat_mapping = {i: l for i, l in enumerate(v.levels)}
                    result[v.name] = result[v.name].replace(cat_mapping)

        result = self.transform.un_transform(
            result,
            categorical_method=self.categorical_method,
            min_max_scale_inputs=True,
            standardized_outputs=True,
        )

        # Add metadata
        result[("strategy", "METADATA")] = "STBO"
        return result

    def _get_fixed_features(self):
        combos = self.domain.get_categorical_combinations()
        encoded_combos = {
            v.name: self.transform.encoders[v.name].transform(combos[[v.name]])
            for v in self.domain.input_variables
            if v.variable_type == "categorical"
        }
        fixed_features_list = []
        for i in range(len(combos)):
            fixed_features = {}
            k = 0
            for v in self.domain.input_variables:
                # One-hot encoding
                if v.variable_type == "categorical":
                    for j in range(encoded_combos[v.name].shape[1]):
                        fixed_features[k] = float(encoded_combos[v.name][i, j])
                        k += 1
                else:
                    k += 1
            fixed_features_list.append(fixed_features)
        return fixed_features_list

    def _get_bounds(self):
        bounds = []
        for v in self.domain.input_variables:
            if isinstance(v, ContinuousVariable):
                var_min, var_max = v.bounds[0], v.bounds[1]
                # mean = self.transform.input_means[v.name]
                # std = self.transform.input_stds[v.name]
                v_bounds = np.array(v.bounds)
                # v_bounds = (v_bounds - mean) / std
                v_bounds = (v_bounds - var_min) / (var_max - var_min)
                bounds.append(v_bounds)
            elif (
                isinstance(v, CategoricalVariable)
                and self.categorical_method == "one-hot"
            ):
                bounds += [[0, 1] for _ in v.levels]
            elif isinstance(v, CategoricalVariable) and self.categorical_method is None:
                bounds.append([0, len(v.levels)])
        return torch.tensor(bounds, dtype=dtype, device=self.device).T

    def reset(self):
        """Reset MTBO state"""
        self.all_experiments = None
        self.iterations = 0
        self.fbest = (
            float("inf") if self.domain.output_variables[0].maximize else -float("inf")
        )