import argparse

import numpy as np
import pandas as pd
from os import path
import seaborn as sns
from scipy import stats
import matplotlib.pyplot as plt


def contour_plot(figure_dir, hpo_result):
    df_hpo_result = pd.read_csv(hpo_result, index_col=[0])

    df_hpo_result = df_hpo_result.drop(columns=['number', 'datetime_start', 'datetime_complete', 'duration', 'state'])
    df_hpo_result = df_hpo_result.replace(np.nan, 0)

    sns_pair_plot = sns.pairplot(df_hpo_result)
    sns_pair_plot.savefig(figure_dir + '/hpo_params_pair_plot.svg')

    sns_pair_grid = sns.PairGrid(df_hpo_result)
    sns_pair_grid.map_diag(sns.kdeplot)
    sns_pair_grid.map_offdiag(sns.kdeplot, n_levels=6)
    sns_pair_grid.savefig(figure_dir + '/hpo_params_pair_grid.svg')


def init_plot():
    sns.set(font_scale=1)
    sns.set_style('whitegrid',
                  {'axes.grid': True,
                   'grid.linestyle': u'',
                   'axes.edgecolor': '0.1',
                   'axes.labelcolor': '0',
                   'axes.labelsize': 15,
                   'axes.titlesize': 15,
                   'legend.fontsize': 15,
                   'xtick.labelsize': 15,
                   'ytick.labelsize': 15,
                   })
    plt.rcParams["patch.force_edgecolor"] = True


def plot(file_path, df, col_1, col_2):
    g = sns.jointplot(x=col_1, y=col_2, data=df,
                      kind='reg', scatter_kws={'alpha': 0.5, 's': 20}, height=5)

    g.ax_marg_x.set_xlim(min([df[col_1].min(), df[col_2].min()]),
                         max([df[col_1].max(), df[col_2].max()]))
    g.ax_marg_y.set_ylim(min([df[col_1].min(), df[col_2].min()]),
                         max([df[col_1].max(), df[col_2].max()]))

    g.ax_joint.set_xlabel(f"{col_1} (%)")
    g.ax_joint.set_ylabel(f"{col_2} (%)")

    slope, intercept, r_value, p_value, std_err = stats.linregress(df[col_1], df[col_2])

    text = "r = {:0.2}".format(r_value)
    plt.annotate(text,
                 xy=(0.1, 0.95),
                 xycoords='axes fraction')
    plt.savefig(file_path)


def prediction_plot(figure_dir, train_pred, val_pred, test_pred, cols):
    init_plot()

    for i, (col_1, col_2) in enumerate(cols):
        if train_pred is not None:
            plot(figure_dir + f"train_reg_{i+1}.svg", train_pred, col_1, col_2)
        if val_pred is not None:
            plot(figure_dir + f"val_reg_{i+1}.svg", val_pred, col_1, col_2)
        if test_pred is not None:
            plot(figure_dir + f"test_reg_{i+1}.svg", test_pred, col_1, col_2)


def enter():
    parser = argparse.ArgumentParser()
    parser.add_argument('--contour', type=str, default=False)
    parser.add_argument('--regression', type=str, default=False)
    parser.add_argument('--figure_dir', type=str, default='.')
    parser.add_argument('--df1', type=str, default='.')
    parser.add_argument('--df2', type=str, default='.')
    parser.add_argument('--df3', type=str, default='.')
    args = parser.parse_args()

    if args.contour:
        contour_plot(args.figure_dir, args.df1)
        # example:
        # python ./py4ml/visualization/util_sns_plot.py --contour True --figure_dir ./logs/hpo_20201204_135305/ --df1 ./logs/hpo_20201204_135305/hpo_result.csv
    if args.regression:
        prediction_plot(args.figure_dir, args.df1, args.df2, args.df3)
        # example:
        # python ./py4ml/visualization/util_sns_plot.py --regression True
        # --figure_dir ./logs/hpo_20201204_135305/best_trial_retrain/trial_18/
        # --df1 ./logs/hpo_20201204_135305/best_trial_retrain/trial_18/predictions_training_set.csv
        # --df1 ./logs/hpo_20201204_135305/best_trial_retrain/trial_18/predictions_validation_set.csv
        # --df1 ./logs/hpo_20201204_135305/best_trial_retrain/trial_18/predictions_test_set.csv

if __name__ == '__main__':
    enter()