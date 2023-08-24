import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
import sys
import json

#from caresjpsutil import PythonLogger

AVOIDANCE_COST_CSV = 'data/input/CO2_avoidance_cost_{}_carbon.csv'
AVOIDANCE_COST_PNG = '/images/{}_carbon_avoidance_cost.png'

if __name__ == "__main__":
    #pythonLogger = PythonLogger('AvoidanceCostPlotter.py')
    #pythonLogger.postInfoToLogServer('start of AvoidanceCostPlotter.py')

    try:
        rootPath = json.loads(sys.argv[1])
        pathsDict = {}

        x = 0
        for carbonPrice in [0, 10, 20, 50]:
            # ### get Xs and Ys

            # load the powerplant database

            df = pd.read_csv(rootPath + AVOIDANCE_COST_CSV.format(carbonPrice),
                              header='infer', sep=',')

            if carbonPrice == 0:
                x = df.loc[:, ('year')].values
            elif carbonPrice == 20 or carbonPrice == 50:
                df = df.clip(lower=0)

            y_pc_l = df.loc[:, ('PC_avoid_cost_low')].values
            y_pc_m = df.loc[:, ('PC_avoid_cost_middle')].values
            y_pc_h = df.loc[:, ('PC_avoid_cost_high')].values

            y_ngcc_l = df.loc[:, ('NGCC_avoid_cost_low')].values
            y_ngcc_m = df.loc[:, ('NGCC_avoid_cost_middle')].values
            y_ngcc_h = df.loc[:, ('NGCC_avoid_cost_high')].values


            # ### plot

            sns.set_style("white")
            sns.set_context("paper", font_scale=1)

            plt.clf()
            figure, ax = plt.subplots(1, 1, figsize=(3, 2.5))

            # plot the PC cost

            ax.plot(x, y_pc_m, '-', color='#CC4F1B', label='PC', linewidth=1)
            ax.plot(x, y_pc_l, '--', color='#CC4F1B', linewidth=1)
            ax.plot(x, y_pc_h, '--', color='#CC4F1B', linewidth=1)

            # fill the region between y_pc_l and y_pc_h

            ax.fill_between(x, y_pc_h, y_pc_l,
                            alpha=0.5, edgecolor='#CC4F1B', facecolor='#FF9848')

            # plot the NGCC plant cost

            ax.plot(x, y_ngcc_m, '-', color='#1B2ACC', label='NGCC',linewidth=1)
            ax.plot(x, y_ngcc_l, '--', color='#1B2ACC', linewidth=1)
            ax.plot(x, y_ngcc_h, '--', color='#1B2ACC', linewidth=1)


            # fill the region between y_ngcc_l and y_ngcc_h
            plt.fill_between(x, y_ngcc_h, y_ngcc_l,
                             alpha=0.5, edgecolor='#1B2ACC', facecolor='#1B2ACC')

            ax.legend(loc='best', ncol=1)

            # set X and Y coordinates of text according to carbon price
            textXCoordinate = 2040 if carbonPrice == 0 else 2027
            textYCoordinate = 12 if carbonPrice == 0 else 42

            plt.text(textXCoordinate, textYCoordinate,
                     'Carbon price\n${}/ton'.format(carbonPrice),
                     bbox=dict(facecolor='grey', alpha=0.5), ha='center', va='top')

            ax.set(xlabel='Year', ylabel='CO2 avoidance cost ($/ton)')
            ax.set_xlim([2016, 2050])

            # set lower range of yticks according to carbon price
            ticksYLower = -5 if carbonPrice == 50 else 0

            xticks = [2016, 2020, 2030, 2040, 2050]
            yticks = np.arange(ticksYLower, 50, 5)

            ax.set_xticks(xticks)
            ax.set_yticks(yticks)

            pathsDict['carbon{}AvoidanceCost'.format(carbonPrice)] = AVOIDANCE_COST_PNG.format(carbonPrice)
            plt.savefig(rootPath + 'public' + AVOIDANCE_COST_PNG.format(carbonPrice),
                        bbox_inches='tight', dpi=800)

        print(json.dumps(pathsDict))
    except Exception as e:
        print(e)
        #pythonLogger.postInfoToLogServer('end of AvoidanceCostPlotter.py')