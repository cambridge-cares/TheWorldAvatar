import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
import sys
import json

#from caresjpsutil import PythonLogger

PRICE_ANALYSIS_CSV = 'data/input/price_analysis_{}_carbon.csv'
CARBON_PRICE_PNG = '/images/{}_carbon_price.png'

if __name__ == "__main__":
    #pythonLogger = PythonLogger('GenerationCostPlotter.py')
    #pythonLogger.postInfoToLogServer('start of GenerationCostPlotter.py')

    try:
        rootPath = json.loads(sys.argv[1])
        pathsDict = {}

        x = 0
        for carbonPrice in [0, 10, 20, 50]:
            df = pd.read_csv(rootPath + PRICE_ANALYSIS_CSV.format(carbonPrice),
                             header='infer', sep=',')
            if carbonPrice == 0:
                x = df.loc[:, ('year')].values

            y_base = df.loc[:, ('reference_cost')].values
            y_pc = df.loc[:, ('PC_CCS')].values
            y_ngcc = df.loc[:, ('NGCC_CCS')].values


            y_pc_l = df.loc[:, ('PC_CCS_low')].values
            y_pc_m = df.loc[:, ('PC_CCS_mid')].values
            y_pc_h = df.loc[:, ('PC_CCS_high')].values


            y_ngcc_l = df.loc[:, ('NGCC_CCS_low')].values
            y_ngcc_m = df.loc[:, ('NGCC_CCS_mid')].values
            y_ngcc_h = df.loc[:, ('NGCC_CCS_high')].values

            sns.set_style("white")
            sns.set_context("paper", font_scale=1)

            plt.clf()

            figure, ax = plt.subplots(1, 1, figsize=(3, 2.5))

            # plot the reference cost
            ax.plot(x, y_base, '-k', label='Baseline', linewidth=1.5)

            # plot the PC cost
            ax.plot(x, y_pc_m, '-', color='#CC4F1B', label='PC+CCS', linewidth=1)
            ax.plot(x, y_pc_l, '--', color='#CC4F1B', linewidth=1)
            ax.plot(x, y_pc_h, '--', color='#CC4F1B', linewidth=1)

            # fill the region between y_pc_l and y_pc_h
            ax.fill_between(x, y_pc_h, y_pc_l, alpha=0.5, edgecolor='#CC4F1B', facecolor='#FF9848')

            # plot the NGCC plant cost
            ax.plot(x, y_ngcc_m, '-', color='#1B2ACC', label='NGCC+CCS', linewidth=1)
            ax.plot(x, y_ngcc_l, '--', color='#1B2ACC', linewidth=1)
            ax.plot(x, y_ngcc_h, '--', color='#1B2ACC', linewidth=1)

            # fill the region between y_ngcc_l and y_ngcc_h
            plt.fill_between(x, y_ngcc_h, y_ngcc_l, alpha=0.5, edgecolor='#1B2ACC', facecolor='#1B2ACC')

            ax.legend(loc='best', ncol=1)

            plt.text(2027,
                     115,
                     'Carbon price\n${}/ton'.format(carbonPrice),
                     bbox=dict(facecolor='grey', alpha=0.5),
                     ha='center',
                     va='top')

            ax.set(xlabel='Year',
                   ylabel='Power plant generation cost ($/MWh)')
            ax.set_xlim([2016, 2050])

            xticks = [2016, 2020, 2030, 2040, 2050]
            yticks = np.arange(50, 130, 10)

            ax.set_xticks(xticks)
            ax.set_yticks(yticks)

            pathsDict['carbon{}Price'.format(carbonPrice)] = CARBON_PRICE_PNG.format(carbonPrice)

            plt.savefig(rootPath + 'public' + CARBON_PRICE_PNG.format(carbonPrice),
                        bbox_inches='tight',
                        dpi=800)

        print(json.dumps(pathsDict))
    except Exception as e:
        print(e)
        #pythonLogger.postInfoToLogServer('end of GenerationCostPlotter.py')
