import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
import sys
import json

#from caresjpsutil import PythonLogger

CARBON_ANNUAL_PNG = "/images/{}_carbon_annual.png"
CARBON_LOCK_PNG = "/images/{}_carbon_lock.png"

if __name__ == "__main__":
    #pythonLogger = PythonLogger('CommittedEmissionPlotter.py')
    #pythonLogger.postInfoToLogServer('start of CommittedEmissionPlotter.py')

    try:
        rootPath = json.loads(sys.argv[1])
        # carbonPrice = float(json.loads(sys.argv[2]))
        df_baseline = pd.read_csv(rootPath + 'data/input/baseline.csv', header='infer', sep=',')

        pathsDict = {}

        for carbonPrice in [0, 10, 20, 50]:
            # load the annual emission scenario data
            df_annual = pd.read_csv(rootPath + 'data/input/{}_carbon_annual.csv'.format(carbonPrice),
                                    header='infer', sep=',')

            # load the lock emission scenario data
            df_lock = pd.read_csv(rootPath + 'data/input/{}_carbon_lock.csv'.format(carbonPrice),
                                  header='infer', sep=',')

            # ## get the Xs and Ys in different scenario

            # ### annual and lock emission in baseline scenario

            # baseline scenario
            x = df_baseline.loc[:,('year')].values

            y1_baseline = df_annual.loc[:,('annual_emission')].values
            y2_baseline = df_lock.loc[:,('lock_emission')].values


            # ### annual and lock emission in CCS scenario

            # no learn scenario
            y1_ccs = df_annual.loc[:,('ccs_annual_emission')].values
            y2_ccs = df_lock.loc[:,('ccs_lock_emission')].values

            # high learn scenario
            y1_ccs_h = df_annual.loc[:,('ccs_annual_emission_h')].values
            y2_ccs_h = df_lock.loc[:,('ccs_lock_emission_h')].values

            # moderate learn scenario
            y1_ccs_m = df_annual.loc[:,('ccs_annual_emission_m')].values
            y2_ccs_m = df_lock.loc[:,('ccs_lock_emission_m')].values


            # low learn scenario
            y1_ccs_l = df_annual.loc[:,('ccs_annual_emission_l')].values
            y2_ccs_l = df_lock.loc[:,('ccs_lock_emission_l')].values


            # ### annual and lock emission in be scenario

            # high learn scenario
            y1_be_h = df_annual.loc[:,('be_annual_emission_h')].values
            y2_be_h = df_lock.loc[:,('be_lock_emission_h')].values

            # moderate learn scenario
            y1_be_m = df_annual.loc[:,('be_annual_emission_m')].values
            y2_be_m = df_lock.loc[:,('be_lock_emission_m')].values

            # low learn scenario
            y1_be_l = df_annual.loc[:,('be_annual_emission_l')].values
            y2_be_l = df_lock.loc[:,('be_lock_emission_l')].values


            # ### annual and lock emission in beccs scenario

            # high learn scenario
            y1_beccs_h = df_annual.loc[:,('beccs_annual_emission_h')].values
            y2_beccs_h = df_lock.loc[:,('beccs_lock_emission_h')].values

            # moderate learn scenario
            y1_beccs_m = df_annual.loc[:,('beccs_annual_emission_m')].values
            y2_beccs_m = df_lock.loc[:,('beccs_lock_emission_m')].values

            # low learn scenario
            y1_beccs_l = df_annual.loc[:,('beccs_annual_emission_l')].values
            y2_beccs_l = df_lock.loc[:,('beccs_lock_emission_l')].values

            # ## plot the annual and lock emission in different scenarios
            # ### annual emission
            sns.set_style("white")
            sns.set_context("paper", font_scale=1)

            plt.clf()

            f, ax = plt.subplots(1, 1, figsize=(3, 2.5))

            # plot the baseline scenario emission
            ax.plot(x, y1_baseline, '-k', label='Baseline', linewidth=1.5)

            # plot the CCS scenario emission with high, moderate, and low learning rate
            ax.plot(x, y1_ccs_m, '-', color='#CC4F1B', label='CCS', linewidth=1)
            ax.plot(x, y1_ccs_l, '--', color='#CC4F1B', linewidth=1)
            ax.plot(x, y1_ccs_h, '--', color='#CC4F1B', linewidth=1)

            # fill the region between y_CCS_l and y_CCS_h
            plt.fill_between(x, y1_ccs_h, y1_ccs_l, alpha=0.5,
                             edgecolor='#CC4F1B', facecolor='#FF9848')

            # plot the bioenergy scenario emission with high, moderate, and low learning rate
            ax.plot(x, y1_be_m, '-', color='#1B2ACC', label='Bioenergy', linewidth=1)
            ax.plot(x, y1_be_l, '--', color='#1B2ACC', linewidth=1)
            ax.plot(x, y1_be_h, '--', color='#1B2ACC', linewidth=1)

            # fill the region between y_be_l and y_be_h
            plt.fill_between(x, y1_be_h, y1_be_l, alpha=0.5,
                             edgecolor='#1B2ACC', facecolor='#1B2ACC')

            # plot the BECCS scenario emission with high, moderate, and low learning rate
            ax.plot(x, y1_beccs_m, '-', color='#6EFF33', label='BECCS', linewidth=1)
            ax.plot(x, y1_beccs_l, '--', color='#6EFF33', linewidth=1)
            ax.plot(x, y1_beccs_h, '--', color='#6EFF33', linewidth=1)


            # fill the region between y_Ren_l and y_Ren_h
            plt.fill_between(x, y1_beccs_h, y1_beccs_l,
                             alpha=0.5, edgecolor='#6EFF33', facecolor='#6EFF33')

            plt.text(2038, 5, 'Carbon price\n${}/ton'.format(carbonPrice),
                     bbox=dict(facecolor='grey', alpha=0.5), ha='center', va='top')

            # box = ax.get_position()

            # ax.set_position([box.x0, box.y0, box.width*0.95, box.height])
            ax.legend(loc='lower left', bbox_to_anchor=(0, 0), ncol=1)

            ax.set(xlabel='Year', ylabel='CO2 annual emission (Gt/year)')

            ax.set_xlim([2015, 2050])
            xticks = [2015, 2020, 2030, 2040, 2050]
            yticks = np.arange(0, 22, 2.5)

            ax.set_xticks(xticks)
            ax.set_yticks(yticks)

            plt.savefig(rootPath + 'public' + CARBON_ANNUAL_PNG.format(carbonPrice),
                        bbox_inches='tight', dpi=500)


            # ### lock emission
            sns.set_style("white")
            sns.set_context("paper", font_scale=1)

            plt.clf()

            f, ax = plt.subplots(1, 1, figsize=(3, 2.5))

            # plot the baseline scenario emission
            ax.plot(x, y2_baseline, '-k', label='Baseline', linewidth=1.5)

            # plot the CCS scenario emission with high, moderate, and low learning rate
            ax.plot(x, y2_ccs_m, '-', color='#CC4F1B', label='CCS', linewidth=1)
            ax.plot(x, y2_ccs_l, '--', color='#CC4F1B', linewidth=1)
            ax.plot(x, y2_ccs_h, '--', color='#CC4F1B', linewidth=1)

            # fill the region between y_CCS_l and y_CCS_h
            plt.fill_between(x, y2_ccs_h, y2_ccs_l,
                             alpha=0.5, edgecolor='#CC4F1B', facecolor='#FF9848')


            # plot the bioenergy scenario emission with high, moderate, and low learning rate
            ax.plot(x, y2_be_m, '-', color='#1B2ACC', label='Bioenergy', linewidth=1)
            ax.plot(x, y2_be_l, '--', color='#1B2ACC', linewidth=1)
            ax.plot(x, y2_be_h, '--', color='#1B2ACC', linewidth=1)


            # fill the region between y_be_l and y_be_h
            plt.fill_between(x, y2_be_h, y2_be_l,
                             alpha=0.5, edgecolor='#1B2ACC', facecolor='#1B2ACC')


            # plot the BECCS scenario emission with high, moderate, and low learning rate
            ax.plot(x, y2_beccs_m, '-', color='#6EFF33', label='BECCS', linewidth=1)
            ax.plot(x, y2_beccs_l, '--', color='#6EFF33', linewidth=1)
            ax.plot(x, y2_beccs_h, '--', color='#6EFF33', linewidth=1)

            # fill the region between y_Ren_l and y_Ren_h
            plt.fill_between(x, y2_beccs_h, y2_beccs_l,
                             alpha=0.5, edgecolor='#6EFF33', facecolor='#6EFF33')

            ax.set(xlabel='Year', ylabel='Carbon lock-in (Gt)')
            plt.text(2038, 100, 'Carbon price\n${}/ton'.format(carbonPrice),
                     bbox=dict(facecolor='grey', alpha=0.5), ha='center', va='top')


            # box = ax.get_position()
            # ax.set_position([box.x0, box.y0, box.width*0.95, box.height])

            ax.legend(loc='lower left', bbox_to_anchor=(0, 0), ncol=1)
            ax.set_xlim([2015, 2050])

            xticks = [2015, 2020, 2030, 2040, 2050]
            yticks = np.arange(0, 410, 50)

            ax.set_xticks(xticks)
            ax.set_yticks(yticks)

            plt.savefig(rootPath + 'public' + CARBON_LOCK_PNG.format(carbonPrice),
                        bbox_inches='tight', dpi=500)

            pathsDict['carbon{}Annual'.format(carbonPrice)] = CARBON_ANNUAL_PNG.format(carbonPrice)
            pathsDict['carbon{}Lock'.format(carbonPrice)] = CARBON_LOCK_PNG.format(carbonPrice)

        print(json.dumps(pathsDict))
    except Exception as e:
        print(e)
        #pythonLogger.postInfoToLogServer('end of CommittedEmissionPlotter.py')