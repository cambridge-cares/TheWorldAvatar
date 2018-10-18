import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import sys
import json

#from caresjpsutil import PythonLogger

FOSSIL_CAP = "/images/1_top_ten_emission_countries_fossil_ele_cap.png"
FOSSIL_CAP_PER = "/images/1_top_ten_emission_countries_fossil_ele_cap_per.png"
FOSSIL_GEN = "/images/1_top_ten_emission_countries_fossil_ele_gen.png"
FOSSIL_GEN_PER = "/images/1_top_ten_emission_countries_fossil_ele_gen_per.png"

if __name__ == "__main__":
    #pythonLogger = PythonLogger('EmissionPlotter.py')
    #pythonLogger.postInfoToLogServer('start of EmissionPlotter.py')

    try:
        modelsPath = json.loads(sys.argv[1])

        # ### plot top ten emission countries versus fossil powerplant capacity
        df = pd.read_csv(modelsPath + 'data/input/top_ten_emission_countries.csv', header='infer', sep=',')

        df1 = df[df.Year == 2000]
        my_list = df1["Country"].tolist()
        my_list1 = ['CN', 'US', 'IN', 'RU', 'JP', 'DE', 'IR', 'KR', 'CA', 'SA']

        df = df.sort_values(by=['Country', 'Year'], ascending=[True, True])

        # ### plot1 - ele_cap versus year
        df1 = df.loc[:, ['Country', 'Year', 'ele_cap_fossil_mw']]
        df1.columns = ['Fossil fuel power capacity (MW)', 'Year', 'ele_cap_fossil_mw']

        vmax1 = df1[['ele_cap_fossil_mw']].max(axis=0)
        vmin1 = df1[['ele_cap_fossil_mw']].min(axis=0)

        df1['Year'] = df1['Year'].astype(int)
        df1 = df1.pivot(index='Fossil fuel power capacity (MW)', columns='Year', values='ele_cap_fossil_mw')
        df1 = df1.reindex(my_list)
        df1 = df1.reindex(my_list1)

        csfont = {'fontname':'Times New Roman'}

        plt.clf()

        plt.figure(figsize=(3.3, 1.6))

        sns.set_style("dark")

        sns.set_context("paper", font_scale=0.9)

        ax = sns.heatmap(df1, cmap='Greens', cbar=False, vmin=20000, vmax=1200000)

        cbar = ax.figure.colorbar(ax.collections[0])

        cbar.set_label("GW")

        cbar.set_ticks([20000, 1200000])
        cbar.set_ticklabels(["20", "1200"])

        ax.set_xlabel('')
        ax.set_ylabel('Installed capacity \n')

        ax.set(xticklabels=[])

        plt.xticks(rotation=0)
        plt.yticks(rotation=0)

        plt.tight_layout()

        plt.savefig(modelsPath + 'public' + FOSSIL_CAP, dpi=1000)
        plt.close()


        # ### plot2 - ele_cap percentage versus year

        df2 = df.loc[:, ['Country', 'Year', 'ele_cap_fossil_per']]

        df2.columns = ['Fossil fuel power capacity percentage', 'Year', 'ele_cap_fossil_per']

        df2['Year'] = df2['Year'].astype(int)

        df2 = df2.pivot(index='Fossil fuel power capacity percentage', columns='Year', values='ele_cap_fossil_per')

        df2 = df2.reindex(my_list)


        plt.clf()

        plt.figure(figsize=(3.1, 1.6))

        sns.set_context("paper", font_scale=0.9)

        ax = sns.heatmap(df2, cmap='Greens', cbar=False, vmin=0, vmax=100)

        cbar = ax.figure.colorbar(ax.collections[0])

        cbar.set_label("Percentage")

        cbar.set_ticks([0, 100])
        cbar.set_ticklabels(["0%", "100%"])

        ax.set_xlabel('')
        ax.set_ylabel('')

        ax.set(xticklabels=[])
        ax.set(yticklabels=[])

        # plt.xticks(rotation=45)

        plt.tight_layout()

        plt.savefig(modelsPath + 'public' + FOSSIL_CAP_PER, dpi=1000)

        plt.close()


        # ### plot3 - ele_gen versus year

        df3 = df.loc[:, ['Country', 'Year', 'ele_gen_fossil_gwh']]

        vmax3 = df3[['ele_gen_fossil_gwh']].max(axis=0)
        vmin3 = df3[['ele_gen_fossil_gwh']].min(axis=0)

        df3.columns = ['Fossil fuel power generation (GWh)', 'Year', 'ele_gen_fossil_gwh']

        df3['Year'] = df3['Year'].astype(int)

        df3 = df3.pivot(index='Fossil fuel power generation (GWh)', columns='Year', values='ele_gen_fossil_gwh')

        df3 = df3.reindex(my_list)


        plt.clf()

        plt.figure(figsize=(3.3, 1.7))

        sns.set_context("paper", font_scale=0.9)

        ax = sns.heatmap(df3, cmap='Greens', cbar=False, vmin=110000, vmax=4300000)

        cbar = ax.figure.colorbar(ax.collections[0])

        cbar.set_label("TWh")

        cbar.set_ticks([110000, 4300000])
        cbar.set_ticklabels(["110", "4300"])

        ax.set_xticklabels(['2000', '', '', '', '', '2005', '', '', '', '', '2010', '', '', '', '2014'])

        ax.set_xlabel('Year')
        ax.set_ylabel('Annual generation \n')

        # ax.set(yticklabels=[])

        plt.xticks(rotation=0)

        plt.tight_layout()

        plt.savefig(modelsPath + 'public' + FOSSIL_GEN, dpi=1000)

        plt.close()


        # ### plot4 - ele_gen percentage versus year

        df4 = df.loc[:, ['Country', 'Year', 'ele_gen_fossil_per']]

        df4.columns = ['Fossil fuel power generation percentage', 'Year', 'ele_gen_fossil_per']

        df4['Year'] = df4['Year'].astype(int)

        df4 = df4.pivot(index='Fossil fuel power generation percentage', columns = 'Year', values = 'ele_gen_fossil_per')

        df4 = df4.reindex(my_list)


        plt.clf()

        plt.figure(figsize=(3.3, 1.7))

        sns.set_context("paper", font_scale=0.9)

        ax = sns.heatmap(df4, cmap='Greens', cbar=False, vmin=0, vmax=100)

        cbar = ax.figure.colorbar(ax.collections[0])

        cbar.set_label("Percentage")

        cbar.set_ticks([0, 100])
        cbar.set_ticklabels(["0%", "100%"])

        ax.set_xlabel('Year')
        ax.set_ylabel('')
        ax.set(yticklabels=[])

        ax.set_xticklabels(['2000', '', '', '', '', '2005', '', '', '', '', '2010', '', '', '', '2014'])

        plt.xticks(rotation=0)

        plt.tight_layout()

        plt.savefig(modelsPath + 'public' + FOSSIL_GEN_PER, dpi=1000)

        plt.close()


        # #### color palette test

        plt.clf()

        plt.figure(figsize=(8, 4))

        sns.set_context("paper", font_scale=1.3)

        ax = sns.heatmap(df4, cmap='PuBu', cbar=False, vmin=0, vmax=100)

        cbar = ax.figure.colorbar(ax.collections[0])

        cbar.set_ticks([0, 100])

        cbar.set_ticklabels(["0%", "100%"])

        plt.tight_layout()

        plt.savefig(modelsPath + 'public/images/color_palette_test/PuBu_rect.png', dpi=1000)

        plt.close()


        plt.clf()

        sns.set_context("paper")

        ax = sns.heatmap(df4, cmap='PuBu', cbar=False, vmin=0, vmax=100)

        ax.set_ylabel('')

        # ax.set(yticklabels=[])

        # ax.set_aspect("equal")

        cbar = ax.figure.colorbar(ax.collections[0])

        cbar.set_ticks([0, 100])

        cbar.set_ticklabels(["0%", "100%"])

        plt.tight_layout()

        plt.savefig(modelsPath + 'public/images/color_palette_test/PuBu_square.png', dpi=1000)

        plt.close()


        # ## overall figure

        plt.close()

        plt.clf()

        plt.figure(figsize=(30,6))

        sns.set_context("paper", font_scale=1.2)

        fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, sharex=True, sharey=True)


        g1 = sns.heatmap(df1, cmap='Greens', vmin=20000, vmax=1200000, ax=ax1)

        # cbar = g1.figure.colorbar(g1.collections[0])

        # cbar.set_ticks([20000, 1200000])

        # cbar.set_ticklabels(["20", "1200"])

        g1.set_xlabel('')
        g1.set_ylabel('')



        g2 = sns.heatmap(df3, cmap='Greens', ax=ax2)

        # cbar = g2.figure.colorbar(g2.collections[0])

        # cbar.set_ticks([20000, 1200000])

        # cbar.set_ticklabels(["10", "1200"])

        g2.set_xlabel('')
        g2.set_ylabel('')
        # g2.set_yticks([])


        g3 = sns.heatmap(df2, cmap='Greens', ax=ax3)

        # cbar = g3.figure.colorbar(g3.collections[0])

        # cbar.set_ticks([20000, 1200000])

        # cbar.set_ticklabels(["30", "1200"])

        g3.set_xlabel('')
        g3.set_ylabel('')


        g4 = sns.heatmap(df4, cmap='Greens', ax=ax4)

        # cbar = g4.figure.colorbar(g4.collections[0])

        # cbar.set_ticks([20000, 1200000])

        # cbar.set_ticklabels(["40", "1200"])

        g4.set_xlabel('')
        g4.set_ylabel('')
        # g4.set_yticks([])


        plt.tight_layout()

        pathsDict = {
            "fossilCap": FOSSIL_CAP,
            "fossilCapPer": FOSSIL_CAP_PER,
            "fossilGen": FOSSIL_GEN,
            "fossilGenPer": FOSSIL_GEN_PER
        }

        print(json.dumps(pathsDict))
    except Exception as e:
        print(e)
        #pythonLogger.postInfoToLogServer('end of EmissionPlotter.py')