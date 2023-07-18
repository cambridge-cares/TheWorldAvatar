##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 June 2023         #
##########################################

"""This method is created for ploting multiple Pareto Front lines in the same fig with the weight"""

import numpy
from pymoo.decomposition.asf import ASF
import matplotlib.pyplot as plt
import matplotlib as mpl
from sklearn.linear_model import RANSACRegressor
from sklearn.preprocessing import PolynomialFeatures

def ParetoFrontCreator(filePath_GAResults, weightList, SMRList, SMRUnitCapacity, figSavePath, selectedSMRNumber:list, selectedWeight:list, LineToBeDrew, ifLeanFig):
    ## Form the weight list
    weightNumpyMatrix = numpy.zeros((len(weightList),2))
    
    for i in range(len(weightList)):
        if float(weightList[i]) > 1 or float(weightList[i]) < 0:
            raise ValueError("Invalid weight: %s" % weightList[i])
        if float(weightList[i]) < 0.000001:
            weightList[i] = float(weightList[i]) + 0.000001
        elif float(weightList[i]) > 9.999999 or abs(float(weightList[i]) -1) < 0.000001:
            weightList[i] = float(weightList[i]) - 0.000001

        weightNumpyMatrix[i, 0] = weightList[i]
        weightNumpyMatrix[i, 1] = float(1 - weightList[i])

    ## Read the GA results from local nyp file
    GAResults = (numpy.load(filePath_GAResults, allow_pickle=True)) ## 0: F, 1: approx_ideal_feasibleNormalised, 2: approx_nadir_feasibleNormalised
    if ifLeanFig:
        lines = []
        for ld in LineToBeDrew:
            index_lineToDraw = SMRList.index(ld)
            lines.append(index_lineToDraw)
                   
    ## indexOfOptimaforEachWeight_forEachSMRDesign = []

    ## adjusted
    adjusted_approx_ideal_feasibleNormalised_1 = min([ ga1[0] for ga1 in GAResults[:, 1]])
    adjusted_approx_ideal_feasibleNormalised_2 = min([ ga1[1] for ga1 in GAResults[:, 1]])
    adjusted_approx_ideal_feasibleNormalised = numpy.array([adjusted_approx_ideal_feasibleNormalised_1, adjusted_approx_ideal_feasibleNormalised_2])
    
    adjusted_approx_nadir_feasibleNormalised_1 = max([ ga2[0] for ga2 in GAResults[:, 2]])
    adjusted_approx_nadir_feasibleNormalised_2 = max([ ga2[1] for ga2 in GAResults[:, 2]])
    adjusted_approx_nadir_feasibleNormalised = numpy.array([adjusted_approx_nadir_feasibleNormalised_1, adjusted_approx_nadir_feasibleNormalised_2])

    ## set up the clourmap
    ## Pareto Front
    cmap = mpl.cm.get_cmap("PuBu", len(SMRList)+ 4)
    colors = cmap(numpy.linspace(0, 1, len(SMRList) + 4))
    ## Selected SMR number
    cmap2 = mpl.cm.get_cmap("YlOrBr", len(selectedSMRNumber)+ 4)
    colors2 = cmap2(numpy.linspace(0, 1, len(selectedSMRNumber) + 4))

    list_weight025_x = []
    list_weight025_y = []
    list_weight05_x = []
    list_weight05_y = []
    list_weight075_x = []
    list_weight075_y = []
    
    ## selected design index
    selectedSMRIndex = []
    for sd in selectedSMRNumber:
        selectedSMRIndex.append(SMRList.index(sd))

    for i_ga, ga in enumerate(GAResults):
        F = ga[0]
        ## Normalisation the objective space according to the space of the same SMR
        approx_ideal = F.min(axis = 0)
        approx_nadir = F.max(axis = 0)
        nF = (F - approx_ideal) / (approx_nadir - approx_ideal)
        ## Decomposition method called Augmented Scalarization Function (ASF),
        decomp = ASF()
        indexOfOptima = []
        for weights in weightNumpyMatrix:
            print(weights)
            i = decomp.do(nF, 1/weights).argmin()
            indexOfOptima.append(i) ## to keep the result picked is the same one picked from the main algorithm
        # indexOfOptimaforEachWeight_forEachSMRDesign.append(indexOfOptima) 

        ## Normalised feasible points
        nF_feasibleNormalised = (F - adjusted_approx_ideal_feasibleNormalised) / (adjusted_approx_nadir_feasibleNormalised - adjusted_approx_ideal_feasibleNormalised)
        
        if i_ga in lines:
            numberOfSMR = SMRList[i_ga]
            if numberOfSMR == 41:
                numberOfSMR = 40
            plt.scatter(nF_feasibleNormalised[:, 0], nF_feasibleNormalised[:, 1], alpha = 0.9, s = 20, facecolors = colors[i_ga+4], edgecolors = 'none')
            if numberOfSMR == 60:
                plt.text(min(nF_feasibleNormalised[:, 0]) + 0.01, max(nF_feasibleNormalised[:, 1]) - 0.01, str(numberOfSMR) + ' SMRs', fontsize = 7.5, alpha = 0.85) 
                plt.text(min(nF_feasibleNormalised[:, 0]) + 0.02, max(nF_feasibleNormalised[:, 1]) - 0.03, str(round(numberOfSMR * SMRUnitCapacity/1E3, 1)) + ' GW', fontsize = 6, alpha = 0.75) 
            elif numberOfSMR == 32:
                plt.text(min(nF_feasibleNormalised[:, 0]) -0.07, max(nF_feasibleNormalised[:, 1]) + 0.03, str(numberOfSMR) + ' SMRs', fontsize = 7.5, alpha = 0.85) 
                plt.text(min(nF_feasibleNormalised[:, 0]) -0.06, max(nF_feasibleNormalised[:, 1]) + 0.01, str(round(numberOfSMR * SMRUnitCapacity/1E3, 1)) + ' GW', fontsize = 6, alpha = 0.75) 
            else:
                plt.text(min(nF_feasibleNormalised[:, 0]) -0.06, max(nF_feasibleNormalised[:, 1]) + 0.03, str(numberOfSMR) + ' SMRs', fontsize = 7.5, alpha = 0.85) 
                plt.text(min(nF_feasibleNormalised[:, 0]) -0.05, max(nF_feasibleNormalised[:, 1]) + 0.01, str(round(numberOfSMR * SMRUnitCapacity/1E3, 1)) + ' GW', fontsize = 6, alpha = 0.75) 
                
        ## highlight the Pareto front corresponding to the planed capacity for nuclear 
        if SMRList.index(52) == i_ga:
            plt.plot(sorted(nF_feasibleNormalised[:, 0].tolist()), sorted(nF_feasibleNormalised[:, 1].tolist(), reverse=True), linestyle = 'dashed', color = "#9e2f29", linewidth = 1)
            plt.text(max(nF_feasibleNormalised[:, 0]) -0.1, min(nF_feasibleNormalised[:, 1]) - 0.025, '~52 SMRs', fontsize = 7.5, color = "#ad4742", alpha = 0.85) 
            plt.text(max(nF_feasibleNormalised[:, 0]) -0.09, min(nF_feasibleNormalised[:, 1]) - 0.045, '24 GW', fontsize = 6, color = "#ad4742", alpha = 0.75) 

        if SMRList.index(36) == i_ga:
            plt.plot(sorted((nF_feasibleNormalised[:, 0]).tolist()), sorted((nF_feasibleNormalised[:, 1]).tolist(), reverse=True), linestyle = 'dashed', color = "#ad4742", linewidth = 1)
            plt.text(max(nF_feasibleNormalised[:, 0]) -0.035, min(nF_feasibleNormalised[:, 1]) - 0.025, '~36 SMRs', fontsize = 7.5, color = "#ad4742",alpha = 0.85) 
            plt.text(max(nF_feasibleNormalised[:, 0]) -0.025, min(nF_feasibleNormalised[:, 1]) - 0.045, '16.9 GW', fontsize = 6, color = "#ad4742",alpha = 0.75) 
             
        for i_optima, j in enumerate(indexOfOptima):
            
            if i_optima == 1:
                list_weight025_x.append(nF_feasibleNormalised[j, 0])
                list_weight025_y.append(nF_feasibleNormalised[j, 1])
            elif i_optima == 2:
                list_weight05_x.append(nF_feasibleNormalised[j, 0])
                list_weight05_y.append(nF_feasibleNormalised[j, 1])
            elif i_optima == 3:
                list_weight075_x.append(nF_feasibleNormalised[j, 0])
                list_weight075_y.append(nF_feasibleNormalised[j, 1])

            # if i_ga in selectedSMRIndex:
            #     for sw in selectedWeight: 
            #         if weightList[i_optima] == sw:
            #             plt.scatter(nF_feasibleNormalised[j, 0], nF_feasibleNormalised[j, 1], marker="x", alpha=0.68, s=10, color = colors2[int(selectedSMRIndex.index(i_ga)) + 4])

    plt.gca().set_aspect('equal', adjustable='box')
    # plt.xlim(-0.05, 0.85)
    #plt.ylim(-0.05, 0.85)
    # plt.axis('equal')
    plt.tick_params(direction='in')
    ## plt.gca().set_aspect(1)

    for i, sw in enumerate(selectedWeight):
        if i == 0:
            ## plot the fitting lines for the weight 
            x = numpy.array(list_weight025_x)
            y = numpy.array(list_weight025_y)
            weightLabel = "Weight = 0.25"
        elif i == 1:
            x = numpy.array(list_weight05_x)
            y = numpy.array(list_weight05_y)
            weightLabel = "Weight = 0.50"
        else:
            x = numpy.array(list_weight075_x)
            y = numpy.array(list_weight075_y)
            weightLabel = "Weight = 0.75"

        # Reshape x to a 2D array for PolynomialFeatures
        x_reshape = x.reshape(-1, 1)

        # Generate polynomial features
        degree = 2
        poly_features = PolynomialFeatures(degree=degree)
        X_poly = poly_features.fit_transform(x_reshape)

        # Fit RANSAC regressor to polynomial features
        ransac = RANSACRegressor()
        ransac.fit(X_poly, y)

        # Generate x values for the fitted curve
        x_fit = numpy.linspace(min(x), max(x), 1000)
        x_fit_reshape = x_fit.reshape(-1, 1)
        X_fit_poly = poly_features.transform(x_fit_reshape)

        # Predict y values using the fitted RANSAC model
        y_fit = ransac.predict(X_fit_poly)

        ## Plotting the original points and the fitted line
        plt.plot(x_fit, y_fit, linestyle = 'dotted', color = "#c48b4f")
        plt.text(max(x_fit) + 0.01, max(y_fit), weightLabel, color = "#c48b4f", fontsize = 7, alpha = 0.85) 
 
    ## Fill the area between the lines
    ##plt.fill_between(list_weight05_x, list_weight05_y, list_weight02_y,  where=(list_weight05_y >= list_weight02_y), color='gray', alpha=0.4)
     
    plt.xlabel("Normalised SMR investment and risk cost (-)", fontsize = 12)
    plt.ylabel("Normalised site-demand distance (-)", fontsize = 12) 
    
    # plt.legend(
    #         loc="upper center", 
    #         fontsize = 12, 
    #         ncol=2, 
    #         bbox_to_anchor=(0.5, -0.16), 
    #         frameon=False)
    #  plt.tight_layout()
    plt.savefig(figSavePath + 'ParetoFront_allSMRDesign.pdf', dpi = 1200, bbox_inches='tight')
    plt.clf()
    plt.cla()
    return

if __name__ == '__main__': 
    filePath_GAResults = "/mnt/d/wx243/FromAW/npy/np_ParetoFrontResult_eachSMRDesign.npy"
    weightList = [0, 0.25, 0.5, 0.75, 1] 
    SMRList = [5, 10, 12, 15, 17,  18, 20, 24, 26,  28, 30, 31, 32, 33, 34, 36, 41, 42, 47,  48, 49, 50, 52,  55, 56, 57, 58,  60]
    figSavePath = "/mnt/d/wx243/FromAW/ParetoFront/"
    selectedSMRNumber = [12, 18, 32, 42, 48]
    LineToBeDrew = [10, 20, 32, 41, 50, 60]
    selectedWeight = [0.25, 0.5, 0.75]
    SMRUnitCapacity = 470

    ifLeanFig = True

    ParetoFrontCreator(filePath_GAResults, weightList, SMRList, SMRUnitCapacity, figSavePath, selectedSMRNumber, selectedWeight, LineToBeDrew, ifLeanFig)