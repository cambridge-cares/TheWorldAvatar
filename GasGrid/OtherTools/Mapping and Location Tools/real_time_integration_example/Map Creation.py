import folium 
import time
import branca
from tqdm import tqdm
from datetime import datetime 
import requests
import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt  
import codecs
from folium.features import DivIcon
from nts_data_collect import terminal_intakes
from bokeh.io import save
from bokeh.plotting import figure, output_file, save

def bokeh_plot(data):
    '''
    INPUTS: 
        'data', numpy array, first column is each label, first row is x-axis (here time)
        every remaining row is a variable to be plotted against x-axis.
        i.e. np.array([['Time',0,1,2,3...],['Gas Input',4,3,5,7...]...])
    
    DESCRIPTION:
        Defining a function that, given a table of inputs,
        creates and saves HTML files containing plots
        of the timeseries of each variable in the table
    '''
    # iterating over rows
    for i in range(1,len(data[1:,0])+1):
        # name of html file produced
        output_file('Terminal Plots/'+str(data[i,0])+".html")
        # creating base plot
        p = figure(plot_width=400,plot_height=200,\
            x_axis_type="datetime", title=data[i,0])
        # axis labels
        p.xaxis.axis_label = 'Time'
        p.yaxis.axis_label = 'Instantaneous flow (mcm/day)'
        # plotting line (time is always first row)
        p.line(data[0,1:],data[i,1:])
        # saving plot given prev specified output file name
        save(p)
    return     


def createplots():
    '''
    Main creation of folium plot, takes locations, geoJSON files,
    and terminal intake data and combines.
    '''
    
    # collection of initial data of gsa coming into UK terminals 
    terminal_data = terminal_intakes()
    # conversion of string to official datetime format 
    for i in range(1,len(terminal_data[0,:])):
            terminal_data[0,i] = datetime.strptime(terminal_data[0,i],'%Y-%m-%d %H:%M:%S')
    
    # importing locations and parsing the file
    locations = pd.read_excel(r'location.xlsx')
    locations = locations.to_numpy()[:,1:]
    titles = locations[0,:]
    i = 0
    # following section stops importing when there are no coordinates
    # can occur in some types of csv file 
    while True:
        try:
            if titles[i] != titles[i]:
                titles = np.delete(titles,i)
                i -= 1
            i += 1
        except:
            break
    
    location_names = locations[1:,0]
    locations = locations[1:,:]
    c = ['blue']
    location_index = np.arange(0,len(locations[0,:]),3)
    overall_location = np.array([[0,0]])
    
    # while loop to coninually add new data to plots
    while True:
        
        # creation of base-map, tiles defines the style (see docs for more)
        # coords specify centering on UK and appropriate zoom
        m = folium.Map(location=[54.213730,-3.105027],zoom_start=6,tiles='cartodbpositron')

        # creating html files (plots) of each terminals data
        bokeh_plot(terminal_data)
       

        # iterating over location_indexes (in this case only 1 aka terminals) 
        # lots of the following code is only relevent if different markers need to 
        # represent different classes of Slocation
        # this can be added in the location.xlsx file as new columns 
        for j in location_index:
                class_location = np.array([[-1,0]])
                for i in range(len(locations[:,1+j])):
                    loc = locations[i,j+1:j+3] 
                    if loc[0] != loc[0]:
                        break
                    class_location = np.append(class_location,[loc],axis=0)
                class_location = class_location[1:,:]
                for i in range(len(class_location)):   
                    # reading html files created earlier
                    f = codecs.open('Terminal Plots/'+str(location_names[i])+'.html','r')
                    html = f.read()
                    # creating a 'frame' to place them in 
                    iframe = branca.element.IFrame(html=html,width=420,height=230)
                    # defining the popup of a marker
                    popup = folium.Popup(iframe,max_width=5000)
                    # plotting each marker with appropriate popup containing
                    # relevant html plot
                    folium.Marker([class_location[i,0],class_location[i,1]],\
                        popup=popup,\
                            icon=folium.Icon(color=c[j],icon='graph_up')).add_to(m)
                    
        # adding toggle-able layers 
        folium.LayerControl().add_to(m)
        # saving map
        m.save(r'folium_map.html')
        
        terminal_data_pd = pd.DataFrame(terminal_data)
        terminal_data_pd.to_excel('terminal_data.xlsx')
        
        # waiting 10 minutes to update terminal data
        for i in tqdm(range(720)):
            time.sleep(1)
            
        # getting new terminal data and adding it to old data (more rows)
        new_terminal_data = terminal_intakes()[:,1:]
        for i in range(len(new_terminal_data[0,:])):
            new_terminal_data[0,i] = datetime.strptime(new_terminal_data[0,i],'%Y-%m-%d %H:%M:%S')
            
        terminal_data = np.append(terminal_data,new_terminal_data,axis=1)
        
        print('UPDATED DATA')
    return

createplots()






