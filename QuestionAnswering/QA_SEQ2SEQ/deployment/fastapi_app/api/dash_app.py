# dash_app.py
from dash import Dash, html, dcc
from dash.dependencies import Input, Output
from api.plot import SparqlService
import numpy as np

def create_dash_app(pathname_prefix, dropdown_options):
    dash_app = Dash(__name__, requests_pathname_prefix='/dash_' + pathname_prefix + '/')

    # Dash app layout
    layout_components = [
        html.Div([
            html.Div([
                html.Label('X-Axis:'),
                dcc.Dropdown(
                    id='x_axis_dropdown',
                    options=dropdown_options,
                    value=dropdown_options[0]['value']  # Default value for X-Axis
                ),
            ], style={'width': '33%', 'display': 'inline-block', 'margin-right': '10px'}),
            
            html.Div([
                html.Label('Y-Axis:'),
                dcc.Dropdown(
                    id='y_axis_dropdown',
                    options=dropdown_options,
                    value=dropdown_options[0]['value']  # Default value for Y-Axis
                ),
            ], style={'width': '33%', 'display': 'inline-block', 'margin-right': '10px'}),
            
            html.Div([
                html.Label('Colorbar:'),
                dcc.Dropdown(
                    id='color_dropdown',
                    options=dropdown_options + [{'label': 'None', 'value': None}],
                    value=None  # Default value for color variable
                ),
            ], style={'width': '33%', 'display': 'inline-block'}),
        ], style={'display': 'flex', 'justify-content': 'space-between'}),
        
        dcc.Graph(id='scatter-plot'),
    ]
    
    # Conditionally add the searchable dropdown if 'ontospecies' is in pathname_prefix
    if 'ontospecies' in pathname_prefix:
        sparql_service = SparqlService(endpoint_url=f"http://178.128.105.213:3838/blazegraph-dev/ui/namespace/{pathname_prefix}/sparql")
        results = sparql_service.get_chemical_class_list()
        suggestions = [
            {"label": item['label']['value'], "value": item['label']['value']}
            for item in results
        ]
        searchable_dropdown = html.Div([
            html.Label('Chemical Class:'),
            dcc.Dropdown(
                id='my_searchable_dropdown',
                options=suggestions,  # Assuming suggestions is defined
                searchable=True,
                placeholder="Type to search...",
                value="alkene"
            ),
        ], style={'width': '100%', 'margin-bottom': '20px'})
        
        layout_components.insert(-1, searchable_dropdown)
        layout_components.append(dcc.Location(id='url', refresh=False))

    dash_app.layout = html.Div(layout_components, style={'width': '100%'})
    layout_components.append(html.Div(id='hidden-div-for-dropdown', style={'display': 'none'}))

    @dash_app.callback(
    Output('hidden-div-for-dropdown', 'children'),
    [Input('my_searchable_dropdown', 'value')]
    )
    def update_hidden_div(value):
        return value

    # Callback to update the scatter plot
    @dash_app.callback(
        Output('scatter-plot', 'figure'),
        [Input('x_axis_dropdown', 'value'), Input('y_axis_dropdown', 'value'), Input('color_dropdown', 'value'), Input('hidden-div-for-dropdown', 'children')]  # Include pathname and make searchable_dropdown optional
    )
    def update_graph(x_axis_value, y_axis_value, color_variable, chemical_class):
        if 'ontospecies' not in pathname_prefix:
            chemical_class = None

        sparql_service = SparqlService(endpoint_url=f"http://178.128.105.213:3838/blazegraph-dev/ui/namespace/{pathname_prefix}/sparql")
        results = sparql_service.plot(x_axis_value, y_axis_value, color_variable, chemical_class)

        x_values, y_values, labels, colors = [], [], [], []
        if color_variable:
            for result in results:
                if x_axis_value.replace('?','') in result and y_axis_value.replace('?','') in result and color_variable.replace('?','') in result:
                    x_values.append(float(result[x_axis_value.replace('?','')]['value']))
                    y_values.append(float(result[y_axis_value.replace('?','')]['value']))
                    colors.append(float(result[color_variable.replace('?','')]['value']))
                    labels.append(result['id']['value'])
            
            x_axis = find_label_by_value(x_axis_value, dropdown_options)
            y_axis = find_label_by_value(y_axis_value, dropdown_options)
            c_axis = find_label_by_value(color_variable, dropdown_options)
            corr = np.corrcoef(x_values, y_values)[0, 1]

            figure = {
                'data': [
                    {
                        'x': x_values,
                        'y': y_values,
                        'text': labels,  # Add labels as text to data points
                        'mode': 'markers',
                        'marker': {
                            'size': 10,
                            'opacity': 0.7,
                            'color': colors,  # Use the third variable for colors
                            'colorscale': 'Viridis',  # Customize the color scale
                            'colorbar': {'title': c_axis}  # Customize the color bar
                        }
                    }
                ],
                'layout': {
                    'xaxis': {'title': x_axis},
                    'yaxis': {'title': y_axis},
                    'hovermode': 'closest'
                }
            }
        else:
            for result in results:
                if x_axis_value.replace('?','') in result and y_axis_value.replace('?','') in result:
                    x_values.append(float(result[x_axis_value.replace('?','')]['value']))
                    y_values.append(float(result[y_axis_value.replace('?','')]['value']))
                    labels.append(result['id']['value'])
            
            x_axis = find_label_by_value(x_axis_value, dropdown_options)
            y_axis = find_label_by_value(y_axis_value, dropdown_options)
            corr = np.corrcoef(x_values, y_values)[0, 1]

            figure = {
                'data': [
                    {
                        'x': x_values,
                        'y': y_values,
                        'text': labels,  # Add labels as text to data points
                        'mode': 'markers',
                        'marker': {
                            'size': 10,
                            'opacity': 0.7
                        }
                    }
                ],
                'layout': {
                    'xaxis': {'title': x_axis},
                    'yaxis': {'title': y_axis},
                    'hovermode': 'closest'
                }
            }
        
        # Update the layout of the scatter plot to include correlation coefficient
        figure['layout']['annotations'] = [
            {
                'x': 0.5,  # X-coordinate for the text annotation (centered)
                'y': 1.05,  # Y-coordinate for the text annotation (above the graph)
                'xref': 'paper',
                'yref': 'paper',
                'text': f'Correlation Coefficient: {corr:.2f}',  # Display correlation coefficient value
                'showarrow': False,
                'font': {'size': 14}
            }
        ]
        return figure
    
    return dash_app

def find_label_by_value(value_to_find, options_list):
    for option in options_list:
        if option['value'] == value_to_find:
            return option['label']
    return None  # Return None if value is not found



    
