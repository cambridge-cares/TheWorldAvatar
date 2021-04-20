import os, sys
current_dir = os.path.dirname(os.path.realpath(__file__))
parent_dir = os.path.dirname(current_dir)
sys.path.append(parent_dir)

from GoogleAPI import GoogleAPI

html_template = '''
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" crossorigin="anonymous">
    <title>Test HTML for google visual results</title>
    <style>
        .head { width: 600px; padding:  6px; padding-left: 10px; text-color:#222;   text-align: left; font-family: arial,sans-serif; 	font-size:14px}
		.value {font-size: 35px;   vertical-align: middle;   display: inline-block; margin-left: 10px }
		.key_img { margin-left: 300px}
		.main_result {width: 600px;}
		.description {width: 600px;  padding-left: 10px; padding:  6px;}
		.google_result {margin-left: 20px; margin-top: 10px}
		.sideways {margin-right: 30px; }
		.sideways-container {	display:flex;
								flex-direction:row;
								width: 600px;
								}
		.webanswers-webanswers_table__webanswers-table {padding: 10px}
        .g {padding: 10px}
        .extra {width: 600px}
	

    </style>
</head>
<body>
<div> The question is: %s  </div>
<br/> 
 
%s 
</div>
</body>
'''


google_api = GoogleAPI()

# TODO: let the api return the combined html with modified classes
# TODO: use the html returned to generate the files, with an index

question_set = ['what is the boiling point of water',
                'what is the boiling point of benzene',
                'heat capacity of h2so4',
                'Molecular weight of ch4',
                'chemical structure of benzene',
                'standard enthalpy of formation of methane',
                'what is the heat capacity of ethanedionic acid',
                'mass of aromatic hydrocarbons',
                'Molecular model of aromatic hydrocarbons',
                'Chemical structure of aromatic hydrocarbons']

# question_set = ['heat capacity of h2so4']

question_set_2 = ['who is the president of china', 'where is the capital of Japan']
# q = 'show me the vibration frequency of H2O2'
# q = 'what is the spin multiplicity of C8H14'
q = 'What is the geometry type of C2H2O2'
html = google_api.run(q)
print('the result is', html)
with open('../debug.html', 'w', encoding='UTF-8') as f:
    f.write(html_template % (q, html))
    f.close()

counter = 0
for q in question_set:
    counter = counter + 1
    html = google_api.run(q)
    with open('1-%s.html' % str(counter), 'w', encoding='UTF-8') as f:
        f.write(html_template % (q, html))
        f.close()
#
counter_2 = 0
for q2 in question_set_2:
    counter_2 = counter_2 + 1
    html = google_api.run(q2)
    with open('2-%s.html' % str(counter_2), 'w', encoding='UTF-8') as f:
        f.write(html_template % (q2, html))
        f.close()
