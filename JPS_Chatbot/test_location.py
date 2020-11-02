import sys, os
# print('real path',os.path.realpath(os.path.dirname(__file__)))
#
# real_path = os.path.realpath(os.path.dirname(__file__))
# UI_source_dir = os.path.join(real_path, 'UI\source')

from flask import Flask
from UI.source.run import app

app.run()