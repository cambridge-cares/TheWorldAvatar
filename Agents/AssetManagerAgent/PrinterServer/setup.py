from cx_Freeze import setup, Executable

includes = [ 'jinja2' , 'jinja2.ext'] 
excludes = ['Tkinter']

setup(
 name='Printer server',
 version = '1.0',
 description = 'Server for printing to a registered printer. Tested on Windows',
 options = {'build_exe':   {'excludes':excludes, 'includes':includes}},
 executables = [Executable('PrinterServer.py')]
)