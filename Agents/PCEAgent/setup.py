from setuptools import setup, find_packages
from os import path

cwd_abspath= path.abspath(path.dirname(__file__))

setup(
    name='oscml',
    version='1.0.0-SNAPSHOT',
    author='Andreas Eibeck, Daniel Nurkowski, Angiras Menon, Jiaru Bai',
    license='MIT',
    description='Organic solar cells PCE prediction models',
    long_description=open('README.md').read(),
    packages=find_packages(exclude=("tests")),
    python_requires='>=3.7, <4',
    entry_points={
        'console_scripts': [
             'oscml=oscml.driver:run'
        ],
    },
    include_package_data=True
)