from setuptools import setup, find_packages
from os import path

cwd_abspath= path.abspath(path.dirname(__file__))

setup(
    name='py4ml',
    version='1.0.0-SNAPSHOT',
    author='Andreas Eibeck, Daniel Nurkowski, Angiras Menon, Jiaru Bai',
    license='MIT',
    description='Generic ML code.',
    long_description=open('README.md').read(),
    packages=find_packages(exclude=("tests")),
    python_requires='>=3.7, <4',
    entry_points={
        'console_scripts': [
             'py4ml=py4ml.driver:run'
        ],
    },
    include_package_data=True
)