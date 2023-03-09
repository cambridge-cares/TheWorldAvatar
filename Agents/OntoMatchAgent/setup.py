from setuptools import setup, find_packages

setup(
    name='ontomatch',
    version='1.0.0',
    author='Andreas Eibeck, Shaocong Zhang',
    license='MIT',
    description='ontomatch',
    long_description=open('README.md').read(),
    packages=find_packages(exclude=("tests")),
    python_requires='>=3.7, <4',
)