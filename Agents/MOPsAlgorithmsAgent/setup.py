from setuptools import setup, find_packages
import os.path

cwd_abspath= os.path.abspath(os.path.dirname(__file__))

setup(
    name='mopsagent',
    version='1.0.0',
    author='Daniel Nurkowski',
    author_email='',
    license='MIT',
    python_requires='>=3.5, <3.8',
    packages=find_packages(exclude=('tests')),
    long_description=open('README.md').read(),
    install_requires= ["py4jps", "docopt"],
    include_package_data=True,
    entry_points={
        'console_scripts': [
            'mopsagent=mopsagent.main:start'
            ]
        }
)
