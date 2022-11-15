from setuptools import setup, find_packages

setup(
    name='kingslynnagent',
    version='1.0.0',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    python_requires=' >=3.6',
    packages=find_packages(exclude=('tests')),
    long_description=open('README.md').read(),
    #install_requires=[""],
    #include_package_data=True,
    entry_points={  # Optional
        'console_scripts': [
            'get_buildings=kingslynnagent.query_buildings:main'
        ],
    }
)