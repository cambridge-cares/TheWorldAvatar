from setuptools import setup, find_packages

setup(
    name='epcdata',
    version='1.0.0',
    author='Markus Hofmeister, Srishti Ganguly',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    description="The `epcdata` agent retrieves Energy Performance Certificate (EPC) data from three UK EPC API endpoints and instantiates them as part of The World Avatar project.",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    url="",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'celery[redis]==5.2.7',
        'docopt~=0.6',
        'flask==2.1.0',
        'geojson-rewind==1.0.3',
        'JayDeBeApi==1.2.3',
        'pandas==1.5.3',
        'pyderivationagent==1.4.4',
        'pyproj==3.4.1',
        'py4jps==1.0.34', 
        'requests==2.28.2'
    ]
)
