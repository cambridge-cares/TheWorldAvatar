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
        'apscheduler~=3.9',
        'docopt~=0.6',
        'flask~=2.1',
        'geojson-rewind~=1.0',
        'JayDeBeApi~=1.2',
        'pandas~=1.3',
        'pyproj~=3.3',
        'py4jps>=1.0.30', 
        'requests~=2.27'
    ]
)
