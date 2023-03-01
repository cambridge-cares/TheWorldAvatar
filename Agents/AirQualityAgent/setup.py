from setuptools import setup, find_packages

setup(
    name='airquality',
    version='1.0.0',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    description="The `airquality` agent retrieves air quality data from the UK-AIR Sensor Observation Service and instantiates it as part of The World Avatar project.",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AirQualityAgent",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'APScheduler==3.9.1',
        'Flask==2.2.2',
        #TODO: uncomment if necessary to interact with PostGIS
        #'JayDeBeApi==1.2.3',
        'pandas~=1.4',
        'py4jps==1.0.30', 
        'requests==2.28.1',
    ]
)