from setuptools import setup, find_packages

setup(
    name='floodwarnings',
    version='1.0.0',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    description="The `floodwarnings` agent retrieves flood alerts and warnings from the UK Environment Agency Real Time flood-monitoring API and instantiates them as part of The World Avatar project.",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FloodWarningAgent",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'APScheduler==3.10.0',
        'Flask==2.2.2',
        'geojson==3.0.0',
        'gunicorn==20.1.0',
        'JayDeBeApi==1.2.3',
        'py4jps==1.0.33', 
        'requests==2.28.2',
        'shapely==2.0.1',
    ]
)