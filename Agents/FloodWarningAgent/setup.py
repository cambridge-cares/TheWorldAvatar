from setuptools import setup, find_packages

setup(
    name='floodwarnings',
    version='1.1.3',
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
        'Flask==2.1.0',
        'geojson==3.0.0',
        'gunicorn==20.0.4',
        'JayDeBeApi==1.2.3',
        'pyderivationagent==1.4.4',
        'py4jps==1.0.34', 
        'requests==2.28.2',
        'shapely==2.0.1',
        'werkzeug==2.3.6'
    ]
)