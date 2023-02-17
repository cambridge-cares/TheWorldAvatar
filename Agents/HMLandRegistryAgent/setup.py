from setuptools import setup, find_packages

setup(
    name='landregistry',
    version='1.0.0',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    description="The `landregistry` agent retrieves data about previous property transactions from the HM Land Registry API and instantiates it as part of The World Avatar project.",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'apscheduler~=3.9',
        'docopt~=0.6',
        'flask~=2.2',
        'JayDeBeApi~=1.2',
        'pandas~=1.3',
        'py4jps>=1.0.30', 
        'pyproj~=3.3',
        'requests~=2.28',
        'fuzzywuzzy~=0.18',
        'python-Levenshtein~=0.20',
        'geojson-rewind~=1.0'
    ]
)