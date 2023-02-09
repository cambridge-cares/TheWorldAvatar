from setuptools import setup, find_packages

setup(
    name='floodwarnings',
    version='0.1.0',
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
        'APScheduler==3.9.1',
        'docopt==0.6.2',
        'Flask==2.2.2',
        'py4jps==1.0.30', 
        'requests==2.28.1',
        'gunicorn==20.1.0',
    ],
    # extras_require={
    #     "dev": [
    #         "pytest==7.2.0",
    #         "pytest-mock==3.10.0",
    #         "testcontainers==3.7.0",
    #         "psycopg2==2.9.5"
    #     ]
    #}
)