from setuptools import setup, find_namespace_packages 

setup(
    name='forecasting',
    version='0.0.1',
    author='Markus Hofmeister, Magnus Mueller',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    description="The `Forecasting Agent` predicts a time series and instantiates the forecast into a given KG as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent/",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'flask~=2.2.2',
        'pandas~=1.5.1',
        'py4jps~=1.0.30', 
        'requests~=2.28.1',
        'darts~=0.21.0',
        'configobj~=5.0.6',
        'fire~=0.4.0'
    ],
    extras_require={
        "dev": [
            "testcontainers==3.7.0",
            "pytest==7.2.0",
            "pytest-docker-compose==3.2.1",
            "pytest-rerunfailures==10.2",
            "psycopg2==2.9.5"
        ],
    }
)