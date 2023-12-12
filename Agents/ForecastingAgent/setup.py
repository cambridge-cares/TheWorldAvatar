from setuptools import setup, find_namespace_packages 

setup(
    name='forecastingagent',
    version='2.2.1',
    author='Markus Hofmeister, Magnus Mueller',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    description="The `Forecasting Agent` predicts a time series and instantiates the forecast as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent/",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'flask~=2.1.0',
        'pandas~=1.5.1',
        'requests~=2.28.1',
        'configobj~=5.0.6',
        'fire~=0.4.0',
        'py4jps~=1.0.38', 
        'pyderivationagent~=1.6.0',
        # To ensure loading of GPU-trained model on CPU-only machines, specific 
        # versions of darts, torchmetrics and pytorch-lightning are required.
        'darts==0.21.0',
        'torchmetrics==0.9.3',
        'pytorch-lightning==1.7.7',
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