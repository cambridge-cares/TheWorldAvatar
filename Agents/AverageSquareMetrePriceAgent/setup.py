from setuptools import setup, find_namespace_packages

setup(
    name='avgsqmprice',
    version='0.0.1',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    description="The `avgsqmprice` agent calculates the average square metre price of properties for a particular postcode and populates the result to knowledge graph as part of The World Avatar project.",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=["tests", "tests.*"]),
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AverageSquareMetrePriceAgent",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'flask~=2.1.0',
        'JayDeBeApi~=1.2.3',
        'pandas~=1.5.1',
        'py4jps~=1.0.29',
        'requests~=2.28.1',
        'pyderivationagent~=1.4.1'
    ],
    extras_require={
        "dev": [
            "testcontainers==3.7.0",
            "pytest==7.2.0",
            "pytest-docker-compose==3.2.1",
            "pytest-rerunfailures==10.2",
            "pytest-mock==3.10.0",
            "psycopg2==2.9.5"
        ],
    }
)
