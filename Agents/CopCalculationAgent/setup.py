from setuptools import setup, find_namespace_packages

setup(
    name='copcalculationa',
    version='0.0.1',
    author='Jieyang Xu',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    python_requires='>=3.8',
    description="The `copcalculation` agent calculates the coefficient of performance for a particular month in a Lower Super Output Area in the UK \
                and populates the result to knowledge graph as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-heat-pump-migration-to-stack-2/Agents/CopCalculationAgent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    include_package_data=True,
    install_requires= [
        'tqdm==4.64.1',
        'numpy==1.21.6',
        'uuid==1.30',
        'datetime==4.7',
        'flask==2.1.0',
        'JayDeBeApi==1.2.3',
        'pandas==1.5.1',
        'py4jps==1.0.29', 
        'requests==2.28.1',
        'pyderivationagent==1.4.1'
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
