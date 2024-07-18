from setuptools import setup, find_namespace_packages 

setup(
    name='dhemission',
    version='1.1.0',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    description="The `Emission Agent` converts consumed gas and generated heat amounts into associated emission values.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingEmissionEstimationAgent/",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'CoolProp~=6.5.0',
        'flask~=2.1.0',
        'pandas~=2.0.3',
        'py4jps~=1.0.38', 
        'pyderivationagent~=1.6.0'
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