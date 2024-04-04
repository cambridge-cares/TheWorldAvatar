from setuptools import setup, find_namespace_packages 

setup(
    name='dhoptimisation',
    version='1.0.0',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    description="The `District Heating Optimisation Agent` optimises the generation cost for the Pirmasens district heating network use case.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingOptimisationAgent/",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'CoolProp~=6.5.0',
        'flask~=2.1.0',
        'py4jps~=1.0.38', 
        'pyderivationagent~=1.6.0',
        'pandas~=2.1.0',
        'numpy~= 1.26.0',
        'scikit-learn~=1.3.0',
        'matplotlib~=3.8.0'
    ]
)
