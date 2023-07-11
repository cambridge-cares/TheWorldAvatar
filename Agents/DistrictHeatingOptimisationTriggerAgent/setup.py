from setuptools import setup, find_namespace_packages 

setup(
    name='dhoptimisationtrigger',
    version='1.0.0',
    author='Markus Hofmeister',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    description="The `District Heating Optimisation Trigger Agent` instantiates all inputs to initiate (recurring) optimisation cascades of the district heating system.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingOptimisationTriggerAgent/",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'flask~=2.1.0',
        'gunicorn~=20.0.4',
        'requests~=2.31.0',
        'py4jps~=1.0.36', 
        'pyderivationagent~= 1.5.0'
    ]
)