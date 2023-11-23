from setuptools import setup, find_namespace_packages 

setup(
    name='dhoptimisationtrigger',
    version='1.1.1',
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
        'celery~=5.3.1',
        'flask~=2.1.0',
        'gunicorn~=20.0.4',
        'py4jps~=1.0.38', 
        'pyderivationagent~=1.6.0',
        'redis~=4.6.0',
    ]
)