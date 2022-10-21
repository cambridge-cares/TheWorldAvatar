from setuptools import setup, find_packages

setup(
    name='squaremetreprice',
    version='0.0.1',
    author='Markus Hofmeister',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    url="",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'flask~=2.1',
        'JayDeBeApi~=1.2',
        'pandas~=1.3',
        'py4jps>=1.0.26', 
        'requests~=2.28',
        'pyderivationagent==1.2.2'
    ]
)