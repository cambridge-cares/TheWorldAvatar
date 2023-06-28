from setuptools import setup, find_packages

setup(
    name='pubchemagent',
    version='1.0.0',
    author='Laura Pascazio, Ali Naseri',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    url="",
    python_requires='>=3.5',
    include_package_data=True,
    install_requires= ['numpy','pyderivationagent==1.4.4','docopt','py4jps==1.0.34', 'pint', 'rdkit', 'flask', 'periodictable', 'bioservices'],
    entry_points={
        'console_scripts': [
            'pubchemagent=pubchemagent.driver:main',
       ],
    }
)