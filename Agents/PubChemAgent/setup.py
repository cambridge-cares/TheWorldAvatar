from setuptools import setup, find_packages

setup(
    name='pubchemagent',
    version='1.1.0',
    author='Laura Pascazio, Ali Naseri',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    url="",
    python_requires='>=3.5',
    include_package_data=True,
    install_requires=[
        'numpy<2.0',
        'pyderivationagent==1.5.0',
        'docopt',
        'py4jps==1.0.36',
        'pint==0.21.1',
        'flexparser',
        'flexcache',
        'rdkit',  
        'Flask==2.1.0',  
        'Werkzeug>=2.1,<2.2',  
        'periodictable',
        'bioservices',
        'unit-parse'  # Verify the correct package name on PyPI
    ],
    extras_require={
        "no_apscheduler": []
    },
    entry_points={
        'console_scripts': [
            'pubchemagent=pubchemagent.driver:main',
       ],
    }
)