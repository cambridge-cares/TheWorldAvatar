from setuptools import setup, find_packages

setup(
    name='entityrdfizer', # Required
    version='1.0.3', # Required
    # This should be your name or the name of the organization which owns the
    # project.
    author='Feroz Farazi', # Optional
    license='MIT',
    long_description=open('README.md').read(), # Optional
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=("tests")),
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/EntityRDFizer",
    python_requires='>=3.5',
    include_package_data=True,
    install_requires= ['rdflib>=4.2, <6.0', 'docopt', 'py4jps>=1.0.6'],
    entry_points={  # Optional
        'console_scripts': [
            'csv2rdf=entityrdfizer.driver:main',
        ],
    }
)