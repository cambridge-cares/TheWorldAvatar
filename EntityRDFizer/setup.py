from setuptools import setup, find_packages

setup(
    name='entityrdfizer', # Required
    version='1.0.0', # Required
    # This should be your name or the name of the organization which owns the
    # project.
    author='Feroz Farazi', # Optional
    license='MIT',
    long_description=open('README.md').read(), # Optional
    packages=find_packages(exclude=("tests")),
    python_requires='>=3.5',
    include_package_data=True,
    install_requires= ['rdflib>=4.2', 'docopt'],
    entry_points={  # Optional
        'console_scripts': [
            'csv2rdf=entityrdfizer.driver:main',
        ],
    }
)