from setuptools import setup, find_packages

setup(
    name='ukgasflows',
    version='1.0.0',
    author='Markus Hofmeister (based on work from Tom Savage)',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    python_requires=' >=3.5, <3.8',
    packages=find_packages(exclude=('tests')),
    long_description=open('README.txt').read(),
    #install_requires=[""],
    #include_package_data=True,
    entry_points={  # Optional
        'console_scripts': [
            'update_gas_flows=ukgasflows.terminal_update:main',
            'retrieve_gas_flows=ukgasflows.output_flow_data:main'
        ],
    }
)