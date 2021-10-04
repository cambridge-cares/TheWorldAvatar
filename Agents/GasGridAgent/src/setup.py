from setuptools import setup, find_packages

setup(
    name='gasgridagent',
    version='1.0.0',
    author='Markus Hofmeister (based on work from Tom Savage)',
    author_email='mh807@cam.ac.uk',
    license='MIT',
    python_requires=' >=3.5',
    packages=find_packages(exclude=('tests')),
    long_description=open('README.txt').read(),
    #install_requires=[""],
    #include_package_data=True,
    entry_points={  # Optional
        'console_scripts': [
            'update_gas_flows=gasgridagent.input_flow_data:main',
            'retrieve_gas_flows=gasgridagent.output_flow_data:main'
        ],
    }
)