from setuptools import setup, find_packages

setup(
    name='Environment Exposure Agent',  # Change this to your project's name
    version='1.0.0',  # Increment version as needed
    packages=find_packages(),  # Automatically find all packages 
    install_requires=[
        'Flask',
        'numpy',
        'pandas',
        'twa',
        'pydantic',
        'psycopg2'
    ],
    extras_require={
        'dev': [
            'pytest==7.2.0',
            'pytest-mock==3.10.0',
            'testcontainers==3.7.0'
        ]
    }
)
