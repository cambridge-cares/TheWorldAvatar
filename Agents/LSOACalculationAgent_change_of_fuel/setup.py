from setuptools import setup, find_packages

setup(name='myproject', version='1.0', 
      packages=find_packages(exclude=("tests")),
      install_requires= [
        'numpy==1.21.6',
        'docopt==0.6.2',
        'Flask==2.2.2',
        'pandas==1.3.5',
        'JayDeBeApi==1.2.3',
        'py4jps==1.0.27', 
        'requests==2.28.1',
        'gunicorn==20.1.0',
    ],
    extras_require={
        "dev": [
            "pytest==7.2.0",
            "pytest-mock==3.10.0",
            "testcontainers==3.7.0",
            "psycopg2==2.9.5"
        ]
    }
      )