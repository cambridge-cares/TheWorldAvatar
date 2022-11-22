from setuptools import setup, find_namespace_packages 

setup(
    name='forecasting',
    version='0.0.1',
    author='Magnus Mueller',
    license='MIT',
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages( exclude=("tests")),
    url="",
    python_requires='>=3.7',
    include_package_data=True,
    install_requires= [
        'flask>=2.2.2',
        'pandas>=1.3',
        'py4jps>=1.0.26', 
        'requests>=2.28',
        'darts~=0.21.0',
        'configobj>=5.0.6'
    ]
)