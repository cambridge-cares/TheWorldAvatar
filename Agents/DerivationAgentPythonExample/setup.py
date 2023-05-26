from setuptools import setup, find_namespace_packages

# This file is used to install the agent as a python package
# For more information, see https://docs.python.org/3/distutils/setupscript.html

setup(
    # NOTE Ideally, the package name should be the same as the name of the directory containing source codes
    # NOTE Avoid using hyphens in the package name, as it will cause problems when importing the package
    # NOTE Make package name to be specific enough to avoid name conflicts with other packages
    name='derivationagentpythonexample',
    version='0.0.1',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.8',
    description="derivationagentpythonexample is an example of derivation agent in python as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DerivationAgentPythonExample",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    # NOTE find_namespace_packages() is recommended for all python3 projects
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    # NOTE install_requires is used to specify the dependencies of the package, their versions should be kept as loose as possible
    # NOTE The dependencies should be specified in the requirements.txt file, see README.md for more information
    install_requires=['pyderivationagent>=1.4.3', 'pydantic==1.9.0'],
    extras_require={
        "dev": [
            "testcontainers>=3.4.2",
            "pytest>=6.2.3",
            "pytest-docker-compose>=3.2.1",
            "pytest-rerunfailures>=10.2"
        ],
    },
    # NOTE use include_package_data to include non-python files in the package, this should be used with MANIFEST.in and find_namespace_packages() above
    include_package_data=True
)
