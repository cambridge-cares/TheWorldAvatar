from setuptools import setup, find_namespace_packages

setup(
    name='obuagent',
    version='0.0.1',
    author='Aleksandar Kondinski; Jiaru Bai',
    author_email='aleksandar.kondinski@cares.cam.ac.uk; jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.9',
    description="",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OBUAgent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    install_requires=['pyderivationagent>=1.4.3', 'rdkit', 'rdflib'],
    extras_require={
        "dev": [
            "testcontainers>=3.4.2",
            "pytest>=6.2.3",
            "pytest-docker-compose>=3.2.1",
            "pytest-rerunfailures>=10.2"
        ],
    },
    include_package_data=True
)
