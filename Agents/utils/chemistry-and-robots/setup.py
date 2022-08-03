from setuptools import setup, find_packages

setup(
    name='chemistry_and_robots',
    version='0.0.1',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.7',
    description="chemistryandrobots contains dataclasses and sparql queries for concepts related to chemistry-and-robots as part of The World Avatar project.",
    # url="https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/utils/chemistry-and-robots",
    # long_description=open('README.md').read(),
    # long_description_content_type="text/markdown",
    packages=find_packages(exclude=['tests','tests.*']),
    install_requires=['pyderivationagent>=1.1.0', 'pydantic==1.9.0', 'xlrd', 'xlwt'],
    extras_require={
        "dev": [
            "testcontainers>=3.4.2",
            "pytest>=6.2.3",
        ],
    },
    include_package_data= True
)
