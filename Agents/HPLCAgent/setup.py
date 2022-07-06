from setuptools import setup, find_packages

setup(
    name='hplcagent',
    version='0.0.1',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.7',
    description="hplcagent is capable of monitoring the local report folder of the configured HPLC and uploading the new reports to knowledge graph as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/HPLCAgent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=('tests')),
    install_requires=['pyderivationagent', 'pandas', 'pydantic'
    # 'agentlogging @ git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils'
    # 'chemistry_and_robots @ git+https://github.com/cambridge-cares/TheWorldAvatar@134-dev-lab-equipment-digital-twin#subdirectory=Agents/utils/chemistry-and-robots'
    ],
    extras_require={
        "dev": [
            "testcontainers>=3.4.2",
            "pytest>=6.2.3",
            "pytest-docker-compose>=3.2.1",
            "pytest-rerunfailures>=10.2"
        ],
    },
    include_package_data= True
)
