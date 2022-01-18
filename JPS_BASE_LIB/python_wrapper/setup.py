from setuptools import setup, find_packages
import os.path

setup(
    name='py4jps',
    version='1.0.14',
    author='Daniel Nurkowski',
    author_email='danieln@cmclinnovations.com',
    license='MIT',
    python_requires='>=3.5',
    description="Py4jps is a thin Python wrapper for the TheWorldAvatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/python_wrapper",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=('tests')),
    install_requires= ['py4j==0.10.9.1','docopt'],
    include_package_data= True,
    entry_points={
        'console_scripts': [
            'jpsrm = py4jps.resRegistry.resManager:start',
        ]
     },
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Development Status :: 4 - Beta",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
		"Programming Language :: Python :: 3.8",
		"Programming Language :: Python :: 3.9"
    ]
)