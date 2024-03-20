from setuptools import setup, find_namespace_packages

setup(
    name='twa',
    version='0.0.1',
    author='Jiaru Bai; Daniel Nurkowski',
    author_email='jb2197@cam.ac.uk; danieln@cmclinnovations.com',
    license='MIT',
    python_requires='>=3.8',
    description="twa is a thin Python wrapper for the TheWorldAvatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_wrapper",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests', 'tests.*']),
    install_requires=['py4j==0.10.9.1','docopt','concurrent_log_handler'],
    include_package_data= True,
    entry_points={
        'console_scripts': [
            'jpsrm = twa.resRegistry.resManager:start',
        ]
     },
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Development Status :: 4 - Beta",
		"Programming Language :: Python :: 3.8",
		"Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
    ]
)
