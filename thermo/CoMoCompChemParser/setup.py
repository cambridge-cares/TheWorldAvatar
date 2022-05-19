from setuptools import setup, find_packages

setup(
    name='compchemparser',
    version='1.0.2',
    author='Daniel Nurkowski and Angiras Menon',
    author_email='danieln@cmclinnovations.com',
    license='MIT',
    long_description=open('README.md').read(), # Optional
    # When your source code is in a subdirectory under the project root, e.g.
    # `src/`, it is necessary to specify the `package_dir` argument.
    packages=find_packages(exclude=("tests")),
    long_description_content_type="text/markdown",
    description="The `compchemparser` package provides parsers to convert quantum chemistry log files into a more condensed JSON format.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/thermo/CoMoCompChemParser",
    # Specify which Python versions you support pip install' will check this
    # and refuse to install the project if the version does not match.
    python_requires='>=3.5, <4',
    include_package_data=True,
    # If there are data files included in your packages that need to be
    # installed, specify them here.
    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # `pip` to create the appropriate form of executable for the target
    # platform.
    install_requires= ["docopt", "cclib"],
    # For example, the following would provide a command called `sample` which
    # executes the function `main` from this package when invoked:
    entry_points={  # Optional
        'console_scripts': [
             'ccparse=compchemparser.main:main'
        ],
    },
)