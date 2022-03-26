from setuptools import setup, find_packages

setup(
    name="chemaboxwriters",
    version="1.0.0",
    author="Daniel Nurkowski",
    author_email="",
    license="MIT",
    python_requires=">=3.7",
    packages=find_packages(exclude=("tests")),
    long_description=open("README.md").read(),
    install_requires=[
        "docopt",
        "py4jps>=1.0.7, <=1.0.14",
        "pubchempy",
        "entityrdfizer==1.0.5",
        "compchemparser==1.0.2",
        "pyuploader==1.1.0",
        "SPARQLWrapper",
        "PyYAML",
    ],
    include_package_data=True,
    entry_points={
        "console_scripts": ["aboxwriter=chemaboxwriters.abox_driver:start"],
    },
)
