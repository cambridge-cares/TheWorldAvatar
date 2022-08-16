import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="agentlogging",
    version="1.0.0",
    author="CMCL Innovations",
    author_email="support@cmclinnovations.com",
    description="Agent Logging Library",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/cambridge-cares/TheWorldAvatar",
    packages=setuptools.find_packages(),
    package_data={"": ["*.conf"]},
    classifiers=[
        "Programming Language :: Python :: 3",
        "Operating System :: OS Independent",
    ],
    install_requires=[
           "concurrent_log_handler",
    ]
)
