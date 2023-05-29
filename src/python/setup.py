
import setuptools
import os

version = os.environ.get('ALEPH_VERSION', '0.0.0.1')

with open("../../README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="aleph-lang",
    version=version,
    author="Andres Paz",
    author_email="anpaz@cs.washington.edu",
    description="A programming model to develope large scale quantum hybrid applications.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/anpaz/aleph",
    packages=setuptools.find_namespace_packages(include=["aleph_lang"]),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires=[
        'requests',
    ]
)