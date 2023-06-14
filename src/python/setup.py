
import setuptools
import os

version = os.environ.get('ALEPH_VERSION', '0.9.0')

with open("./README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="aleph-lang",
    version=version,
    author="Andrés Paz",
    author_email="anpaz@cs.washington.edu",
    description="Python bindings for aleph, a high level programming model that can be embedded into classical languages to develop large scale quantum hybrid applications, without the quantum mechanics.",
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