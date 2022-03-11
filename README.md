# Antibody-and-antiviral-deployment

This is the code and configuration for an OpenSAFELY analysis of antibody and antiviral deployment via NHS COVID-19 Medicine Delivery Units. An accompanying pre-print [_Trends, regional variation and clinical characteristics of recipients of antivirals and neutralising monoclonal antibodies for non-hospitalised COVID-19: a descriptive cohort study of 23.4 million people in OpenSAFELY_ is available on MedRxi](https://doi.org/10.1101/2022.03.07.22272026).

You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/Antibody-and-antiviral-deployment)

* The paper is [here]()
* Raw model outputs, including charts, crosstabs, etc, are in `released_outputs/`
* If you are interested in how we defined our variables, take a look at the [study definition](analysis/study_definition.py); this is written in `python`, but non-programmers should be able to understand what is going on there
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).
* Developers and epidemiologists interested in the framework should review [the OpenSAFELY documentation](https://docs.opensafely.org)

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 
