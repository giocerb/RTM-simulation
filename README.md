**Project Overview**

This repository documents a Risk Terrain Modeling (RTM) simulation applied to the city of Paris. This project is submitted as part of the evaluation for the Data and Algorithms for Public Policy course at Sciences Po.

The objective is to construct a predictive spatial model that identifies environmental risk factors associated with urban incidents, primarily serving as a proof-of-concept inspired by operational RTM methods, such as those used by the Metropolitan Police in London.

**Information**

The RTM simulation is performed on a 200m×200m spatial grid covering the Paris arrondissements.

The Target Variable are  public service reports from the "Dans Ma Rue" application.

The model uses distance metrics to the following location types as risk factors: nightclubs, fast food locations, clubs and police stations.

The statistical model used is  Negative Binomial Generalized Linear Model (GLM.NB). This approach is used because incident count data is typically over-dispersed (variance exceeds the mean), which is standard practice in applied RTM. The model uses an area offset to predict the rate or density of incidents.

**Acknowledgement**

This work follows the conceptual and coding framework established in "Public Policy Analytics: Code & Context for Data Science in Government" by Ken Steif, Ph.D. Significant parts of this code are co-authored by Gemini and Claude Sonnet 4.5.

Author: Giovanni Cerboni

Disclaimer: All model predictions are purely speculative, intended for academic demonstration, and should not be used for operational public policy decisions.
