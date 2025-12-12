# C-14 Optimisation Project

## Overview

This project focuses on **parameter estimation for Carbon-14 (C-14) decay modeling** using two numerical optimization methods implemented in **MATLAB**. The goal is to fit a C-14 decay model to experimental data by minimizing the error between the model and observations.

The work includes both **model formulation** and **algorithmic implementation**, allowing a comparison of performance and convergence behavior between different optimization strategies.

## Contents

The repository contains the following MATLAB files:
- modelisation_C14.m : defines the C-14 decay model, constructs the objective function, and evaluates model accuracy based on input data.
- algo_Gauss_Newton.m : Implements the **Gauss-Newton algorithm**, a gradient-based optimization method tailored for least-squares problems. This version leverages the structure of the residuals to accelerate convergence in well-conditioned scenarios.
- algo_newton.m : Implements the **full Newton method**, using the Hessian matrix for second-order optimization. This approach is more general and robust, especially when the Gauss-Newton approximation is insufficient.

## Notes

- My university provides MATLAB with specialized academic extensions.
- As a result, some scripts may **not run** on the standard free or trial versions of MATLAB.