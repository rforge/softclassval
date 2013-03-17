---
layout: paper
title: 'Validation of Soft Classification Models using Partial Class Memberships: An Extended Concept of Sensitivity &amp; Co. applied to Grading of Astrocytoma Tissues'
tags: 
- literature
- reference
- article
authors: 'C. Beleites, R. Salzer and V. Sergo'
journal: Chemom. Intell. Lab. Syst.
doi: '10.1016/j.chemolab.2012.12.003'
number: 122
pages: 12 - 22
year: 2013
arxiv: 1301.0264
comment: Paper deriving and explaining the theory behind <tt>softclassval</tt>
keywords: soft classification, partial class membership, classifier validation, borderline cases, ambiguous reference, biomedical spectroscopy
key: Beleites2013a
---
We use partial class memberships in soft classification to model uncertain labelling
and mixtures of classes. Partial class memberships are not restricted to predictions, but may also
occur in reference labels (ground truth, gold standard diagnosis) for training and validation data.

<!-- end excerpt -->
Classifier performance is usually expressed as fractions of the confusion matrix, like sensitivity,
specificity, negative and positive predictive values. We extend this concept to soft classification
and discuss the bias and variance properties of the extended performance measures. Ambiguity in
reference labels translates to differences between best-case, expected and worst-case performance.

We show a second set of measures comparing expected and ideal performance which is closely related to
regression performance, namely the root mean squared error RMSE and the mean absolute error MAE.  All
calculations apply to classical crisp as well as to soft classification (partial class memberships as
well as one-class classifiers).

The proposed performance measures allow to test classifiers with actual borderline cases. In
addition, hardening of e.g. posterior probabilities into class labels is not necessary, avoiding the
corresponding information loss and increase in variance.

We implemented the proposed performance measures in R package “softclassval” which is available from
CRAN and at [softclassval.r-forge.r-project.org](http://softclassval.r-forge.r-project.org).

Our reasoning as well as the importance of partial memberships for chemometric classification is
illustrated by a real-word application: astrocytoma brain tumor tissue grading (80 patients, 37 000
spectra) for finding surgical excision borders. As borderline cases are the actual target of the
analytical technique, samples which are diagnosed to be borderline cases must be included in the
validation.  
