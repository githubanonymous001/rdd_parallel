# rdd_parallel
Parallel approach for RDD Computation<BR>
Auxiliary resource for paper:<BR>
Brazil's Bolsa Familia and Young Adult Workers: A Parallel RDD Approach to Large Datasets<BR>

Files:<BR>
open-source-rdd-eval.R: script for simulated data generation and evaluation of rdd and rdrobust packages<BR>
graph-analysis.R: graphic analysis of Bolsa Familia Data<BR>
evaluation-par.R: rdd_parallel evaluation - script for comparison between rdd package and its novel parallel version<BR>
rdestimate-par.R: main rdd function, modified for parallel execution<BR>
functions-par.R: inner functions, modified for parallel execution. Includes the main modification in hat_values.ivreg<BR>

Instructions:<BR>
Place all files at a folder named '~/rdd_parallel'
Bolsa Familia data is not provided, due to confidentiality issues, but the tests results are available in Rda format

