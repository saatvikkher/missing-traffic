# Measuring the Impact of Missingness on Racial Bias in Traffic Stop Data

Official implementation for
<pre>
<b>Measuring the Impact of Missingness on Racial Bias in Traffic Stop Data</b>
<a href="https://saatvikkher.github.io/">Saatvik Kher</a>, <a href="https://hardin47.netlify.app/">Johanna Hardin</a>, <a href="">Amber Lee</a>
<a href="">link</a> 
</pre>

<p align="center">
 <img src="figures/DAG.png" alt="DAG" width="650"/>
</p>


## Setup environment

We use R Studio (2024.04.2+764).

We user docker to run our sensitivity analysis for bounds on the ATE (Knox et al., 2020).

```bash
sudo docker run -p 8888:8888 -it gjardimduarte/autolab:v4
```

All datasets can be found at [https://openpolicing.stanford.edu/data/](https://openpolicing.stanford.edu/data/). We download the datasets as RMD files and save them in `data/`

## Replicate Results
### Diagnosing non-MCAR (Section [])

### Outcome Test (Section [])

- `outcome_test_plots.R` reproduces Figures [] for the outcome test
- `outcome_test_disparity_table.R` reproduces Table []

The files can also be run in parallel using `outcome_test_plots_parallel.R`

### Bounds on ATE (Section [])

- Run the file `autobounds_script.py` in the Docker container to produce files in `ate_results/`.
- `ate_plots.R` reproduces Figures []
