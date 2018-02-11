<h1>Multi-Attribute Decision Making - R</h1>
<h3> <em>A highly robust tool for decision analysts to obtain quick and meaningful results on any raw decision matrix. </em></h3> 

<h2>How to install</h2>
<p>The easiest way to install <code>madmR</code> is through R the <code>devtools</code> package in R: </p>
<code>devtools::install_github("conradbm/madmR", upgrade_dependencies = FALSE)</code><br><br>
<p>Installation directly through <code>CRAN</code> can be done in the traditional manner: </p>
<code>install.packages('madmR')</code><br>
<code>library(madmR)</code>
<h2> Core functionality </h2>
<p>Version <code>0.0.0.1000</code></p>
<ul> 
<li>Technique for Order of Preference by Similarity to Ideal Solution (<strong>TOPSIS</strong>)</li>
<li>Multi-Attribute Utility Theory (<strong>MAUT</strong>)
    <ul> 
        <li> <em>Risk Agnostic</em> Scaling (<code>Linear</code>) </li>
        <li> <em>Risk Averse Scaling</em> (<code>Exponential</code>) </li>
        <li> <em>Risk Prone Scaling</em> (<code>Logarithmic</code>) </li>
    </ul>
</li>
<li> Sensitivity Analysis including:
    <ul> 
        <li> Specific or <code>any number of examinable attributes</code> </li>
        <li> Specific or exhaustive <code>sensitivity searching</code> to determine tailored importance weighting combinations</li>
        <li> Specific or <code>any number of madmR methods</code> (see above) supported in the latest version of the package </li>
        <li> Optional user defined <code>step sizes</code> allowing variable fidelity </li>
        <li> Optional user defined <code>window sizes</code> allowing only thresholds of sensitivty interest </li>
        <li> Optional user defined <code>splitting of percentages</code> as weights fluctuate accross the sensitivity, cultivating human interaction and bias to make a more rich and realistic analysis</li>
        <li> Provision to <code>as much analysis output</code> as generated, fostering analysts independent exploration via output in a well known and exportable format</li>
    </ul>
</li>
</ul>
<h2>References:</h2>
<ul> 
    <li><em>Multiple Attribute Decision Making, An Introduction</em> -- K. Paul Yoon and Ching-Lai Hwang</li>
    <li><em>Multi-Criteria Decision Analysis, Methods and Software</em> -- Alessio Ishizaka and Philippe Nemery</li>
</ul>
