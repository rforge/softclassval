---
layout: default
title: Download & Installation
---
#Download & Installation

There are several ways to obtain `softclassval`. 

* [Install stable version](#cran)
* [Install nightly build](#rforge)
* [Install from .tar.gz or .zip archive](#tgz)
* [Clone svn repository](#svn)

<div class = "post">

<h2 id="cran">Install stable version</h2>

<p>Inside R type:

{% highlight rconsole %}
> install.packages ("softclassval")
{% endhighlight %}

to install the stable version from <a href="http://cran.r-project.org/web/packages/softclassval/index.html">CRAN</a>.</p>

&raquo; <a href="#download__installation">top</a>
</div>

<div class = "post">
<h2 id="rforge">Install nightly build</h2>
To install the nightly build (development version) from <a href="https://r-forge.r-project.org/R/?group_id=891">R-Forge</a>, inside R type:

{% highlight rconsole %}
> install.packages ("softclassval", repos="http://r-forge.r-project.org")
{% endhighlight %}

Nightly builds of Windows binaries are sometimes a few day delayed.

<br/>&raquo; <a href="#download__installation">top</a>

</div>

<div class = "post">
<h2 id="tgz">Install from .tar.gz or .zip archive</h2>
<p>Source (.tar.gz) and Windows binary (.zip) packages can be installed by </p>

<h3>command line:</h3>
<p>
{% highlight console %}
$ R CMD INSTALL filename.tar.gz
{% endhighlight %}
</p>

<h3>in <a href="http://www.rstudio.org/">RStudio</a></h3>
<p>
click "Install Packages" in the package tab and then select "Install from: Package Archive"
<br/>

<i>Do not unpack the archive.</i>
</p>
&raquo; <a href="#download__installation">top</a>
</div>

<div class = "post">
<h2 id ="svn">Clone svn repository</h2>
<h3><tt>svn</tt> users:</h3>

<p>checkout using your favourite <tt>svn</tt> client program, or  
{% highlight console %}
$ svn checkout svn://svn.r-forge.r-project.org/svnroot/softclassval/pkg
{% endhighlight %}
</p>       

<h3><tt>git</tt> users:</h3>
<p>
<tt>git-svn</tt> is highly recommended:

{% highlight console %}
$ git svn clone  svn://svn.r-forge.r-project.org/svnroot/softclassval/pkg \
> softclassval
{% endhighlight %}

<code>-rHEAD</code> makes a shallow copy retrieving only the most recent revision.
</p>
<h3>Install from repository clone</h3>
<p>
Build and install the package:  
{% highlight console %}
$ R CMD build pkg/
$ R CMD INSTALL softclassval_0.xx-yyyymmdd.tar.gz
{% endhighlight %}
</p>
&raquo; <a href="#download__installation">top</a>
</div>
<!---
<div class = "post">
<h2><a name=""></a></h2>
<p>

</p>&raquo; <a href="#download__installation">top</a>
</div>
--->
