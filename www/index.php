
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<?php
	function TableOfContents($depth)
	/*AutoTOC function written by Alex Freeman
	* Released under CC-by-sa 3.0 license
	* http://www.10stripe.com/  */
	{
	$filename = __FILE__;
	//read in the file
	$file = fopen($filename,"r");
	$html_string = fread($file, filesize($filename));
	fclose($file);
 
	//get the headings down to the specified depth
	$pattern = '/<h[1-'.$depth.']*[^>]*>.*?<\/h[1-'.$depth.']>/';
	$whocares = preg_match_all($pattern,$html_string,$winners);
 
	//reformat the results to be more usable
	$heads = implode("\n",$winners[0]);
	$heads = str_replace('<a name="','<a href="#',$heads);
	$heads = str_replace('</a>','',$heads);
	$heads = preg_replace('/<h([1-'.$depth.'])[^>]*>/','<li class="toc$1">',$heads);
	$heads = preg_replace('/<\/h[1-'.$depth.']>/','</a></li>',$heads);
 
	//plug the results into appropriate HTML tags
	$contents = '<div id="toc"> 
	<p id="toc-header"><strong>Contents</strong></p>
	<ul>
	'.$heads.'
	</ul>
	</div>';
	echo $contents;
	}
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr>
  <td></td>
  <td align = "justify">
    <h1 align = "center">Welcome to <tt>softclassval</tt>!</h1>
		<p><tt>softclassval</tt> provides classifier performance measures such as sensitivity and specifictity for soft classification (i.e. reference labels and/or classifier output can take any class membership value between 0 and 1).</p>
  </td>
  <td align ="right"><a href="/"><img src="<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td>
</tr>
</table>

<?php TableOfContents (1); ?>

<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->
<p> The package is publicly accessible from the
  <a href="http://<?php echo $domain; ?>/scm/?group_id=<?php echo $group_id; ?>">SVN repository</a>.
</p>
<p> There is also a <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">R-forge project summary page</a>. </p>

<h1><a name="Installation">Installation</a></h1>
<h2><a name="Installation in R">Inside R</a></h2>
<ul>
	<li>To install the latest stable version from CRAN, type in R: 
		<pre>install.packages("softclassval")</pre>
	</li>
	<li>To install the latest nightly build (development version), type in R: 
		<pre>install.packages("softclassval",repos="http://R-Forge.R-project.org")</pre>
	</li>
</ul>
<h2><a name="Installation archive">From source or binary archives automatically built by r-forge (nightly build)</a></h2>
<ol>
<li><a href="http://<?php echo $domain; ?>/R/?group_id=<?php echo $group_id; ?>">Download the appropriate file</a>.</li>
<li>Install the package:<br/>
<tt>R CMD INSTALL <i>filename</i></tt></li>
</ol>
Please note that the automatic windows build on r-forge is often one or two days behind. Windows
users: do not unzip the archive.

<h2><a name="Installation svn">From svn source</a></h2>
<ol>
  <li>get an svn checkout using your favourite svn client program, or<br/>
    <tt>svn checkout svn://svn.r-forge.r-project.org/svnroot/softclassval/pkg</tt>
  </li>
  <li> Install the package:<br/>
    <tt>R CMD INSTALL <i>pkg-directory</i></tt> <br/>
    where <tt><i>pkg-directory</i></tt> is the directory where the svn checkout went (default is ./pkg).
  </li>
</ol>
<h1><a name="Presentations about softclassval">Presentations about softclassval</a></h1>
<ul>
<?php include ('about-softclassval.html') ?>
</ul>
<h1><a name="Contact">Contact</a></h1>
<p>
Claudia Beleites<br/>
Dept. of Spectroscopy and Microscopy,<br/> 
IPHT Jena<br/>
e-mail: claudia dot beleites at ipht minus jena dot de
</p>
</body>
</html>
