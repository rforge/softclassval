---
layout: default
title: Literature about softclassval
---

If you use `softclassval`, please cite it.  
{% highlight rconsole%}
> citation ("softclassval")
{% endhighlight %}
gives the citations both in readable text and BibTeX.

### Papers about `softclassval`:

<ul>
{% for post in site.posts%}
{% for tag in post.tags %}
{% if tag == 'application' %}
{% for tag in post.tags %}
{% if tag == 'article' %}
<li><a href="{{ post.url }}">{{ post.authors}}:<br/>
<i> {{ post.title }} </i><br/>
{{ post.journal }}, <strong> {{ post.number }} </strong> ({{ post.year}}), {{ post.pages }}.
</a>
</li>
{% endif %}
{% endfor %}
{% endif %}
{% endfor %}
{% endfor %}
</ul>

### Presentations about `softclassval`:

<ul>
{% for post in site.posts%}
{% for tag in post.tags %}
{% if tag == 'reference' %}
{% for tag in post.tags %}
{% if tag == 'presentation' %}
<li><a href="{{ post.url }}">{{ post.authors}}:<br/>
<i> {{ post.title }} </i><br/>
<strong> {{ post.meeting }} </strong>, {{ post.date }}.
</a>
</li>
{% endif %}
{% endfor %}
{% endif %}
{% endfor %}
{% endfor %}
</ul>

