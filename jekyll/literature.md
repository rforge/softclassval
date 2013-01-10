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
{% if tag == 'reference' %}
{% for tag in post.tags %}
{% if tag == 'article' %}
<li class="literature"><a href="{{ post.url }}">{{ post.authors}}:<br/>
<span class="title"> {{ post.title }} </span><br/>
{{ post.journal }}, <strong> {{ post.number }} </strong> ({{ post.year}}), {{ post.pages }}.<br/>
{{ post.comment }}
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
<li class="literature"><a href="{{ post.url }}">{{ post.authors}}:<br/>
<span class="title"> {{ post.title }} </span><br/>
{{ post.meeting }}, {{ post.date  | date: "%b %d <strong>%Y</strong>"}}.
</a>
</li>
{% endif %}
{% endfor %}
{% endif %}
{% endfor %}
{% endfor %}
</ul>

