---
layout: default
title: <tt>softclassval</tt> FAQ
---
# NEWS

<ul>
{% for post in site.posts %}
{% for tag in post.tags %}
{% if tag == 'news' %}
<li><span>{{ post.date | date_to_string }}</span> &raquo; 
	 <a href="{{ post.url }}">{{ post.title }}</a>
    </li>
{% endif %}
{% endfor %}
{% endfor %}
</ul>

# Excerpts
{% for post in site.posts %}
{% for tag in post.tags %}
{% if tag == 'news' %}
{% include excerpt.md %}
{% endif %}
{% endfor %}
{% endfor %}
