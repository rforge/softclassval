---
layout: default
title: <tt>softclassval</tt> News
---

<ul>
{% for post in site.posts%}
{% for tag in post.tags %}
{% if tag == 'news' %}
<li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ post.url }}">{{ post.title}}
</a>
</li>
{% endif %}
{% endfor %}
{% endfor %}
</ul>
