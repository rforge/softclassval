---
layout: default
title: <tt>softclassval</tt> FAQ
---
# Frequently asked Questions

{% for post in site.posts %}
{% for tag in post.tags %}
{% if tag == 'faq' %}

- {{ post.date | date_to_string }} &raquo; <a href="{{ post.url }}">{{ post.title }}</a>

{% endif %}
{% endfor %}
{% endfor %}


# Excerpts
{% for post in site.posts %}
{% for tag in post.tags %}
{% if tag == 'faq' %}
{% include excerpt.md %}
{% endif %}
{% endfor %}
{% endfor %}
