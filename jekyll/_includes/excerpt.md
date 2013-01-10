<div class="post">
<h2><a id="{{ post.id }}" href="{{ post.url }}">{{ post.title }}</a></h2>
{% if post.comment %}
<i>{{ post.comment }}</i>
{% endif %}
{{ post.content | split: "<!-- end excerpt -->" | first }}
<a href="{{ post.url }}">... read more ...</a>
<p id="footer">
{% for tag in post.tags %}
	<span id="tag"> {{ tag }} </span>
{% endfor %}	 
{{ post.date | date_to_string }}
<br/>
&raquo; <a href="#all_posts">top</a>
</p>
</div>
