---
layout: archive
title: "CV"
permalink: /cv/
author_profile: true
redirect_from:
  - /resume
---

{% include base_path %}

Education
======
* B.Sc. in Business Information Systems, University of Applied Sciences Karlsruhe, 2019
* M.Sc. in Data Science, University of Mannheim, 2022
* Currently: PhD Student and Research Associate, Technical University of Darmstadt
  
Skills
======
* Programming: R, Python, JavaScript, HTML
* Databases: SQL, MongoDB

Publications
======
  <ul>{% for post in site.publications %}
    {% include archive-single-cv.html %}
  {% endfor %}</ul>
  
Teaching
======
  <ul>{% for post in site.teaching %}
    {% include archive-single-cv.html %}
  {% endfor %}</ul>
