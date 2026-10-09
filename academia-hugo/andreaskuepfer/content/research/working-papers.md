+++
widget = "pages"
headless = true
active = true
weight = 20

title = "Working Papers"
subtitle = ""

[content]
  page_type = "publication"
  count = 0
  offset = 0
  # Sort by the `wp_rank` front matter of each paper (R&R, then Under Review, then the rest)
  sort_by = "Params.wp_rank"
  order = "asc"

  [content.filters]
    tag = ""
    category = ""
    publication_type = "3"
    exclude_featured = false

[design]
  view = 1

[design.background]
  gradient_start = "DeepSkyBlue"

[advanced]
 css_style = ""
 css_class = ""
+++
