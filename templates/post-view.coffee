$ ->
   selectors = [".content-align " + n for n in ["h1", "h2", "h3", "h4", "h5", "h6"]]
   $(selectors.reduce (x, y) -> x + ", " + y).each (_, x) ->
      url = document.URL.replace(/(#[^#]*)?$/, "#") + $(x).attr("id")
      $(x).append $("<a href=" + url + "/>").append $("<span class=icon-pin title='Link here.'/>")
