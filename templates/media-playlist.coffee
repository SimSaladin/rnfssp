
class Playlist
   constructor: (@cont, @head) ->
      that = this
      @cont.ready(-> that.load_playlist.apply(that) )

   data: {}

   handle_new: (data) ->
      if data[0] is 0
         @data = data[1]
         @update_playlist()
      else
         alert("Playlist action failed: " + data[1])

   set_data: (x) ->
      @data = if (x[0] == 0) then x[1] else {}

   ### Loads active playlist, or creates a new playlist. ###
   load_playlist: ->
      that = this
      $.ajax
         url:       '@{PlaylistR "select"}'
         type:      'POST'
         dataType:  'json'
         success:   (x) -> that.set_data(x)
         error:     -> alert("playlist loading failed!")
         complete:  -> that.update_playlist()

   ### redraw playlist and add default title if none is specified. ###
   update_playlist: ->
      that = this
      @cont.empty()
      @head.empty()

      if @data["title"]
         @head.text(@data["title"])
      else
         @head.append( $('<i/>').text("Default playlist") )
      if @data.hasOwnProperty("elems")
         $.each @data["elems"], (index, value) -> that.add_entry(index, value)
      else
         @cont.append($("</i>").text("Something went wrong while loading the playlist.  Try refreshing the page."))

   add_entry: (index, value) ->
      that = this
      @cont.append(
         $('<div class="entry"/>').append(
            $('<div class="controls">').append(
               $('<a class="icon-minus href=#" />').bind('click', -> that.delete_elem(index-1))
            )
         ).append( $('<span title="'+value[1]+'"/>').text(value[1].split("/").pop())
         ).append( $('<span class="misc" />').text(value[0])
         )
      )

   ### add `what' from `area' to the playlist ###
   to_playlist: (area, paths) ->
      that = this
      $.ajax
         url:        '@{PlaylistR "insert"}'
         type:       'POST'
         dataType:   'json'
         data:       JSON.stringify([area, paths])
         success:    (x) -> that.handle_new(x)

   add_from_element_contents: (elements, area) ->
       xs = []
       $.each elements, -> xs.push this.innerText
       @to_playlist area, xs

   ### clear every element from the playlist. ###
   clear_playlist: ->
      that = this
      $.ajax
         url:       '@{PlaylistR "clear"}'
         type:      'POST'
         dataType:  'json'
         success:    (x) -> that.handle_new(x)

   delete_elem: (indeces) ->
      that = this
      $.ajax
         url:        '@{PlaylistR "delete"}'
         type:       'POST'
         dataType:   'json'
         data:       JSON.stringify(indeces)
         success:    (x) -> that.handle_new(x)

   ### save playlist. title is optional and default is used if == null ###
   save_playlist_custom: (title) ->
      alert "TODO"

$ ->
   ### NOTE: uses a global, window.playlist. ###
   if window.playlist
      alert("Playlist is already active! (only one instance per page allowed)")
      throw "Playlist is already initialized (window.playlist is set)"

   window.playlist = new Playlist $("#%{rawJS contI}"), $("#%{rawJS headI}")
