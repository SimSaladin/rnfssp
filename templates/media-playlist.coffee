
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
      console.log this
      that = this
      @cont.empty()
      @head.empty()

      if @data["title"]
         @head.text(@data["title"])
      else
         @head.append( $('<i/>').text("Default playlist") )
      if @data.hasOwnProperty("elems")
         $.each @data["elems"], (index, value) ->
            that.cont.append(
               $('<tr/>').append(
                  $('<td/>').append(
                     $('<a class="icon-minus icon-white" />').bind('click', -> that.delete_elem(index-1))
                  )
               ).append( $('<td/>').text(value[0])).append(
                  $('<td title="'+value[1]+'"/>').text(value[1])
               )
            )
      else
         @cont.append($("</i>").text("Something went wrong while loading the playlist.  Try refreshing the page."))

   ### add `what' from `area' to the playlist ###
   to_playlist: (area, what) ->
      that = this
      $.ajax
         url:        '@{PlaylistR "insert"}'
         type:       'POST'
         dataType:   'json'
         data:       JSON.stringify([area, what])
         success:    (x) -> that.handle_new(x)

   ### clear every element from the playlist. ###
   clear_playlist: ->
      $.post('@{PlaylistR "clear"}', handle_new)

   delete_elem: (indeces) ->
      $.post('@{PlaylistR "delete"}', JSON.stringify(indeces), handle_new)

   ### save playlist. title is optional and default is used if == null ###
   save_playlist_custom: (title) ->
      alert "TODO"

$ ->
   ### NOTE: uses a global, window.playlist. ###
   if window.playlist
      alert("Playlist is already active! (only one instance per page allowed)")
      throw "Playlist is already initialized (window.playlist is set)"

   window.playlist = new Playlist $("#%{rawJS contI}"), $("#%{rawJS headI}")
