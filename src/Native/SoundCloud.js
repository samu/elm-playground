Elm.Native.SoundCloud = {};
Elm.Native.SoundCloud.make = function(localRuntime) {
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.SoundCloud = localRuntime.Native.SoundCloud || {};
  if (localRuntime.Native.SoundCloud.values)
  {
    return localRuntime.Native.SoundCloud.values;
  }

  var players = {};

  function doEmbed(id) {
    setTimeout(function() { document.getElementById(id).innerHTML = players[id]; }, 400);
    // document.getElementById(id).innerHTML = players[id];
  }

  function test(values) {
    var id = values.id;
    var url = values.url;
    console.log(values);
    console.log("soundcloud requested for id ", id);
    if (players[id] != undefined) {
      doEmbed(id);
    } else {
      SC.oEmbed(url, {
        maxheight: "100"
      }).then(function(embed) {
        console.log(embed);
        players[id] = embed.html
        doEmbed(id);
      });
    }

    return "";
  }

  function customNode() {
    return document.createElement("BUTTON");
    // return "abc";
  }

  return localRuntime.Native.SoundCloud.values = {
    test: test,
    customNode: customNode
  };
};
