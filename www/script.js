// DT row button click handler

function get_id(clicked_id) {
    Shiny.setInputValue("loggersManage-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("surveys-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("queryRecords-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("enterRecords-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("importRecords-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("dataSharing-current_id", clicked_id, {priority: "event"});
  }

// Login controls

$(document).keyup(function(event) {
    if (($("#loginForm-username").is(":focus") || $("#loginForm-password").is(":focus")) && (event.key == "Enter")) {
        $("#loginForm-loginButton").click();
    }
});

$("#loginForm-loginButton").click(
  function(){
    $("#username").html(
      $("#loginForm-username").val()
    )  
  }
)

// Record entry controls

$("#enterRecords-add_record").click(
  function(){
  $("#enterRecords-taxon_nbn-selectized").click()
  }
)

var down = {};
$(document).keydown(function(e) {
    down[e.keyCode] = true;
}).keyup(function(e) {
    if (down[18] && down[65]) {
      $("#enterRecords-add_record").click()
    }
    down[e.keyCode] = false;
});