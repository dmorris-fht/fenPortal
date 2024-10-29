// DT row button click handler

function get_id(clicked_id) {
    Shiny.setInputValue("surveys-current_id", clicked_id, {priority: "event"});
    
    Shiny.setInputValue("queryRecords-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("enterRecords-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("importRecords-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("vegManage-current_id", clicked_id, {priority: "event"});
        
    Shiny.setInputValue("dataSharing-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("installsManage-current_id", clicked_id, {priority: "event"});
    Shiny.setInputValue("loggersManage-current_id", clicked_id, {priority: "event"});
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

// Old keyboard shortcut for add record in enterRecords module

/*
var down = {};
$(document).keydown(function(e) {
    down[e.keyCode] = true;
}).keyup(function(e) {
    if (down[18] && down[65]) {
      $("#enterRecords-add_record").click()
    }
    down[e.keyCode] = false;
});
*/

// Query records map select 

$("#queryRecords-resultsMap").on('mousemove',function(event){
    if($('.leaflet-draw-draw-rectangle').hasClass('leaflet-draw-toolbar-button-enabled') == true){
        Shiny.setInputValue("queryRecords-map_mode", 'draw', {priority: "event"});
    }else{
        Shiny.setInputValue("queryRecords-map_mode", 'click', {priority: "event"});
    }
    
  }
);

// Open base65 image in new

function openImg(data){
  let w = window.open('about:blank');
  let image = new Image();
  image.src = data;
  image.style.maxHeight = "100%";
  setTimeout(function(){
    w.document.getElementsByTagName('body')[0].innerHTML = image.outerHTML;
  }, 0);
}


