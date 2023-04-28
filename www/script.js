function get_id(clicked_id) {
     Shiny.setInputValue("loggersManage-current_id", clicked_id, {priority: "event"});
}

$(document).keyup(function(event) {
    if (($("#loginForm-username").is(":focus") || $("#loginForm-password").is(":focus")) && (event.key == "Enter")) {
        $("#loginForm-loginButton").click();
    }
});