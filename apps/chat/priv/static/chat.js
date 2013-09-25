var socket;

function add_message(message) {
    $('#messages').append('<p></p>').children().last().text(message);
}

function read_message_input() {
    return $('#message').val();
}

function connect_to_chat() {

    socket = new WebSocket("ws://localhost:8080/ws");

    socket.onopen = function() {
        add_message("Connected.")
    };

    socket.onmessage = function(event) {
        add_message(event.data);
    };

    socket.onclose = function() {
        add_message("Connection closed.");
    };
}

function send_message(e) {
    var message = read_message_input();
    add_message(message);
    socket.send(message);
    $('#message').val("");
}

$(document).ready(function() {
    connect_to_chat();
    $('#send-button').click(send_message);
})
