$(function () {
    console.log("establishing shiny-ws-heartbeat-mechanism")
    var socket_timeout_interval;
    var n = 0;
    // ws-heartbeat fix
    // kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
    $(document).on('shiny:connected', function (event) {
        socket_timeout_interval = setInterval(function () {
            Shiny.onInputChange('heartbeat', n++)
        }, 9000);
    });

    $(document).on('shiny:disconnected', function (event) {
        clearInterval(socket_timeout_interval)
    });

});
