var flags = JSON.parse(localStorage.getItem("model"));
var app = Elm.Main.init({ flags: flags });

app.ports.saveModel.subscribe(function(data) {
    localStorage.setItem("model", JSON.stringify(data));
});

app.ports.selectAll.subscribe(function(id) {
    requestAnimationFrame(function() {
        var el = document.getElementById(id);
        if (el) {
            el.focus();
            el.select();
        }
    });
});

document.addEventListener("visibilitychange", function() {
    if (document.visibilityState === "visible") {
        app.ports.documentBecameVisible.send(null);
    }
});
