// Copyright (C) 2026 Mitchell Dalvi Rosen
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General
// Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License along with this program. If not, see
// <https://www.gnu.org/licenses/>.

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
