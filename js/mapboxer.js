!function(t) {
    var e = {};
    function n(o) {
        if (e[o])
            return e[o].exports;
        var r = e[o] = {
            i: o,
            l: !1,
            exports: {}
        };
        return t[o].call(r.exports, r, r.exports, n),
        r.l = !0,
        r.exports
    }
    n.m = t,
    n.c = e,
    n.d = function(t, e, o) {
        n.o(t, e) || Object.defineProperty(t, e, {
            enumerable: !0,
            get: o
        })
    }
    ,
    n.r = function(t) {
        "undefined" != typeof Symbol && Symbol.toStringTag && Object.defineProperty(t, Symbol.toStringTag, {
            value: "Module"
        }),
        Object.defineProperty(t, "__esModule", {
            value: !0
        })
    }
    ,
    n.t = function(t, e) {
        if (1 & e && (t = n(t)),
        8 & e)
            return t;
        if (4 & e && "object" == typeof t && t && t.__esModule)
            return t;
        var o = Object.create(null);
        if (n.r(o),
        Object.defineProperty(o, "default", {
            enumerable: !0,
            value: t
        }),
        2 & e && "string" != typeof t)
            for (var r in t)
                n.d(o, r, function(e) {
                    return t[e]
                }
                .bind(null, r));
        return o
    }
    ,
    n.n = function(t) {
        var e = t && t.__esModule ? function() {
            return t.default
        }
        : function() {
            return t
        }
        ;
        return n.d(e, "a", e),
        e
    }
    ,
    n.o = function(t, e) {
        return Object.prototype.hasOwnProperty.call(t, e)
    }
    ,
    n.p = "",
    n(n.s = 4)
}([function(t, e, n) {
    t.exports = function() {
        "use strict";
        /*!
   * mustache.js - Logic-less {{mustache}} templates with JavaScript
   * http://github.com/janl/mustache.js
   */
        var t = Object.prototype.toString
          , e = Array.isArray || function(e) {
            return "[object Array]" === t.call(e)
        }
        ;
        function n(t) {
            return "function" == typeof t
        }
        function o(t) {
            return t.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, "\\$&")
        }
        function r(t, e) {
            return null != t && "object" == typeof t && e in t
        }
        var i = RegExp.prototype.test
          , s = /\S/;
        function a(t) {
            return !function(t, e) {
                return i.call(t, e)
            }(s, t)
        }
        var c = {
            "&": "&amp;",
            "<": "&lt;",
            ">": "&gt;",
            '"': "&quot;",
            "'": "&#39;",
            "/": "&#x2F;",
            "`": "&#x60;",
            "=": "&#x3D;"
        }
          , l = /\s*/
          , u = /\s+/
          , p = /\s*=/
          , d = /\s*\}/
          , f = /#|\^|\/|>|\{|&|=|!/;
        function h(t) {
            this.string = t,
            this.tail = t,
            this.pos = 0
        }
        function m(t, e) {
            this.view = t,
            this.cache = {
                ".": this.view
            },
            this.parent = e
        }
        function v() {
            this.templateCache = {
                _cache: {},
                set: function(t, e) {
                    this._cache[t] = e
                },
                get: function(t) {
                    return this._cache[t]
                },
                clear: function() {
                    this._cache = {}
                }
            }
        }
        h.prototype.eos = function() {
            return "" === this.tail
        }
        ,
        h.prototype.scan = function(t) {
            var e = this.tail.match(t);
            if (!e || 0 !== e.index)
                return "";
            var n = e[0];
            return this.tail = this.tail.substring(n.length),
            this.pos += n.length,
            n
        }
        ,
        h.prototype.scanUntil = function(t) {
            var e, n = this.tail.search(t);
            switch (n) {
            case -1:
                e = this.tail,
                this.tail = "";
                break;
            case 0:
                e = "";
                break;
            default:
                e = this.tail.substring(0, n),
                this.tail = this.tail.substring(n)
            }
            return this.pos += e.length,
            e
        }
        ,
        m.prototype.push = function(t) {
            return new m(t,this)
        }
        ,
        m.prototype.lookup = function(t) {
            var e, o, i, s = this.cache;
            if (s.hasOwnProperty(t))
                e = s[t];
            else {
                for (var a, c, l, u = this, p = !1; u; ) {
                    if (t.indexOf(".") > 0)
                        for (a = u.view,
                        c = t.split("."),
                        l = 0; null != a && l < c.length; )
                            l === c.length - 1 && (p = r(a, c[l]) || (o = a,
                            i = c[l],
                            null != o && "object" != typeof o && o.hasOwnProperty && o.hasOwnProperty(i))),
                            a = a[c[l++]];
                    else
                        a = u.view[t],
                        p = r(u.view, t);
                    if (p) {
                        e = a;
                        break
                    }
                    u = u.parent
                }
                s[t] = e
            }
            return n(e) && (e = e.call(this.view)),
            e
        }
        ,
        v.prototype.clearCache = function() {
            void 0 !== this.templateCache && this.templateCache.clear()
        }
        ,
        v.prototype.parse = function(t, n) {
            var r = this.templateCache
              , i = t + ":" + (n || g.tags).join(":")
              , s = void 0 !== r
              , c = s ? r.get(i) : void 0;
            return null == c && (c = function(t, n) {
                if (!t)
                    return [];
                var r, i, s, c = !1, m = [], v = [], y = [], b = !1, x = !1, w = "", _ = 0;
                function C() {
                    if (b && !x)
                        for (; y.length; )
                            delete v[y.pop()];
                    else
                        y = [];
                    b = !1,
                    x = !1
                }
                function T(t) {
                    if ("string" == typeof t && (t = t.split(u, 2)),
                    !e(t) || 2 !== t.length)
                        throw new Error("Invalid tags: " + t);
                    r = new RegExp(o(t[0]) + "\\s*"),
                    i = new RegExp("\\s*" + o(t[1])),
                    s = new RegExp("\\s*" + o("}" + t[1]))
                }
                T(n || g.tags);
                for (var k, j, M, L, S, O, P = new h(t); !P.eos(); ) {
                    if (k = P.pos,
                    M = P.scanUntil(r))
                        for (var E = 0, F = M.length; E < F; ++E)
                            a(L = M.charAt(E)) ? (y.push(v.length),
                            w += L) : (x = !0,
                            c = !0,
                            w += " "),
                            v.push(["text", L, k, k + 1]),
                            k += 1,
                            "\n" === L && (C(),
                            w = "",
                            _ = 0,
                            c = !1);
                    if (!P.scan(r))
                        break;
                    if (b = !0,
                    j = P.scan(f) || "name",
                    P.scan(l),
                    "=" === j ? (M = P.scanUntil(p),
                    P.scan(p),
                    P.scanUntil(i)) : "{" === j ? (M = P.scanUntil(s),
                    P.scan(d),
                    P.scanUntil(i),
                    j = "&") : M = P.scanUntil(i),
                    !P.scan(i))
                        throw new Error("Unclosed tag at " + P.pos);
                    if (S = ">" == j ? [j, M, k, P.pos, w, _, c] : [j, M, k, P.pos],
                    _++,
                    v.push(S),
                    "#" === j || "^" === j)
                        m.push(S);
                    else if ("/" === j) {
                        if (!(O = m.pop()))
                            throw new Error('Unopened section "' + M + '" at ' + k);
                        if (O[1] !== M)
                            throw new Error('Unclosed section "' + O[1] + '" at ' + k)
                    } else
                        "name" === j || "{" === j || "&" === j ? x = !0 : "=" === j && T(M)
                }
                if (C(),
                O = m.pop())
                    throw new Error('Unclosed section "' + O[1] + '" at ' + P.pos);
                return function(t) {
                    for (var e, n = [], o = n, r = [], i = 0, s = t.length; i < s; ++i)
                        switch ((e = t[i])[0]) {
                        case "#":
                        case "^":
                            o.push(e),
                            r.push(e),
                            o = e[4] = [];
                            break;
                        case "/":
                            r.pop()[5] = e[2],
                            o = r.length > 0 ? r[r.length - 1][4] : n;
                            break;
                        default:
                            o.push(e)
                        }
                    return n
                }(function(t) {
                    for (var e, n, o = [], r = 0, i = t.length; r < i; ++r)
                        (e = t[r]) && ("text" === e[0] && n && "text" === n[0] ? (n[1] += e[1],
                        n[3] = e[3]) : (o.push(e),
                        n = e));
                    return o
                }(v))
            }(t, n),
            s && r.set(i, c)),
            c
        }
        ,
        v.prototype.render = function(t, e, n, o) {
            var r = this.parse(t, o)
              , i = e instanceof m ? e : new m(e,void 0);
            return this.renderTokens(r, i, n, t, o)
        }
        ,
        v.prototype.renderTokens = function(t, e, n, o, r) {
            for (var i, s, a, c = "", l = 0, u = t.length; l < u; ++l)
                a = void 0,
                "#" === (s = (i = t[l])[0]) ? a = this.renderSection(i, e, n, o) : "^" === s ? a = this.renderInverted(i, e, n, o) : ">" === s ? a = this.renderPartial(i, e, n, r) : "&" === s ? a = this.unescapedValue(i, e) : "name" === s ? a = this.escapedValue(i, e) : "text" === s && (a = this.rawValue(i)),
                void 0 !== a && (c += a);
            return c
        }
        ,
        v.prototype.renderSection = function(t, o, r, i) {
            var s = this
              , a = ""
              , c = o.lookup(t[1]);
            if (c) {
                if (e(c))
                    for (var l = 0, u = c.length; l < u; ++l)
                        a += this.renderTokens(t[4], o.push(c[l]), r, i);
                else if ("object" == typeof c || "string" == typeof c || "number" == typeof c)
                    a += this.renderTokens(t[4], o.push(c), r, i);
                else if (n(c)) {
                    if ("string" != typeof i)
                        throw new Error("Cannot use higher-order sections without the original template");
                    null != (c = c.call(o.view, i.slice(t[3], t[5]), (function(t) {
                        return s.render(t, o, r)
                    }
                    ))) && (a += c)
                } else
                    a += this.renderTokens(t[4], o, r, i);
                return a
            }
        }
        ,
        v.prototype.renderInverted = function(t, n, o, r) {
            var i = n.lookup(t[1]);
            if (!i || e(i) && 0 === i.length)
                return this.renderTokens(t[4], n, o, r)
        }
        ,
        v.prototype.indentPartial = function(t, e, n) {
            for (var o = e.replace(/[^ \t]/g, ""), r = t.split("\n"), i = 0; i < r.length; i++)
                r[i].length && (i > 0 || !n) && (r[i] = o + r[i]);
            return r.join("\n")
        }
        ,
        v.prototype.renderPartial = function(t, e, o, r) {
            if (o) {
                var i = n(o) ? o(t[1]) : o[t[1]];
                if (null != i) {
                    var s = t[6]
                      , a = t[5]
                      , c = t[4]
                      , l = i;
                    return 0 == a && c && (l = this.indentPartial(i, c, s)),
                    this.renderTokens(this.parse(l, r), e, o, l, r)
                }
            }
        }
        ,
        v.prototype.unescapedValue = function(t, e) {
            var n = e.lookup(t[1]);
            if (null != n)
                return n
        }
        ,
        v.prototype.escapedValue = function(t, e) {
            var n = e.lookup(t[1]);
            if (null != n)
                return g.escape(n)
        }
        ,
        v.prototype.rawValue = function(t) {
            return t[1]
        }
        ;
        var g = {
            name: "mustache.js",
            version: "4.0.1",
            tags: ["{{", "}}"],
            clearCache: void 0,
            escape: void 0,
            parse: void 0,
            render: void 0,
            Scanner: void 0,
            Context: void 0,
            Writer: void 0,
            set templateCache(t) {
                y.templateCache = t
            },
            get templateCache() {
                return y.templateCache
            }
        }
          , y = new v;
        return g.clearCache = function() {
            return y.clearCache()
        }
        ,
        g.parse = function(t, e) {
            return y.parse(t, e)
        }
        ,
        g.render = function(t, n, o, r) {
            if ("string" != typeof t)
                throw new TypeError('Invalid template! Template should be a "string" but "' + (e(i = t) ? "array" : typeof i) + '" was given as the first argument for mustache#render(template, view, partials)');
            var i;
            return y.render(t, n, o, r)
        }
        ,
        g.escape = function(t) {
            return String(t).replace(/[&<>"'`=\/]/g, (function(t) {
                return c[t]
            }
            ))
        }
        ,
        g.Scanner = h,
        g.Context = m,
        g.Writer = v,
        g
    }()
}
, function(t, e, n) {
    "use strict";
    n.d(e, "a", (function() {
        return o
    }
    ));
    const o = "MAPBOXER"
}
, function(t, e, n) {
    "use strict";
    var o = {};
    n.r(o),
    n.d(o, "TextControl", (function() {
        return i
    }
    )),
    n.d(o, "MousePositionControl", (function() {
        return s
    }
    )),
    n.d(o, "FilterControl", (function() {
        return a
    }
    ));
    var r = n(0);
    class i {
        constructor(t) {
            this._options = t || {}
        }
        onAdd(t) {
            return this._map = t,
            this._container = document.createElement("div"),
            this._container.classList.add("mapboxgl-ctrl", "mapboxer-text-ctrl"),
            this._container.style.cssText = this._options.cssText || "",
            this._container.innerHTML = this._options.text || "Hello mapboxer!",
            this._container
        }
        onRemove() {
            this._container.parentNode.removeChild(this._container),
            this._map = void 0
        }
    }
    class s {
        constructor(t) {
            this._options = t || {}
        }
        onAdd(t) {
            this._map = t,
            this._container = document.createElement("div"),
            this._container.classList.add("mapboxgl-ctrl", "mapboxer-mouse-position-ctrl"),
            this._container.style.cssText = this._options.cssText || "";
            const e = this._options.mustacheTemplate || "{{lng}}, {{lat}}";
            return this._map.on("mousemove", t=>{
                this._container.innerHTML = Object(r.render)(e, t.lngLat)
            }
            ),
            this._map.on("mouseout", t=>{
                this._container.innerHTML = "",
                this._container.style.display = "none"
            }
            ),
            this._map.on("mouseover", t=>{
                this._container.style.display = "block"
            }
            ),
            this._container
        }
        onRemove() {
            this._container.parentNode.removeChild(this._container),
            this._map = void 0
        }
    }
    class a {
        constructor(t) {
            this._options = t || {}
        }
        onAdd(t) {
            this._map = t,
            this._container = document.createElement("div"),
            this._container.classList.add("mapboxgl-ctrl", "mapboxer-filter-ctrl"),
            this._container.style.cssText = this._options.cssText || "";
            const e = document.createElement("textarea");
            return Object.assign(e, {
                spellcheck: !1,
                id: "filter"
            }, this._options.textareaAttributes),
            this._options.filter && (e.value = JSON.stringify(this._options.filter),
            t.setFilter(this._options.layerId, this._options.filter)),
            e.addEventListener("keyup", n=>{
                const o = e.value;
                try {
                    const e = JSON.parse(o);
                    t.setFilter(this._options.layerId, e)
                } catch (t) {}
            }
            ),
            this._container.append(e),
            this._container
        }
        onRemove() {
            this._container.parentNode.removeChild(this._container),
            this._map = void 0
        }
    }
    var c = n(1);
    e.a = {
        addControl: function(t) {
            const e = new mapboxgl[t.controlName](t.options);
            this.addControl(e, t.pos)
        },
        addSource: function(t) {
            this.addSource(t.id, t.source)
        },
        addLayer: function(t) {
            const e = this;
            t.style.source = t.style.source || c.a,
            e.addLayer(t.style);
            const n = t.style.id;
            e.on("mouseenter", n, ()=>e.getCanvas().style.cursor = "pointer"),
            e.on("mouseleave", n, ()=>e.getCanvas().style.cursor = ""),
            HTMLWidgets.shinyMode && e.on("click", t.style.id, t=>{
                const n = e.getContainer().id
                  , o = t.features[0]
                  , r = {
                    coords: t.lngLat,
                    props: o.properties,
                    layer_id: o.layer.id,
                    widget_id: n
                };
                console.log(r),
                Shiny.onInputChange(n + "_onclick", r)
            }
            )
        },
        addPopups: function(t) {
            const e = this
              , n = t.layerId;
            e.on("click", n, n=>{
                const o = Object.values(n.lngLat)
                  , i = n.features[0]
                  , s = Object(r.render)(t.popup, i.properties);
                (new mapboxgl.Popup).setLngLat(o).setHTML(s).addTo(e)
            }
            )
        },
        addTooltips: function(t) {
            const e = this
              , n = t.layerId
              , o = new mapboxgl.Popup({
                closeButton: !1,
                closeOnClick: !1
            });
            e.on("mousemove", n, n=>{
                const i = Object.values(n.lngLat)
                  , s = n.features[0]
                  , a = Object(r.render)(t.tooltip, s.properties);
                o.setLngLat(i).setHTML(a).addTo(e)
            }
            ),
            e.on("mouseleave", n, ()=>{
                o.remove()
            }
            )
        },
        addMarker: function(t) {
            const e = (new mapboxgl.Marker).setLngLat([t.lng, t.lat]);
            e.getElement().style.cursor = "pointer",
            t.popup && e.setPopup((new mapboxgl.Popup).setHTML(t.popup)),
            e.addTo(this)
        },
        addCustomControl: function(t) {
            const e = new o[t.controlName](t.options);
            this.addControl(e, t.pos)
        },
        customControls: o,
        setFilter: function(t) {
            this.setFilter(t.layerId, t.filter)
        },
        setPaintProperty: function(t) {
            this.setPaintProperty(t.layer, t.property, t.value)
        },
        setLayoutProperty: function(t) {
            this.setLayoutProperty(t.layer, t.property, t.value)
        },
        setData: function(t) {
            const e = t.source || c.a;
            this.getSource(e).setData(t.data)
        },
        fitBounds: function(t) {
            this.fitBounds(t.bounds, t.options || {})
        },
        setStyle: function(t) {
            this.setStyle(t.style)
        },
        addDrawControl: function(t) {
            const e = new MapboxDraw(t.options);
            this.addControl(e, t.pos),
            t.data && e.add(t.data),
            this.on("draw.create", t=>{
                console.log(t.features),
                console.log('hi it me'),
                console.log(e.getAll())
                dawg=e.getAll()
            }
            )
        }
    }
}
, function(t, e, n) {
    "use strict";
    (function(t) {
        n(6);
        var o = n(2)
          , r = n(1);
        const i = t.mapboxer = {
            methods: o.a,
            _widget: {}
        };
        e.a = function(t, e, n) {
            let s = null;
            const a = i._widget[t.id] = {};
            return HTMLWidgets.shinyMode && Shiny.addCustomMessageHandler("mapboxer", e=>{
                console.log("proxyObject", e),
                e.id === t.id && (console.log("Updating " + t.id),
                e.widgetData.calls.forEach(({methodName: t, args: e})=>o.a[t].call(s, e)))
            }
            ),
            {
                renderValue: function(e) {
                    console.log("mapboxgl", mapboxgl.version),
                    console.log("widgetData", e);
                    const n = e.mapProps.style;
                    "object" == typeof n && (n.sources = n.sources || {},
                    Object.values(n.sources).forEach(t=>{
                        t.hasOwnProperty("tiles") & !Array.isArray(t.tiles) && (t.tiles = Array(t.tiles))
                    }
                    )),
                    mapboxgl.accessToken = e.accessToken || null,
                    e.mapProps.container = t.id,
                    s = a.map = new mapboxgl.Map(e.mapProps),
                    s.on("error", t=>{
                        throw t.error
                    }
                    ),
                    e.source && s.on("load", ()=>o.a.addSource.call(s, {
                        id: r.a,
                        source: e.source
                    })),
                    s.on("load", ()=>e.calls.forEach(({methodName: t, args: e})=>{
                        o.a[t].call(s, e)
                    }
                    ))
                },
                resize: function(t, e) {}
            }
        }
    }
    ).call(this, n(5))
}
, function(t, e, n) {
    "use strict";
    n.r(e);
    var o = n(3);
    HTMLWidgets.widget({
        name: "mapboxer",
        type: "output",
        factory: o.a
    })
}
, function(t, e) {
    var n;
    n = function() {
        return this
    }();
    try {
        n = n || new Function("return this")()
    } catch (t) {
        "object" == typeof window && (n = window)
    }
    t.exports = n
}
, function(t, e, n) {
    var o = n(7)
      , r = n(8);
    "string" == typeof (r = r.__esModule ? r.default : r) && (r = [[t.i, r, ""]]);
    var i = {
        insert: "head",
        singleton: !1
    };
    o(r, i);
    t.exports = r.locals || {}
}
, function(t, e, n) {
    "use strict";
    var o, r = function() {
        return void 0 === o && (o = Boolean(window && document && document.all && !window.atob)),
        o
    }, i = function() {
        var t = {};
        return function(e) {
            if (void 0 === t[e]) {
                var n = document.querySelector(e);
                if (window.HTMLIFrameElement && n instanceof window.HTMLIFrameElement)
                    try {
                        n = n.contentDocument.head
                    } catch (t) {
                        n = null
                    }
                t[e] = n
            }
            return t[e]
        }
    }(), s = [];
    function a(t) {
        for (var e = -1, n = 0; n < s.length; n++)
            if (s[n].identifier === t) {
                e = n;
                break
            }
        return e
    }
    function c(t, e) {
        for (var n = {}, o = [], r = 0; r < t.length; r++) {
            var i = t[r]
              , c = e.base ? i[0] + e.base : i[0]
              , l = n[c] || 0
              , u = "".concat(c, " ").concat(l);
            n[c] = l + 1;
            var p = a(u)
              , d = {
                css: i[1],
                media: i[2],
                sourceMap: i[3]
            };
            -1 !== p ? (s[p].references++,
            s[p].updater(d)) : s.push({
                identifier: u,
                updater: v(d, e),
                references: 1
            }),
            o.push(u)
        }
        return o
    }
    function l(t) {
        var e = document.createElement("style")
          , o = t.attributes || {};
        if (void 0 === o.nonce) {
            var r = n.nc;
            r && (o.nonce = r)
        }
        if (Object.keys(o).forEach((function(t) {
            e.setAttribute(t, o[t])
        }
        )),
        "function" == typeof t.insert)
            t.insert(e);
        else {
            var s = i(t.insert || "head");
            if (!s)
                throw new Error("Couldn't find a style target. This probably means that the value for the 'insert' parameter is invalid.");
            s.appendChild(e)
        }
        return e
    }
    var u, p = (u = [],
    function(t, e) {
        return u[t] = e,
        u.filter(Boolean).join("\n")
    }
    );
    function d(t, e, n, o) {
        var r = n ? "" : o.media ? "@media ".concat(o.media, " {").concat(o.css, "}") : o.css;
        if (t.styleSheet)
            t.styleSheet.cssText = p(e, r);
        else {
            var i = document.createTextNode(r)
              , s = t.childNodes;
            s[e] && t.removeChild(s[e]),
            s.length ? t.insertBefore(i, s[e]) : t.appendChild(i)
        }
    }
    function f(t, e, n) {
        var o = n.css
          , r = n.media
          , i = n.sourceMap;
        if (r ? t.setAttribute("media", r) : t.removeAttribute("media"),
        i && btoa && (o += "\n/*# sourceMappingURL=data:application/json;base64,".concat(btoa(unescape(encodeURIComponent(JSON.stringify(i)))), " */")),
        t.styleSheet)
            t.styleSheet.cssText = o;
        else {
            for (; t.firstChild; )
                t.removeChild(t.firstChild);
            t.appendChild(document.createTextNode(o))
        }
    }
    var h = null
      , m = 0;
    function v(t, e) {
        var n, o, r;
        if (e.singleton) {
            var i = m++;
            n = h || (h = l(e)),
            o = d.bind(null, n, i, !1),
            r = d.bind(null, n, i, !0)
        } else
            n = l(e),
            o = f.bind(null, n, e),
            r = function() {
                !function(t) {
                    if (null === t.parentNode)
                        return !1;
                    t.parentNode.removeChild(t)
                }(n)
            }
            ;
        return o(t),
        function(e) {
            if (e) {
                if (e.css === t.css && e.media === t.media && e.sourceMap === t.sourceMap)
                    return;
                o(t = e)
            } else
                r()
        }
    }
    t.exports = function(t, e) {
        (e = e || {}).singleton || "boolean" == typeof e.singleton || (e.singleton = r());
        var n = c(t = t || [], e);
        return function(t) {
            if (t = t || [],
            "[object Array]" === Object.prototype.toString.call(t)) {
                for (var o = 0; o < n.length; o++) {
                    var r = a(n[o]);
                    s[r].references--
                }
                for (var i = c(t, e), l = 0; l < n.length; l++) {
                    var u = a(n[l]);
                    0 === s[u].references && (s[u].updater(),
                    s.splice(u, 1))
                }
                n = i
            }
        }
    }
}
, function(t, e, n) {
    (e = n(9)(!1)).push([t.i, ".mapboxer-text-ctrl, .mapboxer-mouse-position-ctrl /*, .mapboxer-filter-ctrl */ {\n  background: #FFFFFF;\n  border-radius: 4px;\n  padding: 8px;\n}\n\n.mapboxer-mouse-position-ctrl {\n  width: 300px;\n  text-align: center;\n  display: none;\n}\n\n.mapboxer-mouse-position-ctrl:empty {\n  display: none;\n}\n\n.mapboxer-filter-ctrl-unused {\n  /* background: #323232; */\n  background: none;\n  color: #FFFFFF;\n  /* display: flex; */\n  padding: 2px;\n}\n\n/* input[type='text'] */\n.mapboxer-filter-ctrl textarea {\n  /* font: 10pt \"mono\", sans-serif; */\n  font: 16px/normal 'Monaco', 'Menlo', 'Ubuntu Mono', 'Consolas', 'source-code-pro', monospace;\n  padding: 5px;\n  background: rgba(0, 0, 0, 0.7);\n  color: #FFFFFF;\n  /* border: 1px solid #46514E; */\n  resize: none;\n  vertical-align: middle;\n}\n", ""]),
    t.exports = e
}
, function(t, e, n) {
    "use strict";
    t.exports = function(t) {
        var e = [];
        return e.toString = function() {
            return this.map((function(e) {
                var n = function(t, e) {
                    var n = t[1] || ""
                      , o = t[3];
                    if (!o)
                        return n;
                    if (e && "function" == typeof btoa) {
                        var r = (s = o,
                        a = btoa(unescape(encodeURIComponent(JSON.stringify(s)))),
                        c = "sourceMappingURL=data:application/json;charset=utf-8;base64,".concat(a),
                        "/*# ".concat(c, " */"))
                          , i = o.sources.map((function(t) {
                            return "/*# sourceURL=".concat(o.sourceRoot || "").concat(t, " */")
                        }
                        ));
                        return [n].concat(i).concat([r]).join("\n")
                    }
                    var s, a, c;
                    return [n].join("\n")
                }(e, t);
                return e[2] ? "@media ".concat(e[2], " {").concat(n, "}") : n
            }
            )).join("")
        }
        ,
        e.i = function(t, n, o) {
            "string" == typeof t && (t = [[null, t, ""]]);
            var r = {};
            if (o)
                for (var i = 0; i < this.length; i++) {
                    var s = this[i][0];
                    null != s && (r[s] = !0)
                }
            for (var a = 0; a < t.length; a++) {
                var c = [].concat(t[a]);
                o && r[c[0]] || (n && (c[2] ? c[2] = "".concat(n, " and ").concat(c[2]) : c[2] = n),
                e.push(c))
            }
        }
        ,
        e
    }
}
]);
