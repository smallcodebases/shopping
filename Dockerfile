# Build and minify JS and CSS
FROM --platform=$BUILDPLATFORM node:alpine AS elm-build
WORKDIR /stuff
RUN npm install -g elm@0.19.1 terser lightningcss-cli
COPY elm.json Main.elm index.js main.css ./
RUN elm make Main.elm --optimize --output elm.js \
  && terser elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe,passes=2' --mangle --output main.js \
  && terser index.js --compress --mangle --output index.min.js \
  && lightningcss --minify main.css --output-file main.min.css

# Hash static assets and patch source files
FROM --platform=$BUILDPLATFORM alpine AS hash-and-patch
WORKDIR /stuff
COPY --from=elm-build /stuff/index.min.js ./index.js
COPY --from=elm-build /stuff/main.js ./main.js
COPY --from=elm-build /stuff/main.min.css ./main.css
COPY favicon.svg index.html main.go manifest.webmanifest ./
RUN INDEX_JS_HASH=$(sha256sum index.js | cut -c1-64) \
  && FAVICON_SVG_HASH=$(sha256sum favicon.svg | cut -c1-64) \
  && MAIN_CSS_HASH=$(sha256sum main.css | cut -c1-64) \
  && MAIN_JS_HASH=$(sha256sum main.js | cut -c1-64) \
  && sed -i -e "s|favicon\.svg|${FAVICON_SVG_HASH}|" manifest.webmanifest \
  && MANIFEST_WEBMANIFEST_HASH=$(sha256sum manifest.webmanifest | cut -c1-64) \
  && sed -i \
    -e "s|index\.js|${INDEX_JS_HASH}|" \
    -e "s|favicon\.svg|${FAVICON_SVG_HASH}|" \
    -e "s|main\.css|${MAIN_CSS_HASH}|" \
    -e "s|main\.js|${MAIN_JS_HASH}|" \
    -e "s|manifest\.webmanifest|${MANIFEST_WEBMANIFEST_HASH}|" \
    index.html \
  && sed -i \
    -e "s|serveStaticFile(mux,|serveHashedStaticFile(mux,|" \
    -e "s|index\.js|${INDEX_JS_HASH}|g" \
    -e "s|favicon\.svg|${FAVICON_SVG_HASH}|g" \
    -e "s|main\.css|${MAIN_CSS_HASH}|g" \
    -e "s|main\.js|${MAIN_JS_HASH}|g" \
    -e "s|manifest\.webmanifest|${MANIFEST_WEBMANIFEST_HASH}|g" \
    main.go \
  && mkdir static \
  && mv favicon.svg static/${FAVICON_SVG_HASH} \
  && mv index.html static/index.html \
  && mv index.js static/${INDEX_JS_HASH} \
  && mv main.css static/${MAIN_CSS_HASH} \
  && mv main.js static/${MAIN_JS_HASH} \
  && mv manifest.webmanifest static/${MANIFEST_WEBMANIFEST_HASH}

# Build the Go binary
FROM --platform=$BUILDPLATFORM golang:1.26.0-alpine AS go-build
WORKDIR /stuff
ARG TARGETARCH TARGETOS
COPY go.mod go.sum ./
RUN go mod download
COPY --from=hash-and-patch /stuff/main.go main.go
COPY migrations/ migrations/
RUN --mount=type=cache,target=/root/.cache/go-build CGO_ENABLED=0 GOOS=$TARGETOS GOARCH=$TARGETARCH go build -ldflags="-s -w" -trimpath -o shopping .
RUN mkdir -p /var/lib/shopping

# Final image: just the binary and static files
FROM scratch
WORKDIR /shopping
COPY --from=go-build /var/lib/shopping /var/lib/shopping
COPY --from=go-build /stuff/shopping /bin/shopping
COPY --from=hash-and-patch /stuff/static ./
EXPOSE 80
ENTRYPOINT ["/bin/shopping"]
